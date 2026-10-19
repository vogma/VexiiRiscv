package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

object VectorAluOp {
  val ADD = 0
  val SUB = 1
  val RSUB = 2
  val AND = 3
  val OR = 4
  val XOR = 5
  val MOVE = 6
}

// Step function of a command: which word of each operand a step reads and which destination word it writes
object VectorShape {
  val SINGLE = 0       // vd, vs2 and vs1 at SEW, step k on word k of each group
  val SCALAR_READ = 1  // element 0 of vs2 to the scalar result, also when vl is zero
  val SCALAR_WRITE = 2 // the scalar to element 0 of vd
  def width = 4
}

case class VectorArithmeticCmd(p: VectorRegFileParam, xlen: Int) extends Bundle {
  val vd, vs1, vs2 = UInt(5 bits)
  val vl = UInt(log2Up(p.vlen) + 1 bits)
  val vstart = UInt(log2Up(p.vlen) bits)
  val sew = UInt(2 bits)
  val shape = UInt(VectorShape.width bits)
  val op = UInt(3 bits)
  val scalar = Bits(xlen bits)
  val scalarOperand, unmasked = Bool()
  val signed, saturate = Bool()
  val vxrm = Bits(2 bits)
}

// One step per cycle: issue synchronous reads, compute in X1 and X2, then write the destination word with byte enables.
class VectorArithmetic(val p: VectorRegFileParam, val xlen: Int) extends Component {
  require(p.dlen == 64 && p.readPorts >= 3)
  require(xlen == 32 || xlen == 64)
  val io = new Bundle {
    val cmd = slave(Flow(VectorArithmeticCmd(p, xlen)))
    val reads = Vec.fill(3)(master(VrfReadPort(p)))
    val write = master(Flow(VrfWriteCmd(p)))
    val v0 = in Bits(p.vlen bits)
    val busy, done = out Bool()
    val saturated = out Bool() // an active element saturated; holds from done until the next command
    val result = out Bits(xlen bits)
  }

  val command = Reg(VectorArithmeticCmd(p, xlen))
  val scalarRead = command.shape === VectorShape.SCALAR_READ
  val scalarWrite = command.shape === VectorShape.SCALAR_WRITE
  val subtract = command.op === VectorAluOp.SUB || command.op === VectorAluOp.RSUB
  val running = RegInit(False)
  val busy = RegInit(False)
  val saturated = RegInit(False)
  val step = Reg(UInt(log2Up(p.beatsPerReg * 8) bits)) init(0)
  val result = Reg(Bits(xlen bits))
  io.result := result
  io.busy := busy
  io.saturated := saturated

  def word(reg: UInt, index: UInt): UInt = ((reg.resize(p.addressWidth) << p.beatWidth) + index).resize(p.addressWidth)
  // Most significant byte of the element holding byte j
  def topOf(j: Int): UInt = command.sew.mux(U(0) -> U(j, 3 bits), U(1) -> U(j | 1, 3 bits), default -> U(j | 3, 3 bits))

  when(io.cmd.valid) {
    command := io.cmd.payload
    running := True
    busy := True
    saturated := False
    step := 0
  }

  val issue = new Area {
    val enables = Bits(8 bits)
    for (j <- 0 until 8) {
      val element = (((step.resize(log2Up(p.vlen) + 3) << 3) + j) >> command.sew).resize(command.vl.getWidth)
      enables(j) := element >= command.vstart.resize(element.getWidth) && element < command.vl &&
        (command.unmasked || io.v0(element.resize(log2Up(p.vlen)))) &&
        (!scalarWrite || element === 0)
    }
    val nextElement = (((step.resize(log2Up(p.vlen) + 3) + 1) << 3) >> command.sew).resize(command.vl.getWidth)
    val active = running && (enables.orR || scalarRead)
    val last = running && (scalarRead || scalarWrite || nextElement >= command.vl)

    // Ports A, B and C; scalar reads always use vs2, including when vl is zero
    io.reads(0).cmd.valid := active && (scalarRead || !scalarWrite && command.op =/= VectorAluOp.MOVE)
    io.reads(0).cmd.payload := word(command.vs2, step)
    io.reads(1).cmd.valid := active && !command.scalarOperand && !scalarRead
    io.reads(1).cmd.payload := word(command.vs1, step)
    io.reads(2).cmd.valid := False
    io.reads(2).cmd.payload := word(command.vd, step)

    when(running) {
      step := step + 1
      when(last) { running := False }
    }
  }

  // Adder, logic and overflow flags on the read data
  val x1 = new Area {
    val valid = RegNext(issue.active) init(False)
    val last = RegNext(issue.last) init(False)
    val address = RegNext(word(command.vd, step))
    val mask = RegNext(issue.enables)

    val scalarBeat = command.sew.mux(
      U(0) -> (command.scalar(7 downto 0) #* 8),
      U(1) -> (command.scalar(15 downto 0) #* 4),
      default -> (command.scalar(31 downto 0) #* 2)
    )
    val a = io.reads(0).rsp
    val b = command.scalarOperand ? scalarBeat | io.reads(1).rsp
    val left = (command.op === VectorAluOp.RSUB) ? b | a
    val right = (command.op === VectorAluOp.RSUB) ? a | b
    val sum = Bits(64 bits)
    val carries = Vec(Bool(), 9)
    // Per byte, meaningful at the most significant byte of each element: the result does not fit SEW bits
    val overflow = Bits(8 bits)
    carries(0) := subtract
    for (j <- 0 until 8) {
      val boundary = if (j % 4 == 0) True else if (j % 2 == 0) command.sew =/= 2 else command.sew === 0
      val carry = boundary ? subtract | carries(j)
      val rhs = right(j * 8, 8 bits) ^ B(8 bits, default -> subtract)
      val value = left(j * 8, 8 bits).asUInt.resize(9) + rhs.asUInt.resize(9) + carry.asUInt
      sum(j * 8, 8 bits) := value(7 downto 0).asBits
      carries(j + 1) := value.msb
      val leftSign = left(j * 8 + 7)
      overflow(j) := command.signed ? (leftSign === rhs.msb && value(7) =/= leftSign) | (value.msb ^ subtract)
    }
    val data = command.op.mux(
      U(VectorAluOp.AND) -> (a & b),
      U(VectorAluOp.OR) -> (a | b),
      U(VectorAluOp.XOR) -> (a ^ b),
      U(VectorAluOp.MOVE) -> b,
      default -> sum
    )
    val negative = Bits(8 bits)
    for (j <- 0 until 8) negative(j) := a(j * 8 + 7)

    when(valid && scalarRead) {
      result := command.sew.mux(
        U(0) -> a(7 downto 0).asSInt.resize(xlen).asBits,
        U(1) -> a(15 downto 0).asSInt.resize(xlen).asBits,
        default -> a(31 downto 0).asSInt.resize(xlen).asBits
      )
    }
  }

  // Saturation of whole elements (norm:vsaddu_vsadd_vssubu_vssub_op_overflow_vxsat_op_vsaddsub)
  val x2 = new Area {
    val valid = RegNext(x1.valid) init(False)
    val last = RegNext(x1.last) init(False)
    val address = RegNext(x1.address)
    val mask = RegNext(x1.mask)
    val data = RegNext(x1.data)
    val overflow = RegNext(x1.overflow)
    val negative = RegNext(x1.negative)

    val clipped = Bits(8 bits)
    val value = Bits(64 bits)
    for (j <- 0 until 8) {
      val top = topOf(j)
      val upper = top === U(j, 3 bits)
      val signedLimit = negative(top) ? (upper ? B"x80" | B"x00") | (upper ? B"x7f" | B"xff")
      clipped(j) := command.saturate && overflow(top)
      value(j * 8, 8 bits) := clipped(j) ? (command.signed ? signedLimit | (subtract ? B"x00" | B"xff")) | data(j * 8, 8 bits)
    }
    // Only active elements set vxsat; their bytes are the enabled ones
    when(valid && (clipped & mask).orR) { saturated := True }
  }

  val writeback = new Area {
    val valid = RegNext(x2.valid && !scalarRead) init(False)
    val last = RegNext(x2.last) init(False)
    val address = RegNext(x2.address)
    val mask = RegNext(x2.mask)
    val data = RegNext(x2.value)
    io.write.valid := valid
    io.write.address := address
    io.write.mask := mask
    io.write.data := data
  }

  // The VRF holds every written word when done rises
  io.done := RegNext(writeback.last) init(False)
  when(io.done) { busy := False }
}
