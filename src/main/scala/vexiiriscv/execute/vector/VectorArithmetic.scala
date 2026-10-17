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

case class VectorArithmeticCmd(p: VectorRegFileParam, xlen: Int) extends Bundle {
  val vd, vs1, vs2 = UInt(5 bits)
  val vl = UInt(log2Up(p.vlen) + 1 bits)
  val vstart = UInt(log2Up(p.vlen) bits)
  val sew = UInt(2 bits)
  val op = UInt(3 bits)
  val scalar = Bits(xlen bits)
  val scalarOperand, unmasked, scalarRead, scalarWrite = Bool()
}

// Issue synchronous source reads, then write the preceding beat using byte enables.
class VectorArithmetic(val p: VectorRegFileParam, val xlen: Int) extends Component {
  require(p.dlen == 64 && p.readPorts >= 2)
  require(xlen == 32 || xlen == 64)
  val io = new Bundle {
    val cmd = slave(Flow(VectorArithmeticCmd(p, xlen)))
    val reads = Vec.fill(2)(master(VrfReadPort(p)))
    val write = master(Flow(VrfWriteCmd(p)))
    val v0 = in Bits(p.vlen bits)
    val busy, done = out Bool()
    val result = out Bits(xlen bits)
  }

  val command = Reg(VectorArithmeticCmd(p, xlen))
  val running, draining = RegInit(False)
  val beat = Reg(UInt(log2Up(p.beatsPerReg * 8) bits)) init(0)
  val pending = RegInit(False)
  val writeAddress = Reg(UInt(p.addressWidth bits))
  val writeMask = Reg(Bits(8 bits))
  val result = Reg(Bits(xlen bits))
  io.result := result
  io.busy := running || draining
  io.done := RegNext(draining) init(False)

  when(io.cmd.valid) {
    command := io.cmd.payload
    running := True
    beat := 0
  }

  val enables = Bits(8 bits)
  for (j <- 0 until 8) {
    val element = (((beat.resize(log2Up(p.vlen) + 3) << 3) + j) >> command.sew).resize(command.vl.getWidth)
    enables(j) := element >= command.vstart.resize(element.getWidth) && element < command.vl &&
      (command.unmasked || io.v0(element.resize(log2Up(p.vlen)))) &&
      (!command.scalarWrite || element === 0)
  }
  val issue = running && (enables.orR || command.scalarRead)
  for (((port, reg), index) <- io.reads.zip(Seq(command.vs2, command.vs1)).zipWithIndex) {
    port.cmd.valid := issue && (if (index == 0) !command.scalarWrite && command.op =/= VectorAluOp.MOVE else !command.scalarOperand && !command.scalarRead)
    port.cmd.payload := ((reg.resize(p.addressWidth) << p.beatWidth) + beat).resized
  }
  // Scalar reads always use vs2, including when vl is zero.
  when(command.scalarRead) { io.reads(0).cmd.valid := issue }

  pending := issue
  when(running) {
    beat := beat + 1
    writeAddress := ((command.vd.resize(p.addressWidth) << p.beatWidth) + beat).resized
    writeMask := enables
    val nextElement = (((beat.resize(log2Up(p.vlen) + 3) + 1) << 3) >> command.sew).resize(command.vl.getWidth)
    when(command.scalarRead || command.scalarWrite || nextElement >= command.vl) {
      running := False
      draining := True
    }
  }
  when(draining) { draining := False }

  val scalarBeat = Bits(64 bits)
  scalarBeat := command.sew.mux(
    U(0) -> (command.scalar(7 downto 0) #* 8),
    U(1) -> (command.scalar(15 downto 0) #* 4),
    default -> (command.scalar(31 downto 0) #* 2)
  )
  val a = io.reads(0).rsp
  val b = command.scalarOperand ? scalarBeat | io.reads(1).rsp
  val left = (command.op === VectorAluOp.RSUB) ? b | a
  val right = (command.op === VectorAluOp.RSUB) ? a | b
  val subtract = command.op === VectorAluOp.SUB || command.op === VectorAluOp.RSUB
  val sum = Bits(64 bits)
  val carries = Vec(Bool(), 9)
  carries(0) := subtract
  for (j <- 0 until 8) {
    val boundary = if (j % 4 == 0) True else if (j % 2 == 0) command.sew =/= 2 else command.sew === 0
    val carry = boundary ? subtract | carries(j)
    val rhs = right(j * 8, 8 bits) ^ B(8 bits, default -> subtract)
    val value = left(j * 8, 8 bits).asUInt.resize(9) + rhs.asUInt.resize(9) + carry.asUInt
    sum(j * 8, 8 bits) := value(7 downto 0).asBits
    carries(j + 1) := value.msb
  }
  val data = command.op.mux(
    U(VectorAluOp.AND) -> (a & b),
    U(VectorAluOp.OR) -> (a | b),
    U(VectorAluOp.XOR) -> (a ^ b),
    U(VectorAluOp.MOVE) -> b,
    default -> sum
  )
  io.write.valid := pending && !command.scalarRead
  io.write.address := writeAddress
  io.write.mask := writeMask
  io.write.data := data
  when(pending && command.scalarRead) {
    result := command.sew.mux(
      U(0) -> a(7 downto 0).asSInt.resize(xlen).asBits,
      U(1) -> a(15 downto 0).asSInt.resize(xlen).asBits,
      default -> a(31 downto 0).asSInt.resize(xlen).asBits
    )
  }
}
