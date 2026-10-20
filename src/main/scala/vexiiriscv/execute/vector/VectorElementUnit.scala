package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

object VectorElementOp {
  val MUL = 0   // low SEW bits of vs2 * vs1
  val MULH = 1  // high SEW bits of vs2 * vs1
  val WMUL = 2  // 2*SEW-bit vs2 * vs1
  val MACC = 3  // vd +/- vs1 * vs2, low SEW bits
  val MADD = 4  // vs2 +/- vs1 * vd, low SEW bits
  val WMACC = 5 // 2*SEW-bit vd + vs1 * vs2
  val SMUL = 6  // vs2 * vs1 >> (SEW - 1), rounded by vxrm and saturated
  def width = 3
}

case class VectorElementCmd(p: VectorRegFileParam, xlen: Int) extends Bundle {
  val vd, vs1, vs2 = UInt(5 bits)
  val vl = UInt(log2Up(p.vlen) + 1 bits)
  val sew = UInt(2 bits)
  val op = UInt(VectorElementOp.width bits)
  val scalar = Bits(xlen bits)
  val scalarOperand, unmasked = Bool()
  val signed = Bool()    // vs2, or vd as the MADD multiplicand
  val signedVs1 = Bool() // vs1 or the scalar
  val negate = Bool()    // MACC and MADD subtract the product
  val vxrm = Bits(2 bits)
}

// One element per cycle from element 0: read in cycle t, extract and extend in t+1, multiply in t+2 and t+3,
// finish in t+4 and write the element with its byte enables in t+5. Inactive elements take a cycle and no reads.
class VectorElementUnit(val p: VectorRegFileParam, val xlen: Int) extends Component {
  require(p.dlen == 64 && p.readPorts >= 3)
  require(xlen == 32 || xlen == 64)
  val io = new Bundle {
    val cmd = slave(Flow(VectorElementCmd(p, xlen)))
    val reads = Vec.fill(3)(master(VrfReadPort(p)))
    val write = master(Flow(VrfWriteCmd(p)))
    val v0 = in Bits(p.vlen bits)
    val busy, done = out Bool()
    val saturated = out Bool() // an active element saturated; holds from done until the next command
  }

  val command = Reg(VectorElementCmd(p, xlen))
  def is(op: Int): Bool = command.op === op
  val widen = is(VectorElementOp.WMUL) || is(VectorElementOp.WMACC)
  val madd = is(VectorElementOp.MADD)
  val readVd = is(VectorElementOp.MACC) || madd || is(VectorElementOp.WMACC)
  val running = RegInit(False)
  val busy = RegInit(False)
  val saturated = RegInit(False)
  val index = Reg(UInt(log2Up(p.vlen) bits)) init(0)
  io.busy := busy
  io.saturated := saturated

  // Word of a register group holding its byte offset
  val offsetWidth = log2Up(p.vlen) + 1
  def word(reg: UInt, offset: UInt): UInt =
    ((reg.resize(p.addressWidth) << p.beatWidth) + (offset >> log2Up(p.beatBytes)).resize(p.addressWidth)).resize(p.addressWidth)

  when(io.cmd.valid) {
    command := io.cmd.payload
    running := True
    busy := True
    saturated := False
    index := 0
  }

  val issue = new Area {
    val active = running && command.vl =/= 0 && (command.unmasked || io.v0(index))
    val last = running && (command.vl === 0 || index.resize(offsetWidth) + 1 >= command.vl)
    val offset = (index.resize(offsetWidth) << command.sew).resize(offsetWidth)
    val vdOffset = widen ? (offset << 1).resize(offsetWidth) | offset

    io.reads(0).cmd.valid := active
    io.reads(0).cmd.payload := word(command.vs2, offset)
    io.reads(1).cmd.valid := active && !command.scalarOperand
    io.reads(1).cmd.payload := word(command.vs1, offset)
    io.reads(2).cmd.valid := active && readVd
    io.reads(2).cmd.payload := word(command.vd, vdOffset)

    when(running) {
      index := index + 1
      when(last) { running := False }
    }
  }

  // Element extraction from the read words, extended to 33 bits
  val x1 = new Area {
    val valid = RegNext(issue.active) init(False)
    val last = RegNext(issue.last) init(False)
    val byte = RegNext(issue.offset.resize(log2Up(p.beatBytes)))
    val vdByte = RegNext(issue.vdOffset.resize(log2Up(p.beatBytes)))
    val address = RegNext(word(command.vd, issue.vdOffset))

    def extend(v: Bits, size: UInt, signed: Bool): SInt = size.mux(
      U(0) -> (signed ? v(7 downto 0).asSInt.resize(33) | v(7 downto 0).asUInt.resize(33).asSInt),
      U(1) -> (signed ? v(15 downto 0).asSInt.resize(33) | v(15 downto 0).asUInt.resize(33).asSInt),
      default -> (signed ? v(31 downto 0).asSInt.resize(33) | v(31 downto 0).asUInt.resize(33).asSInt)
    )
    def element(data: Bits, at: UInt): Bits = (data >> (at << 3)).resize(32)
    val a = element(io.reads(0).rsp, byte)
    val b = command.scalarOperand ? command.scalar.resize(32) | element(io.reads(1).rsp, byte)
    val c = element(io.reads(2).rsp, vdByte)
    // vmadd/vnmsub multiply vd and add vs2; the others multiply vs2 and add vd
    val x = extend(madd ? c | a, command.sew, command.signed)
    val y = extend(b, command.sew, command.signedVs1)
    val addend = madd ? a | c
    val intMin = command.sew.mux(U(0) -> S(-128, 33 bits), U(1) -> S(-32768, 33 bits), default -> S(BigInt(-1) << 31, 33 bits))
    val overflow = x === intMin && y === intMin // the only vsmul product that does not fit (norm:vsmul_op)
  }

  // 33 x 33 signed multiplier with two pipeline registers
  val operands = new Area {
    val valid = RegNext(x1.valid) init(False)
    val last = RegNext(x1.last) init(False)
    val vdByte = RegNext(x1.vdByte)
    val address = RegNext(x1.address)
    val x = RegNext(x1.x)
    val y = RegNext(x1.y)
    val addend = RegNext(x1.addend)
    val overflow = RegNext(x1.overflow)
  }
  val m1 = new Area {
    val valid = RegNext(operands.valid) init(False)
    val last = RegNext(operands.last) init(False)
    val vdByte = RegNext(operands.vdByte)
    val address = RegNext(operands.address)
    val product = RegNext(operands.x * operands.y)
    val addend = RegNext(operands.addend)
    val overflow = RegNext(operands.overflow)
  }
  val m2 = new Area {
    val valid = RegNext(m1.valid) init(False)
    val last = RegNext(m1.last) init(False)
    val vdByte = RegNext(m1.vdByte)
    val address = RegNext(m1.address)
    val product = RegNext(m1.product)
    val addend = RegNext(m1.addend)
    val overflow = RegNext(m1.overflow)
  }

  // Half selection, accumulation, vsmul rounding (norm:vxrm_op) and saturation (norm:vxsat_op_vsmul)
  val finish = new Area {
    val product = m2.product.asBits
    val low = product(31 downto 0)
    val term = command.negate ? (~low.asUInt + 1) | low.asUInt
    val sum = (m2.addend.asUInt + term).asBits
    val high = command.sew.mux(
      U(0) -> product(15 downto 8).resize(32),
      U(1) -> product(31 downto 16).resize(32),
      default -> product(63 downto 32)
    )
    val shifted = command.sew.mux(U(0) -> product(38 downto 7), U(1) -> product(46 downto 15), default -> product(62 downto 31))
    val lsb = command.sew.mux(U(0) -> product(7), U(1) -> product(15), default -> product(31))
    val half = command.sew.mux(U(0) -> product(6), U(1) -> product(14), default -> product(30))
    val sticky = command.sew.mux(U(0) -> product(5 downto 0).orR, U(1) -> product(13 downto 0).orR, default -> product(29 downto 0).orR)
    val rounded = (shifted.asUInt + VectorFixedPoint.roundIncrement(lsb, half, sticky, command.vxrm).asUInt).asBits
    val intMax = command.sew.mux(U(0) -> B(0x7f, 32 bits), U(1) -> B(0x7fff, 32 bits), default -> B(0x7fffffff, 32 bits))
    val fractional = m2.overflow ? intMax | rounded
    val result = command.op.mux(
      U(VectorElementOp.MULH) -> high,
      U(VectorElementOp.MACC) -> sum,
      U(VectorElementOp.MADD) -> sum,
      U(VectorElementOp.WMACC) -> sum,
      U(VectorElementOp.SMUL) -> fractional,
      default -> low
    )
    when(m2.valid && is(VectorElementOp.SMUL) && m2.overflow) { saturated := True }

    val eew = widen ? (command.sew + 1) | command.sew
    val data = eew.mux(U(0) -> (result(7 downto 0) #* 8), U(1) -> (result(15 downto 0) #* 4), default -> (result #* 2))
    val bytes = eew.mux(U(0) -> B"0001", U(1) -> B"0011", default -> B"1111")
    val mask = (bytes.resize(p.beatBytes) << m2.vdByte).resize(p.beatBytes)
  }

  val writeback = new Area {
    val valid = RegNext(m2.valid) init(False)
    val last = RegNext(m2.last) init(False)
    val address = RegNext(m2.address)
    val data = RegNext(finish.data)
    val mask = RegNext(finish.mask)
    io.write.valid := valid
    io.write.address := address
    io.write.data := data
    io.write.mask := mask
  }

  // The VRF holds every written element when done rises
  io.done := RegNext(writeback.last) init(False)
  when(io.done) { busy := False }
}
