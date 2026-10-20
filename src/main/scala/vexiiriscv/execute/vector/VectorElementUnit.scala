package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._
import vexiiriscv.misc.DivRadix

object VectorElementOp {
  val MUL = 0   // low SEW bits of vs2 * vs1
  val MULH = 1  // high SEW bits of vs2 * vs1
  val WMUL = 2  // 2*SEW-bit vs2 * vs1
  val MACC = 3  // vd +/- vs1 * vs2, low SEW bits
  val MADD = 4  // vs2 +/- vs1 * vd, low SEW bits
  val WMACC = 5 // 2*SEW-bit vd + vs1 * vs2
  val SMUL = 6  // vs2 * vs1 >> (SEW - 1), rounded by vxrm and saturated
  val DIV = 7   // quotient, signed when command.signed
  val REM = 8   // remainder, signed when command.signed
  val CPOP = 9
  val FIRST = 10
  val IOTA = 11
  val SLIDEUP = 12
  val SLIDEDOWN = 13
  val SLIDE1UP = 14
  val SLIDE1DOWN = 15
  val GATHER = 16
  val GATHEREI16 = 17
  val COMPRESS = 18
  def width = 5
}

case class VectorElementCmd(p: VectorRegFileParam, xlen: Int) extends Bundle {
  val vd, vs1, vs2 = UInt(5 bits)
  val vl = UInt(log2Up(p.vlen) + 1 bits)
  val vlmax = UInt(log2Up(p.vlen) + 1 bits)
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
// finish in t+4 and write in t+5. Division serializes active elements; inactive elements take a cycle and no reads.
class VectorElementUnit(val p: VectorRegFileParam, val xlen: Int) extends Component {
  require(p.dlen == 64 && p.readPorts >= 3)
  require(xlen == 32 || xlen == 64)
  val io = new Bundle {
    val cmd = slave(Flow(VectorElementCmd(p, xlen)))
    val reads = Vec.fill(3)(master(VrfReadPort(p)))
    val write = master(Flow(VrfWriteCmd(p)))
    val v0 = in Bits(p.vlen bits)
    val busy, done = out Bool()
    val result = out Bits(xlen bits)
    val saturated = out Bool() // an active element saturated; holds from done until the next command
  }

  val command = Reg(VectorElementCmd(p, xlen))
  def is(op: Int): Bool = command.op === op
  val widen = is(VectorElementOp.WMUL) || is(VectorElementOp.WMACC)
  val divide = is(VectorElementOp.DIV) || is(VectorElementOp.REM)
  val scan = is(VectorElementOp.CPOP) || is(VectorElementOp.FIRST) || is(VectorElementOp.IOTA)
  val permute = command.op >= VectorElementOp.SLIDEUP
  val count = Reg(UInt(log2Up(p.vlen) + 1 bits)) init(0)
  val result = Reg(Bits(xlen bits))
  io.result := result
  val divPending = RegInit(False)
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
    count := 0
    result := (io.cmd.op === VectorElementOp.FIRST) ? B(xlen bits, default -> True) | B(0, xlen bits)
  }

  val issue = new Area {
    val advance = running && !divPending && !permute
    val active = advance && command.vl =/= 0 && (command.unmasked || io.v0(index))
    val last = advance && (command.vl === 0 || index.resize(offsetWidth) + 1 >= command.vl)
    val offset = (index.resize(offsetWidth) << command.sew).resize(offsetWidth)
    val vdOffset = widen ? (offset << 1).resize(offsetWidth) | offset

    io.reads(0).cmd.valid := active
    io.reads(0).cmd.payload := word(command.vs2, scan ? (index.resize(offsetWidth) >> 3) | offset)
    io.reads(1).cmd.valid := active && !command.scalarOperand && !scan
    io.reads(1).cmd.payload := word(command.vs1, offset)
    io.reads(2).cmd.valid := active && readVd
    io.reads(2).cmd.payload := word(command.vd, vdOffset)

    when(active && divide) { divPending := True }
    when(advance) {
      index := index + 1
      when(last) { running := False }
    }
  }

  // Element extraction from the read words, extended to 33 bits
  val x1 = new Area {
    val valid = RegNext(issue.active) init(False)
    val last = RegNext(issue.last) init(False)
    val maskBit = RegNext(index.resize(6))
    val elementIndex = RegNext(index)
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
    val hit = io.reads(0).rsp(maskBit)
    when(valid && scan) {
      count := count + hit.asUInt
      when(is(VectorElementOp.CPOP)) { result := (count + hit.asUInt).resize(xlen).asBits }
      when(is(VectorElementOp.FIRST) && hit && result.msb) { result := elementIndex.resize(xlen).asBits }
    }
    val overflow = x === intMin && y === intMin // the only vsmul product that does not fit (norm:vsmul_op)
  }

  // Unsigned division of magnitudes, then restore the quotient or remainder sign
  val division = new Area {
    val core = new DivRadix(32, 2)
    core.io.flush := False
    core.io.cmd.valid := x1.valid && divide
    core.io.cmd.normalized := False
    core.io.cmd.iterations := 0
    core.io.cmd.a := (x1.x.msb ? -x1.x | x1.x).asUInt.resized
    core.io.cmd.b := (x1.y.msb ? -x1.y | x1.y).asUInt.resized
    core.io.rsp.ready := True
    val address = RegNextWhen(x1.address, core.io.cmd.fire)
    val vdByte = RegNextWhen(x1.vdByte, core.io.cmd.fire)
    val last = RegNextWhen(x1.last, core.io.cmd.fire) init(False)
    val revert = RegNextWhen(is(VectorElementOp.REM) ? x1.x.msb |
      ((x1.x.msb ^ x1.y.msb) && x1.y =/= 0), core.io.cmd.fire)
    val magnitude = is(VectorElementOp.REM) ? core.io.rsp.remain | core.io.rsp.result
    val result = (revert ? (~magnitude + 1) | magnitude).asBits
    when(core.io.rsp.fire) { divPending := False }
    assert(!(core.io.cmd.valid && !core.io.cmd.ready), "vector divide issued while busy")
  }

  // 33 x 33 signed multiplier with two pipeline registers
  val operands = new Area {
    val valid = RegNext(x1.valid && !divide && !scan) init(False)
    val last = RegNext(x1.last && !divide && !scan) init(False)
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
    val multiplyResult = command.op.mux(
      U(VectorElementOp.MULH) -> high,
      U(VectorElementOp.MACC) -> sum,
      U(VectorElementOp.MADD) -> sum,
      U(VectorElementOp.WMACC) -> sum,
      U(VectorElementOp.SMUL) -> fractional,
      default -> low
    )
    when(m2.valid && is(VectorElementOp.SMUL) && m2.overflow) { saturated := True }

    val result = divide ? division.result | multiplyResult
    val vdByte = divide ? division.vdByte | m2.vdByte
    val eew = widen ? (command.sew + 1) | command.sew
    val data = eew.mux(U(0) -> (result(7 downto 0) #* 8), U(1) -> (result(15 downto 0) #* 4), default -> (result #* 2))
    val bytes = eew.mux(U(0) -> B"0001", U(1) -> B"0011", default -> B"1111")
    val mask = (bytes.resize(p.beatBytes) << vdByte).resize(p.beatBytes)
  }

  val writeback = new Area {
    val valid = RegNext(m2.valid || division.core.io.rsp.fire || (scan && is(VectorElementOp.IOTA) && x1.valid)) init(False)
    val last = RegNext((scan && x1.last) || m2.last || (divide && ((division.core.io.rsp.fire && division.last) || (x1.last && !x1.valid)))) init(False)
    val address = RegNext(scan ? x1.address | (divide ? division.address | m2.address))
    val scanData = command.sew.mux(U(0) -> (count.resize(8).asBits #* 8),
      U(1) -> (count.resize(16).asBits #* 4), default -> (count.resize(32).asBits #* 2))
    val scanBytes = command.sew.mux(U(0) -> B"0001", U(1) -> B"0011", default -> B"1111")
    val scanMask = (scanBytes.resize(p.beatBytes) << x1.vdByte).resize(p.beatBytes)
    val data = RegNext(scan ? scanData | finish.data)
    val mask = RegNext(scan ? scanMask | finish.mask)
    io.write.valid := valid
    io.write.address := address
    io.write.data := data
    io.write.mask := mask
  }

  // Gather indices are read first; the following cycle reads the selected data word.
  val permutation = new Area {
    val gather = is(VectorElementOp.GATHER) || is(VectorElementOp.GATHEREI16)
    val compress = is(VectorElementOp.COMPRESS)
    val vectorIndex = gather && !command.scalarOperand
    val indexSew = is(VectorElementOp.GATHEREI16) ? U(1, 2 bits) | command.sew
    val advance = running && permute
    val active = advance && command.vl =/= 0 && (compress || command.unmasked || io.v0(index))
    val last = advance && (command.vl === 0 || index.resize(offsetWidth) + 1 >= command.vl)
    val indexOffset = (index.resize(offsetWidth) << indexSew).resize(offsetWidth)
    val maskOffset = index.resize(offsetWidth) >> 3
    when(permute) {
      io.reads(0).cmd.valid := active && (vectorIndex || compress)
      io.reads(0).cmd.payload := word(command.vs1, compress ? maskOffset | indexOffset)
      io.reads(1).cmd.valid := False
      io.reads(2).cmd.valid := False
    }
    when(advance) {
      index := index + 1
      when(last) { running := False }
    }

    val valid1 = RegNext(active) init(False)
    val last1 = RegNext(last) init(False)
    val index1 = RegNext(index)
    val byte1 = RegNext(indexOffset.resize(log2Up(p.beatBytes)))
    val rawIndex = (io.reads(0).rsp >> (byte1 << 3)).resize(32)
    val selectedIndex = indexSew.mux(U(0) -> rawIndex(7 downto 0).asUInt.resize(xlen),
      U(1) -> rawIndex(15 downto 0).asUInt.resize(xlen), default -> rawIndex.asUInt.resize(xlen))
    val offset = (vectorIndex ? selectedIndex | command.scalar.asUInt).resize(xlen + 1)
    val source = UInt(xlen + 1 bits)
    val keep = Bool()
    val scalar = Bool()
    source := index1.resize(xlen + 1)
    keep := True
    scalar := False
    switch(command.op) {
      spinal.core.is(VectorElementOp.SLIDEUP) { source := index1.resize(xlen + 1) - offset; keep := index1.resize(xlen + 1) >= offset }
      spinal.core.is(VectorElementOp.SLIDEDOWN) { source := index1.resize(xlen + 1) + offset }
      spinal.core.is(VectorElementOp.SLIDE1UP) { source := index1.resize(xlen + 1) - 1; scalar := index1 === 0 }
      spinal.core.is(VectorElementOp.SLIDE1DOWN) { source := index1.resize(xlen + 1) + 1; scalar := index1.resize(offsetWidth) + 1 === command.vl }
      spinal.core.is(VectorElementOp.GATHER) { source := offset }
      spinal.core.is(VectorElementOp.GATHEREI16) { source := offset }
    }
    val selected = !compress || io.reads(0).rsp(index1.resize(6))
    val write1 = valid1 && keep && selected
    val inRange = source < command.vlmax.resize(xlen + 1)
    val sourceOffset = (source.resize(offsetWidth) << command.sew).resize(offsetWidth)
    val dest = compress ? count | index1.resize(offsetWidth)
    val destOffset = (dest << command.sew).resize(offsetWidth)
    when(permute) {
      io.reads(1).cmd.valid := write1 && inRange && !scalar
      io.reads(1).cmd.payload := word(command.vs2, sourceOffset)
    }
    when(permute && valid1 && compress && selected) { count := count + 1 }

    val valid2 = RegNext(write1) init(False)
    val last2 = RegNext(last1) init(False)
    val byte2 = RegNext(sourceOffset.resize(log2Up(p.beatBytes)))
    val destByte2 = RegNext(destOffset.resize(log2Up(p.beatBytes)))
    val address2 = RegNext(word(command.vd, destOffset))
    val zero2 = RegNext(!inRange)
    val scalar2 = RegNext(scalar)
    val data2 = scalar2 ? command.scalar.resize(32) | (zero2 ? B(0, 32 bits) | (io.reads(1).rsp >> (byte2 << 3)).resize(32))
    val replicated = command.sew.mux(U(0) -> (data2(7 downto 0) #* 8),
      U(1) -> (data2(15 downto 0) #* 4), default -> (data2 #* 2))
    val bytes = command.sew.mux(U(0) -> B"0001", U(1) -> B"0011", default -> B"1111")
    val valid3 = RegNext(valid2) init(False)
    val last3 = RegNext(last2) init(False)
    val address3 = RegNext(address2)
    val data3 = RegNext(replicated)
    val mask3 = RegNext((bytes.resize(p.beatBytes) << destByte2).resize(p.beatBytes))
    when(permute) {
      io.write.valid := valid3
      io.write.address := address3
      io.write.data := data3
      io.write.mask := mask3
    }
  }

  // The VRF holds every written element when done rises
  io.done := RegNext(writeback.last || permutation.last3) init(False)
  when(io.done) { busy := False }
}
