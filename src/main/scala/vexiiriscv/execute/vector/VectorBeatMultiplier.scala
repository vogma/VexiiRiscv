package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

// A shared bank of signed 17 x 17 cells. Inputs are destination-word operands;
// widening selects one half of the narrow source word before the bank.
class VectorBeatMultiplier(val dlen: Int, val elen: Int = 64) extends Component {
  require(dlen == 64 || dlen == 128)
  val io = new Bundle {
    val valid, pass, wide, half = in Bool()
    val sew = in UInt(2 bits)
    val op = in UInt(VectorElementOp.width bits)
    val signedA, signedB, negate = in Bool()
    val vxrm = in Bits(2 bits)
    val a, b, c = in Bits(dlen bits)
    val result = out Bits(dlen bits)
    val saturated = out Bits(dlen / 8 bits)
    val outputValid = out Bool()
  }
  val madd = io.op === VectorElementOp.MADD
  def source(v: Bits): Bits = io.wide ? (io.half ? v(dlen - 1 downto dlen / 2).resize(dlen) |
    v(dlen / 2 - 1 downto 0).resize(dlen)) | v
  val a = source(madd ? io.c | io.a)
  val b = source(io.b)
  def limb(v: Bits, at: Int, width: Int, signed: Bool): SInt =
    ((signed && v(at + width - 1)) ## v(at, width bits)).asSInt.resize(17)

  val cells = (0 until dlen / 8).map { i =>
    val x, y = SInt(17 bits)
    val e = i / 4; val ai = (i % 4) / 2; val bi = i % 2
    x := limb(a, e * 32 + ai * 16, 16, io.signedA && Bool(ai == 1))
    y := limb(b, e * 32 + bi * 16, 16, io.signedB && Bool(bi == 1))
    when(io.sew === 0) {
      x := limb(a, i * 8, 8, io.signedA)
      y := limb(b, i * 8, 8, io.signedB)
    }
    when(io.sew === 1) {
      if (i < dlen / 16) {
        x := limb(a, i * 16, 16, io.signedA)
        y := limb(b, i * 16, 16, io.signedB)
      } else { x := 0; y := 0 }
    }
    if (elen == 64) when(io.sew === 3) {
      // Pass 0: low x low. Pass 1: low x high and high x low.
      val lane = i / 8; val cross = (i % 8) / 4
      x := limb(a, lane * 64 + cross * 32 + ai * 16, 16, False)
      y := limb(b, lane * 64 + (1 - cross) * 32 + bi * 16, 16, False)
      when(!io.pass) {
        if (i < dlen / 64 * 4) {
          x := limb(a, (i / 4) * 64 + ai * 16, 16, False)
          y := limb(b, (i / 4) * 64 + bi * 16, 16, False)
        } else { x := 0; y := 0 }
      }
    }
    val product = x * y
    product.addAttribute("use_dsp", "yes")
    RegNext(product)
  }
  val sew1 = RegNext(io.sew)
  val pass1 = RegNext(io.pass)
  def join(xs: Seq[Bits]): Bits = xs.reverse.reduce(_ ## _)
  def full32(base: Int): Bits = {
    val terms = for (i <- 0 until 2; j <- 0 until 2) yield
      (cells(base + 2 * i + j).resize(64) << (16 * (i + j))).resize(64)
    terms.reduce { (a, b) => val sum = a + b; sum.addAttribute("use_dsp", "no"); sum }.asBits
  }
  val p8 = join(cells.map(_.resize(16).asBits))
  val p16 = join(cells.take(dlen / 16).map(_.resize(32).asBits))
  val p32 = join((0 until dlen / 32).map(i => full32(4 * i)))
  val reconstructed = Bits(2 * dlen bits)
  reconstructed := sew1.mux(U(0) -> p8, U(1) -> p16, default -> p32)
  if (elen == 64) when(sew1 === 3) {
    for (i <- 0 until dlen / 64) {
      val low = full32(4 * i)
      val crossSum = full32(8 * i)(31 downto 0).asUInt + full32(8 * i + 4)(31 downto 0).asUInt
      crossSum.addAttribute("use_dsp", "no")
      val cross = crossSum.asBits
      reconstructed(128 * i, 128 bits) := (pass1 ? (cross ## B(0, 32 bits)) | low).resize(128)
    }
  }
  val product = RegNext(reconstructed)
  val sew = RegNext(sew1)
  val wide = RegNext(RegNext(io.wide))
  val pass = RegNext(pass1)
  val op = RegNext(RegNext(io.op))
  val negate = RegNext(RegNext(io.negate))
  val vxrm = RegNext(RegNext(io.vxrm))
  val addend = RegNext(RegNext(madd ? io.a | io.c))
  val valid = RegNext(RegNext(io.valid) init(False)) init(False)
  val split = if (elen == 64) sew === 3 else False
  io.outputValid := valid && (!split || pass)

  val partial = if (elen == 64) Vec.fill(dlen / 64)(Reg(UInt(64 bits))) else null
  val accumulate = op === VectorElementOp.MACC || op === VectorElementOp.MADD || op === VectorElementOp.WMACC
  def finish(w: Int, widening: Boolean): (Bits, Bits) = {
    val dst = if (widening) 2 * w else w
    val values = Bits(dlen bits)
    val clips = Bits(dlen / 8 bits)
    for (e <- 0 until dlen / dst) {
      val prod = product(2 * w * e, 2 * w bits)
      val low = if (w == 64) {
        when(valid && split && !pass) { partial(e) := prod(63 downto 0).asUInt }
        val sum = partial(e) + prod(63 downto 0).asUInt
        sum.addAttribute("use_dsp", "no")
        sum.asBits
      } else prod(dst - 1 downto 0)
      val term = negate ? (~low.asUInt + 1) | low.asUInt
      val base = accumulate ? addend(e * dst, dst bits).asUInt | U(0, dst bits)
      val sum = base + term
      sum.addAttribute("use_dsp", "no")
      val value = sum.asBits
      val clip = Bool()
      clip := False
      values(e * dst, dst bits) := value
      if (!widening && w <= 32) {
        when(op === VectorElementOp.MULH) { values(e * dst, dst bits) := prod(2 * w - 1 downto w) }
        val overflow = prod === B(BigInt(1) << (2 * w - 2), 2 * w bits)
        val increment = VectorFixedPoint.roundIncrement(prod(w - 1), prod(w - 2), prod(w - 3 downto 0).orR, vxrm)
        val rounded = (prod(2 * w - 2 downto w - 1).asUInt + increment.asUInt).asBits
        when(op === VectorElementOp.SMUL) {
          values(e * dst, dst bits) := overflow ? B((BigInt(1) << (w - 1)) - 1, w bits) | rounded
          clip := overflow
        }
      }
      for (j <- 0 until dst / 8) clips(e * dst / 8 + j) := clip
    }
    (values, clips)
  }
  val results = for (w <- Seq(8, 16, 32)) yield {
    val single = finish(w, false)
    val widened = finish(w, true)
    ((wide ? widened._1 | single._1), (wide ? widened._2 | single._2))
  }
  val last = if (elen == 64) finish(64, false) else results.last
  io.result := sew.mux(U(0) -> results(0)._1, U(1) -> results(1)._1, U(2) -> results(2)._1, default -> last._1)
  io.saturated := sew.mux(U(0) -> results(0)._2, U(1) -> results(1)._2, U(2) -> results(2)._2, default -> last._2)
}
