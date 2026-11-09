package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

// Beat-unit slides and scalar-index gathers against an element model (norm:vslideup_op, norm:vslidedown_op,
// norm:vslide1up-vx_op, norm:vslide1down-vx_op, norm:vrgather-vx_vrgather-vi_op)
class VectorBeatPermuteTest extends AnyFunSuite {
  // Command slide field: bit 0 down, bit 1 by one element with the scalar inserted
  val kinds = Seq("vslideup" -> 0, "vslidedown" -> 1, "vslide1up" -> 2, "vslide1down" -> 3, "vrgather" -> 0)

  def run(vlen: Int, xlen: Int, elen: Int, dlen: Int = 64): Unit = {
    val p = VectorRegFileParam(vlen = vlen, dlen = dlen)
    val wb = dlen / 8
    val maxSew = log2Up(elen / 8)
    SimConfig.compile(new VectorArithmeticHarness(p, xlen, elen, slides = true)).doSim { dut =>
      val random = new Random(37 + xlen + vlen + elen + dlen - 64)
      val regBytes = vlen / 8
      val bytes = Array.fill(32, regBytes)(random.nextInt(256))
      val xmask = (BigInt(1) << xlen) - 1
      def word(reg: Int, offset: Int): BigInt =
        (0 until wb).map(b => BigInt(bytes(reg)(offset + b)) << (b * 8)).reduce(_ | _)
      def tick(): Unit = {
        dut.clockDomain.risingEdge(); sleep(5)
        dut.clockDomain.fallingEdge(); sleep(5)
      }
      val cmd = dut.io.cmd
      dut.clockDomain.fallingEdge()
      cmd.valid #= false
      cmd.vd #= 0; cmd.vs1 #= 0; cmd.vs2 #= 0
      cmd.vl #= 0; cmd.vstart #= 0; cmd.sew #= 0; cmd.op #= VectorAluOp.ADD
      cmd.shape #= VectorShape.SLIDE; cmd.vxrm #= 0; cmd.nreg #= 1
      cmd.scalar #= 0; cmd.scalarOperand #= true; cmd.unmasked #= true
      cmd.signed #= false; cmd.saturate #= false; cmd.maskData #= false
      cmd.vlmax #= 0; cmd.slide #= 0
      dut.io.debug #= true
      dut.io.access.valid #= false
      dut.io.access.address #= 0; dut.io.access.data #= 0; dut.io.access.mask #= 0
      dut.io.readAddress #= 0
      dut.clockDomain.deassertReset(); sleep(1)
      dut.clockDomain.assertReset(); sleep(1); tick()
      dut.clockDomain.deassertReset()
      for (_ <- 0 until p.wordCount) tick()
      assert(!dut.io.initHold.toBoolean)
      for (a <- 0 until p.wordCount) {
        dut.io.access.valid #= true
        dut.io.access.address #= a
        dut.io.access.mask #= (1 << wb) - 1
        dut.io.access.data #= word(a / p.beatsPerReg, (a % p.beatsPerReg) * wb)
        tick()
      }
      dut.io.access.valid #= false

      var cases = 0
      var writes = 0
      val seen = mutable.Map[String, Int]().withDefaultValue(0)
      val latencies = mutable.Set[Int]()
      def execute(kind: String, sew: Int, lmul: Int, vl: Int, masked: Boolean, scalar: BigInt): Unit = {
        val size = 1 << sew
        val width = 8 * size
        val mask = (BigInt(1) << width) - 1
        val group = 1 << (lmul max 0)
        val vlmax = (vlen / width * math.pow(2, lmul)).toInt
        def pick(): Int = random.nextInt(32 / group) * group
        val vd = Iterator.continually(pick()).find(r => !masked || r != 0).get
        val down = kind == "vslidedown" || kind == "vslide1down"
        // Only slide-downs may overlap; aligned groups of one LMUL are then identical (norm:vslideup_vreg_constr)
        val vs2 = if (down && random.nextInt(3) == 0) vd else Iterator.continually(pick()).find(r => down || r != vd).get
        val old = bytes.map(_.clone)
        def element(reg: Int, i: Int): BigInt = {
          val index = i * size
          val r = reg + index / regBytes
          (0 until size).map(b => BigInt(old(r)(index % regBytes + b)) << (8 * b)).reduce(_ | _)
        }
        def active(i: Int) = !masked || ((old(0)(i / 8) >> (i % 8)) & 1) != 0
        // With SEW > XLEN the scalar is sign-extended (norm:vslide1up-vx_op)
        val inserted = (if (width > xlen && scalar.testBit(xlen - 1)) scalar - (BigInt(1) << xlen) else scalar) & mask
        for (i <- 0 until vl if active(i)) {
          val value: Option[BigInt] = kind match {
            case "vslideup" => if (BigInt(i) >= scalar) Some(element(vs2, i - scalar.toInt)) else None
            case "vslidedown" =>
              if (scalar + i >= vlmax) seen("zero fill") += 1
              Some(if (scalar + i < vlmax) element(vs2, i + scalar.toInt) else BigInt(0))
            case "vslide1up" => Some(if (i == 0) inserted else element(vs2, i - 1))
            case "vslide1down" => Some(if (i == vl - 1) inserted else element(vs2, i + 1))
            case "vrgather" => Some(if (scalar >= vlmax) BigInt(0) else element(vs2, scalar.toInt))
          }
          for (v <- value; b <- 0 until size) {
            val byte = i * size + b
            bytes(vd + byte / regBytes)(byte % regBytes) = ((v >> (8 * b)) & 255).toInt
          }
        }
        if (vs2 == vd) seen("in place") += 1
        if (vl > 0 && scalar >= vlmax && kind != "vslide1up" && kind != "vslide1down") seen(s"$kind amount >= VLMAX") += 1
        if (scalar > (xmask >> 1) && kind != "vslide1up" && kind != "vslide1down") seen("amount with the top bit") += 1
        if (scalar >= (BigInt(1) << 32) && kind != "vslide1up" && kind != "vslide1down") seen("amount >= 2^32") += 1
        if (width > xlen && scalar.testBit(xlen - 1) && kind.startsWith("vslide1")) seen("scalar sign-extended") += 1
        if (vlmax == vlen && vl == vlmax) seen("VLMAX = VLEN") += 1
        if (lmul < 0) seen("fractional") += 1
        if (masked && kind == "vslide1down" && vl > 0 && !active(vl - 1)) seen("vslide1down last inactive") += 1

        dut.io.debug #= false
        cmd.valid #= true
        cmd.vd #= vd; cmd.vs2 #= vs2; cmd.vl #= vl; cmd.sew #= sew
        cmd.shape #= (if (kind == "vrgather") VectorShape.SPLAT else VectorShape.SLIDE)
        cmd.scalar #= scalar; cmd.unmasked #= !masked
        cmd.vlmax #= vlmax; cmd.slide #= kinds.find(_._1 == kind).get._2
        tick()
        cmd.valid #= false
        val steps = math.max(1, (vl * size + wb - 1) / wb)
        // Source words of the group: VLMAX elements, fewer than a register at fractional LMUL
        val sourceWords = (vlmax * size + wb - 1) / wb
        var cycles = 0
        while (!dut.io.done.toBoolean && cycles < 300) {
          sleep(1)
          assert(dut.io.busy.toBoolean, s"busy before done case $cases")
          val rv = dut.io.readValid.toInt
          assert((rv & 4) == 0, s"port C read case $cases")
          for (port <- 0 to 1 if ((rv >> port) & 1) != 0) {
            val w = dut.io.readAddresses(port).toInt - vs2 * p.beatsPerReg
            assert(w >= 0 && w < sourceWords, s"read outside vs2 case $cases $kind sew $sew lmul $lmul port $port word $w")
          }
          if (dut.io.write.valid.toBoolean) {
            val w = dut.io.write.address.toInt - vd * p.beatsPerReg
            assert(w >= 0 && w < steps, s"write outside the body words case $cases $kind")
            writes += 1
          }
          tick(); cycles += 1
        }
        assert(dut.io.done.toBoolean, s"timeout case $cases")
        latencies += cycles - steps
        sleep(1); tick()
        assert(!dut.io.busy.toBoolean, s"busy after done case $cases")
        dut.io.debug #= true
        for (a <- 0 until p.wordCount) {
          dut.io.readAddress #= a; tick()
          assert(dut.io.readData.toBigInt == word(a / p.beatsPerReg, (a % p.beatsPerReg) * wb),
            s"readback case $cases $kind sew $sew lmul $lmul vl $vl masked $masked amount $scalar vd $vd vs2 $vs2 word $a")
        }
        cases += 1
      }

      for (sew <- 0 to maxSew; lmul <- -3 to 3 if (8 << sew) <= elen * math.pow(2, lmul)) {
        val vlmax = (vlen / (8 << sew) * math.pow(2, lmul)).toInt
        val perWord = wb >> sew
        // With 128-bit words also the 64-bit sub-word edge
        val half = if (wb > 8) Seq((8 >> sew) - 1, 8 >> sew, (8 >> sew) + 1) else Nil
        val amounts = ((Seq(0, 1, 2, 3, perWord - 1, perWord, perWord + 1) ++ half ++ Seq(vlmax - 1, vlmax, vlmax + 1)).filter(_ >= 0).map(BigInt(_)) ++
          Seq(xmask, BigInt(xlen, random), BigInt(random.nextInt(vlmax + 2))) ++
          (if (xlen == 64) Seq(BigInt(1) << 32, (BigInt(1) << 32) + 1) else Nil)).distinct
        for ((kind, _) <- kinds; vl <- Seq(0, 1, vlmax - 1, vlmax, 1 + random.nextInt(vlmax)).distinct; masked <- Seq(false, true)) {
          val scalars = if (kind.startsWith("vslide1")) Seq.fill(2)(BigInt(xlen, random))
            else if (vl == vlmax) amounts else Seq.fill(3)(amounts(random.nextInt(amounts.size)))
          for (s <- scalars) execute(kind, sew, lmul, vl, masked, s)
        }
      }
      val wanted = Seq("in place", "zero fill", "vslideup amount >= VLMAX", "vslidedown amount >= VLMAX", "vrgather amount >= VLMAX",
        "amount with the top bit", "VLMAX = VLEN", "fractional", "vslide1down last inactive") ++
        (if (xlen == 64) Seq("amount >= 2^32") else Nil) ++ (if (elen > xlen) Seq("scalar sign-extended") else Nil)
      for (w <- wanted) assert(seen(w) > 0, s"never: $w")
      // Every form streams like SINGLE: done a constant number of cycles after the last step
      assert(latencies.size == 1, s"done latencies $latencies")
      println(s"VectorBeatPermute VLEN=$vlen DLEN=$dlen XLEN=$xlen ELEN=$elen: $cases cases, $writes writes, done ${latencies.head} cycles " +
        "after the last step, coverage " + seen.toSeq.sorted.map { case (k, v) => s"$k=$v" }.mkString(", ") +
        ", reads checked inside the vs2 group, full physical readback after every case")
    }
  }
  test("beat slides and gathers on RV32") { run(128, 32, 32) }
  test("beat slides and gathers at SEW 64 on RV32") { run(128, 32, 64) }
  test("beat slides and gathers on RV64") { run(128, 64, 64) }
  test("beat slides and gathers with VLMAX 256 on RV32") { run(256, 32, 32) }
  test("beat slides and gathers with VLMAX 256 on RV64") { run(256, 64, 64) }
  test("beat slides and gathers with 128-bit words") { run(128, 64, 64, 128) }
  test("beat slides and gathers with 128-bit words at VLEN 256") { run(256, 64, 64, 128) }
}
