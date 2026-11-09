package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

// Beat-unit vcompress.vm against an element model (norm:vcompress_op): selected vs2 elements packed into vd from
// element 0, the rest of vd undisturbed
class VectorBeatCompressTest extends AnyFunSuite {
  def run(vlen: Int, xlen: Int, elen: Int, dlen: Int = 64): Unit = {
    val p = VectorRegFileParam(vlen = vlen, dlen = dlen)
    val wb = dlen / 8
    val maxSew = log2Up(elen / 8)
    SimConfig.compile(new VectorArithmeticHarness(p, xlen, elen, slides = true, beatCompress = true)).doSim { dut =>
      val random = new Random(53 + xlen + vlen + elen + dlen - 64)
      val regBytes = vlen / 8
      val bytes = Array.fill(32, regBytes)(random.nextInt(256))
      def word(reg: Int, offset: Int): BigInt =
        (0 until wb).map(b => BigInt(bytes(reg)(offset + b)) << (b * 8)).reduce(_ | _)
      def tick(): Unit = {
        dut.clockDomain.risingEdge(); sleep(5)
        dut.clockDomain.fallingEdge(); sleep(5)
      }
      def store(a: Int): Unit = {
        dut.io.access.valid #= true
        dut.io.access.address #= a
        dut.io.access.mask #= (1 << wb) - 1
        dut.io.access.data #= word(a / p.beatsPerReg, (a % p.beatsPerReg) * wb)
        tick()
        dut.io.access.valid #= false
      }
      val cmd = dut.io.cmd
      dut.clockDomain.fallingEdge()
      cmd.valid #= false
      cmd.vd #= 0; cmd.vs1 #= 0; cmd.vs2 #= 0
      cmd.vl #= 0; cmd.vstart #= 0; cmd.sew #= 0; cmd.op #= VectorAluOp.ADD
      cmd.shape #= VectorShape.SINGLE; cmd.vxrm #= 0; cmd.nreg #= 1
      cmd.scalar #= 0; cmd.scalarOperand #= false; cmd.unmasked #= true
      cmd.signed #= false; cmd.saturate #= false; cmd.maskData #= false
      cmd.vlmax #= 0; cmd.slide #= 0; cmd.compress #= true
      dut.io.debug #= true
      dut.io.access.valid #= false
      dut.io.access.address #= 0; dut.io.access.data #= 0; dut.io.access.mask #= 0
      dut.io.readAddress #= 0
      dut.clockDomain.deassertReset(); sleep(1)
      dut.clockDomain.assertReset(); sleep(1); tick()
      dut.clockDomain.deassertReset()
      for (_ <- 0 until p.wordCount) tick()
      assert(!dut.io.initHold.toBoolean)
      for (a <- 0 until p.wordCount) store(a)

      var cases = 0
      var writes = 0
      val seen = mutable.Map[String, Int]().withDefaultValue(0)
      val pairs = mutable.Set[(Int, Int, Int)]() // (sew, fill, count) entering a step
      val latencies = mutable.Map[Boolean, mutable.Set[Int]]()
      def execute(sew: Int, lmul: Int, vl: Int, density: Double, nested: Boolean, chosen: Option[Int => Boolean] = None): Unit = {
        val size = 1 << sew
        val perWord = wb / size
        val group = 1 << (lmul max 0)
        val vlmax = (vlen / (8 * size) * math.pow(2, lmul)).toInt
        def pick(): Int = random.nextInt(32 / group) * group
        val vs2 = pick()
        val vd = Iterator.continually(pick()).find(_ != vs2).get
        // vs1 is one register outside vd; inside the vs2 group when nested
        val vs1 = if (nested) vs2 + random.nextInt(group) else Iterator.continually(random.nextInt(32)).find(r => r < vd || r >= vd + group).get
        // New mask bits, also above vl
        for (i <- 0 until vlen) {
          val bit = if (chosen.map(c => i < vl && c(i)).getOrElse(random.nextDouble() < density)) 1 else 0
          bytes(vs1)(i / 8) = (bytes(vs1)(i / 8) & ~(1 << (i % 8))) | (bit << (i % 8))
        }
        for (w <- 0 until p.beatsPerReg) store(vs1 * p.beatsPerReg + w)
        val old = bytes.map(_.clone)
        def bit(i: Int) = ((old(vs1)(i / 8) >> (i % 8)) & 1) == 1
        val selected = (0 until vl).filter(bit)
        for ((src, dst) <- selected.zipWithIndex; b <- 0 until size) {
          val s = src * size + b
          val d = dst * size + b
          bytes(vd + d / regBytes)(d % regBytes) = old(vs2 + s / regBytes)(s % regBytes)
        }
        // Fill and count entering each step, and the last step's case
        val steps = math.max(1, (vl + perWord - 1) / perWord)
        var fill = 0
        var extra = false
        for (k <- 0 until steps) {
          val n = (k * perWord until math.min(vl, (k + 1) * perWord)).count(bit)
          if (vl > 0) pairs += ((sew, fill, n))
          if (k == steps - 1) {
            val t = fill + n * size
            extra = t > wb
            seen(s"e${8 * size} last ${if (t == 0) "none" else if (t < wb) "merged" else if (t == wb) "exact" else "extra flush"}") += 1
          }
          fill = (fill + n * size) % wb
        }
        if (vl == 0) seen("vl 0") += 1
        if (vl > 0 && selected.isEmpty) seen("no element selected") += 1
        if (nested) seen("vs1 in the vs2 group") += 1
        if ((vl until vlen).exists(bit)) seen("mask bits above vl") += 1
        if (vl > dlen) seen("second mask word") += 1
        if (vl > 3 * dlen) seen("fourth mask word") += 1
        if (lmul < 0) seen("fractional") += 1

        dut.io.debug #= false
        cmd.valid #= true
        cmd.vd #= vd; cmd.vs2 #= vs2; cmd.vs1 #= vs1; cmd.vl #= vl; cmd.sew #= sew; cmd.vlmax #= vlmax
        tick()
        cmd.valid #= false
        val writtenWords = (selected.size * size + wb - 1) / wb
        var cycles = 0
        while (!dut.io.done.toBoolean && cycles < 300) {
          sleep(1)
          assert(dut.io.busy.toBoolean, s"busy before done case $cases")
          val rv = dut.io.readValid.toInt
          assert((rv & 4) == 0, s"port C read case $cases")
          if ((rv & 1) != 0) {
            val w = dut.io.readAddresses(0).toInt - vs2 * p.beatsPerReg
            assert(w >= 0 && w < steps, s"vs2 read outside the body case $cases word $w")
          }
          if ((rv & 2) != 0) {
            val w = dut.io.readAddresses(1).toInt - vs1 * p.beatsPerReg
            assert(w >= 0 && w < p.beatsPerReg, s"vs1 read outside the register case $cases word $w")
          }
          if (dut.io.write.valid.toBoolean) {
            val w = dut.io.write.address.toInt - vd * p.beatsPerReg
            assert(w >= 0 && w < writtenWords, s"write outside the packed words case $cases word $w")
            writes += 1
          }
          tick(); cycles += 1
        }
        assert(dut.io.done.toBoolean, s"timeout case $cases")
        latencies.getOrElseUpdate(extra, mutable.Set[Int]()) += cycles - steps
        sleep(1)
        assert(!dut.io.write.valid.toBoolean, s"write at done case $cases")
        tick()
        assert(!dut.io.busy.toBoolean, s"busy after done case $cases")
        dut.io.debug #= true
        for (a <- 0 until p.wordCount) {
          dut.io.readAddress #= a; tick()
          assert(dut.io.readData.toBigInt == word(a / p.beatsPerReg, (a % p.beatsPerReg) * wb),
            s"readback case $cases sew $sew lmul $lmul vl $vl vd $vd vs2 $vs2 vs1 $vs1 word $a")
        }
        cases += 1
      }

      val densities = Seq(0.0, 1.0, 0.5, 0.15, 0.85)
      for (sew <- 0 to maxSew; lmul <- -3 to 3 if (8 << sew) <= elen * math.pow(2, lmul)) {
        val vlmax = (vlen / (8 << sew) * math.pow(2, lmul)).toInt
        val perWord = wb >> sew
        val vls = (Seq(0, 1, perWord - 1, perWord, perWord + 1, 2 * perWord - 1, 2 * perWord + 1, vlmax - 1, vlmax) ++
          Seq.fill(2)(1 + random.nextInt(vlmax))).filter(v => v >= 0 && v <= vlmax).distinct
        for (vl <- vls; d <- densities) execute(sew, lmul, vl, d, nested = random.nextInt(4) == 0)
        for (_ <- 0 until 12) execute(sew, lmul, vlmax, random.nextDouble(), nested = random.nextInt(4) == 0)
      }
      // Each fill and count directly: the first word selects fill bytes, the second count elements
      for (sew <- 0 to maxSew; fill <- 0 until wb by (1 << sew); n <- 0 to (wb >> sew)) {
        val perWord = wb >> sew
        execute(sew, 1, 2 * perWord, 0, nested = false, Some(i => if (i < perWord) i < (fill >> sew) else i - perWord < n))
      }
      // Only one element selected, at the edges of 64- and 128-bit mask words
      for (e <- Seq(63, 64, 127, 128, 255) if e < vlen) execute(0, 3, vlen, 0, nested = false, Some(_ == e))
      // Every reachable fill and count entering a step: fill a multiple of the element size, count up to a word
      for (sew <- 0 to maxSew; fill <- 0 until wb by (1 << sew); n <- 0 to (wb >> sew)) assert(pairs((sew, fill, n)), s"never: e${8 << sew} fill $fill count $n")
      val wanted = Seq("vl 0", "no element selected", "vs1 in the vs2 group", "mask bits above vl", "fractional") ++
        (0 to maxSew).flatMap(s => (Seq("none", "exact") ++ (if ((1 << s) < wb) Seq("merged", "extra flush") else Nil)).map(c => s"e${8 << s} last $c")) ++
        (if (vlen >= 2 * dlen) Seq("second mask word") else Nil) ++ (if (vlen >= 4 * dlen) Seq("fourth mask word") else Nil)
      for (w <- wanted) assert(seen(w) > 0, s"never: $w")
      // done a constant number of cycles after the last step, one more with the extra flush
      assert(latencies(false).size == 1 && latencies(true) == mutable.Set(latencies(false).head + 1), s"done latencies $latencies")
      println(s"VectorBeatCompress VLEN=$vlen DLEN=$dlen XLEN=$xlen ELEN=$elen: $cases cases, $writes writes, done ${latencies(false).head} cycles " +
        s"after the last step (+1 with the extra flush), ${pairs.size} fill/count pairs, coverage " +
        seen.toSeq.sorted.map { case (k, v) => s"$k=$v" }.mkString(", ") + ", full physical readback after every case")
    }
  }
  test("beat compress on RV32") { run(128, 32, 32) }
  test("beat compress at SEW 64 on RV32") { run(128, 32, 64) }
  test("beat compress on RV64") { run(128, 64, 64) }
  test("beat compress with VLMAX 256 on RV32") { run(256, 32, 32) }
  test("beat compress with VLMAX 256 on RV64") { run(256, 64, 64) }
  test("beat compress with 128-bit words") { run(128, 64, 64, 128) }
  test("beat compress with 128-bit words at VLEN 256") { run(256, 64, 64, 128) }
}
