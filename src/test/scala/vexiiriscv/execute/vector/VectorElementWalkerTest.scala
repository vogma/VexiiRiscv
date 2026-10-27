package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// Expected (element, field, half) lists come from a direct model of [vstart, vl), the mask, the field count
// and, with split, whether each field takes two halves.
class VectorElementWalkerTest extends AnyFunSuite {
  def run(vlen: Int, split: Boolean = false): Unit = {
    SimConfig.compile(new VectorElementWalker(vlen, split)).doSim { dut =>
      val random = new Random(77 + vlen + (if (split) 1 else 0))
      dut.clockDomain.forkStimulus(10)
      dut.io.start #= false
      dut.io.advance #= false
      if (split) dut.io.wide #= false
      dut.clockDomain.waitSampling()

      def mask(kind: Int): BigInt = kind match {
        case 0 => BigInt(0)
        case 1 => (BigInt(1) << vlen) - 1
        case 2 => BigInt(1) << random.nextInt(vlen)
        case _ => BigInt(vlen, random)
      }
      var cases, uops, empty, wides = 0
      val corners = Seq(0, 1, vlen - 1, vlen)
      for (iteration <- 0 until 3000) {
        val vl = if (iteration % 4 == 0) corners(random.nextInt(corners.size)) else random.nextInt(vlen + 1)
        val vstartCorners = Seq(0, vl - 1, vl, vlen - 1).map(v => v.max(0).min(vlen - 1))
        val vstart: Int = if (iteration % 5 == 0) vstartCorners(random.nextInt(4)) else random.nextInt(vlen)
        val unmasked = random.nextBoolean()
        val v0 = mask(random.nextInt(4))
        val fields = if (random.nextBoolean()) 0 else random.nextInt(8)
        val startField = if (random.nextBoolean()) 0 else random.nextInt(fields + 1)
        val elements = (vstart until vl).filter(i => unmasked || v0.testBit(i))
        val wide = split && random.nextBoolean()
        val halves = if (wide) Seq(0, 1) else Seq(0)
        val expected = elements.zipWithIndex.flatMap { case (e, n) =>
          ((if (n == 0) startField else 0) to fields).flatMap(f => halves.map(h => (e, f, h))) }

        dut.io.evl #= vl
        dut.io.vstart #= vstart
        dut.io.unmasked #= unmasked
        dut.io.v0 #= v0
        dut.io.fields #= fields
        dut.io.startField #= startField
        if (split) dut.io.wide #= wide
        dut.io.start #= true
        dut.clockDomain.waitSampling()
        dut.io.start #= false
        // Random cycles without an advance, as when dispatch waits; the outputs must hold.
        while (random.nextInt(3) == 0) dut.clockDomain.waitSampling()

        def at() = (dut.io.index.toInt, dut.io.field.toInt, if (split && dut.io.half.toBoolean) 1 else 0)
        val seen = ArrayBuffer[(Int, Int, Int)]()
        var done = false
        while (!done) {
          sleep(1)
          val any = dut.io.any.toBoolean
          val last = dut.io.last.toBoolean
          assert(any == expected.nonEmpty, s"any: vl=$vl vstart=$vstart")
          if (any) seen += at()
          done = last
          assert(seen.size <= expected.size.max(1), s"too many elements: vl=$vl vstart=$vstart")
          if (random.nextInt(4) == 0) {
            dut.io.advance #= false
            dut.clockDomain.waitSampling()
            sleep(1)
            assert(dut.io.last.toBoolean == last && (!any || at() == seen.last), "outputs changed without advance")
          }
          dut.io.advance #= true
          dut.clockDomain.waitSampling()
          dut.io.advance #= false
        }
        assert(seen == expected, s"vl=$vl vstart=$vstart unmasked=$unmasked fields=$fields startField=$startField: got $seen, expected $expected")
        cases += 1
        uops += expected.size
        if (expected.isEmpty) empty += 1
        if (wide) wides += 1
      }
      println(s"VLEN=$vlen split=$split cases=$cases micro-ops=$uops empty=$empty wide=$wides")
    }
  }

  test("element walker VLEN 128") { run(128) }
  test("element walker VLEN 256") { run(256) }
  test("element walker with split wide elements VLEN 128") { run(128, split = true) }
  test("element walker with split wide elements VLEN 256") { run(256, split = true) }
}
