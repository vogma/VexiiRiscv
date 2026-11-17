package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

class VectorScalarResultTest extends AnyFunSuite {
  for ((xlen, elen, vlen, dlen) <- Seq((32, 32, 128, 64), (32, 64, 128, 64),
      (64, 64, 128, 64), (64, 64, 256, 128))) {
    test(s"scalar result is valid at done RV$xlen ELEN$elen VLEN$vlen DLEN$dlen") {
      val p = VectorRegFileParam(vlen = vlen, dlen = dlen)
      SimConfig.compile(new VectorScanHarness(p, xlen, elen)).doSim { dut =>
        def tick(): Unit = {
          dut.clockDomain.risingEdge(); sleep(5)
          dut.clockDomain.fallingEdge(); sleep(5)
        }
        dut.clockDomain.fallingEdge()
        for (cmd <- Seq(dut.io.arithmetic, dut.io.element)) cmd.payload.flatten.foreach {
          case b: Bool => b #= false
          case u: UInt => u #= 0
          case b: Bits => b #= 0
        }
        dut.io.arithmetic.valid #= false; dut.io.element.valid #= false
        dut.io.useElement #= false; dut.io.debug #= true
        dut.io.access.valid #= false; dut.io.access.address #= 0
        dut.io.access.data #= 0; dut.io.access.mask #= 0; dut.io.readAddress #= 0
        dut.clockDomain.assertReset(); tick(); dut.clockDomain.deassertReset()
        for (_ <- 0 until p.wordCount) tick()
        val xmask = (BigInt(1) << xlen) - 1
        val all = (BigInt(1) << vlen) - 1
        val patterns = Seq(BigInt(0), all, BigInt(1), BigInt(1) << (vlen - 1),
          (0 until vlen).filter(i => (i * 37 + 11) % 13 < 5).map(BigInt(1) << _).reduce(_ | _))
        val active = (0 until vlen).filter(_ % 3 != 1).map(BigInt(1) << _).reduce(_ | _)
        def write(reg: Int, bits: BigInt): Unit = {
          dut.io.debug #= true
          for (k <- 0 until p.beatsPerReg) {
            dut.io.access.valid #= true
            dut.io.access.address #= reg * p.beatsPerReg + k
            dut.io.access.mask #= (1 << p.beatBytes) - 1
            dut.io.access.data #= (bits >> (k * dlen)) & ((BigInt(1) << dlen) - 1)
            tick()
          }
          dut.io.access.valid #= false; dut.io.debug #= false
        }
        var cases = 0
        def finish(expected: BigInt): Unit = {
          var cycles = 0
          while (!dut.io.done.toBoolean && cycles < vlen + 20) {
            assert(!dut.io.write.valid.toBoolean, "scalar result wrote the VRF")
            tick(); cycles += 1
          }
          assert(dut.io.done.toBoolean, "scalar result timed out")
          assert(dut.io.result.toBigInt == (expected & xmask), s"value at done case $cases")
          tick()
          assert(!dut.io.done.toBoolean && !dut.io.busy.toBoolean)
          assert(dut.io.result.toBigInt == (expected & xmask), s"value after done case $cases")
          cases += 1
        }
        write(0, active)
        for (bits <- patterns; vl <- Seq(0, 1, dlen - 1, dlen, dlen + 1, vlen).filter(_ <= vlen).distinct;
             masked <- Seq(false, true); op <- Seq(VectorElementOp.CPOP, VectorElementOp.FIRST)) {
          write(8, bits)
          val selected = bits & ((BigInt(1) << vl) - 1) & (if (masked) active else all)
          val expected = if (op == VectorElementOp.CPOP) BigInt(selected.bitCount)
            else BigInt(if (selected == 0) -1 else selected.lowestSetBit)
          dut.io.useElement #= true
          val c = dut.io.element
          c.op #= op; c.vs2 #= 8; c.vl #= vl; c.unmasked #= !masked
          c.valid #= true; tick(); c.valid #= false
          finish(expected)
        }
        for (sew <- 0 to log2Up(elen / 8); vl <- Seq(0, 1);
             value <- Seq(BigInt(0), BigInt(1), BigInt(-1), -(BigInt(1) << ((8 << sew) - 1)), (BigInt(1) << ((8 << sew) - 1)) - 1)) {
          val width = 8 << sew
          write(3, value & ((BigInt(1) << width) - 1))
          dut.io.useElement #= false
          val c = dut.io.arithmetic
          c.shape #= VectorShape.SCALAR_READ; c.vs2 #= 3; c.vl #= vl; c.sew #= sew; c.unmasked #= true
          c.valid #= true; tick(); c.valid #= false
          finish(value)
        }
        println(s"Scalar result at done: RV$xlen ELEN$elen VLEN$vlen DLEN$dlen $cases cases")
      }
    }
  }
}
