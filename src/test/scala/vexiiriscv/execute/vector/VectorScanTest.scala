package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random

class VectorScanHarness(p: VectorRegFileParam, xlen: Int) extends Component {
  val io = new Bundle {
    val arithmetic = slave(Flow(VectorArithmeticCmd(p, xlen)))
    val element = slave(Flow(VectorElementCmd(p, xlen)))
    val useElement, debug = in Bool()
    val access = slave(Flow(VrfWriteCmd(p)))
    val readAddress = in UInt(p.addressWidth bits)
    val readData = out Bits(64 bits)
    val result = out Bits(xlen bits)
    val done, busy, saturated, initHold = out Bool()
    val write = master(Flow(VrfWriteCmd(p)))
    val reads = out Bits(3 bits)
    val addresses = out Vec(UInt(p.addressWidth bits), 3)
  }
  val vrf = new VectorRegFile(p)
  val a = new VectorArithmetic(p, xlen)
  val e = new VectorElementUnit(p, xlen)
  a.io.cmd := io.arithmetic
  e.io.cmd := io.element
  a.io.v0 := vrf.io.v0
  e.io.v0 := vrf.io.v0
  vrf.io.write := a.io.write
  when(io.useElement) { vrf.io.write := e.io.write }
  io.write := vrf.io.write
  when(io.debug) { vrf.io.write := io.access }
  for (i <- 0 until 3) {
    vrf.io.reads(i).cmd := a.io.reads(i).cmd
    when(io.useElement) { vrf.io.reads(i).cmd := e.io.reads(i).cmd }
    a.io.reads(i).rsp := vrf.io.reads(i).rsp
    e.io.reads(i).rsp := vrf.io.reads(i).rsp
    io.reads(i) := vrf.io.reads(i).cmd.valid
    io.addresses(i) := vrf.io.reads(i).cmd.payload
  }
  when(io.debug) {
    vrf.io.reads(0).cmd.valid := True
    vrf.io.reads(0).cmd.payload := io.readAddress
  }
  io.readData := vrf.io.reads(0).rsp
  io.result := e.io.result
  io.done := io.useElement ? e.io.done | a.io.done
  io.busy := io.useElement ? e.io.busy | a.io.busy
  io.saturated := io.useElement ? e.io.saturated | a.io.saturated
  io.initHold := vrf.io.initHold
}

class VectorScanTest extends AnyFunSuite {
  def run(vlen: Int, xlen: Int): Unit = {
    val p = VectorRegFileParam(vlen = vlen)
    SimConfig.compile(new VectorScanHarness(p, xlen)).doSim { dut =>
      val rng = new Random(1717 + vlen + xlen)
      val nbytes = vlen / 8
      def tick(): Unit = {
        dut.clockDomain.risingEdge(); sleep(5)
        dut.clockDomain.fallingEdge(); sleep(5)
      }
      dut.clockDomain.fallingEdge()
      dut.io.arithmetic.valid #= false; dut.io.element.valid #= false
      dut.io.arithmetic.payload.flatten.foreach {
        case b: Bool => b #= false
        case u: UInt => u #= 0
        case b: Bits => b #= 0
      }
      dut.io.element.payload.flatten.foreach {
        case b: Bool => b #= false
        case u: UInt => u #= 0
        case b: Bits => b #= 0
      }
      dut.io.useElement #= false; dut.io.debug #= true
      dut.io.access.valid #= false; dut.io.access.address #= 0
      dut.io.access.data #= 0; dut.io.access.mask #= 0; dut.io.readAddress #= 0
      dut.clockDomain.assertReset(); tick(); dut.clockDomain.deassertReset()
      for (_ <- 0 until p.wordCount) tick()
      assert(!dut.io.initHold.toBoolean)
      val names = Seq("vcpop", "vfirst", "viota", "vmsbf", "vmsif", "vmsof")
      var cases, writes = 0
      val crossed = scala.collection.mutable.Set[String]()
      for (name <- names; sew <- 0 to 2; lmul <- -2 to 3 if (8 << sew) <= 32 * scala.math.pow(2, lmul)) {
        val max = ((vlen / (8 << sew)) * scala.math.pow(2, lmul)).toInt
        for (vl <- Seq(0, 1, 63, 64, 65, max - 1, max).filter(_ <= max).distinct;
             masked <- Seq(false, true); pattern <- 0 until 5) {
          val scalar = name == "vcpop" || name == "vfirst"
          val element = scalar || name == "viota"
          val vd = if (scalar) 9 else if (!masked && pattern == 4) 0 else 8
          val vs2 = if (pattern == 3) 0 else 23 // legal unaligned mask source at any LMUL
          val bytes = Array.fill(32, nbytes)(rng.nextInt(256))
          for (b <- 0 until nbytes) {
            bytes(0)(b) = if (pattern == 0) 0 else if (pattern == 1) 255 else rng.nextInt(256)
            bytes(vs2)(b) = if (pattern == 0) 0 else if (pattern == 1) 255 else if (pattern == 2) 0 else bytes(vs2)(b)
          }
          if (pattern == 2) bytes(vs2)(nbytes - 1) = 128 // only the final mask bit is set
          def word(r: Int, k: Int): BigInt = (0 until 8).map(b => BigInt(bytes(r)(k * 8 + b)) << (8 * b)).reduce(_ | _)
          dut.io.debug #= true
          for (r <- 0 until 32; k <- 0 until p.beatsPerReg) {
            dut.io.access.valid #= true; dut.io.access.address #= r * p.beatsPerReg + k
            dut.io.access.mask #= 255; dut.io.access.data #= word(r, k); tick()
          }
          dut.io.access.valid #= false
          val expected = bytes.map(_.clone())
          def bit(r: Int, i: Int) = ((bytes(r)(i / 8) >> (i % 8)) & 1) != 0
          var count = 0
          var first = -1
          for (i <- 0 until vl if !masked || bit(0, i)) {
            val hit = bit(vs2, i)
            if (!scalar) {
              if (element) {
                val size = 1 << sew
                for (b <- 0 until size) {
                  val at = i * size + b
                  expected(vd + at / nbytes)(at % nbytes) = (count >> (8 * b)) & 255
                }
              } else {
                val value = name match {
                  case "vmsbf" => first == -1 && !hit
                  case "vmsif" => first == -1
                  case "vmsof" => first == -1 && hit
                }
                val old = expected(vd)(i / 8)
                expected(vd)(i / 8) = (old & ~(1 << (i % 8))) | (if (value) 1 << (i % 8) else 0)
              }
            }
            if (hit) { count += 1; if (first == -1) first = i }
          }
          dut.io.debug #= false; dut.io.useElement #= element
          val a = dut.io.arithmetic; val e = dut.io.element
          a.vd #= vd; a.vs2 #= vs2; a.vs1 #= 31; a.vl #= vl; a.sew #= sew
          a.shape #= VectorShape.MASK_MM; a.unmasked #= !masked
          a.op #= (name match { case "vmsbf" => VectorAluOp.MSBF; case "vmsif" => VectorAluOp.MSIF; case _ => VectorAluOp.MSOF })
          e.vd #= vd; e.vs2 #= vs2; e.vs1 #= 31; e.vl #= vl; e.sew #= sew; e.unmasked #= !masked
          e.op #= (name match { case "vcpop" => VectorElementOp.CPOP; case "vfirst" => VectorElementOp.FIRST; case _ => VectorElementOp.IOTA })
          a.valid #= !element; e.valid #= element; tick()
          a.valid #= false; e.valid #= false
          var cycles, readCount, writeCount = 0
          while (!dut.io.done.toBoolean && cycles < vlen + 20) {
            sleep(1)
            assert(dut.io.busy.toBoolean)
            val reads = dut.io.reads.toInt
            assert((reads & 2) == 0, s"$name unexpectedly reads port B")
            if (scalar) assert((reads & 4) == 0, s"$name unexpectedly reads vd")
            if ((reads & 1) != 0) {
              val address = dut.io.addresses(0).toInt
              assert(address >= vs2 * p.beatsPerReg && address < (vs2 + 1) * p.beatsPerReg)
              readCount += 1
              if (address > vs2 * p.beatsPerReg) crossed += name
            }
            if (dut.io.write.valid.toBoolean) {
              assert(!scalar)
              val address = dut.io.write.address.toInt
              val mask = dut.io.write.mask.toInt
              val data = dut.io.write.data.toBigInt
              for (b <- 0 until 8 if (mask & (1 << b)) != 0) {
                val reg = address / p.beatsPerReg
                val at = (address % p.beatsPerReg) * 8 + b
                assert(((data >> (8 * b)) & 255).toInt == expected(reg)(at), s"$name case=$cases reg=$reg byte=$at")
              }
              writeCount += 1; writes += 1
            }
            tick(); cycles += 1
          }
          assert(dut.io.done.toBoolean, s"$name timeout vl=$vl")
          assert(!dut.io.saturated.toBoolean)
          if (vl == 0) assert(readCount == 0 && writeCount == 0)
          if (scalar) {
            val value = if (name == "vcpop") BigInt(count) else BigInt(first) & ((BigInt(1) << xlen) - 1)
            assert(dut.io.result.toBigInt == value, s"$name scalar ${dut.io.result.toBigInt} != $value")
          }
          tick(); assert(!dut.io.busy.toBoolean)
          dut.io.debug #= true
          for (r <- 0 until 32; k <- 0 until p.beatsPerReg) {
            dut.io.readAddress #= r * p.beatsPerReg + k; tick()
            val want = (0 until 8).map(b => BigInt(expected(r)(k * 8 + b)) << (8 * b)).reduce(_ | _)
            assert(dut.io.readData.toBigInt == want, s"$name full readback case=$cases reg=$r word=$k")
          }
          cases += 1
        }
      }
      assert(names.forall(crossed.contains), s"mask-word coverage $crossed")
      println(s"VectorScan VLEN=$vlen XLEN=$xlen: $cases cases, $writes writes; all six forms cross mask words; full physical readback")
    }
  }
  test("mask scans on RV32") { run(128, 32) }
  test("scalar results on RV64") { run(128, 64) }
  test("multiple mask words and counter width") { run(256, 64) }
}
