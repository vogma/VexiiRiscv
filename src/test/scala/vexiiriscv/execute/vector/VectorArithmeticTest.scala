package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class VectorArithmeticHarness(p: VectorRegFileParam, xlen: Int) extends Component {
  val io = new Bundle {
    val cmd = slave(Flow(VectorArithmeticCmd(p, xlen)))
    val debug = in Bool()
    val access = slave(Flow(VrfWriteCmd(p)))
    val readAddress = in UInt(p.addressWidth bits)
    val readData = out Bits(64 bits)
    val initHold, done = out Bool()
    val result = out Bits(xlen bits)
    val write = master(Flow(VrfWriteCmd(p)))
    val readValid = out Bits(2 bits)
    val readAddresses = out Vec(UInt(p.addressWidth bits), 2)
  }
  val vrf = new VectorRegFile(p)
  val arithmetic = new VectorArithmetic(p, xlen)
  arithmetic.io.cmd := io.cmd
  arithmetic.io.v0 := vrf.io.v0
  vrf.io.write := arithmetic.io.write
  when(io.debug) { vrf.io.write := io.access }
  for (i <- 0 until 2) {
    vrf.io.reads(i).cmd := arithmetic.io.reads(i).cmd
    arithmetic.io.reads(i).rsp := vrf.io.reads(i).rsp
    io.readValid(i) := arithmetic.io.reads(i).cmd.valid
    io.readAddresses(i) := arithmetic.io.reads(i).cmd.payload
  }
  when(io.debug) {
    vrf.io.reads(0).cmd.valid := True
    vrf.io.reads(0).cmd.payload := io.readAddress
  }
  vrf.io.reads(2).cmd.valid := False
  vrf.io.reads(2).cmd.payload := 0
  io.readData := vrf.io.reads(0).rsp
  io.initHold := vrf.io.initHold
  io.done := arithmetic.io.done
  io.result := arithmetic.io.result
  io.write := arithmetic.io.write
}

class VectorArithmeticTest extends AnyFunSuite {
  def run(vlen: Int, xlen: Int): Unit = {
    val p = VectorRegFileParam(vlen = vlen)
    SimConfig.compile(new VectorArithmeticHarness(p, xlen)).doSim { dut =>
      val random = new Random(901 + xlen + vlen)
      val bytes = Array.fill(32, vlen / 8)(random.nextInt(256))
      val xmask = (BigInt(1) << xlen) - 1
      def word(reg: Int, offset: Int): BigInt =
        (0 until 8).map(b => BigInt(bytes(reg)(offset + b)) << (b * 8)).reduce(_ | _)
      def tick(): Unit = {
        dut.clockDomain.risingEdge(); sleep(5)
        dut.clockDomain.fallingEdge(); sleep(5)
      }
      dut.clockDomain.fallingEdge()
      dut.io.cmd.valid #= false
      dut.io.cmd.vd #= 0; dut.io.cmd.vs1 #= 0; dut.io.cmd.vs2 #= 0
      dut.io.cmd.vl #= 0; dut.io.cmd.vstart #= 0; dut.io.cmd.sew #= 0; dut.io.cmd.op #= 0
      dut.io.cmd.scalar #= 0; dut.io.cmd.scalarOperand #= false; dut.io.cmd.unmasked #= true
      dut.io.cmd.scalarRead #= false; dut.io.cmd.scalarWrite #= false
      dut.io.debug #= true
      dut.io.access.valid #= false
      dut.io.access.address #= 0; dut.io.access.data #= 0; dut.io.access.mask #= 0
      dut.io.readAddress #= 0
      dut.clockDomain.deassertReset(); sleep(1)
      dut.clockDomain.assertReset(); sleep(1); tick()
      dut.clockDomain.deassertReset()
      for (_ <- 0 until p.wordCount) tick()
      assert(!dut.io.initHold.toBoolean)
      // Distinct data in every byte exposes addressing, overlap and lane carry errors.
      for (a <- 0 until p.wordCount) {
        dut.io.access.valid #= true
        dut.io.access.address #= a
        dut.io.access.mask #= 255
        dut.io.access.data #= word(a / p.beatsPerReg, (a % p.beatsPerReg) * 8)
        tick()
      }
      dut.io.access.valid #= false
      var cases = 0
      var writes = 0
      def execute(op: Int, sew: Int, lmul: Int, vl: Int, start: Int, masked: Boolean,
                  scalarOperand: Boolean, scalarRead: Boolean = false, scalarWrite: Boolean = false,
                  overlap: Boolean = false): Unit = {
        val width = 8 << sew
        val size = width / 8
        val mask = (BigInt(1) << width) - 1
        val vd = if (overlap) 8 else 16
        val vs1 = if (scalarRead || scalarWrite) 11 else 8
        val vs2 = if (scalarRead) 13 else 16
        val scalar = BigInt(xlen, random)
        def element(reg: Int, i: Int): BigInt = {
          val index = i * size
          val r = reg + index / (vlen / 8)
          val offset = index % (vlen / 8)
          (0 until size).map(b => BigInt(bytes(r)(offset + b)) << (8 * b)).reduce(_ | _)
        }
        val expectedResult = if (scalarRead) {
          val raw = element(vs2, 0)
          (if (raw.testBit(width - 1)) raw - (BigInt(1) << width) else raw) & xmask
        } else BigInt(0)
        val expected = ArrayBuffer[(Int, BigInt, Int)]()
        val expectedReads = ArrayBuffer[Int]()
        if (!scalarRead) {
          val updates = ArrayBuffer[(Int, BigInt)]()
          for (i <- 0 until vl if i >= start && (!scalarWrite || i == 0) &&
            (!masked || ((bytes(0)(i / 8) >> (i % 8)) & 1) != 0)) {
            val a = element(vs2, i)
            val b = if (scalarOperand) scalar & mask else element(vs1, i)
            val value = (op match {
              case 0 => a + b
              case 1 => a - b
              case 2 => b - a
              case 3 => a & b
              case 4 => a | b
              case 5 => a ^ b
              case 6 => b
            }) & mask
            updates += i -> value
          }
          val perBeat = updates.groupBy(_._1 * size / 8).toSeq.sortBy(_._1)
          for ((beat, entries) <- perBeat) {
            val address = vd * p.beatsPerReg + beat
            var byteMask = 0
            var data = BigInt(0)
            for ((i, value) <- entries; b <- 0 until size) {
              val byte = i * size + b
              val pos = byte % 8
              val valueByte = ((value >> (8 * b)) & 255).toInt
              byteMask |= 1 << pos
              data |= BigInt(valueByte) << (8 * pos)
              bytes(vd + byte / (vlen / 8))(byte % (vlen / 8)) = valueByte
            }
            expected += ((address, data, byteMask))
            expectedReads += beat
          }
        } else expectedReads += 0
        dut.io.debug #= false
        dut.io.cmd.valid #= true
        val cmd = dut.io.cmd
        cmd.vd #= vd; cmd.vs1 #= vs1; cmd.vs2 #= vs2
        cmd.vl #= vl; cmd.vstart #= start; cmd.sew #= sew; cmd.op #= op
        cmd.scalar #= scalar; cmd.scalarOperand #= scalarOperand
        cmd.unmasked #= !masked; cmd.scalarRead #= scalarRead; cmd.scalarWrite #= scalarWrite
        tick()
        cmd.valid #= false
        var cycles = 0
        var writeIndex = 0
        val actualReads = ArrayBuffer[Int]()
        var previousWrite = -1
        while (!dut.io.done.toBoolean && cycles < 50) {
          sleep(1)
          val rv = dut.io.readValid.toInt
          val readVs2 = scalarRead || !scalarWrite && op != VectorAluOp.MOVE
          val readVs1 = !scalarOperand && !scalarRead
          val expectedValid = (if (readVs2) 1 else 0) | (if (readVs1) 2 else 0)
          if (rv != 0) {
            assert(rv == expectedValid)
            val port = if (readVs2) 0 else 1
            val base = if (readVs2) vs2 else vs1
            val beat = dut.io.readAddresses(port).toInt - base * p.beatsPerReg
            actualReads += beat
            if (readVs2 && readVs1) assert(dut.io.readAddresses(1).toInt == vs1 * p.beatsPerReg + beat)
          }
          if (dut.io.write.valid.toBoolean) {
            assert(writeIndex < expected.size, s"unexpected write in case $cases")
            val (address, data, byteMask) = expected(writeIndex)
            assert(dut.io.write.address.toInt == address, s"address case $cases")
            assert(dut.io.write.mask.toInt == byteMask, s"byte enables case $cases")
            val bitMask = (0 until 8).filter(b => (byteMask & (1 << b)) != 0).map(b => BigInt(255) << (8 * b)).reduce(_ | _)
            assert((dut.io.write.data.toBigInt & bitMask) == data, s"data case $cases op $op sew $sew")
            if (!masked && previousWrite >= 0) assert(cycles == previousWrite + 1, "active beats must stream every cycle")
            previousWrite = cycles
            writeIndex += 1; writes += 1
          }
          tick(); cycles += 1
        }
        assert(dut.io.done.toBoolean, s"timeout case $cases")
        assert(writeIndex == expected.size)
        if (scalarRead) assert(dut.io.result.toBigInt == expectedResult)
        if (!scalarOperand || scalarRead || op != VectorAluOp.MOVE)
          assert(actualReads.toSeq == expectedReads.toSeq, s"read issue/skip case $cases: $actualReads vs $expectedReads")
        // Check all physical words, including untouched bytes and registers.
        dut.io.debug #= true
        for (a <- 0 until p.wordCount) {
          dut.io.readAddress #= a; tick()
          assert(dut.io.readData.toBigInt == word(a / p.beatsPerReg, (a % p.beatsPerReg) * 8), s"readback case $cases word $a")
        }
        cases += 1
      }
      for (sew <- 0 to 2; lmul <- -2 to 3 if (8 << sew) <= 32 * scala.math.pow(2, lmul)) {
        val vlmax = ((vlen / (8 << sew)) * scala.math.pow(2, lmul)).toInt
        for (op <- 0 to 6; scalar <- Seq(false, true); vl <- Seq(0, 1, vlmax - 1, vlmax).distinct; masked <- Seq(false, true))
          execute(op, sew, lmul, vl, 0, masked, scalar, overlap = random.nextBoolean())
        for (start <- Seq(1, vlmax - 1, vlmax, scala.math.min(vlen - 1, vlmax + 3)).distinct.filter(_ < vlen))
          execute(0, sew, lmul, vlmax, start, true, false)
      }
      for (sew <- 0 to 2; vl <- Seq(0, 1, 3); read <- Seq(false, true))
        execute(VectorAluOp.MOVE, sew, 3, vl, 0, false, true, scalarRead = read, scalarWrite = !read)
      println(s"VectorArithmetic VLEN=$vlen XLEN=$xlen: $cases cases, $writes writes, full physical readback after every case")
    }
  }
  test("64-bit beats and byte preservation on RV32") { run(128, 32) }
  test("scalar truncation and sign extension on RV64") { run(128, 64) }
  test("larger vector register addressing") { run(256, 64) }
}
