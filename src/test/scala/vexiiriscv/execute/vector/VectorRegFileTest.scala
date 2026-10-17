package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.util.Random

// Compare reads, byte writes and the v0 shadow against an independent byte-array model.
class VectorRegFileTest extends AnyFunSuite {

  class Model(p: VectorRegFileParam) {
    val bytes = Array.fill(p.wordCount, p.beatBytes)(0)

    def write(address: Int, data: BigInt, mask: Int): Unit = {
      for (b <- 0 until p.beatBytes if ((mask >> b) & 1) == 1) {
        bytes(address)(b) = ((data >> (8 * b)) & 0xff).toInt
      }
    }

    def word(address: Int): BigInt =
      (0 until p.beatBytes).map(b => BigInt(bytes(address)(b)) << (8 * b)).reduce(_ | _)

    def v0: BigInt =
      (0 until p.beatsPerReg).map(beat => word(beat) << (beat * p.dlen)).reduce(_ | _)
  }

  def run(p: VectorRegFileParam, cycles: Int, seed: Long): Unit = {
    SimConfig.compile(new VectorRegFile(p)).doSim(seed.toInt) { dut =>
      val rand = new Random(seed)
      val model = new Model(p)
      val fullMask = (1 << p.beatBytes) - 1
      // rsp keeps its last value while the command is not valid
      val lastRsp = Array.fill(p.readPorts)(BigInt(-1))
      var checks = 0L
      var writes = 0L
      var partialWrites = 0L
      var v0Writes = 0L

      dut.clockDomain.fallingEdge()
      dut.io.write.valid #= false
      dut.io.write.address #= 0
      dut.io.write.data #= 0
      dut.io.write.mask #= 0
      for (port <- dut.io.reads) {
        port.cmd.valid #= false
        port.cmd.payload #= 0
      }

      def tick(): Unit = {
        dut.clockDomain.risingEdge()
        sleep(5)
        dut.clockDomain.fallingEdge()
        sleep(5)
      }

      def resetAndClear(): Unit = {
        dut.clockDomain.deassertReset()
        sleep(1)
        dut.clockDomain.assertReset()
        sleep(1)
        tick()
        dut.clockDomain.deassertReset()
        // Normal writes and reads must not interfere with clearing, including its last cycle.
        dut.io.write.valid #= true
        dut.io.write.address #= p.wordCount - 1
        dut.io.write.data #= (BigInt(1) << p.dlen) - 1
        dut.io.write.mask #= fullMask
        for (port <- dut.io.reads) {
          port.cmd.valid #= true
          port.cmd.payload #= p.wordCount - 1
        }
        sleep(1)
        for (a <- 0 until p.wordCount) {
          assert(dut.io.initHold.toBoolean, s"initHold dropped before clear word $a")
          assert(dut.io.writeEvent.valid.toBoolean)
          assert(dut.io.writeEvent.address.toInt == a)
          assert(dut.io.writeEvent.data.toBigInt == 0)
          assert(dut.io.writeEvent.mask.toInt == fullMask)
          tick()
        }
        assert(!dut.io.initHold.toBoolean, s"initHold lasted more than ${p.wordCount} cycles")
        assert(dut.io.v0.toBigInt == 0, "v0 shadow was not cleared")
        dut.io.write.valid #= false
        dut.io.reads.foreach(_.cmd.valid #= false)
        model.bytes.foreach(java.util.Arrays.fill(_, 0))
        lastRsp.indices.foreach(i => lastRsp(i) = BigInt(-1))
        sleep(1)
        assert(!dut.io.writeEvent.valid.toBoolean, "clear write continued after initHold")
      }
      resetAndClear()

      // One cycle: drive inputs, clock, then check against the model state
      // from before this cycle's write (read-first).
      def cycle(write: Option[(Int, BigInt, Int)], reads: Seq[Option[Int]]): Unit = {
        write match {
          case Some((a, d, m)) =>
            dut.io.write.valid #= true
            dut.io.write.address #= a
            dut.io.write.data #= d
            dut.io.write.mask #= m
          case None =>
            dut.io.write.valid #= false
            dut.io.write.address #= rand.nextInt(p.wordCount)
            dut.io.write.data #= BigInt(p.dlen, rand)
            dut.io.write.mask #= rand.nextInt(fullMask + 1)
        }
        for ((port, r) <- dut.io.reads.zip(reads)) r match {
          case Some(a) => port.cmd.valid #= true; port.cmd.payload #= a
          case None => port.cmd.valid #= false; port.cmd.payload #= rand.nextInt(p.wordCount)
        }
        val expected = reads.zipWithIndex.map {
          case (Some(a), i) => lastRsp(i) = model.word(a); lastRsp(i)
          case (None, i) => lastRsp(i)
        }
        write.foreach { case (a, d, m) =>
          model.write(a, d, m)
          writes += 1
          if (m != fullMask) partialWrites += 1
          if (a < p.beatsPerReg) v0Writes += 1
        }

        sleep(1)
        assert(!dut.io.initHold.toBoolean, "initializer restarted without reset")
        assert(dut.io.writeEvent.valid.toBoolean == write.isDefined)
        write.foreach { case (a, d, m) =>
          assert(dut.io.writeEvent.address.toInt == a)
          assert(dut.io.writeEvent.data.toBigInt == d)
          assert(dut.io.writeEvent.mask.toInt == m)
        }
        tick()

        for (i <- 0 until p.readPorts if expected(i) >= 0) {
          val got = dut.io.reads(i).rsp.toBigInt
          assert(got == expected(i), f"read port $i: got 0x$got%x, expected 0x${expected(i)}%x")
          checks += 1
        }
        // The shadow is a register: after the edge it includes this cycle's write.
        val gotV0 = dut.io.v0.toBigInt
        assert(gotV0 == model.v0, f"v0 shadow: got 0x$gotV0%x, expected 0x${model.v0}%x")
        checks += 1
      }

      def randomData = BigInt(p.dlen, rand)
      def noReads = Seq.fill(p.readPorts)(None)

      // Read every cleared word through all ports before any software write.
      for (a <- 0 until p.wordCount) cycle(None, Seq.fill(p.readPorts)(Some(a)))
      for (a <- 0 until p.wordCount) cycle(Some((a, randomData, fullMask)), noReads)

      // 2. Directed byte-enable cases on v0 and on an ordinary register.
      val last = p.wordCount - 1
      val directed = Seq(
        (0, 0),                    // valid write, empty mask: no change
        (0, 1),                    // lowest byte of v0
        (0, 1 << (p.beatBytes - 1)), // highest byte of v0 beat 0
        (p.beatsPerReg - 1, 0x5 & fullMask), // last beat of v0, bytes 0 and 2
        (p.beatsPerReg, fullMask), // v1 beat 0: must not touch the shadow
        (last, 0xA & fullMask)     // v31, last beat
      )
      for ((a, m) <- directed) {
        cycle(Some((a, randomData, m)), Seq.tabulate(p.readPorts)(i => Some(if (i == 0) a else rand.nextInt(p.wordCount))))
        cycle(None, Seq.fill(p.readPorts)(Some(a)))
      }

      // 3. Read-first: write and read the same word in one cycle.
      for (a <- Seq(0, p.beatsPerReg, last)) {
        cycle(Some((a, randomData, fullMask)), Seq.fill(p.readPorts)(Some(a)))
        cycle(None, Seq.fill(p.readPorts)(Some(a)))
      }

      // 4. Random traffic.
      for (_ <- 0 until cycles) {
        val w = if (rand.nextInt(10) < 7) {
          val a = if (rand.nextInt(4) == 0) rand.nextInt(p.beatsPerReg) else rand.nextInt(p.wordCount)
          val m = rand.nextInt(4) match {
            case 0 => fullMask
            case 1 => 1 << rand.nextInt(p.beatBytes)
            case _ => rand.nextInt(fullMask + 1)
          }
          Some((a, randomData, m))
        } else None
        val reads = Seq.fill(p.readPorts)(if (rand.nextInt(10) < 8) Some(rand.nextInt(p.wordCount)) else None)
        cycle(w, reads)
      }

      // 5. Read back every word.
      for (a <- 0 until p.wordCount) cycle(None, Seq.fill(p.readPorts)(Some(a)))

      // Clear nonzero state again and check every word and every held read response.
      resetAndClear()
      for (a <- 0 until p.wordCount) cycle(None, Seq.fill(p.readPorts)(Some(a)))
      for (_ <- 0 until 4) cycle(None, noReads)

      println(s"[VectorRegFileTest] VLEN=${p.vlen} DLEN=${p.dlen} readPorts=${p.readPorts}: " +
        s"$writes writes ($partialWrites partial, $v0Writes to v0), $checks checks passed")
    }
  }

  test("VLEN=128 DLEN=64 3R1W (chosen)") {
    run(VectorRegFileParam(vlen = 128, dlen = 64, readPorts = 3), cycles = 20000, seed = 1)
  }
  test("VLEN=128 DLEN=64 2R1W") {
    run(VectorRegFileParam(vlen = 128, dlen = 64, readPorts = 2), cycles = 5000, seed = 2)
  }
  test("VLEN=256 DLEN=64 3R1W") {
    run(VectorRegFileParam(vlen = 256, dlen = 64, readPorts = 3), cycles = 5000, seed = 3)
  }
  test("VLEN=128 DLEN=128 3R1W") {
    run(VectorRegFileParam(vlen = 128, dlen = 128, readPorts = 3), cycles = 5000, seed = 4)
  }
}
