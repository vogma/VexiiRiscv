package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import scala.util.Random

class VectorPermuteTest extends AnyFunSuite {
  import VectorElementOp._
  val ops = Seq(SLIDEUP, SLIDEDOWN, SLIDE1UP, SLIDE1DOWN, GATHER, GATHEREI16, COMPRESS)

  def run(vlen: Int, xlen: Int): Unit = {
    val p = VectorRegFileParam(vlen = vlen)
    SimConfig.compile(new VectorElementHarness(p, xlen)).doSim { dut =>
      val rng = new Random(1818 + vlen + xlen)
      val nbytes = vlen / 8
      def tick(): Unit = {
        dut.clockDomain.risingEdge(); sleep(5)
        dut.clockDomain.fallingEdge(); sleep(5)
      }
      dut.clockDomain.fallingEdge()
      val cmd = dut.io.cmd
      cmd.valid #= false
      cmd.payload.flatten.foreach {
        case b: Bool => b #= false
        case u: UInt => u #= 0
        case b: Bits => b #= 0
      }
      dut.io.debug #= true; dut.io.access.valid #= false
      dut.io.access.address #= 0; dut.io.access.data #= 0; dut.io.access.mask #= 0
      dut.io.readAddress #= 0
      dut.clockDomain.deassertReset(); sleep(1)
      dut.clockDomain.assertReset(); sleep(1); tick()
      dut.clockDomain.deassertReset()
      for (_ <- 0 until p.wordCount) tick()
      assert(!dut.io.initHold.toBoolean)
      var cases, writes = 0
      val crossings = scala.collection.mutable.Set[Int]()
      for (op <- ops; sew <- 0 to 2; lmul <- -2 to 3 if (8 << sew) <= 32 * scala.math.pow(2, lmul)
           if !(op == GATHEREI16 && lmul + 1 - sew > 3)) {
        val size = 1 << sew
        val max = (vlen / (8 << sew) * scala.math.pow(2, lmul)).toInt
        val vls = Seq(0, 1, 2, max / 2, max - 1, max).distinct.filter(_ <= max)
        for (vl <- vls; masked <- Seq(false, true) if op != COMPRESS || !masked; pattern <- 0 until 5) {
          val vector = op == GATHEREI16 || (op == GATHER && pattern % 2 == 0)
          val overlap = (op == SLIDEDOWN || op == SLIDE1DOWN) && pattern % 2 == 0
          val vd = if (!masked && pattern == 4 && !overlap) 0 else 8
          val vs2 = if (overlap) vd else 16
          val vs1 = if (op == COMPRESS) 31 else 24
          val scalar = (pattern match {
            case 0 => BigInt(0)
            case 1 => BigInt(1)
            case 2 => BigInt(max - 1)
            case 3 => BigInt(1) << (xlen - 1)
            case _ => (BigInt(1) << xlen) - 1
          })
          val bytes = Array.fill(32, nbytes)(rng.nextInt(256))
          def get(r: Int, i: Int, n: Int): BigInt = (0 until n).map { b =>
            val at = i * n + b
            BigInt(bytes(r + at / nbytes)(at % nbytes)) << (8 * b)
          }.reduce(_ | _)
          def set(dst: Array[Array[Int]], r: Int, i: Int, n: Int, value: BigInt): Unit = {
            for (b <- 0 until n) {
              val at = i * n + b
              dst(r + at / nbytes)(at % nbytes) = ((value >> (8 * b)) & 255).toInt
            }
          }
          def bit(r: Int, i: Int) = (bytes(r)(i / 8) & (1 << (i % 8))) != 0
          for (b <- 0 until nbytes) {
            bytes(0)(b) = if (pattern == 0) 0 else if (pattern == 1) 255 else bytes(0)(b)
            if (op == COMPRESS) bytes(vs1)(b) = if (pattern == 0) 0 else if (pattern == 1) 255 else bytes(vs1)(b)
          }
          val indexBytes = if (op == GATHEREI16) 2 else size
          if (vector) for (i <- 0 until max) {
            val value = Seq(BigInt(0), BigInt(max - 1), BigInt(max), BigInt(max + 1),
              (BigInt(1) << (indexBytes * 8)) - 1, BigInt(rng.nextInt(max)))(i % 6)
            set(bytes, vs1, i, indexBytes, value)
          }
          val expected = bytes.map(_.clone())
          var pos, expectedWrites = 0
          for (i <- 0 until vl if op == COMPRESS || !masked || bit(0, i)) {
            val at = op match {
              case SLIDEUP => BigInt(i) - scalar
              case SLIDEDOWN => BigInt(i) + scalar
              case SLIDE1UP => BigInt(i - 1)
              case SLIDE1DOWN => BigInt(i + 1)
              case GATHER | GATHEREI16 => if (vector) get(vs1, i, indexBytes) else scalar
              case COMPRESS => BigInt(i)
            }
            if ((op != SLIDEUP || at >= 0) && (op != COMPRESS || bit(vs1, i))) {
              val inserted = (op == SLIDE1UP && i == 0) || (op == SLIDE1DOWN && i == vl - 1)
              val value = if (inserted) scalar else if (at < 0 || at >= max) BigInt(0) else get(vs2, at.toInt, size)
              set(expected, vd, if (op == COMPRESS) pos else i, size, value)
              expectedWrites += 1
              if (op == COMPRESS) pos += 1
            }
          }
          dut.io.debug #= true
          for (r <- 0 until 32; k <- 0 until p.beatsPerReg) {
            dut.io.access.valid #= true; dut.io.access.address #= r * p.beatsPerReg + k
            dut.io.access.mask #= 255
            dut.io.access.data #= (0 until 8).map(b => BigInt(bytes(r)(k * 8 + b)) << (8 * b)).reduce(_ | _)
            tick()
          }
          dut.io.access.valid #= false; dut.io.debug #= false
          cmd.vd #= vd; cmd.vs2 #= vs2; cmd.vs1 #= vs1; cmd.vl #= vl; cmd.vlmax #= max
          cmd.sew #= sew; cmd.op #= op; cmd.scalar #= scalar; cmd.scalarOperand #= !vector
          cmd.unmasked #= !masked; cmd.valid #= true; tick(); cmd.valid #= false
          var cycles, written = 0
          while (!dut.io.done.toBoolean && cycles < vlen + 20) {
            sleep(1)
            assert(dut.io.busy.toBoolean)
            assert(!dut.io.divCommand.toBoolean)
            val reads = dut.io.readValid.toInt
            assert((reads & 4) == 0)
            if ((reads & 1) != 0) {
              assert(vector || op == COMPRESS)
              val a = dut.io.readAddresses(0).toInt
              val span = if (op == COMPRESS) nbytes else max * indexBytes
              assert(a >= vs1 * p.beatsPerReg && a < vs1 * p.beatsPerReg + (span + 7) / 8)
            }
            if ((reads & 2) != 0) {
              val a = dut.io.readAddresses(1).toInt
              assert(a >= vs2 * p.beatsPerReg && a < vs2 * p.beatsPerReg + (max * size + 7) / 8)
              if (a >= (vs2 + 1) * p.beatsPerReg) crossings += op
            }
            if (vl == 0) assert(reads == 0)
            if (dut.io.write.valid.toBoolean) {
              val a = dut.io.write.address.toInt
              val mask = dut.io.write.mask.toInt
              val data = dut.io.write.data.toBigInt
              assert(Integer.bitCount(mask) == size)
              for (b <- 0 until 8 if (mask & (1 << b)) != 0) {
                val r = a / p.beatsPerReg; val at = (a % p.beatsPerReg) * 8 + b
                assert(((data >> (8 * b)) & 255).toInt == expected(r)(at),
                  s"op=$op sew=$sew lmul=$lmul vl=$vl pattern=$pattern masked=$masked reg=$r byte=$at")
              }
              written += 1; writes += 1
            }
            tick(); cycles += 1
          }
          assert(dut.io.done.toBoolean, s"op=$op timeout")
          assert(written == expectedWrites, s"op=$op writes=$written != $expectedWrites")
          assert(!dut.io.saturated.toBoolean)
          tick(); assert(!dut.io.busy.toBoolean)
          dut.io.debug #= true
          for (r <- 0 until 32; k <- 0 until p.beatsPerReg) {
            dut.io.readAddress #= r * p.beatsPerReg + k; tick()
            val want = (0 until 8).map(b => BigInt(expected(r)(k * 8 + b)) << (8 * b)).reduce(_ | _)
            assert(dut.io.readData.toBigInt == want, s"op=$op full readback case=$cases reg=$r word=$k")
          }
          cases += 1
        }
      }
      assert(ops.forall(crossings.contains), s"group-crossing coverage $crossings")
      println(s"VectorPermute VLEN=$vlen XLEN=$xlen: $cases cases, $writes writes; full physical readback")
    }
  }
  test("permutations on RV32") { run(128, 32) }
  test("full XLEN offsets on RV64") { run(128, 64) }
  test("larger register groups") { run(256, 64) }
}
