package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.collection.mutable

// Queue, beat unit and register file with ordinary and multiply commands sharing the issue stream
class VectorClassOverlapHarness(p: VectorRegFileParam, xlen: Int, elen: Int, depth: Int, chain: Boolean, classOverlap: Boolean) extends Component {
  val io = new Bundle {
    val cmd = slave(Flow(VectorArithmeticCmd(p, xlen, true, true, true)))
    val reads, writes = in Bits(32 bits)
    val load = slave(Flow(VrfWriteCmd(p)))
    val full, idle, initHold = out Bool()
    val read = slave(VrfReadPort(p))
    val issued, completed, saturated = out Bool()
    val issueTag, doneTag = out UInt(3 bits)
  }
  val q = new VectorQueue(p, xlen, depth, true, true, true, true, chain, classOverlap)
  val a = new VectorArithmetic(p, xlen, elen, true, true, true, beatMul = true, chaining = chain, classOverlap = classOverlap)
  val v = new VectorRegFile(p)
  q.push.valid := io.cmd.valid
  q.push.element := False; q.push.elem.assignFromBits(B(0, q.push.elem.getBitsWidth bits))
  q.push.arith := io.cmd.payload; q.push.reads := io.reads; q.push.writes := io.writes
  q.done := a.io.done; q.beatReady := a.io.ready
  if (classOverlap) q.beatTailMultiply := a.io.tailMultiply
  a.io.cmd.valid := q.pop; a.io.cmd.payload := q.head.arith; a.io.tag := q.head.tag
  a.io.v0 := v.io.v0
  v.io.write := a.io.write
  when(io.load.valid) { v.io.write := io.load }
  for (i <- 0 until 3) v.io.reads(i).cmd := a.io.reads(i).cmd
  when(q.idle) { v.io.reads(0).cmd := io.read.cmd }
  io.read.rsp := v.io.reads(0).rsp
  if (chain) {
    q.push.shadow := False
    q.words.progress := a.io.progress; q.words.consumer := a.io.readTag; q.words.reads := a.io.candidates
    a.io.sourceReady := q.words.ready
    val f = new VectorReadForwarding(p)
    f.io.write := v.io.writeEvent
    f.io.older := q.words.older(a.io.writeTag, a.io.readTag) && !v.io.initHold
    for (i <- 0 until 3) {
      f.io.reads(i).cmd := a.io.reads(i).cmd; f.io.raw(i) := v.io.reads(i).rsp
      a.io.reads(i).rsp := f.io.reads(i).rsp
    }
  } else for (i <- 0 until 3) a.io.reads(i).rsp := v.io.reads(i).rsp
  io.full := q.full; io.idle := q.idle; io.initHold := v.io.initHold
  io.issued := a.io.reads.map(_.cmd.valid).orR
  io.completed := a.io.done; io.saturated := a.io.saturated
  io.issueTag := RegNextWhen(q.head.tag, q.pop) init(0); io.doneTag := a.io.doneTag
}

class VectorClassOverlapTest extends AnyFunSuite {
  val ADD = 0; val MUL = 1; val MACC = 2; val SMUL = 3; val SADD = 4
  for ((xlen, elen, vlen, dlen, depth) <- Seq((32, 32, 128, 64, 1), (64, 64, 128, 64, 3), (64, 64, 256, 128, 3), (64, 64, 256, 128, 4));
       chain <- Seq(false, true))
    test(s"class overlap XLEN=$xlen ELEN=$elen VLEN=$vlen DLEN=$dlen depth=$depth chain=$chain") {
      val p = VectorRegFileParam(vlen, dlen)
      SimConfig.workspacePath(s"build/class-overlap/unit-$vlen-$xlen-$elen-$dlen-$depth-$chain")
        .compile(new VectorClassOverlapHarness(p, xlen, elen, depth, chain, true)).doSim { d =>
        d.clockDomain.forkStimulus(10)
        d.io.cmd.valid #= false; d.io.load.valid #= false; d.io.read.cmd.valid #= false
        d.io.load.address #= 0; d.io.load.data #= 0; d.io.load.mask #= 0
        d.io.read.cmd.payload #= 0; d.io.reads #= 0; d.io.writes #= 0
        val first = mutable.Map[Int, Int](); val done = mutable.Map[Int, Int]()
        val seen = mutable.Map[Int, Int]() // first issue per tag within one spacing batch
        var cycle = 0; var finishes = 0
        val pending = mutable.Queue[(Int, Boolean)]() // tag, expected saturation, in completion order
        def tick(): Unit = {
          sleep(1)
          if (d.io.issued.toBoolean && !first.contains(d.io.issueTag.toInt)) first(d.io.issueTag.toInt) = cycle
          if (d.io.issued.toBoolean) seen.getOrElseUpdate(d.io.issueTag.toInt, cycle)
          if (d.io.completed.toBoolean) {
            val (tag, sat) = pending.dequeue()
            assert(d.io.doneTag.toInt == tag, s"done tag ${d.io.doneTag.toInt}, expected $tag")
            assert(d.io.saturated.toBoolean == sat, s"saturation of tag $tag at cycle $cycle")
            done(tag) = cycle; first.remove(tag); finishes += 1
          }
          d.clockDomain.waitSampling(); sleep(1); cycle += 1; assert(cycle < 60000)
        }
        for (_ <- 0 until 5) tick()
        while (d.io.initHold.toBoolean) tick()
        val bytes = Array.tabulate(32 * vlen / 8)(i => (i * 53 + 11) & 255)
        for (r <- Seq(4, 5)) for (i <- 0 until vlen / 8) bytes(r * vlen / 8 + i) = if (i % 4 == 3) 0x80 else 0
        def get(reg: Int, i: Int, w: Int): BigInt = (0 until w / 8).map(j => BigInt(bytes(reg * vlen / 8 + i * w / 8 + j)) << (8 * j)).reduce(_ | _)
        def put(reg: Int, i: Int, w: Int, v: BigInt): Unit = for (j <- 0 until w / 8) bytes(reg * vlen / 8 + i * w / 8 + j) = ((v >> (8 * j)) & 255).toInt
        def sx(v: BigInt, w: Int): BigInt = { val m = v & ((BigInt(1) << w) - 1); if (m.testBit(w - 1)) m - (BigInt(1) << w) else m }
        for (at <- 0 until p.wordCount) {
          d.io.load.valid #= true; d.io.load.address #= at; d.io.load.mask #= (BigInt(1) << p.beatBytes) - 1
          d.io.load.data #= (0 until p.beatBytes).map(j => BigInt(bytes(at * p.beatBytes + j)) << (8 * j)).reduce(_ | _); tick()
        }
        d.io.load.valid #= false
        var tag = 0
        val order = mutable.ArrayBuffer[(Int, Int)]() // tag, kind
        def group(reg: Int, vl: Int, w: Int): BigInt = ((BigInt(1) << ((vl * w + vlen - 1) / vlen).max(1)) - 1) << reg
        def send(kind: Int, vd: Int, vs2: Int, w: Int, vl: Int, s: BigInt): Unit = {
          while (d.io.full.toBoolean) tick()
          val c = d.io.cmd; c.valid #= true; c.vd #= vd; c.vs2 #= vs2; c.vs1 #= 0; c.vl #= vl; c.vstart #= 0; c.sew #= log2Up(w / 8)
          c.shape #= VectorShape.SINGLE; c.op #= VectorAluOp.ADD; c.scalar #= s & ((BigInt(1) << xlen) - 1); c.scalarOperand #= true
          c.unmasked #= true; c.maskData #= false; c.signed #= kind == SADD || kind == SMUL; c.saturate #= kind == SADD
          c.vxrm #= 0; c.nreg #= 1; c.vlmax #= 8 * vlen / w; c.slide #= 0; c.compress #= false
          val multiply = kind == MUL || kind == MACC || kind == SMUL
          c.multiply #= multiply; c.signedVs1 #= kind == SMUL; c.negate #= false
          c.mulOp #= (if (kind == MUL) VectorElementOp.MUL else if (kind == MACC) VectorElementOp.MACC else VectorElementOp.SMUL)
          d.io.reads #= group(vs2, vl, w) | (if (kind == MACC) group(vd, vl, w) else BigInt(0))
          d.io.writes #= group(vd, vl, w) & ((BigInt(1) << 32) - 1)
          var sat = false
          val olds = (0 until vl).map(i => (get(vs2, i, w), get(vd, i, w)))
          for (i <- 0 until vl) {
            val (a, old) = olds(i)
            val value = kind match {
              case ADD => a + s
              case MUL => a * s
              case MACC => old + a * s
              case SADD =>
                val r = sx(a, w) + sx(s, w); val c = r.max(-(BigInt(1) << (w - 1))).min((BigInt(1) << (w - 1)) - 1)
                if (c != r) sat = true; c
              case SMUL =>
                val prod = sx(a, w) * sx(s, w); val r = (prod >> (w - 1)) + ((prod >> (w - 2)) & 1)
                if (r > (BigInt(1) << (w - 1)) - 1) { sat = true; (BigInt(1) << (w - 1)) - 1 } else r
            }
            put(vd, i, w, value)
          }
          pending.enqueue((tag, sat)); order += ((tag, kind)); tag = (tag + 1) % 8
          tick(); c.valid #= false
        }
        def isMul(k: Int) = k == MUL || k == MACC || k == SMUL
        val words = dlen / 8
        def drain(): Unit = while (!d.io.idle.toBoolean) tick()
        // Independent single-word commands alternating classes: exact issue spacing and overlap
        val spacing = mutable.ArrayBuffer[(Int, Int, Int, Int)]()
        for (w <- Seq(8, 16, 32, 64) if w <= elen) {
          drain(); seen.clear()
          val cycles = mutable.ArrayBuffer[(Int, Int)]()
          for (k <- 0 until 8) {
            if (k % 2 == 0) send(MUL, 24, 8, w, dlen / w, 3) else send(ADD, 16, 9, w, dlen / w, 1)
            cycles += ((order.last._1, order.last._2))
          }
          drain()
          for (((ta, ka), (tb, kb)) <- cycles.zip(cycles.tail).drop(2) if seen.contains(ta) && seen.contains(tb))
            spacing += ((ka, kb, seen(tb) - seen(ta), w))
        }
        // SEW 64 multiplies issue twice per word; the ordinary command waits for the second pass
        val wordCases = Seq(1, 2, 4, 16).filter(n => n * dlen <= 8 * vlen)
        for (w <- Seq(8, 32, 64) if w <= elen; n <- wordCases) {
          val vl = n * dlen / w
          for (_ <- 0 until 3) {
            send(MUL, 16, 8, w, vl, 5)        // M
            send(ADD, 8, 16, w, vl, 3)        // A, depends on M
            send(MACC, 16, 8, w, vl, 7)       // M, depends on A and on its own old vd
            send(ADD, 24, 0, w, vl, 1)        // A, independent
            send(MUL, 0, 24, w, vl, 9)        // M into v0, depends on A
            send(ADD, 8, 0, w, vl, 2)         // A, depends on M
          }
          // Empty commands of both classes at every boundary
          send(MUL, 16, 8, w, 0, 3); send(ADD, 8, 16, w, 0, 1); send(MACC, 16, 8, w, 0, 1); send(ADD, 16, 8, w, 1, 1)
        }
        // Saturation attributed per done: exactly one class saturates, or neither
        for (w <- Seq(8, 16, 32) if w <= elen) {
          val vl = dlen / w; val minimum = -(BigInt(1) << (w - 1))
          send(SMUL, 12, 4, w, vl, minimum); send(ADD, 13, 9, w, vl, 1); send(SMUL, 14, 9, w, vl, 3)
          send(SADD, 13, 4, w, vl, minimum); send(SMUL, 12, 9, w, vl, 5); send(SADD, 14, 9, w, vl, 0)
          send(SMUL, 12, 5, w, vl, minimum); send(SADD, 15, 5, w, vl, minimum); send(MUL, 12, 9, w, vl, 3)
        }
        drain()
        assert(finishes == order.length && pending.isEmpty)
        def gaps(f: (Int, Int) => Boolean, w: Int) = spacing.filter(s => f(s._1, s._2) && s._4 == w).map(_._3).distinct
        for (w <- Seq(8, 16, 32, 64) if w <= elen)
          println(s"CLASS_SPACING vlen=$vlen dlen=$dlen chain=$chain sew=$w mul->alu=${gaps((a, b) => isMul(a) && !isMul(b), w)} alu->mul=${gaps((a, b) => !isMul(a) && isMul(b), w)}")
        println(s"CLASS_COMMANDS vlen=$vlen dlen=$dlen chain=$chain commands=${order.length} cycles=$cycle")
        // Never closer than the rule; a full queue only widens gaps, so the rule's minimum must occur when depth allows.
        // SEW 64 multiplies issue each word twice, so the ordinary command follows the second pass.
        for (w <- Seq(8, 16, 32, 64) if w <= elen) {
          val ma = gaps((a, b) => isMul(a) && !isMul(b), w); val am = gaps((a, b) => !isMul(a) && isMul(b), w)
          val rule = if (w == 64) 3 else 2
          assert(ma.nonEmpty && am.nonEmpty && ma.min == rule && am.min >= 1, s"class spacing at SEW $w: $ma $am")
          if (depth >= 3) assert(am.min == 1, s"ordinary -> multiply spacing at SEW $w: $am")
        }
        for (at <- 0 until p.wordCount) {
          d.io.read.cmd.valid #= true; d.io.read.cmd.payload #= at; tick()
          val want = (0 until p.beatBytes).map(j => BigInt(bytes(at * p.beatBytes + j)) << (8 * j)).reduce(_ | _)
          assert(d.io.read.rsp.toBigInt == want, s"word=$at got=${d.io.read.rsp.toBigInt.toString(16)} want=${want.toString(16)}")
        }
      }
    }
}
