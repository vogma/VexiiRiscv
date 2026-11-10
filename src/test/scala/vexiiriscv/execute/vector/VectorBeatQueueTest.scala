package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import scala.util.Random

class VectorBeatQueueHarness(depth: Int) extends Component {
  val io = new Bundle {
    val push, multiply, element, done, ready = in Bool()
    val reads, writes = in Bits(32 bits)
    val full, idle, pop, complete = out Bool()
    val tag, headTag, runningTag = out UInt(3 bits)
    val headMultiply, headElement = out Bool()
  }
  val q = new VectorQueue(VectorRegFileParam(128,64),64,depth,overlap=true,beatMul=true)
  q.push.valid := io.push
  q.push.element := io.element
  q.push.arith.assignFromBits(B(0,q.push.arith.getBitsWidth bits))
  q.push.arith.multiply.allowOverride() := io.multiply
  q.push.elem.assignFromBits(B(0,q.push.elem.getBitsWidth bits))
  q.push.reads := io.reads; q.push.writes := io.writes
  q.done := io.done; q.beatReady := io.ready
  io.full := q.full; io.idle := q.idle; io.pop := q.pop; io.complete := q.complete
  io.tag := q.push.tag; io.headTag := q.head.tag; io.runningTag := q.runningTag
  io.headMultiply := q.head.arith.multiply; io.headElement := q.head.element
}

class VectorBeatQueueTest extends AnyFunSuite {
  case class Entry(tag:Int, multiply:Boolean, element:Boolean, reads:BigInt, writes:BigInt, var finish:Int=0)
  for(depth <- Seq(1,3)) test(s"latency classes, RAW masks and wrap at depth $depth") {
    SimConfig.workspacePath(s"build/beat-mul/unit-queue-$depth").compile(new VectorBeatQueueHarness(depth)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val random=new Random(924+depth)
      val waiting=mutable.Queue[Entry](); val running=mutable.Queue[Entry]()
      var accepted=0;var completed=0;var overlaps=0;var transitions=0;var fullCycles=0;var simultaneous=0
      dut.io.push #=false;dut.io.done #=false;dut.io.ready #=false
      dut.io.multiply #=false;dut.io.element #=false;dut.io.reads #=0;dut.io.writes #=0
      for(cycle <- 0 until 10000) {
        sleep(1)
        val full=waiting.size+running.size==depth+1
        assert(dut.io.full.toBoolean==full)
        assert(dut.io.idle.toBoolean==(waiting.isEmpty&&running.isEmpty))
        if(full)fullCycles+=1
        val done=running.nonEmpty&&running.front.finish<=cycle
        val ready=random.nextInt(4)!=0
        val pending=running.map(_.writes).foldLeft(BigInt(0))(_|_)
        val behind=waiting.nonEmpty&&running.nonEmpty&& !running.front.element&& !waiting.front.element&&
          running.front.multiply==waiting.front.multiply&&ready&&(waiting.front.reads&pending)==0
        val pop=waiting.nonEmpty&&(running.isEmpty||behind)
        val push=cycle<9800&& !full&&random.nextInt(4)!=0
        val incoming=Entry(accepted&7,random.nextBoolean(),random.nextInt(12)==0,
          BigInt(1)<<random.nextInt(8),BigInt(1)<<random.nextInt(8))
        dut.io.done #=done;dut.io.ready #=ready;dut.io.push #=push
        dut.io.multiply #=incoming.multiply;dut.io.element #=incoming.element
        dut.io.reads #=incoming.reads;dut.io.writes #=incoming.writes
        sleep(1)
        assert(dut.io.pop.toBoolean==pop,s"cycle=$cycle")
        if(pop) {
          assert(dut.io.headTag.toInt==waiting.front.tag)
          if(running.nonEmpty)overlaps+=1
          if(running.nonEmpty&&running.front.multiply!=waiting.front.multiply)transitions+=1
        }
        if(done) {
          assert(dut.io.runningTag.toInt==running.front.tag)
          running.dequeue();completed+=1
        }
        if(pop) {
          val e=waiting.dequeue()
          e.finish=(cycle+7).max(if(running.isEmpty)0 else running.last.finish+1)
          running.enqueue(e)
        }
        if(push) { assert(dut.io.tag.toInt==incoming.tag); waiting.enqueue(incoming);accepted+=1 }
        if(push&&pop || push&&done || pop&&done)simultaneous+=1
        dut.clockDomain.waitSampling();sleep(1)
      }
      assert(accepted==completed&&waiting.isEmpty&&running.isEmpty)
      assert(overlaps>30&&fullCycles>30&&simultaneous>0&&transitions==0)
    }
  }
}
