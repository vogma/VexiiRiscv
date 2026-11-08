package vexiiriscv.test

import org.scalatest.funsuite.AnyFunSuite
import rvls.spinal.DummyBackend

import scala.collection.mutable.ArrayBuffer

class VectorTraceBufferTest extends AnyFunSuite {
  class Capture extends DummyBackend {
    val events = ArrayBuffer[String]()
    val data = ArrayBuffer[Array[Byte]]()
    override def deferVector(hart: Int, tag: Int): Unit = { events += s"defer $hart $tag" }
    override def writeVrfDeferred(hart: Int, tag: Int, reg: Int, mask: Array[Byte], bytes: Array[Byte]): Unit = {
      events += s"write $hart $tag $reg ${mask(0)} ${bytes(0)}"
      data += bytes.clone()
    }
    override def completeVector(hart: Int, tag: Int, vxsat: Int): Unit = { events += s"complete $hart $tag $vxsat" }
  }

  test("early writes and completion stay behind commit and keep immutable data") {
    val capture = new Capture
    val buffer = new VectorTraceBuffer(3, () => Seq(capture), 0, 1)
    val tag = buffer.accept()
    val mask = Array(1.toByte)
    val bytes = Array(42.toByte)
    buffer.write(tag, 8, mask, bytes)
    mask(0) = 0; bytes(0) = 99
    buffer.complete(tag, 1)
    buffer.tick(100)
    assert(capture.events.isEmpty)
    buffer.beforeCommit(tag)
    capture.events += "commit"
    assert(capture.events.toList == List("defer 3 0", "commit"))
    buffer.afterCommit(tag, 100)
    assert(capture.events.toList == List("defer 3 0", "commit", "write 3 0 8 1 42", "complete 3 0 1"))
    assert(!buffer.nonEmpty && buffer.writesBeforeCommit == 1 && buffer.completionsBeforeCommit == 1)
  }

  test("late completion is delivered only once both commit and execution finish") {
    val capture = new Capture
    val buffer = new VectorTraceBuffer(0, () => Seq(capture), 0, 2)
    val tag = buffer.accept()
    buffer.beforeCommit(tag)
    buffer.afterCommit(tag, 1)
    assert(buffer.nonEmpty)
    buffer.write(tag, 1, Array(1.toByte), Array(7.toByte))
    buffer.complete(tag, 0)
    buffer.tick(2)
    assert(capture.events.last == "complete 0 0 0")
    assert(buffer.writesBeforeCommit == 0 && buffer.completionsBeforeCommit == 0)
    intercept[NoSuchElementException] { buffer.complete(tag, 0) }
  }

  test("FIFO delays, cap, tag wrap and observer drain with no following sync instruction") {
    val capture = new Capture
    val buffer = new VectorTraceBuffer(0, () => Seq(capture), 3000, 3)
    for(i <- 0 until 40) {
      val tag = buffer.accept()
      assert(tag == (i & 7))
      buffer.beforeCommit(tag)
      buffer.afterCommit(tag, 0)
      buffer.write(tag, 1, Array(1.toByte), Array(i.toByte))
      buffer.complete(tag, 0)
    }
    assert(buffer.maxLive == 5 && buffer.wraps == 5)
    buffer.drain()
    val completions = capture.events.filter(_.startsWith("complete")).map(_.split(" ")(2).toInt)
    assert(completions == (0 until 40).map(_ & 7))
    assert(capture.data.map(_(0).toInt) == (0 until 40))
    assert(buffer.accepted == buffer.completed && !buffer.nonEmpty)
  }

  test("end and observer drain reject unfinished or uncommitted effects") {
    val buffer = new VectorTraceBuffer(0, () => Nil, 0, 4)
    val tag = buffer.accept()
    buffer.complete(tag, 0)
    intercept[IllegalArgumentException] { buffer.drain() }
    buffer.beforeCommit(tag)
    intercept[IllegalArgumentException] { buffer.beforeCommit(tag) }
    intercept[IllegalArgumentException] { buffer.write(tag, 0, Array(1.toByte), Array(0.toByte)) }
    buffer.afterCommit(tag, 1)
    assert(!buffer.nonEmpty)
  }

  test("integer scalar-result instructions are observers even when rd is x0") {
    def op(f6: Int, vs1: Int, f3: Int, vd: Int = 0) = (f6.toLong << 26) | (vs1.toLong << 15) | (f3 << 12) | (vd << 7) | 0x57L
    for(vs1 <- Seq(0, 16, 17); rd <- Seq(0, 5)) assert(!VectorTraceBuffer.queued(op(16, vs1, 2, rd)))
    assert(VectorTraceBuffer.queued(op(16, 16, 6))) // vmv.s.x
    assert(VectorTraceBuffer.queued(op(20, 16, 2))) // viota.m
    assert(VectorTraceBuffer.queued(op(0, 0, 0))) // vadd.vv
    assert(!VectorTraceBuffer.queued(op(0, 0, 7))) // vsetvli
    assert(VectorTraceBuffer.csrObserver(9, false))
    assert(VectorTraceBuffer.csrObserver(15, true))
    assert(!VectorTraceBuffer.csrObserver(0x300, false))
    assert(VectorTraceBuffer.csrObserver(0x100, true))
    assert(!VectorTraceBuffer.csrObserver(0x00A, true))
  }
}
