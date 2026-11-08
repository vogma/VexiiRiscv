package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

// One accepted arithmetic instruction: the command of the unit that runs it and its tag
case class VectorQueueEntry(p: VectorRegFileParam, xlen: Int, tagWidth: Int, slides: Boolean = false,
                            beatCompress: Boolean = false) extends Bundle {
  val element = Bool()
  val arith = VectorArithmeticCmd(p, xlen, slides, beatCompress)
  val elem = VectorElementCmd(p, xlen)
  val tag = UInt(tagWidth bits)
}

/**
 * Waiting FIFO of `depth` accepted arithmetic instructions in front of the beat and element units. The head starts
 * once no instruction runs, so at most depth + 1 are outstanding and they complete in accept order.
 * full and idle are registered from the next state, so they are exact in every cycle.
 */
class VectorQueue(p: VectorRegFileParam, xlen: Int, val depth: Int, slides: Boolean = false, beatCompress: Boolean = false) extends Area {
  require(depth >= 1 && depth <= 4)
  val tagWidth = 3 // 2^3 > depth + 1, so no tag is reused while outstanding

  val push = Flow(VectorQueueEntry(p, xlen, tagWidth, slides, beatCompress)) // accept; the queue sets its tag
  val done = Bool() // the running unit's done

  val entries = Vec.fill(depth)(Reg(VectorQueueEntry(p, xlen, tagWidth, slides, beatCompress)))
  val count = Reg(UInt(log2Up(depth + 1) bits)) init(0)
  val ptrWidth = log2Up(depth) max 1
  val readPtr, writePtr = Reg(UInt(ptrWidth bits)) init(0)
  // Explicit wrap: a binary one is wrong for depth 3
  def next(ptr: UInt): UInt = if (depth == 1) U(0, ptrWidth bits) else (ptr === depth - 1) ? U(0, ptrWidth bits) | ptr + 1
  val nextTag = Reg(UInt(tagWidth bits)) init(0)

  val running = RegInit(False)
  val runningElement = RegInit(False)
  val runningTag = Reg(UInt(tagWidth bits)) init(0)
  val pop = count =/= 0 && !running
  def at(ptr: UInt) = if (depth == 1) entries(0) else entries(ptr)
  val head = at(readPtr)
  val complete = running && done
  push.tag := nextTag

  when(push.valid) {
    at(writePtr) := push.payload
    writePtr := next(writePtr)
    nextTag := nextTag + 1
  }
  when(pop) {
    readPtr := next(readPtr)
    running := True
    runningElement := head.element
    runningTag := head.tag
  }
  when(complete) { running := False }

  val countNext = count + U(push.valid) - U(pop)
  count := countNext
  val full = RegNext(countNext === depth) init(False)
  val idle = RegNext(countNext === 0 && !(running && !complete || pop)) init(True)

  assert(!(push.valid && count === depth), "vector instruction accepted into a full queue")
  assert(!(full =/= (count === depth)), "vector queue full flag out of step")
  assert(!(idle =/= (count === 0 && !running)), "vector queue idle flag out of step")
  // Tags run without gaps from the oldest outstanding instruction to the next accept
  val oldestTag = running ? runningTag | head.tag
  assert(!((count =/= 0 || running) && nextTag =/= oldestTag + count.resize(tagWidth) + U(running).resize(tagWidth)),
    "vector queue tags not contiguous")
}
