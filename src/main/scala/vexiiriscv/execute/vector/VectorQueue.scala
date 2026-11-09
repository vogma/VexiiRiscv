package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

// One accepted arithmetic instruction: the command of the unit that runs it and its tag
case class VectorQueueEntry(p: VectorRegFileParam, xlen: Int, tagWidth: Int, slides: Boolean = false,
                            beatCompress: Boolean = false, overlap: Boolean = false) extends Bundle {
  val element = Bool()
  val arith = VectorArithmeticCmd(p, xlen, slides, beatCompress)
  val elem = VectorElementCmd(p, xlen)
  val tag = UInt(tagWidth bits)
  // Registers the instruction reads and writes, whole registers; used by overlapped issue only
  val reads, writes = overlap generate Bits(32 bits)
}

/**
 * Waiting FIFO of `depth` accepted arithmetic instructions in front of the beat and element units; at most
 * depth + 1 are outstanding and they complete in accept order. Without overlap the head starts once no instruction
 * runs. With overlap a beat command also starts behind running beat commands once the unit is ready and it reads no
 * register they still write; element commands and vcompress run alone. full and idle are registered from the next
 * state, so they are exact in every cycle.
 */
class VectorQueue(p: VectorRegFileParam, xlen: Int, val depth: Int, slides: Boolean = false, beatCompress: Boolean = false,
                  val overlap: Boolean = false) extends Area {
  require(depth >= 1 && depth <= 4)
  val capacity = depth + 1
  val tagWidth = 3 // 2^3 > depth + 1, so no tag is reused while outstanding

  val push = Flow(VectorQueueEntry(p, xlen, tagWidth, slides, beatCompress, overlap)) // accept; the queue sets its tag
  val done = Bool() // the running unit's done, one per instruction in accept order
  val beatReady = overlap generate Bool() // the beat unit can take a command behind the running ones

  val entries = Vec.fill(depth)(Reg(VectorQueueEntry(p, xlen, tagWidth, slides, beatCompress, overlap)))
  val count = Reg(UInt(log2Up(depth + 1) bits)) init(0)
  val ptrWidth = log2Up(depth) max 1
  val readPtr, writePtr = Reg(UInt(ptrWidth bits)) init(0)
  // Explicit wrap: a binary one is wrong for depth 3
  def next(ptr: UInt): UInt = if (depth == 1) U(0, ptrWidth bits) else (ptr === depth - 1) ? U(0, ptrWidth bits) | ptr + 1
  val nextTag = Reg(UInt(tagWidth bits)) init(0)
  def at(ptr: UInt) = if (depth == 1) entries(0) else entries(ptr)
  val head = at(readPtr)
  push.tag := nextTag

  // executing: started and not complete; runningTag is the oldest of them
  val executing = Reg(UInt(log2Up(capacity + 1) bits)) init(0)
  val running = executing =/= 0
  val runningElement = RegInit(False)
  val runningTag = Reg(UInt(tagWidth bits)) init(0)
  val complete = running && done

  val serial = overlap generate new Area {
    def of(e: VectorQueueEntry): Bool = if (beatCompress) e.element || e.arith.compress else e.element
    val running = RegInit(False) // the executing instruction runs alone
    // Destinations of the executing beat commands, by tag; a command counts until its completion
    val writes = Vec.fill(1 << tagWidth)(Reg(Bits(32 bits)))
    val valid = Vec.fill(1 << tagWidth)(RegInit(False))
    val pending = (0 until (1 << tagWidth)).map(i => valid(i) ? writes(i) | B(0, 32 bits)).reduce(_ | _)
    val hazard = (head.reads & pending).orR
    val behind = !running && !of(head) && beatReady && !hazard
  }
  val pop = if (overlap) count =/= 0 && (!running || serial.behind) else count =/= 0 && !running

  when(push.valid) {
    at(writePtr) := push.payload
    writePtr := next(writePtr)
    nextTag := nextTag + 1
  }
  when(complete) {
    runningTag := runningTag + 1
    runningElement := False
  }
  when(pop) {
    readPtr := next(readPtr)
    runningElement := head.element
    when(!running) { runningTag := head.tag }
  }
  if (overlap) {
    when(complete) {
      serial.running := False
      serial.valid(runningTag) := False
    }
    when(pop) {
      serial.running := serial.of(head)
      serial.writes(head.tag) := head.writes
      serial.valid(head.tag) := True
    }
  }

  val countNext = count + U(push.valid) - U(pop)
  val executingNext = executing + U(pop).resized - U(complete).resized
  count := countNext
  executing := executingNext
  val full = RegNext(countNext.resize(executingNext.getWidth + 1) + executingNext === capacity) init(False)
  val idle = RegNext(countNext === 0 && executingNext === 0) init(True)

  assert(!(push.valid && full), "vector instruction accepted into a full queue")
  assert(!(push.valid && count === depth && !pop), "vector queue entry overwritten")
  assert(count <= depth && executing <= capacity, "vector queue over capacity")
  assert(!(full =/= (count.resize(executing.getWidth + 1) + executing === capacity)), "vector queue full flag out of step")
  assert(!(idle =/= (count === 0 && !running)), "vector queue idle flag out of step")
  if (!overlap) assert(executing <= 1, "vector instructions overlap without overlapped issue")
  else {
    assert(!(serial.running && executing > 1), "vector instruction that runs alone overlaps another")
    assert(!(running && !serial.valid(runningTag)), "oldest executing vector instruction not tracked")
  }
  // Tags run without gaps from the oldest outstanding instruction to the next accept
  val oldestTag = running ? runningTag | head.tag
  assert(!((count =/= 0 || running) && nextTag =/= oldestTag + count.resize(tagWidth) + executing.resize(tagWidth)),
    "vector queue tags not contiguous")
}
