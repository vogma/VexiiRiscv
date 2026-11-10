package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

case class VectorWordProgress(p: VectorRegFileParam, tagWidth: Int) extends Bundle {
  val tag = UInt(tagWidth bits)
  val address = UInt(p.addressWidth bits)
}

// One accepted arithmetic instruction: the command of the unit that runs it and its tag
case class VectorQueueEntry(p: VectorRegFileParam, xlen: Int, tagWidth: Int, slides: Boolean = false,
                            beatCompress: Boolean = false, overlap: Boolean = false, beatMul: Boolean = false, chaining: Boolean = false) extends Bundle {
  val shadow = chaining generate Bool()
  val element = Bool()
  val arith = VectorArithmeticCmd(p, xlen, slides, beatCompress, beatMul)
  val elem = VectorElementCmd(p, xlen)
  val tag = UInt(tagWidth bits)
  // Registers the instruction reads and writes, whole registers; used by overlapped issue only
  val reads, writes = overlap generate Bits(32 bits)
}

/**
 * Waiting FIFO of `depth` accepted arithmetic instructions in front of the beat and element units; at most
 * depth + 1 are outstanding and they complete in accept order. Without overlap the head starts once no instruction
 * runs. With overlap a beat command also starts behind running beat commands once the unit is ready and it reads no
 * register they still write; element commands run alone. full and idle are registered from the next
 * state, so they are exact in every cycle.
 */
class VectorQueue(p: VectorRegFileParam, xlen: Int, val depth: Int, slides: Boolean = false, beatCompress: Boolean = false,
                  val overlap: Boolean = false, beatMul: Boolean = false, val chaining: Boolean = false,
                  val classOverlap: Boolean = false) extends Area {
  require(!chaining || overlap)
  require(!classOverlap || beatMul && overlap)
  require(depth >= 1 && depth <= 4)
  val capacity = depth + 1
  val tagWidth = 3 // 2^3 > depth + 1, so no tag is reused while outstanding

  val push = Flow(VectorQueueEntry(p, xlen, tagWidth, slides, beatCompress, overlap, beatMul, chaining)) // accept; the queue sets its tag
  val done = Bool() // the running unit's done, one per instruction in accept order
  val beatReady = overlap generate Bool() // the beat unit can take a command behind the running ones
  val beatTailMultiply = classOverlap generate Bool() // the beat unit is issuing a multiply command

  val entries = Vec.fill(depth)(Reg(VectorQueueEntry(p, xlen, tagWidth, slides, beatCompress, overlap, beatMul, chaining)))
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
    def of(e: VectorQueueEntry): Bool = e.element
    val running = RegInit(False) // the executing instruction runs alone
    // Destinations of the executing beat commands, by tag; a command counts until its completion
    val writes = Vec.fill(1 << tagWidth)(Reg(Bits(32 bits)))
    val compress = beatCompress && chaining generate Vec.fill(1 << tagWidth)(RegInit(False))
    val valid = Vec.fill(1 << tagWidth)(RegInit(False))
    val pending = (0 until (1 << tagWidth)).map(i => valid(i) ? writes(i) | B(0, 32 bits)).reduce(_ | _)
    val hazard = (head.reads & pending).orR
    val multiply = beatMul && !classOverlap generate (RegInit(False))
    val sameClass = beatMul && !classOverlap generate (multiply === head.arith.multiply)
    // Multiply results write one cycle later than ordinary ones, so an ordinary command waits one cycle
    // after a multiply's last issue; write order then follows issue order.
    val classReady = classOverlap generate !(beatTailMultiply && !head.arith.multiply)
    val admissionHazard = if (chaining && beatCompress) head.shadow && pending(0) || head.arith.compress && hazard else if (chaining) head.shadow && pending(0) else hazard
    val behind = if (classOverlap) !running && !of(head) && beatReady && !admissionHazard && classReady
      else if (beatMul) !running && !of(head) && beatReady && !admissionHazard && sameClass
      else !running && !of(head) && beatReady && !admissionHazard
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
      if (beatMul && !classOverlap) serial.multiply := head.arith.multiply
      serial.writes(head.tag) := head.writes
      if (beatCompress && chaining) serial.compress(head.tag) := head.arith.compress
      serial.valid(head.tag) := True
    }
  }

  val words = chaining generate new Area {
    val maxWords = 8 * p.beatsPerReg
    val progress = Flow(VectorWordProgress(p, tagWidth))
    val reads = Vec.fill(3)(Flow(UInt(p.addressWidth bits)))
    val consumer = UInt(tagWidth bits)
    val base = Vec.fill(1 << tagWidth)(Reg(UInt(p.addressWidth bits)))
    val pending = Vec.fill(1 << tagWidth)(Reg(Bits(maxWords bits)))
    // All live tags are contiguous modulo 8; distances from the oldest define age.
    def older(a: UInt, b: UInt): Bool = (a - runningTag) < (b - runningTag)
    val ready = (0 until 3).map { port =>
      val blocked = (0 until (1 << tagWidth)).map { i =>
        val delta = (reads(port).payload - base(i)).resize(p.addressWidth + 1)
        val hit = delta < maxWords && pending(i)(delta.resize(log2Up(maxWords)))
        val finalNow = progress.valid && progress.tag === i && progress.address === reads(port).payload
        serial.valid(i) && older(U(i, tagWidth bits), consumer) && hit && !finalNow
      }.orR
      !reads(port).valid || !blocked
    }.andR
    for (i <- 0 until (1 << tagWidth)) {
      when(progress.valid && progress.tag === i) {
        val delta = progress.address - base(i)
        when(delta < maxWords) { pending(i)(delta.resize(log2Up(maxWords))) := False }
      }
      when(complete && runningTag === i) { pending(i) := 0 }
      when(pop && head.tag === i) {
        base(i) := (head.arith.vd.resize(p.addressWidth) << p.beatWidth).resized
        for (w <- 0 until maxWords) {
          pending(i)(w) := (head.writes >> head.arith.vd).resize(8)(w / p.beatsPerReg)
        }
      }
    }
    assert(!(pop && serial.valid(head.tag)), "vector queue reused a live tag")
    assert(!(progress.valid && !serial.valid(progress.tag)), "vector progress without live producer")
    if (beatCompress) assert(!(progress.valid && serial.compress(progress.tag)), "vector compress emitted word progress")
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
    if (classOverlap) assert(!(pop && running && beatTailMultiply && !head.arith.multiply), "vector ordinary command admitted during a multiply issue")
    if (beatMul) {
      if (!classOverlap) assert(!(pop && running && !serial.sameClass), "vector latency classes overlap")
      assert(!(pop && serial.valid(head.tag)), "vector queue reused a live execution tag")
    }
  }
  // Tags run without gaps from the oldest outstanding instruction to the next accept
  val oldestTag = running ? runningTag | head.tag
  assert(!((count =/= 0 || running) && nextTag =/= oldestTag + count.resize(tagWidth) + executing.resize(tagWidth)),
    "vector queue tags not contiguous")
}
