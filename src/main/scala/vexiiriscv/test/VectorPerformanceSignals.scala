package vexiiriscv.test

import spinal.core._
import spinal.core.sim._
import vexiiriscv.Global
import vexiiriscv.decode.Decode
import vexiiriscv.execute.vector.VectorPlugin

/** Optional simulation observations of accepted work and blocked boundaries. */
class VectorPerformanceSignals(plugin: VectorPlugin) extends Area {
  require(plugin.queueDepth > 0 && plugin.withMemory)
  private val v = plugin.logic
  private val q = v.e0.queue.q
  def tap[T <: Data](d: T): T = CombInit(d).simPublic()
  val pc = tap(Global.expendPc(v.e0(Global.PC), 64))
  val instruction = tap(v.e0(Decode.UOP))
  val accept = tap(v.whitebox.queueAccept)
  val acceptTag = tap(v.whitebox.queueAcceptTag)
  val start = tap(q.pop)
  val startTag = tap(q.head.tag)
  val startElement = tap(q.head.element)
  val complete = tap(v.whitebox.queueComplete)
  val completeTag = tap(v.whitebox.queueRunningTag)
  val issue = tap(v.arithmetic.running.pull() && v.arithmetic.issueFire.pull())
  val issueTag = tap(if (plugin.overlapIssue) v.arithmetic.lifecycle.issueTag.pull() else q.runningTag)
  val issueLast = tap(v.arithmetic.issue.last.pull())
  val rawWait = tap(v.arithmetic.running.pull() && !v.arithmetic.issueFire.pull())
  val write = tap(v.arithmetic.io.write.valid.pull())
  val writeTag = tap(if (plugin.overlapIssue) v.arithmetic.io.writeTag.pull() else q.runningTag)
  val progress = tap(if (plugin.chaining) v.arithmetic.io.progress.valid.pull() else False)
  val progressTag = tap(if (plugin.chaining) v.arithmetic.io.progress.tag.pull() else U(0, 3 bits))
  val compressOverlap = plugin.beatCompress && plugin.overlapIssue
  val compressIssue = tap(if (compressOverlap) v.arithmetic.running.pull() && v.arithmetic.issueFire.pull() && v.arithmetic.pack.issueCompress.pull() else False)
  val reserve = tap(if (compressOverlap) v.arithmetic.pack.issueCompress.pull() && v.arithmetic.issue.last.pull() else False)
  val x2 = tap(if (compressOverlap) v.arithmetic.x2.valid.pull() || v.arithmetic.x2.last.pull() else False)
  val x2Tag = tap(if (compressOverlap) v.arithmetic.packTags.x2.pull() else U(0, 3 bits))
  val x2Last = tap(v.arithmetic.x2.last.pull())
  val flush = tap(if (compressOverlap) v.arithmetic.pack.flushPending.pull() else False)
  val flushTag = tap(if (compressOverlap) v.arithmetic.pack.flushTag.pull() else U(0, 3 bits))
  val flushWrite = tap(RegNext(flush) init(False))
  val syncStart = tap(v.e0.sequencer.unitStarts)
  val syncDone = tap(v.e0.sequencer.unitDone)
  val config = tap(v.whitebox.configValid)
  // Memory behind queued arithmetic: first micro-op with work outstanding, load write and store issue beside it,
  // a load held for a unit write, and the end of a beat command's reads
  private val mo = plugin.memoryOverlap
  val memoryStart = tap(if (mo) v.memory.seq.fire && v.memory.seq.first && !q.idle else False)
  val memoryWrite = tap(if (mo) v.loadWrite && !q.idle else False)
  val memoryStore = tap(if (mo) v.e0.fire && v.e0(plugin.MEM) && v.e0(Decode.UOP)(5) && !q.idle else False)
  val memoryConflict = tap(if (mo) v.memory.complete.conflict else False)
  val drained = tap(if (mo) q.memory.drained.valid else False)
  val drainedTag = tap(if (mo) q.memory.drained.payload else U(0, 3 bits))
  val state = Bits(9 bits)
  state(0) := q.running
  state(1) := v.e0.sel && v.e0.queue.queued && q.full
  state(2) := v.e0.sel && (v.e0(plugin.ARITH) || v.e0(plugin.ELEMENT)) && !v.e0.queue.queued && !q.idle
  state(3) := v.memory.seq.valid && v.memory.seq.first && !q.idle
  state(4) := v.vcs.accessHalted
  state(5) := v.e0.sel && v.e0(plugin.VSET)
  state(6) := v.e0.sel && (v.e0(plugin.ARITH) || v.e0(plugin.ELEMENT)) && !v.e0.queue.queued
  state(7) := v.arithmetic.io.busy || v.element.io.busy
  state(8) := v.memory.seq.valid
  state.simPublic()
}
