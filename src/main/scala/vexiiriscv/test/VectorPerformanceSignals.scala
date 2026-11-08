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
  val syncStart = tap(v.e0.sequencer.unitStarts)
  val syncDone = tap(v.e0.sequencer.unitDone)
  val config = tap(v.whitebox.configValid)
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
