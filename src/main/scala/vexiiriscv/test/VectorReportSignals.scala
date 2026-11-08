package vexiiriscv.test

import spinal.core._
import spinal.core.sim._
import vexiiriscv.execute.vector.VectorPlugin

/** Execution taps added by the simulation wrapper, outside generated production RTL. */
class VectorReportSignals(plugin: VectorPlugin) extends Area {
  private val v = plugin.logic
  val unitStart = CombInit(v.arithmetic.io.cmd.valid || v.element.io.cmd.valid).simPublic()
  val unitDone = CombInit(v.arithmetic.io.done || v.element.io.done).simPublic()
  val unitSaturated = CombInit(v.e0(plugin.ARITH) ? v.arithmetic.io.saturated | v.element.io.saturated).simPublic()
}
