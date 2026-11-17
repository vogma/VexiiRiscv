package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib._
import vexiiriscv.execute.CsrService
import vexiiriscv.schedule.DispatchPlugin

// Run-time enables of the RPT vector options, user CSR 0x8C0: bit 0 line filter, bit 1 same-set bypass, bit 2 issue yield
case class PrefetcherVectorTune(withTrain : Boolean, withYield : Boolean, cp : CsrService) extends Area {
  val bits = RegInit(B((if (withYield) 4 else 0) | (if (withTrain) 3 else 0), 3 bits))
  cp.readWrite(0x8C0, 0 -> bits)
  val filter = withTrain generate bits(0)
  val bypass = withTrain generate bits(1)
  val yieldOn = withYield generate bits(2)
}

// One dispatch bubble per prefetch starved by lane accesses, at most one every gap + 1 cycles
case class PrefetcherVectorYield(cmd : Stream[PrefetchCmd], enable : Bool, gap : Int, dp : DispatchPlugin) extends Area {
  val starved = cmd.valid && !cmd.ready
  val counter = Reg(UInt(log2Up(gap + 1) bits)) init(0)
  val go = starved && counter === 0 && enable
  when(go) { counter := gap } elsewhen(counter =/= 0) { counter := counter - 1 }
  val halt = RegNext(go) init(False)
  dp.haltDispatchWhen(halt)
}
