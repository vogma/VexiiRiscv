package vexiiriscv.tester

import java.io.{File, PrintWriter}
import spinal.core._
import spinal.core.fiber.Fiber
import spinal.core.sim._
import vexiiriscv.execute.lsu.{LsuL1Plugin, LsuPlugin}
import vexiiriscv.misc.TrapReason

/** Logs LSU control-stage events of the first core (cycle, kind, pc, address) for coverage checks.
  * With L1 words wider than LSLEN also: `wide` (a wide access leaves the control stage), `wideupper` (it enables
  * upper-half bytes), `wideredo` (it ends in REDO: retry or element-wise fallback), `widepmp` and `wideio` (fallback
  * causes: upper-half PMP denial, non-cacheable region) and `widerun` (the length, in the address field, of a run of
  * at least two wide accesses in consecutive cycles without REDO). */
object LsuEventMonitor {
  def publish(dut: TestBenchDut): Unit = dut.rework {
    Fiber patch {
      val l = dut.cores.head.host[LsuPlugin].logic
      val r = dut.cores.head.host[LsuL1Plugin].logic.refill.push
      r.valid.simPublic()
      r.address.simPublic()
      l.trapPort.valid.simPublic()
      l.trapPort.exception.simPublic()
      l.trapPort.code.simPublic()
      l.commitProbe.pc.simPublic()
      l.commitProbe.address.simPublic()
      l.onCtrl.traps.l1Failed.simPublic()
      if (l.pmpUpper != null) {
        val w = l.pmpUpper.result
        Seq(w.fire, w.upperBytes, w.upperDenied, w.ioDenied).foreach(_.simPublic())
      }
      if (l.storeBuffer != null) {
        l.storeBuffer.push.valid.simPublic()
        l.storeBuffer.holdHart.waitIt.simPublic()
        l.onCtrl.wb.compatibleOp.simPublic()
        l.onCtrl.wb.notFull.simPublic()
      }
    }
  }

  def attach(dut: TestBenchDut, file: File): Unit = {
    val l = dut.cores.head.host[LsuPlugin].logic
    val refill = dut.cores.head.host[LsuL1Plugin].logic.refill.push
    val sb = Option(l.storeBuffer)
    val wide = Option(l.pmpUpper).map(_.result)
    var run = 0
    val out = new PrintWriter(file)
    var cycle = 0L
    var lastPc = 0L
    var waiting = false
    def log(kind: String, pc: Long, address: Long): Unit = out.println(f"$cycle $kind $pc%x $address%x")
    dut.cores.head.clockDomain.onSamplings {
      val pc = l.commitProbe.pc.toLong
      val address = l.commitProbe.address.toLong
      if (refill.valid.toBoolean) log("refill", pc, refill.address.toLong)
      if (l.trapPort.valid.toBoolean && !l.trapPort.exception.toBoolean) {
        l.trapPort.code.toInt match {
          case TrapReason.MMU_REFILL => log("tlb", pc, address)
          case TrapReason.REDO =>
            log("redo", pc, address)
            if (l.onCtrl.traps.l1Failed.toBoolean) log("l1miss", pc, address)
            if (sb.nonEmpty && l.onCtrl.wb.compatibleOp.toBoolean && !l.onCtrl.wb.notFull.toBoolean) log("sbfull", pc, address)
          case _ =>
        }
      }
      sb.foreach { s =>
        if (s.push.valid.toBoolean) log("l1miss", pc, address)
        val w = s.holdHart.waitIt.toBoolean
        if (w && !waiting) log("hold", lastPc, 0)
        waiting = w
      }
      wide.foreach { w =>
        val redo = l.trapPort.valid.toBoolean && !l.trapPort.exception.toBoolean && l.trapPort.code.toInt == TrapReason.REDO
        if (w.fire.toBoolean) {
          log("wide", pc, address)
          if (w.upperBytes.toBoolean) log("wideupper", pc, address)
          if (w.upperDenied.toBoolean) log("widepmp", pc, address)
          if (w.ioDenied.toBoolean) log("wideio", pc, address)
          if (redo) log("wideredo", pc, address)
        }
        if (w.fire.toBoolean && !redo) run += 1 else {
          if (run >= 2) log("widerun", lastPc, run)
          run = 0
        }
      }
      lastPc = pc
      cycle += 1
    }
    onSimEnd(out.close())
  }
}
