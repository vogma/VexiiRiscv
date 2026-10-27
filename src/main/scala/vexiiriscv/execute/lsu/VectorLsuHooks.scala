package vexiiriscv.execute.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.Global
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.execute.vector.{VectorCsrService, VectorLsuElement}
import vexiiriscv.misc.{LsuTriggerBus, PrivilegedPlugin, Trap}
import vexiiriscv.riscv.{Const, Riscv}
import vexiiriscv.schedule.DispatchPlugin

// Element descriptors use the software LSU port and travel with its lane uop through the shared pipeline.
class VectorLsuHooks(lsu: LsuPlugin, frontend: AguFrontend, ds: DecoderService) extends Area {
  import AguPlugin._
  import Riscv._
  val layer = lsu.layer
  import lsu.timingParameter._
  require(addressAt == 0 && ctrlAt == wbAt, "vector elements require E0 address capture and control-stage writeback")
  lsu.vectorLsuLock.await()
  val port = lsu.vectorLsuPort
  val ELEMENT = Payload(VectorLsuElement(lsu.vectorSplit))
  val DECODE_VECTOR = Payload(Bool())
  for (uop <- frontend.vectorOps) {
    layer(uop).mayFlushUpTo(ctrlAt).setCompletion(ctrlAt)
    layer(uop).dontFlushFrom(ctrlAt + 1)
    lsu.host[DispatchPlugin].fenceOlder(uop)
    ds.addMicroOpDecoding(uop, DECODE_VECTOR, True)
  }
  ds.addMicroOpDecodingDefault(DECODE_VECTOR, False)
  ds.addIllegalCheck(ctrl => ctrl(DECODE_VECTOR) && !lsu.host[VectorCsrService].vectorEnable)
  // Keep memory instructions illegal until an element sequencer is attached.
  if (port.isEmpty) ds.addIllegalCheck(ctrl => ctrl(DECODE_VECTOR))

  val capture = new layer.Execute(0) {
    port match {
      case Some(p) =>
        ELEMENT := p.cmd.payload
        assert(!(isValid && !isCancel && VECTOR) || p.cmd.valid, "vector LSU uop has no descriptor")
      case None => apply(ELEMENT).assignFromBits(B(0, apply(ELEMENT).getBitsWidth bits))
    }
    when(VECTOR) { bypass(SIZE) := apply(ELEMENT).size }
  }

  def size(e: CtrlLaneApi): UInt =
    e(VECTOR) ? e(ELEMENT).size | e(SIZE)

  def access(e: CtrlLaneApi): Bool = !e(VECTOR) || e(ELEMENT).access

  def address(e: CtrlLaneApi, scalar: UInt): UInt = e(VECTOR) ? e(ELEMENT).address | scalar

  def msbFailed(e: CtrlLaneApi, scalar: UInt, extension: Bool): Bool =
    address(e, scalar).dropLow(Global.MIXED_WIDTH).asBools.map(_ =/= extension).orR

  // The low word of a split 8-byte element also needs bit 2 of its address clear
  def missAligned(e: CtrlLaneApi, scalar: Bool): Bool =
    if (lsu.vectorSplit) scalar || e(VECTOR) && e(ELEMENT).wide && e(ELEMENT).address(2) else scalar

  // The low word of a split 8-byte element matches the triggers at both word addresses, so neither half accesses memory
  def triggerHits(e: CtrlLaneApi, bus: LsuTriggerBus): Bits = {
    val pp = lsu.host[PrivilegedPlugin]
    if (!lsu.vectorSplit || !pp.p.debugTriggersLsu || pp.p.debugTriggers == 0) bus.hits else new Area {
      val slots = pp.logic.harts(0).trigger.slots
      val upper = B(bus.virtual + 4)
      val raw = slots.map { s =>
        val m = s.tdata2.lsu.matcher
        val maskHit = (~((upper & m.mask) ^ (s.tdata2.value & m.mask))).andR
        val cmp = U(upper) < U(s.tdata2.value)
        val upperOk = s.tdata1.matcher.mux(0 -> maskHit, 1 -> maskHit, 2 -> !cmp, 3 -> cmp, default -> False)
        s.tdata2.enabled && s.tdata2.lsu.sizeOk && s.tdata2.lsu.virtualTrigger && (m.ok || upperOk)
      }
      val broken = slots.indices.scanLeft(False)((b, i) => slots(i).tdata1.chain && (b || !raw(i))).dropRight(1)
      val span = Cat(slots.indices.map(i => raw(i) && !broken(i) && !slots(i).tdata1.chain).reverse)
      val hits = (e(VECTOR) && e(ELEMENT).wide) ? span | bus.hits
    }.hits
  }

  def storeData(e: CtrlLaneApi, data: Bits): Unit = {
    when(e(VECTOR)) { data := e(ELEMENT).storeData }
  }

  def complete(e: CtrlLaneApi, trap: Bool, trapPort: Flow[Trap], data: Bits, storeBufferEmpty: Bool): Unit = {
    val element = e(ELEMENT)
    when(e.isValid && e(SEL) && e(VECTOR) && !element.last) { e.bypass(Global.COMMIT) := False }
    // Empty instructions finish without any cache, translation or IO access.
    when(e(VECTOR) && !element.access) { trap := False }
    port.foreach { p =>
      p.storeBufferEmpty := storeBufferEmpty
      p.rsp.valid := e.down.isFiring && e(SEL) && e(VECTOR)
      p.rsp.data := data
      p.rsp.load := e(LOAD)
      p.rsp.access := element.access
      p.rsp.last := element.last
      p.rsp.uopId := e(Decode.UOP_ID)
      p.rsp.trap.valid := trap && element.access
      p.rsp.trap.payload.assignSomeByName(trapPort.payload)
    }
  }
}
