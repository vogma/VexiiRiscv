package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.execute.{CsrListFilter, CsrService}
import vexiiriscv.misc.PrivilegedPlugin
import vexiiriscv.riscv._

/**
 * Provide mstatus.VS (also visible in sstatus) and the vector CSRs: vstart, vxsat, vxrm, vcsr (alias of vxrm and
 * vxsat), and the read-only vl, vtype, vlenb. The vector CSRs are illegal to access while VS is Off.
 */
class VectorCsrPlugin(val vlen : Int) extends FiberPlugin with VectorCsrService with VectorDirtyService {
  val api = during build new Area {
    val vs = RegInit(U(0, 2 bits))
    val enable = vs =/= 0
    val vl = Reg(UInt(vlWidth bits)) init(0)
    val vtype = Reg(VType())
    vtype.vill init(True)
    vtype.vma init(False)
    vtype.vta init(False)
    vtype.vsew init(0)
    vtype.vlmul init(B"000")
    val vstart = Reg(UInt(vstartWidth bits)) init(0)
    val vxrm = Reg(Bits(2 bits)) init(0)
    val vxsat = RegInit(False)

    val configWrite = Flow(VectorConfigWrite(vlWidth)).setIdle()
    val vstartWrite = Flow(UInt(vstartWidth bits)).setIdle()
    val vxsatSet = False
    val gotDirty = False
  }

  override def vs : UInt = api.vs
  override def vectorEnable : Bool = api.enable
  override def vl : UInt = api.vl
  override def vtype : VType = api.vtype
  override def vstart : UInt = api.vstart
  override def vxrm : Bits = api.vxrm
  override def vxsat : Bool = api.vxsat
  override def configWrite : Flow[VectorConfigWrite] = api.configWrite
  override def vstartWrite : Flow[UInt] = api.vstartWrite
  override def vxsatSet : Bool = api.vxsatSet
  override def setDirty : Bool = api.gotDirty
  override def gotDirty() : Bool = api.gotDirty

  val logic = during setup new Area {
    val cp = host[CsrService]
    val pp = host[PrivilegedPlugin]
    val buildBefore = retains(cp.csrLock)
    awaitBuild()

    assert(Global.HART_COUNT.get == 1)
    assert(isPow2(vlen))
    assert(!pp.p.withHypervisor, "vsstatus.VS is not implemented")

    // mstatus.VS, sstatus.VS; mstatus writes already flush the younger instructions
    cp.readWrite(CSR.MSTATUS, 9 -> api.vs)
    if (pp.p.withSupervisor) cp.readWrite(CSR.SSTATUS, 9 -> api.vs)
    when(host.list[VectorDirtyService].map(_.gotDirty()).orR) {
      api.vs := 3
    }

    val all = CsrListFilter(List(CSR.VSTART, CSR.VXSAT, CSR.VXRM, CSR.VCSR, CSR.VL, CSR.VTYPE, CSR.VLENB))
    cp.allowCsr(all, vectorEnable)

    cp.readWrite(CSR.VSTART, 0 -> api.vstart)
    cp.readWrite(CSR.VXSAT, 0 -> api.vxsat)
    cp.readWrite(CSR.VXRM, 0 -> api.vxrm)
    cp.readWrite(CSR.VCSR, 1 -> api.vxrm, 0 -> api.vxsat)
    cp.read(CSR.VL, 0 -> api.vl)
    cp.read(CSR.VTYPE, Riscv.XLEN.get - 1 -> api.vtype.vill, 7 -> api.vtype.vma, 6 -> api.vtype.vta, 3 -> api.vtype.vsew, 0 -> api.vtype.vlmul)
    cp.read(CSR.VLENB, 0 -> U(vlen / 8))

    // Dirty on any write by encoding, even if the value does not change
    cp.onWrite(CsrListFilter(List(CSR.VSTART, CSR.VXSAT, CSR.VXRM, CSR.VCSR)), true) {
      api.gotDirty := True
    }

    when(api.configWrite.valid) {
      api.vl := api.configWrite.vl
      api.vtype := api.configWrite.vtype
    }
    when(api.vstartWrite.valid) {
      api.vstart := api.vstartWrite.payload
    }
    api.vxsat setWhen(api.vxsatSet)

    buildBefore.release()

    pp.logic.harts(0).m.status.sd setWhen(api.vs === 3)
  }
}
