package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

// Any source that sets mstatus.VS to Dirty
trait VectorDirtyService {
  def gotDirty() : Bool
}

// vtype without its reserved bits; vill reads at bit XLEN-1
case class VType() extends Bundle {
  val vill = Bool()
  val vma, vta = Bool()
  val vsew = UInt(3 bits)
  val vlmul = Bits(3 bits)
}

case class VectorConfigWrite(vlWidth : Int) extends Bundle {
  val vl = UInt(vlWidth bits)
  val vtype = VType()
}

/**
 * Vector CSR state, mstatus.VS, and the write ports the vector unit uses besides the CSR instructions.
 */
trait VectorCsrService {
  def vlen : Int
  def vlWidth = log2Up(vlen) + 1
  def vstartWidth = log2Up(vlen)

  def vs : UInt                             // mstatus.VS
  def vectorEnable : Bool                   // VS =/= Off
  def vl : UInt
  def vtype : VType
  def vstart : UInt
  def vxrm : Bits
  def vxsat : Bool

  def configWrite : Flow[VectorConfigWrite] // vl and vtype, from vset*
  def vstartWrite : Flow[UInt]
  def vxsatSet : Bool                       // vxsat := 1
  def setDirty : Bool                       // mstatus.VS := Dirty
}
