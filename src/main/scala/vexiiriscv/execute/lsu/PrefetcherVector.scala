package vexiiriscv.execute.lsu

import spinal.core._
import vexiiriscv.execute.CsrService

// Run-time enables of the RPT vector training, user CSR 0x8C0: bit 0 line filter, bit 1 same-set bypass
case class PrefetcherVectorTune(cp : CsrService) extends Area {
  val bits = RegInit(B(3, 2 bits))
  cp.readWrite(0x8C0, 0 -> bits)
  val filter = bits(0)
  val bypass = bits(1)
}
