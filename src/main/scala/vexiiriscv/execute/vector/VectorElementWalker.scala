package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

// Steps through the active elements of [vstart, vl), one per advance; vl, vstart, unmasked and v0 must hold meanwhile.
class VectorElementWalker(val vlen: Int) extends Component {
  val indexWidth = log2Up(vlen)
  val io = new Bundle {
    val start = in Bool() // load the first active element
    val advance = in Bool() // the current element was issued; has priority over start
    val vl = in UInt(indexWidth + 1 bits)
    val vstart = in UInt(indexWidth bits)
    val unmasked = in Bool()
    val v0 = in Bits(vlen bits)
    val index = out UInt(indexWidth bits)
    val last = out Bool() // no active element follows index; true when none is active
    val any = out Bool()
  }

  val active = Bits(vlen bits)
  for (i <- 0 until vlen) active(i) := U(i) >= io.vstart && U(i) < io.vl && (io.unmasked || io.v0(i))

  val index = Reg(UInt(indexWidth bits)) init(0)
  val lastIndex = Reg(UInt(indexWidth bits)) init(0)
  val last = RegInit(True)
  val any = RegInit(False)

  val above = Bits(vlen bits)
  for (i <- 0 until vlen) above(i) := active(i) && U(i) > index
  val next = OHToUInt(OHMasking.first(above))

  when(io.start) {
    val first = OHToUInt(OHMasking.first(active))
    val highest = OHToUInt(OHMasking.last(active))
    index := first
    lastIndex := highest
    last := first === highest
    any := active.orR
  }
  when(io.advance) {
    index := next
    last := next === lastIndex
  }

  io.index := index
  io.last := last
  io.any := any
}
