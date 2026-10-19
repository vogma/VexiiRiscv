package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

// Steps through the fields of the active elements of [vstart, evl), one per advance, starting at startField.
// evl, vstart, unmasked, v0 and fields must hold meanwhile.
class VectorElementWalker(val vlen: Int) extends Component {
  val indexWidth = log2Up(vlen)
  val io = new Bundle {
    val start = in Bool() // load the first active element
    val advance = in Bool() // the current field was issued; has priority over start
    val evl = in UInt(indexWidth + 1 bits)
    val vstart = in UInt(indexWidth bits)
    val unmasked = in Bool()
    val v0 = in Bits(vlen bits)
    val fields = in UInt(3 bits) // fields per element minus one
    val startField = in UInt(3 bits) // field of the first element, at most fields
    val index = out UInt(indexWidth bits)
    val field = out UInt(3 bits)
    val last = out Bool() // no active element or field follows; true when none is active
    val any = out Bool()
  }

  val active = Bits(vlen bits)
  for (i <- 0 until vlen) active(i) := U(i) >= io.vstart && U(i) < io.evl && (io.unmasked || io.v0(i))

  val index = Reg(UInt(indexWidth bits)) init(0)
  val lastIndex = Reg(UInt(indexWidth bits)) init(0)
  val field = Reg(UInt(3 bits)) init(0)
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
    field := io.startField
    last := first === highest
    any := active.orR
  }
  when(io.advance) {
    field := field + 1
    when(field === io.fields) {
      field := 0
      index := next
      last := next === lastIndex
    }
  }

  io.index := index
  io.field := field
  io.last := last && (field === io.fields || !any)
  io.any := any
}
