package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

// Steps through the fields of the active elements of [vstart, evl), one per advance, starting at startField.
// evl, vstart, unmasked, v0 and fields must hold meanwhile. With split, each field of a wide element takes two halves.
// With chunks, an advance instead moves to the first active element of the next chunk; chunks split each group of
// wordMask + 1 elements by the 2-bit chunk numbers of table (entry k for element position k, increasing in k).
class VectorElementWalker(val vlen: Int, val split: Boolean = false, val chunks: Boolean = false) extends Component {
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
    val wide = split generate in(Bool()) // elements take two halves, low first
    val half = split generate out(Bool())
    val chunk = chunks generate in(Bool()) // one element per field, held from the first advance
    val wordMask = chunks generate in(UInt(3 bits)) // elements per VRF word minus one: 7, 3 or 1
    val table = chunks generate in(Vec.fill(8)(UInt(2 bits)))
    val chunkActive = chunks generate out(Bits(8 bits)) // active elements of the current chunk, by word position
  }

  val active = Bits(vlen bits)
  for (i <- 0 until vlen) active(i) := U(i) >= io.vstart && U(i) < io.evl && (io.unmasked || io.v0(i))

  val index = Reg(UInt(indexWidth bits)) init(0)
  val lastIndex = Reg(UInt(indexWidth bits)) init(0)
  val field = Reg(UInt(3 bits)) init(0)
  val last = RegInit(True)
  val any = RegInit(False)
  val half = split generate RegInit(False)

  val above = Bits(vlen bits)
  for (i <- 0 until vlen) above(i) := active(i) && U(i) > index
  // An element of a later chunk lies in a later word, or in the same word with another chunk number
  val chunked = chunks generate new Area {
    val indexOr = index | io.wordMask.resized
    val number = io.table(index(2 downto 0))
    def later(i: UInt, k: UInt): Bool = i > indexOr || i > index && io.table(k) =/= number
    when(io.chunk) { for (i <- 0 until vlen) above(i) := active(i) && later(U(i, indexWidth bits), U(i & 7, 3 bits)) }
    val inLast = !later(lastIndex, lastIndex(2 downto 0))
    val word = io.wordMask.mux(
      7 -> active.subdivideIn(8 bits)((index >> 3).resized),
      3 -> active.subdivideIn(4 bits)((index >> 2).resized).resize(8),
      default -> active.subdivideIn(2 bits)((index >> 1).resized).resize(8))
    val first = index(2 downto 0) & io.wordMask
    for (k <- 0 until 8) io.chunkActive(k) := word(k) && U(k) <= io.wordMask && U(k) >= first && io.table(k) === number
  }
  val next = OHToUInt(OHMasking.first(above))

  when(io.start) {
    val first = OHToUInt(OHMasking.first(active))
    val highest = OHToUInt(OHMasking.last(active))
    index := first
    lastIndex := highest
    field := io.startField
    last := first === highest
    any := active.orR
    if (split) half := False
  }
  def nextField(): Unit = {
    field := field + 1
    when(field === io.fields) {
      field := 0
      index := next
      last := next === lastIndex
    }
  }
  when(io.advance) {
    if (split) {
      half := io.wide && !half
      when(!io.wide || half) { nextField() }
    } else nextField()
  }

  io.index := index
  io.field := field
  if (split) {
    io.last := last && (!any || field === io.fields && (!io.wide || half))
    io.half := half
  } else io.last := last && (field === io.fields || !any)
  if (chunks) when(io.chunk) { io.last := !any || chunked.inLast }
  io.any := any
}
