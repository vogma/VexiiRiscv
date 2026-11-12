package vexiiriscv.execute.vector

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import vexiiriscv.decode.Decode
import vexiiriscv.misc.Trap
import vexiiriscv.riscv.Riscv._

// wordWidth is the L1 word: LSLEN, or wider when beats move wider words than scalar accesses
case class VectorLsuElement(split : Boolean = false, beat : Boolean = false, wordWidth : Int) extends Bundle {
  val address = UInt(XLEN bits)
  val size = UInt(log2Up(log2Up(wordWidth / 8 + 1)) bits)
  val storeData = Bits(wordWidth bits) // element in the low bytes, before LSU replication; a beat's whole word
  val access = Bool() // false for an empty instruction's finish micro-op
  val last = Bool()
  val wide = split generate Bool() // low word of an 8-byte element: alignment and triggers cover all 8 bytes
  val beats = beat generate new Bundle {
    val enable = Bool() // an aligned wordWidth-bit access whose byte mask selects whole elements
    val mask = Bits(wordWidth / 8 bits)
  }
}

case class VectorLsuResult(beat : Boolean = false, wordWidth : Int) extends Bundle {
  val data = Bits(LSLEN bits) // element in the low bytes, without sign extension
  val raw = beat generate Bits(wordWidth bits) // the aligned memory word of a beat
  val fallback = beat generate Bool() // a beat that could not complete as one: the rest goes element-wise
  val load = Bool()
  val access, last = Bool()
  val uopId = Decode.UOP_ID()
  val trap = Flow(Trap(0, false))
}

// Commands accompany the lane's address-stage uop; results fire at the LSU control stage, including traps.
case class VectorLsuPort(split : Boolean = false, beat : Boolean = false, wordWidth : Int) extends Bundle {
  val cmd = Flow(VectorLsuElement(split, beat, wordWidth))
  val rsp = Flow(VectorLsuResult(beat, wordWidth))
  val storeBufferEmpty = Bool()
}

trait VectorLsuService {
  val vectorLsuLock = Retainer()
  var vectorLsuPort = Option.empty[VectorLsuPort]
  def withVector: Boolean
  def vectorElen: Int
  def vectorBeat: Boolean // unit-stride forms move whole L1 words
  def vectorWordWidth: Int = LSLEN.get // L1 word width; beats move words of this size
  // EEW-64 elements take two 32-bit micro-ops, low word first, when the LSU is 32 bits wide
  def vectorSplit: Boolean = withVector && vectorElen == 64 && LSLEN.get < 64
  def newVectorLsuPort(): VectorLsuPort = {
    require(withVector, "vector LSU support is disabled")
    require(vectorLsuPort.isEmpty, "only one vector LSU sequencer is supported")
    val port = VectorLsuPort(vectorSplit, vectorBeat, vectorWordWidth)
    vectorLsuPort = Some(port)
    port
  }
}
