package vexiiriscv.schedule

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.riscv.MicroOp

import scala.collection.mutable

// A sequenced instruction stays the hart's oldest candidate and is inserted once per micro-op until its last one.
case class DispatchSequencerPort() extends Bundle {
  val valid = Bool() // the hart's oldest candidate is a sequenced instruction
  val uop = Decode.UOP()
  val first = Bool() // none of its micro-ops was inserted yet
  val ready = Bool() // from the sequencer: a micro-op may be inserted
  val last = Bool() // from the sequencer: the micro-op inserted now is the last one
  val fire = Bool() // a micro-op is inserted this cycle
  val cancel = Bool() // the candidate is flushed this cycle
}

trait DispatchSequencerService { this: DispatchPlugin =>
  val SEQUENCED = Payload(Bool()).setName("SEQUENCED")
  val sequencedOps = mutable.LinkedHashSet[MicroOp]()

  // Call before elaborationLock is released; the port is then logic.sequencer.port. The first micro-op fences older ones.
  def sequence(ops: Seq[MicroOp]): Unit = {
    val ds = host[DecoderService]
    if (sequencedOps.isEmpty) {
      ds.addMicroOpDecodingDefault(SEQUENCED, False)
      addDispatchCtx(SEQUENCED)
    }
    for (op <- ops) {
      ds.addMicroOpDecoding(op, SEQUENCED, True)
      fenceOlder(op)
      sequencedOps += op
    }
  }
}
