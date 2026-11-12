package vexiiriscv.test

import rvls.spinal.TraceBackend

import scala.collection.mutable
import scala.util.Random

/**
 * Keeps tagged effects behind their owning commit and reports completions in order. Tags count modulo 16, twice the
 * RTL's tag space; once 8 are pending, committed and completed heads are reported early so that tags stay unique.
 */
class VectorTraceBuffer(hartId: Int, backends: () => Seq[TraceBackend], maxDelay: Int, seed: Long, fault: String = "") {
  require(maxDelay >= 0 && maxDelay < Int.MaxValue)
  require(VectorTraceBuffer.faults.contains(fault))
  private class Entry(val tag: Int) {
    val writes = mutable.ArrayBuffer[(Int, Array[Byte], Array[Byte])]()
    var announced, committed = false
    var saturation = Option.empty[Int]
    var due = Long.MaxValue
  }
  private val pending = mutable.LinkedHashMap[Int, Entry]()
  private val random = new Random(seed)
  private var nextTag = 0
  private var lastDue = 0L
  private var faultUsed = false
  var accepted, completed, writesBeforeCommit, completionsBeforeCommit, wraps = 0L
  var maxLive = 0

  def nonEmpty: Boolean = pending.nonEmpty

  def accept(): Int = {
    while(pending.size >= VectorTraceBuffer.emitAt && headReady) emitHead()
    val tag = nextTag
    require(pending.size < VectorTraceBuffer.tags && !pending.contains(tag), s"vector trace tag $tag reused while live")
    nextTag = (tag + 1) % VectorTraceBuffer.tags
    if(nextTag == 0) wraps += 1
    pending(tag) = new Entry(tag)
    accepted += 1
    maxLive = math.max(maxLive, pending.size)
    tag
  }

  def write(tag: Int, reg: Int, mask: Array[Byte], data: Array[Byte]): Unit = {
    val entry = pending(tag)
    require(entry.saturation.isEmpty, s"vector trace write after completion of tag $tag")
    if(!entry.committed) writesBeforeCommit += 1
    entry.writes += ((reg, mask.clone(), data.clone()))
  }

  def complete(tag: Int, vxsat: Int): Unit = {
    val entry = pending(tag)
    require(entry.saturation.isEmpty, s"duplicate vector trace completion of tag $tag")
    require(vxsat == 0 || vxsat == 1)
    if(!entry.committed) completionsBeforeCommit += 1
    entry.saturation = Some(vxsat)
  }

  def beforeCommit(tag: Int): Unit = {
    val entry = pending(tag)
    require(!entry.announced, s"duplicate vector trace commit of tag $tag")
    if(fault == "before-defer" && !faultUsed && entry.writes.nonEmpty) {
      val (reg, mask, data) = entry.writes.head
      faultUsed = true
      backends().foreach(_.writeVrfDeferred(hartId, tag, reg, mask.clone(), data.clone()))
    }
    backends().foreach(_.deferVector(hartId, tag))
    entry.announced = true
  }

  def afterCommit(tag: Int, cycle: Long): Unit = {
    val entry = pending(tag)
    require(entry.announced && !entry.committed)
    entry.committed = true
    entry.due = math.max(cycle + random.nextInt(maxDelay + 1), lastDue)
    lastDue = entry.due
    tick(cycle)
  }

  private def emitHead(): Unit = {
    val (tag, entry) = pending.head
    require(entry.committed && entry.saturation.nonEmpty, s"vector trace tag $tag has not committed and completed")
    val corruptWrite = !faultUsed && entry.writes.nonEmpty && (fault == "missing" || fault == "wrong-tag")
    if(corruptWrite) faultUsed = true
    if(!corruptWrite || fault != "missing") for((reg, mask, data) <- entry.writes) {
      val reportedTag = if(corruptWrite) tag ^ 8 else tag
      backends().foreach(_.writeVrfDeferred(hartId, reportedTag, reg, mask.clone(), data.clone()))
    }
    backends().foreach(_.completeVector(hartId, tag, entry.saturation.get))
    if(fault == "duplicate" && !faultUsed) {
      faultUsed = true
      backends().foreach(_.completeVector(hartId, tag, entry.saturation.get))
    }
    pending.remove(tag)
    completed += 1
  }

  private def headReady: Boolean = pending.headOption.exists { case (_, e) => e.committed && e.saturation.nonEmpty }

  def tick(cycle: Long): Unit = {
    while(headReady && pending.head._2.due <= cycle) emitHead()
  }

  // Reports every committed and completed head, ignoring report delays; stops at the first other head
  def flushReadyPrefix(): Unit = {
    while(headReady) emitHead()
  }

  // committedOnly leaves the accepted but uncommitted entries, which are younger than the retiring observer
  def drain(committedOnly: Boolean = false): Unit = {
    if(fault == "late" && !faultUsed && pending.nonEmpty) {
      require(pending.head._2.committed && pending.head._2.saturation.nonEmpty)
      faultUsed = true
      pending.remove(pending.head._1) // Leave the checker tag live across the next observer.
    }
    while(pending.nonEmpty && (!committedOnly || pending.head._2.committed)) emitHead()
  }

  def stats: String = s"accepted=$accepted completed=$completed maxLive=$maxLive earlyWrites=$writesBeforeCommit earlyCompletions=$completionsBeforeCommit wraps=$wraps"
}

object VectorTraceBuffer {
  val tags = 16
  val emitAt = 8
  val faults = Set("", "missing", "duplicate", "wrong-tag", "late", "before-defer")
  // The three integer scalar-result encodings in Rvv.scala are synchronous observers.
  def queued(instruction: Long): Boolean = {
    val funct3 = (instruction >>> 12) & 7
    val scalarResult = (instruction >>> 26 & 63) == 16 && funct3 == 2 && Set(0L, 16L, 17L).contains(instruction >>> 15 & 31)
    (instruction & 127) == 0x57 && funct3 != 7 && !scalarResult
  }

  def csrObserver(address: Int, writes: Boolean): Boolean =
    address == 0x009 || address == 0x00F || writes && (address == 0x300 || address == 0x100)
}
