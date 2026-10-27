package vexiiriscv.execute.vector

import vexiiriscv.riscv.{MicroOp, Rvv}

// Shared by the LSU frontend and the element sequencer. Data EEWs go up to ELEN; index EEWs up to min(ELEN, XLEN).
case class VectorMemoryOps(elen : Int, xlen : Int) {
  val widths = Rvv.eews.filter(_ <= elen)
  private def allowed(prefix : String, width : Int) = !prefix.contains("x") || width <= xlen
  val ordinary = for (width <- widths; prefix <- Seq("vle", "vse", "vlse", "vsse", "vluxei", "vloxei", "vsuxei", "vsoxei")
                      if allowed(prefix, width))
    yield Rvv.all(s"$prefix$width.v") -> prefix.startsWith("vl")
  val whole = (for (n <- Seq(1, 2, 4, 8); width <- widths) yield Rvv.all(s"vl${n}re$width.v") -> true) ++
    (for (n <- Seq(1, 2, 4, 8)) yield Rvv.all(s"vs${n}r.v") -> false)
  val segment = for (n <- Rvv.segFields; width <- widths;
                     prefix <- Seq("vlseg", "vsseg", "vlsseg", "vssseg", "vluxseg", "vloxseg", "vsuxseg", "vsoxseg") if allowed(prefix, width))
    yield Rvv.all(s"$prefix$n${if (prefix.contains("x")) "ei" else "e"}$width.v") -> prefix.startsWith("vl")
  // Fault-only-first forms in the key order of Rvv's maps over these widths
  val faultOnlyFirst = widths.map(_ -> ()).toMap.keys.toSeq.map(w => Rvv.VLEFF(w) -> true) ++
    (for (n <- Rvv.segFields; w <- widths) yield (n, w) -> ()).toMap.keys.toSeq.map(k => Rvv.VLSEGFF(k) -> true)
  val all: Seq[(MicroOp, Boolean)] = ordinary ++ whole ++ Seq(Rvv.VLM_V -> true, Rvv.VSM_V -> false) ++ segment ++ faultOnlyFirst
}
