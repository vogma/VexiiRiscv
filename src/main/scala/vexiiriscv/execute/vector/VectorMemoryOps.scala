package vexiiriscv.execute.vector

import vexiiriscv.riscv.{MicroOp, Rvv}

// Shared by the LSU frontend and the element sequencer.
object VectorMemoryOps {
  val ordinary = for (width <- Seq(8, 16, 32); prefix <- Seq("vle", "vse", "vlse", "vsse", "vluxei", "vloxei", "vsuxei", "vsoxei"))
    yield Rvv.all(s"$prefix$width.v") -> prefix.startsWith("vl")
  val whole = (for (n <- Seq(1, 2, 4, 8); width <- Seq(8, 16, 32)) yield Rvv.all(s"vl${n}re$width.v") -> true) ++
    (for (n <- Seq(1, 2, 4, 8)) yield Rvv.all(s"vs${n}r.v") -> false)
  val segment = for (n <- Rvv.segFields; width <- Seq(8, 16, 32);
                     prefix <- Seq("vlseg", "vsseg", "vlsseg", "vssseg", "vluxseg", "vloxseg", "vsuxseg", "vsoxseg"))
    yield Rvv.all(s"$prefix$n${if (prefix.contains("x")) "ei" else "e"}$width.v") -> prefix.startsWith("vl")
  val faultOnlyFirst = Rvv.VLEFF.values.toSeq.map(_ -> true) ++ Rvv.VLSEGFF.values.toSeq.map(_ -> true)
  val all: Seq[(MicroOp, Boolean)] = ordinary ++ whole ++ Seq(Rvv.VLM_V -> true, Rvv.VSM_V -> false) ++ segment ++ faultOnlyFirst
}
