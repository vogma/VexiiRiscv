package vexiiriscv.execute.vector

import vexiiriscv.riscv.{MicroOp, Rvv}

// Shared by the LSU frontend and the element sequencer; no segment or fault-only-first forms.
object VectorMemoryOps {
  val ordinary = for (width <- Seq(8, 16, 32); prefix <- Seq("vle", "vse", "vlse", "vsse", "vluxei", "vloxei", "vsuxei", "vsoxei"))
    yield Rvv.all(s"$prefix$width.v") -> prefix.startsWith("vl")
  val whole = (for (n <- Seq(1, 2, 4, 8); width <- Seq(8, 16, 32)) yield Rvv.all(s"vl${n}re$width.v") -> true) ++
    (for (n <- Seq(1, 2, 4, 8)) yield Rvv.all(s"vs${n}r.v") -> false)
  val all: Seq[(MicroOp, Boolean)] = ordinary ++ whole ++ Seq(Rvv.VLM_V -> true, Rvv.VSM_V -> false)
}
