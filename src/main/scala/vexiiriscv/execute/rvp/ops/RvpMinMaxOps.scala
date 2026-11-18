// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core._

// Min/max read-outs of the shared adder. Unlike the compare ops (which emit a mask),
// these select the ORIGINAL operand value per lane, gated by the less-than predicate:
//   predicate p = (rs1 < bEff)  ->  min picks rs1, max picks bEff (complement).
// The predicate is exactly the compare carry, so no extra arithmetic is needed.
object RvpMinMaxOps {
  // Min/max share one byte-select network. `predicates` is already word-fanned;
  // min keeps rs1 when it is smaller, while max complements that choice.
  def select(predicates: Seq[Bool], rs1: UInt, bEff: UInt, isMax: Bool): Bits = {
    require(predicates.length == 4, "min/max select needs four byte predicates")

    def lane(i: Int) = {
      val pickRs1 = predicates(i) ^ isMax
      Mux(pickRs1, rs1(8 * i + 7 downto 8 * i).asBits,
        bEff(8 * i + 7 downto 8 * i).asBits)
    }

    lane(3) ## lane(2) ## lane(1) ## lane(0)
  }
}
