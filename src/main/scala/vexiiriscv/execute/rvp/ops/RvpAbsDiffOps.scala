// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core._

// Absolute-difference read-outs of the shared segmented adder (see RvpPackedAluPlugin).
//
// With DO_SUB the adder computes rs1-rs2 directly. |rs1-rs2| is rd when
// rs1 >= rs2 (predicate false), and the lane two's-complement negation
// -rd = ~rd + 1 = rs2-rs1 when rs1 < rs2 (predicate true). The predicate is
// the same normalized less-than flag the compares use.
//
// The +1 must stay inside its lane. A small segmented incrementer ripples the low
// byte's carry into the high byte only in halfword mode (via `half`), exactly
// mirroring the main adder's guard bits.
object RvpAbsDiffOps {

  // Per-lane |a-b| bytes, gated by the four per-byte less-than predicates.
  // rd = a - b (guards dropped); ~rd + 1 negates each selected lane.
  private def absBytes(rd: Bits, half: Bool, predicates: Seq[Bool]): Seq[Bits] = {
    // Lane-wise increment of ~rd. Byte mode adds 1 to every byte; halfword mode
    // ripples the low byte's carry (inc*(8)) into the high byte so the pair negates
    // as a single 16-bit value.
    val inverted = ~rd
    val inc0 = inverted(7 downto 0).asUInt +^ U(1)
    val inc1 = inverted(15 downto 8).asUInt +^ (half ? inc0(8) | True).asUInt
    val inc2 = inverted(23 downto 16).asUInt +^ U(1)
    val inc3 = inverted(31 downto 24).asUInt +^ (half ? inc2(8) | True).asUInt

    val incrementedBytes = Seq(inc0, inc1, inc2, inc3)

    for (i <- 0 until 4) yield Mux(predicates(i), incrementedBytes(i)(7 downto 0).asBits, rd(8 * i + 7 downto 8 * i))
  }

  // assemble 32-bit vector out of bytes
  private def pack(bytes: Seq[Bits]): Bits = {
    bytes(3) ## bytes(2) ## bytes(1) ## bytes(0)
  }

  def absDiff(rd: Bits, half: Bool, predicates: Seq[Bool]): Bits = {
    pack(absBytes(rd, half, predicates))
  }

  def absDiffSumUnsigned(rd: Bits, predicates: Seq[Bool]): UInt = {
    require(predicates.length == 4, "absolute-difference sum needs four predicates")
    val bytes = absBytes(rd, False, predicates)
    // Keep the four-byte reduction logarithmic. A left-deep reduction puts
    // three dependent adders in front of the 32-bit accumulator used by
    // PABDSUMAU.B; the balanced tree needs only two (9-bit pairs, then a
    // 10-bit total) and preserves the exact 0..1020 result range.
    val pair01 = bytes(0).asUInt +^ bytes(1).asUInt
    val pair23 = bytes(2).asUInt +^ bytes(3).asUInt
    pair01 +^ pair23
  }
}
