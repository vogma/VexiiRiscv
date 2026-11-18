// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core._
import vexiiriscv.execute.rvp.kinds.{RvpElemWidth => EW}

// Averaging read-outs of the shared segmented adder (see RvpPackedAluPlugin).
//
// Averaging computes (a +/- b) >> 1 with no overflow. The plugin's `rd` already
// holds the ordinary lane result (a+b for add, a-b for sub, guards dropped), and
// the normalized lane carry/borrow supplies the true (w+1)-bit MSB.
// Shifting right by one is therefore: keep rd(w-1 .. 1) and inject that carry as
// the new top bit. Because `rd` and the carry both already account for the
// ONE read-out serves both averaging-add and averaging-sub; only the injected
// top bit differs -- unsigned uses the normalized flag, signed uses that flag
// XOR sign-correction (the RvpPackedLanes.signedLessThan predicate).
//
// In halfword mode the low byte of a pair sits below the byte boundary the shift
// crosses, so its new MSB is pulled down from the high byte's LSB (rd bit 8 / 24)
// rather than the carry; the high byte and the halfword carry are unchanged.
object RvpAverageOps {

  // stitch four per-lane top bits onto rd(.. downto 1), dropping each lane's LSB
  private def assemble(rd: Bits, top0: Bool, top1: Bool, top2: Bool, top3: Bool): Bits = {
    val byte0 = top0 ## rd(7 downto 1)
    val byte1 = top1 ## rd(15 downto 9)
    val byte2 = top2 ## rd(23 downto 17)
    val byte3 = top3 ## rd(31 downto 25)
    byte3 ## byte2 ## byte1 ## byte0
  }

  def average(rd: Bits, predicates: Seq[Bool], ew: EW.C): Bits = {
    require(predicates.length == 4, "average needs four byte predicates")
    val half = ew === EW.H
    val word = ew === EW.W

    val packed = assemble(rd,
      half ? rd(8) | predicates(0),
      predicates(1),
      half ? rd(24) | predicates(2),
      predicates(3)
    )
    // Word mode is a single continuous 32-bit shift:
    // the true 33-bit sum is predicate(3) ## rd, and >>1 keeps bits 32..1.
    val wordAvg = predicates(3) ## rd(31 downto 1)
    word ? wordAvg | packed
  }
}
