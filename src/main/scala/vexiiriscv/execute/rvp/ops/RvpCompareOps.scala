// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core.{B, Bits, Bool, IntToBuilder, Mux, UInt}
import vexiiriscv.execute.rvp.kinds.{RvpElemWidth => EW}

// Comparison read-outs of the shared segmented adder (see RvpPackedAluPlugin).
//
// The compare ops feed the adder a = rs1, b = ~rs2 with a carry-in, so each
// lane computes a-b directly. Its raw carry-out is inverted for subtraction
// before it reaches this file, yielding the normalized predicate (a <u b):
//   sumWide(8)  = byte0 carry   sumWide(17) = byte1 carry
//   sumWide(26) = byte2 carry   sumWide(35) = byte3 carry
// In halfword mode the guard between the two bytes of a halfword is set to 1
// (see aGuard in the plugin), so the low byte's carry propagates into the high
// byte and the halfword's real carry appears at bit 17 (hw0) / bit 35 (hw1).
// That is why the low byte of a halfword reads the *upper* carry when half=True.
object RvpPackedLanes {
  // replicate a per-byte predicate into a 32-bit output of either 0xFF or 0x00 bytes
  def mask(p: Seq[Bool]): Bits = {
    require(p.length == 4, "packed lane mask needs four byte predicates")
    def byte(p: Bool) = Mux(p, B(0xFF, 8 bits), B(0x00, 8 bits))

    byte(p(3)) ## byte(p(2)) ## byte(p(1)) ## byte(p(0))
  }

  // Unsigned lane predicates from the segmented adder's normalized carry slots.
  // The low byte of each halfword reads the flag at the top of that halfword.
  def unsignedLessThan(carries: Seq[Bool], ew: EW.C): Seq[Bool] = {
    require(carries.length == 4, "packed lane predicates need four carry flags")
    val half = ew === EW.H
    Seq(
      half ? carries(1) | carries(0),
      carries(1),
      half ? carries(3) | carries(2),
      carries(3)
    )
  }

  // The four per-lane signed less-than predicates, shared by all predicate-based
  // readouts. Uses the identity
  //   a <s b = (a <u b) XOR sign_a XOR sign_b
  // i.e. the unsigned carry, flipped only when the operands' sign bits differ.
  // index 0..3 = byte lanes low..high (also the halfword lanes when half=True).
  def signedLessThan(carries: Seq[Bool], rs1: UInt, bEff: UInt, ew: EW.C): Seq[Bool] = {
    require(carries.length == 4, "packed signed predicates need four carry flags")
    val half = ew === EW.H

    // sign bit of a lane: the byte MSB, or the halfword MSB when half=True
    def sgn(v: UInt, hiByte: Int, hiHalf: Int) = half ? v(hiHalf) | v(hiByte)

    // correction = 1 exactly when the two operands' sign bits differ
    val signCorrection0 = sgn(rs1, 7, 15) ^ sgn(bEff, 7, 15)
    val signCorrection1 = rs1(15) ^ bEff(15)
    val signCorrection2 = sgn(rs1, 23, 31) ^ sgn(bEff, 23, 31)
    val signCorrection3 = rs1(31) ^ bEff(31)

    // unsigned carry (halfword-aware) XOR the sign correction, per lane
    Seq(
      (half ? carries(1) | carries(0)) ^ signCorrection0,
      carries(1) ^ signCorrection1,
      (half ? carries(3) | carries(2)) ^ signCorrection2,
      carries(3) ^ signCorrection3
    )
  }

  // One runtime-selected predicate vector shared by compare, min/max, average
  // and absolute difference.
  def lanePredicates(carries: Seq[Bool], rs1: UInt, bEff: UInt, ew: EW.C,
                     signed: Bool): Seq[Bool] = {
    (signedLessThan(carries, rs1, bEff, ew) zip unsignedLessThan(carries, ew)).map {
      case (s, u) => signed ? s | u
    }
  }

  // A word is one lane, so all four byte readouts take the predicate at bit 31.
  def wordFanout(p: Seq[Bool], word: Bool): Seq[Bool] = {
    require(p.length == 4, "word fanout needs four byte predicates")
    p.indices.map(i => if (i == 3) p(3) else word ? p(3) | p(i))
  }
}

object RvpCompareOps {

  // Set-equal: a == b iff the direct lane subtraction result is zero. For
  // halfwords a lane is equal only when both of its bytes are zero.
  def equalityLanes(rd: Bits, ew: EW.C): Seq[Bool] = {
    val half = ew === EW.H
    val word = ew === EW.W
    val eqByte0 = !rd(7 downto 0).orR
    val eqByte1 = !rd(15 downto 8).orR
    val eqByte2 = !rd(23 downto 16).orR
    val eqByte3 = !rd(31 downto 24).orR
    val halfWord0 = eqByte1 & eqByte0
    val halfWord1 = eqByte3 & eqByte2
    val eqWord = halfWord1 & halfWord0
    Seq(
      word ? eqWord | (half ? halfWord0 | eqByte0),
      word ? eqWord | (half ? halfWord0 | eqByte1),
      word ? eqWord | (half ? halfWord1 | eqByte2),
      word ? eqWord | (half ? halfWord1 | eqByte3)
    )
  }

  def mask(predicates: Seq[Bool]): Bits = RvpPackedLanes.mask(predicates)
}
