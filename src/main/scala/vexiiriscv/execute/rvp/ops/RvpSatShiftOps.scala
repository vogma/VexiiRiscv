// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core._
import vexiiriscv.execute.rvp.kinds.{RvpElemWidth => EW}

import scala.language.postfixOps

// Saturating / rounding shifts (pssha/psshar/psshl/psshlr.hs + scalar ssha/sshar/sshl/sshlr,
// and the shift-left immediates psslai.h/sslai). A distinct datapath from the shared adder:
// a per-lane shifter with left-shift overflow detect + clamp. The shift amount is rs2[7:0]
// read as a SIGNED byte -- its sign picks direction:
//   amt < 0  -> right shift by |amt|, NEVER saturates (arithmetic or logical, optionally rounded)
//   amt >= 0 -> left  shift by amt,   saturates on overflow
// Left overflow clamps to the signed range (INT_MIN/INT_MAX) for the arithmetic forms and to
// unsigned all-ones for the logical forms, as in the spec pseudocode.
//   ew selects lane geometry: EW.W = one 32-bit lane (scalar), EW.H = two 16-bit lanes.
//
// One runtime-selected W / 2xH bank serves each direction. The right side shifts
// `fill ## value ## 0`, so arithmetic/logical and rounded/plain forms are slices and gates
// around one substrate. The left side shifts one zero-extended 64-bit placement; signed
// overflow is reconstructed from the unsigned product with a shared thermometer mask.
object RvpSatShiftOps {

  case class SatShiftMid() extends Bundle {
    val right = Bool()
    // Unclamped left magnitude. The clamp stage derives both geometry-specific
    // thermometer masks once from this value.
    val lMag = UInt(7 bits)

    // Runtime-selected 1xW / 2xH banks. The two ignored top bits of each
    // 18-bit right chunk are carry guards in the clamp-stage incrementer.
    val rightPre = Bits(36 bits)
    val wideU = Bits(64 bits)
    val msb = Bits(2 bits)
  }

  def satShiftShift(rs1: Bits, amount: Bits, ew: EW.C, signedArith: Bool, rounding: Bool): SatShiftMid = {
    val mid = SatShiftMid()
    val word = ew === EW.W
    mid.right := amount.msb
    mid.lMag := amount(6 downto 0).asUInt

    val rMag = (~amount).asUInt + U(1)
    val maxShift = Mux(word, U(32, 6 bits), U(16, 6 bits))
    // The right side saturates one step later, at w+1: that also shifts the
    // guard bit out, so the logical rounding forms reach 0 for |amt| > w
    // (raw-magnitude psshlr/sshlr, spec 35adffd) while every arithmetic or
    // plain form is already all-fill at w and stays unchanged.
    val rMax = Mux(word, U(33, 6 bits), U(17, 6 bits))
    val rClamp = Mux(rMag > rMax, rMax, rMag.resize(6 bits))
    val lClamp = Mux(mid.lMag > maxShift, maxShift, mid.lMag.resize(6 bits))

    // One 36-bit segmented arithmetic-right bank. In W mode the boundary at
    // bit 18 is open; in H mode it injects the lower lane's fill.
    val fillW = signedArith && rs1(31)
    val fillH0 = signedArith && rs1(15)
    val fillH1 = signedArith && rs1(31)
    val rightInW = B(2 bits, default -> fillW) ## fillW.asBits ## rs1 ## False
    val rightInH = fillH1.asBits ## rs1(31 downto 16) ## False.asBits ##
      fillH0.asBits ## rs1(15 downto 0) ## False.asBits
    var rightCur = Mux(word, rightInW, rightInH)
    for (stage <- 0 until 6) {
      val distance = 1 << stage
      val next = Bits(36 bits)
      for (i <- 0 until 36) {
        val fill = if (i < 18) Mux(word, fillW, fillH0) else fillH1
        val shifted = if (i + distance >= 36) fill
        else if (i < 18 && i + distance >= 18) Mux(word, rightCur(i + distance), fill)
        else rightCur(i + distance)
        next(i) := Mux(rClamp(stage), shifted, rightCur(i))
      }
      rightCur = next
    }

    // Only the bits consumed by the post-increment `>> 1` are retained. The
    // zero top bit of each H chunk kills carry into the neighbouring lane.
    val rightPreW = B(0, 3 bits) ## rightCur(32 downto 1) ##
      (rounding && rightCur(0)).asBits
    val rightPreH = False.asBits ## rightCur(34 downto 19) ##
      (rounding && rightCur(18)).asBits ## False.asBits ##
      rightCur(16 downto 1) ## (rounding && rightCur(0)).asBits
    mid.rightPre := Mux(word, rightPreW, rightPreH)

    // One 64-bit segmented left bank. H inputs are independently zero-extended
    // into 32-bit physical lanes; W opens the middle boundary.
    val leftInW = rs1.asUInt.resize(64).asBits
    val leftInH = B(0, 16 bits) ## rs1(31 downto 16) ##
      B(0, 16 bits) ## rs1(15 downto 0)
    var leftCur = Mux(word, leftInW, leftInH)
    for (stage <- 0 until 6) {
      val distance = 1 << stage
      val next = Bits(64 bits)
      for (i <- 0 until 64) {
        val shifted = if (i - distance < 0) False
        else if (i >= 32 && i - distance < 32) Mux(word, leftCur(i - distance), False)
        else leftCur(i - distance)
        next(i) := Mux(lClamp(stage), shifted, leftCur(i))
      }
      leftCur = next
    }
    mid.wideU := leftCur
    mid.msb := rs1(31) ## rs1(15)
    mid
  }

  def satShiftClamp(mid: SatShiftMid, ew: EW.C, signedArith: Bool, rounding : Bool): (Bits, Bool) = {
    val word = ew === EW.W
    val lClampW = Mux(mid.lMag > 32, U(32, 6 bits), mid.lMag.resize(6 bits))
    val lClampH = Mux(mid.lMag > 16, U(16, 6 bits), mid.lMag.resize(6 bits))

    val thermoW = Bits(33 bits)
    for (j <- 0 to 32) thermoW(j) := lClampW < U(j, 6 bits)
    val thermoH = Bits(17 bits)
    for (j <- 0 to 16) thermoH(j) := lClampH < U(j, 6 bits)

    def clampLane(wideU: Bits, msb: Bool, thermo: Bits, w: Int): (Bits, Bool) = {
      val hiU = wideU(2 * w - 1 downto w - 1)
      val hiS = hiU | (thermo & B(w + 1 bits, default -> msb))
      val ovfS = !hiS.andR && hiS.orR
      val maxS = B((BigInt(1) << (w - 1)) - 1, w bits)
      val minS = B(BigInt(1) << (w - 1), w bits)
      val leftS = Mux(ovfS, Mux(msb, minS, maxS), wideU(w - 1 downto 0))

      val ovfU = wideU(2 * w - 1 downto w).orR
      val allOnes = B((BigInt(1) << w) - 1, w bits)
      val leftU = Mux(ovfU, allOnes, wideU(w - 1 downto 0))

      val leftRes = Mux(signedArith, leftS, leftU)
      val leftSat = Mux(signedArith, ovfS, ovfU)
      (leftRes, !mid.right && leftSat)
    }

    val wLeft = clampLane(mid.wideU, mid.msb(1), thermoW, 32)
    val h0Left = clampLane(mid.wideU(31 downto 0), mid.msb(0), thermoH, 16)
    val h1Left = clampLane(mid.wideU(63 downto 32), mid.msb(1), thermoH, 16)

    // One 36-bit segmented incrementer. Bit 17 is a zero carry-kill in H mode;
    // bit 18 receives the upper lane's independent rounding injection.
    val rightValue = Bits(36 bits)
    rightValue := mid.rightPre
    when(!word) { rightValue(17) := False }
    val increment = Bits(36 bits)
    increment := 0
    increment(0) := rounding
    increment(18) := rounding && !word
    val rightSum = rightValue.asUInt + increment.asUInt
    val rightW = rightSum(32 downto 1).asBits
    val rightH = rightSum(34 downto 19).asBits ## rightSum(16 downto 1).asBits

    val leftW = wLeft._1
    val leftH = h1Left._1 ## h0Left._1
    val rd = Mux(word, Mux(mid.right, rightW, leftW),
      Mux(mid.right, rightH, leftH))
    val sat = Mux(word, wLeft._2, h0Left._2 || h1Left._2)
    (rd, sat)
  }
}
