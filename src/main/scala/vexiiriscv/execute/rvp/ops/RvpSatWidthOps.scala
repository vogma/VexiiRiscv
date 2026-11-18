// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core._
import vexiiriscv.execute.rvp.kinds.{RvpElemWidth => EW}

import scala.language.postfixOps

// Saturate-to-immediate-width (sati / usati / psati.h / pusati.h). A per-lane clamp
// that does NOT use the shared segmented adder: each lane is compared against bounds
// derived from a single all-ones << n shift.
//   signed:   [-(2^n), 2^n-1]   (width = n+1)
//   unsigned: [0, 2^n-1]        (width = n)
// ew selects the lane geometry: EW.W = one 32-bit lane, EW.H = two 16-bit lanes.
// At XLEN=32 the EW.W lane IS the scalar sati/usati. At XLEN=64 it is psati.w/pusati.w --
// two of them, one per word -- and the scalar forms take the whole register through
// clampFull() below, with a uimm6 width field instead of uimm5.
object RvpSatWidthOps {

  // Clamp one w-bit lane against precomputed bounds. `unsigned` is a runtime Bool, so one
  // elaboration serves both sati and usati:
  //   signed:   below = v < loZeros (== -(2^n)),  clamp-low = loZeros
  //   unsigned: below = v < 0 (v.msb),            clamp-low = 0
  // The bounds are complementary thermometer codes, so masked OR reductions detect
  // out-of-range magnitude without synthesising signed magnitude comparators.
  private def clampLane(value: Bits, loZeros: Bits, maxBits: Bits, unsigned: Bool): (Bits, Bool) = {
    val w = value.getWidth
    val magnitude = value(w - 2 downto 0)
    val rangeMask = loZeros(w - 2 downto 0)
    val above = !value.msb && (magnitude & rangeMask).orR
    val belowSigned = value.msb && ((~magnitude) & rangeMask).orR
    val below = Mux(unsigned, value.msb, belowSigned)
    val loClamp = Mux(unsigned, B(0, w bits), loZeros)
    val out = Mux(below, loClamp, Mux(above, maxBits, value))
    (out, below || above)
  }

  case class SatBounds(width: Int) extends Bundle {
    val loZeros = Bits(width bits)
    val maxBits = Bits(width bits)
  }

  def satBounds(n: UInt, width: Int = 32): SatBounds = {
    // One barrel shifter builds the bound for every lane. Narrower lanes reuse its low
    // half: (allOnes_W << n)(w-1:0) == (allOnes_w << n) whenever n < w, and no lane ever
    // takes an n wider than itself, so every geometry shares this one generator. At
    // XLEN=64 it is 64 bits wide for the uimm6 scalar forms, and the packed lanes still
    // read its low half.
    val allOnes = B((BigInt(1) << width) - 1, width bits)
    val loZeros = (allOnes << n).resize(width bits) // low n bits 0, rest 1 == -(2^n) pattern
    val maxBits = ~loZeros // == 2^n - 1

    val mid = SatBounds(width)
    mid.maxBits := maxBits
    mid.loZeros := loZeros
    mid
  }

  /** The full-width lane: RV64 sati/usati saturate the whole 64-bit register. */
  def clampFull(rs1: Bits, mid: SatBounds, unsigned: Bool): (Bits, Bool) = {
    require(rs1.getWidth == mid.loZeros.getWidth,
      "clampFull() wants bounds as wide as the source")
    clampLane(rs1, mid.loZeros, mid.maxBits, unsigned)
  }

  def clampToBounds(rs1: Bits, mid: SatBounds, ew: EW.C, unsigned: Bool): (Bits, Bool) = {
    require(rs1.getWidth == 32, "clampToBounds() is the per-32-bit-word network")
    val loZeros32 = mid.loZeros(31 downto 0)
    val maxBits32 = mid.maxBits(31 downto 0)

    // Word lane: the scalar sati/usati at XLEN=32, one of psati.w's two lanes at XLEN=64.
    val (wOut, wSat) = clampLane(rs1, loZeros32, maxBits32, unsigned)

    val loZeros16 = loZeros32(15 downto 0)
    val maxBits16 = maxBits32(15 downto 0)

    // Halfword lanes (packed psati.h/pusati.h) — independent, no cross-lane carry
    val (h0Out, h0Sat) = clampLane(rs1(15 downto 0), loZeros16, maxBits16, unsigned)
    val (h1Out, h1Sat) = clampLane(rs1(31 downto 16), loZeros16, maxBits16, unsigned)
    val hOut = h1Out ## h0Out

    val rd = Mux(ew === EW.W, wOut, hOut)
    val sat = Mux(ew === EW.W, wSat, h0Sat || h1Sat)
    (rd, sat)
  }

}
