// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core._
import vexiiriscv.execute.rvp.kinds.RvpShiftKind._
import vexiiriscv.execute.rvp.kinds.{RvpShiftKind, RvpElemWidth => EW}

import scala.language.postfixOps

// Per-lane packed shifts. Each lane is ew bits wide and takes the FULL 5-bit amount:
// register forms (.bs/.hs) pass rs2[4:0] and rely on lane-width truncation to saturate
// over-shifts; immediate forms pass a pre-masked (<= log2(ew)) amount that never
// over-shifts. One saturating shifter serves both.
//   sll: (v << sh) then truncate to ew  -> zero fill, sh>=ew gives 0
//   srl: (v >> sh) on a fixed-width UInt -> zero fill, sh>=ew gives 0
//   sra: (v >> sh) on a fixed-width SInt -> sign fill, sh>=ew gives all-sign
//   srar: round-half-up arithmetic right shift
object RvpShiftOps {

  // Top bit index of the w-wide lane that bit i belongs to.
  private def laneTop(i: Int, w: Int): Int = (i / w) * w + w - 1

  // Bit i of the 32-bit lane space, in the 35-bit guarded space used by roundHalfUp: one
  // guard bit is inserted above each byte, so byte j starts at 9*j.
  private def guarded(i: Int): Int = i + i / 8

  /**
   * One 32-bit lane-segmented right shifter serving every (kind x lane width) combination.
   * Two moves collapse the whole structure into a single shifter:
   *
   *  1. Every kind becomes a RIGHT shift. Reversing all 32 bits maps lane k onto lane
   *     (n-1-k) with its bits reversed; lane order is irrelevant to a per-lane shift and the
   *     second reversal puts it back, so a left shift is `reverse -> shift right -> reverse`.
   *     Reversal is wiring on an FPGA, the two muxes that select it cost ~2 LUT/bit, and
   *     every left shifter disappears. It is ew-independent precisely because the reversal
   *     is over the whole word rather than within each lane.
   *
   *  2. One shifter serves B/H/W by injecting the fill at lane boundaries. At stage s a bit
   *     takes its neighbour in[i + 2^s] unless that index crosses its lane top, in which
   *     case it takes the fill -- 0 for SRL and reversed-SLL, the lane's sign for SRA/SRAR.
   *     Whether it crosses is a compile-time function of (i, s) per width and is monotone in
   *     the width (inside a byte lane => inside its halfword => inside the word), so the
   *     select degenerates to one of four cases: always take, always fill, `!byte`, `word`.
   *     Nothing outside the boundary bits pays for it.
   *
   * SRAR rides the same shifter one place short of the others -- see roundHalfUp.
   *
   * There is no RV32 psrari.b -- SRARI decodes ELEM_WIDTH -> W -- but under one shared
   * shifter the byte geometry costs nothing extra, so correct byte-lane rounding is kept
   * rather than skipped on the argument that a future psrari.b would never reach it.
   */
  def packed(rs1: UInt, shiftAmount: UInt, elemWidth: EW.C, op: RvpShiftKind.C): Bits = {
    require(shiftAmount.getWidth == 5, "packed() takes the 5-bit lane shift amount")

    val src0 = rs1.asBits
    val half = elemWidth === EW.H
    val word = elemWidth === EW.W
    val byte = !(half || word)

    val isLeft = op === SLL
    val isRound = op === SRAR
    val arith = op === SRA || op === SRAR

    val src = Mux(isLeft, src0.reversed, src0)
    // SRAR shifts one place short and finishes with the rounding increment below.
    val amt = Mux(isRound, shiftAmount - 1, shiftAmount)

    // Fill per byte group: the sign bit of the lane that group belongs to, or 0. Four
    // signals cover all three geometries -- under H both groups of a halfword see bit 15/31,
    // under W all four see bit 31.
    val fill = (0 until 4).map { k =>
      arith && Mux(word, src(31), Mux(half, src(16 * (k / 2) + 15), src(8 * k + 7)))
    }

    var cur = src
    for (s <- 0 until 5) {
      val d = 1 << s
      val nxt = Bits(32 bits)
      for (i <- 0 until 32) {
        // Monotone in the lane width: inByte => inHalf => inWord.
        val inByte = i + d <= laneTop(i, 8)
        val inHalf = i + d <= laneTop(i, 16)
        val inWord = i + d <= 31
        val shifted = (inByte, inHalf, inWord) match {
          case (true, _, _) => cur(i + d)
          case (false, true, _) => Mux(byte, fill(i / 8), cur(i + d))
          case (false, false, true) => Mux(word, cur(i + d), fill(i / 8))
          case _ => fill(i / 8)
        }
        nxt(i) := Mux(amt(s), shifted, cur(i))
      }
      cur = nxt
    }

    val plain = Mux(isLeft, cur.reversed, cur)
    // sh == 0 has no bit below the point to round on, and `amt` wrapped to 31 for it.
    val rounded = Mux(shiftAmount === 0, src0, roundHalfUp(cur, byte, half, word))

    Mux(isRound, rounded, plain)
  }

  /**
   * Per-lane round-half-up of a value already shifted one place short: given
   * T = value >>> (sh-1), returns (T + 1) >>> 1 in each lane, arithmetic.
   *
   * With q = floor(v / 2^sh) and b = bit sh-1 of v, round-half-up is q + b, and
   * T = 2q + b, so (T + 1) >>> 1 = q + b. That is why SRAR needs no shifter of its own:
   * one place of the shared shifter plus this increment avoids a second barrel shifter.
   *
   * The increment is segmented the way RvpPackedAluPlugin segments its adder: a guard bit
   * above each byte holds `prop` (1 lets a carry cross, 0 kills it, since 1 + 0 + c carries
   * c and 0 + 0 + c carries nothing), and the +1 is injected at each active lane's LSB.
   * One 35-bit carry chain covers B, H and W.
   */
  private def roundHalfUp(t: Bits, byte: Bool, half: Bool, word: Bool): Bits = {
    val p0 = half || word // bytes 0|1 share a lane
    val p1 = word         // bytes 1|2 share a lane
    val p2 = half || word // bytes 2|3 share a lane

    val tWide = (t(31 downto 24) ## p2 ## t(23 downto 16) ## p1 ##
      t(15 downto 8) ## p0 ## t(7 downto 0)).asUInt
    // +1 at bit 0 of every lane: byte lanes start at 0/8/16/24 -> guarded 0/9/18/27.
    val incWide = (B(0, 7 bits) ## byte ## B(0, 8 bits) ## !word ##
      B(0, 8 bits) ## byte ## B(0, 8 bits) ## True).asUInt
    val sum = tWide + incWide

    def u(i: Int): Bool = sum(guarded(i))

    val out = Bits(32 bits)
    for (i <- 0 until 32) {
      // Interior bits take the next bit up of T+1 (that is the >>> 1). A lane's top bit
      // takes bit w of the (w+1)-bit sext(T)+1, which is T[top] & (T+1)[top]: T >= 0 gives
      // 0, T == -1 gives 0 (it rounds to 0), T <= -2 stays negative.
      val laneTopBit = t(i) && u(i)
      out(i) := (i match {
        case 31 => laneTopBit
        case 15 => Mux(word, u(16), laneTopBit)
        case 7 | 23 => Mux(byte, laneTopBit, u(i + 1))
        case _ => u(i + 1)
      })
    }
    out
  }

  /**
   * One 64-bit segmented left shifter for every widening-left instruction.
   *
   * Input placement widens the source lanes before they enter the bank: scalar W uses one
   * 64-bit lane, packed H uses two 32-bit lanes, and packed B uses four 16-bit lanes.  The
   * six shift stages then differ only at the physical lane boundaries.  Crossing a 16-bit
   * boundary is allowed for H/scalar, crossing a 32-bit boundary only for scalar, and every
   * other lane-bottom input is zero.  Thus packed shifts of 16 or more naturally produce
   * zero in each byte-derived lane while scalar shifts retain the full 0..63 range.
   *
   * This bank is intentionally separate from packed(): the ordinary packed shift path and
   * its timing-sensitive SHIFT_OP fan-in stay unchanged.
   */
  def wideLeftBank(rs1: UInt, shiftAmount: UInt, elemWidth: EW.C, scalar: Bool, signed: Bool): Bits = {
    require(rs1.getWidth == 32, "wideLeftBank() takes a 32-bit source")
    require(shiftAmount.getWidth == 6, "wideLeftBank() takes a 6-bit shift amount")

    val half = elemWidth === EW.H

    def extend(v: UInt, width: Int): Bits =
      Mux(signed, v.asSInt.resize(width).asBits, v.resize(width).asBits)

    val wordIn = extend(rs1, 64)
    val halfIn = extend(rs1(31 downto 16), 32) ## extend(rs1(15 downto 0), 32)
    val byteIn = extend(rs1(31 downto 24), 16) ## extend(rs1(23 downto 16), 16) ##
      extend(rs1(15 downto 8), 16) ## extend(rs1(7 downto 0), 16)
    val bankIn = Mux(scalar, wordIn, Mux(half, halfIn, byteIn))

    var cur = bankIn
    for (s <- 0 until 6) {
      val d = 1 << s
      val nxt = Bits(64 bits)
      for (i <- 0 until 64) {
        val source = i - d
        val shifted = if (source < 0) {
          False
        } else if (i / 16 == source / 16) {
          cur(source)
        } else if (i / 32 == source / 32) {
          Mux(scalar || half, cur(source), False)
        } else {
          Mux(scalar, cur(source), False)
        }
        nxt(i) := Mux(shiftAmount(s), shifted, cur(i))
      }
      cur = nxt
    }

    cur
  }

  /**
   * One 65-bit segmented right-shift bank: the XLEN=64 scalar shift substrate.
   *
   * `v` is {source, 0} -- bit 64 is the source's sign bit and bit 0 is the
   * rounding guard the callers below consume.  A 7-bit amount builds the
   * variable-shift distance-64 stage; a 6-bit immediate builds only the
   * 0..63 stages needed by srari.
   */
  private def rightBank65Stages(v: Bits, amount: UInt, fill: Bool,
                                firstStage: Int): Bits = {
    require(v.getWidth == 65, "rightBank65() takes a 65-bit value")

    var cur = v
    for (s <- 0 until amount.getWidth) {
      val d = 1 << (firstStage + s)
      val nxt = Bits(65 bits)
      for (i <- 0 until 65) {
        val shifted = if (i + d > 64) fill else cur(i + d)
        nxt(i) := Mux(amount(s), shifted, cur(i))
      }
      cur = nxt
    }
    cur
  }

  private def rightBank65(v: Bits, amount: UInt, fill: Bool): Bits = {
    require(amount.getWidth == 6 || amount.getWidth == 7,
      "rightBank65() takes a 6-bit immediate or 7-bit clamped amount")
    rightBank65Stages(v, amount, fill, firstStage = 0)
  }

  /** RV64 srari: immediate-only arithmetic right shift with round-half-up.
    *
    * Unlike scalarVarShift64, this operation never shifts left and its uimm6
    * cannot name 64 or an out-of-range amount.  Keeping it on this dedicated
    * six-level bank removes the direction/reversal muxes, magnitude clamp and
    * distance-64 level from the timing path.
    */
  def srari64(rs1: Bits, amount: UInt): Bits = {
    require(rs1.getWidth == 64, "srari64() takes a 64-bit source")
    require(amount.getWidth == 6, "srari64() takes a uimm6 amount")

    val shifted = rightBank65(rs1 ## False, amount, rs1.msb)
    val rounded = shifted.asUInt + (amount =/= 0).asUInt
    rounded(64 downto 1).asBits
  }

  /**
   * The RV64 scalar variable shifts sha / shar / shl / shlr, over the whole
   * 64-bit register.
   *
   * `amount` is rs2[7:0] read as a SIGNED byte, the same convention
   * RvpSatShiftOps uses: negative shifts right (arithmetic when `signedArith`,
   * optionally round-half-up), non-negative shifts left. The difference -- the
   * only one -- is that the left side discards what leaves the top instead of
   * clamping, so these four never touch vxsat.
   *
   * The immediate-only srari uses srari64 above; unlike these variable forms,
   * it needs neither direction/magnitude decode nor the distance-64 level.
   *
   * One bank serves both directions. A left shift is `reverse -> shift right ->
   * reverse` (the argument in packed() above: reversal is wiring, and it buys
   * back a whole second 65-bit barrel).
   */
  def scalarVarShift64Low(rs1: Bits, amount: Bits,
                          signedArith: Bool): (Bits, UInt, Bool, Bool) = {
    require(rs1.getWidth == 64, "scalarVarShift64Low() takes a 64-bit source")
    require(amount.getWidth == 8, "scalarVarShift64Low() takes the 8-bit signed amount")

    val right = amount.msb
    // Both magnitudes CLAMP (left at 64, right at 65) instead of wrapping:
    // >= +64 must flush to zero and <= -64 must reach the all-sign / all-zero
    // slice. Masking the amount to six bits gets exactly those boundaries wrong.
    val rMag = (~amount).asUInt + U(1)
    val lMag = amount(6 downto 0).asUInt
    val mag = Mux(right, rMag, lMag)
    // Right clamps at 65 so the guard bit leaves too: shlr's raw magnitude
    // (spec 35adffd) rounds to 0 past -64; sha/shar are all-fill either way.
    val limit = Mux(right, U(65, 7 bits), U(64, 7 bits))
    val clamped = Mux(mag > limit, limit, mag.resize(7 bits))

    val fill = right && signedArith && rs1.msb
    val shifted = rightBank65Stages(
      Mux(right, rs1, rs1.reversed) ## False,
      clamped(3 downto 0), fill, firstStage = 0)

    (shifted, clamped(6 downto 4), right, fill)
  }

  /** Finish the registered upper half of scalarVarShift64Low.
    *
    * Four barrel levels and the direction/magnitude clamp live before the
    * Execute(0 -> 1) register; the remaining three levels, rounding carry and
    * direction mux live after it. This is the physical timing cut for the
    * RV64 sha/shar/shl/shlr bank.
    */
  def scalarVarShift64High(mid: Bits, amountHigh: UInt, right: Bool,
                           fill: Bool, rounding: Bool): Bits = {
    require(mid.getWidth == 65, "scalarVarShift64High() takes a 65-bit intermediate")
    require(amountHigh.getWidth == 3, "scalarVarShift64High() takes three upper shamt bits")

    val shifted = rightBank65Stages(mid, amountHigh, fill, firstStage = 4)

    // Right: (shx + round)[64:1] -- the +1 is the round-half-up increment the
    // guard bit at the bottom was reserved for. Left: the same [64:1] window,
    // un-reversed.
    val rounded = (shifted.asUInt + (rounding && right).asUInt)(64 downto 1).asBits
    Mux(right, rounded, shifted(64 downto 1).reversed)
  }

  def laneSll(rs1: UInt, shiftAmount: UInt, elementWidth: Int): UInt = (rs1 << shiftAmount).resize(elementWidth)

  def laneSrl(rs1: UInt, shiftAmount: UInt, elementWidth: Int): UInt = (rs1 >> shiftAmount).resize(elementWidth)

  def laneSra(rs1: UInt, shiftAmount: UInt, elementWidth: Int): UInt = (rs1.asSInt >> shiftAmount).asUInt.resize(elementWidth)
}
