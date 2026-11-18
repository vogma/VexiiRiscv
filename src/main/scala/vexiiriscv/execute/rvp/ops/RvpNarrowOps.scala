// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core._
import vexiiriscv.execute.rvp.kinds.{RvpElemWidth => EW}

import scala.language.postfixOps

/** Shared datapath for the pair-source narrowing shift and clip families. */
object RvpNarrowOps {

  private def laneTop(i: Int, width: Int): Int = (i / width) * width + width - 1

  /**
   * Shift and round the 64-bit pair source before the Execute(1) register.
   *
   * One six-stage right shifter serves the 1x64, 2x32 and 4x16 geometries. At
   * each stage a bit either takes its upper neighbour or the selected lane's
   * sign/zero fill. The amount-zero bypass is carried in `t`, because amount-1
   * wraps to 63 and the rounding stage must not observe that internal value.
   *
   * `fill` has one bit per 16-bit chunk. Under wider geometries adjacent chunks
   * deliberately carry the same active-lane fill. The final `(T + round) >> 1`
   * uses one 72-bit segmented carry chain, but keeping it on this side of the
   * register restores the pre-N1 timing cut: Execute(2) only narrows/clamps.
   */
  def shiftAndRound(src: UInt, shiftAmount: UInt, elemWidth: EW.C,
                    signed: Bool, round: Bool): (Bits, Bits) = {
    require(src.getWidth == 64, "shiftAndRound() takes the 64-bit pair source")
    require(shiftAmount.getWidth == 6, "shiftAndRound() takes a 6-bit amount")

    val srcBits = src.asBits
    val half = elemWidth === EW.H
    val word = elemWidth === EW.W
    val byte = !(half || word)
    val amountZero = shiftAmount === 0
    val amount = shiftAmount - 1

    val fill = Bits(4 bits)
    for (chunk <- 0 until 4) {
      val laneSign = Mux(word, srcBits(63),
        Mux(half, srcBits(32 * (chunk / 2) + 31), srcBits(16 * chunk + 15)))
      fill(chunk) := signed && laneSign
    }

    var cur = srcBits
    for (stage <- 0 until 6) {
      val distance = 1 << stage
      val next = Bits(64 bits)
      for (i <- 0 until 64) {
        // Crossing is monotone: inside 16 => inside 32 => inside 64.
        val in16 = i + distance <= laneTop(i, 16)
        val in32 = i + distance <= laneTop(i, 32)
        val in64 = i + distance <= 63
        val shifted = (in16, in32, in64) match {
          case (true, _, _) => cur(i + distance)
          case (false, true, _) => Mux(byte, fill(i / 16), cur(i + distance))
          case (false, false, true) => Mux(word, cur(i + distance), fill(i / 16))
          case _ => fill(i / 16)
        }
        next(i) := Mux(amount(stage), shifted, cur(i))
      }
      cur = next
    }

    val t = Mux(amountZero, srcBits, cur)

    def laneEnd(chunk: Int): Bool = chunk match {
      case 3 => True
      case 1 => !word
      case _ => byte
    }

    def laneStart(chunk: Int): Bool = chunk match {
      case 0 => True
      case 2 => !word
      case _ => byte
    }

    def dataChunk(chunk: Int): Bits =
      t(16 * chunk + 15 downto 16 * chunk)

    def valueChunk(chunk: Int): Bits = {
      val end = laneEnd(chunk)
      (!end).asBits ## Mux(end, fill(chunk), True).asBits ## dataChunk(chunk)
    }

    def roundChunk(chunk: Int): Bits =
      B(0, 17 bits) ## (round && laneStart(chunk)).asBits

    val valueWide = (valueChunk(3) ## valueChunk(2) ##
      valueChunk(1) ## valueChunk(0)).asUInt
    val roundWide = (roundChunk(3) ## roundChunk(2) ##
      roundChunk(1) ## roundChunk(0)).asUInt
    val sum = valueWide + roundWide

    def sumData(chunk: Int): Bits =
      sum(18 * chunk + 15 downto 18 * chunk).asBits

    def sumTop(chunk: Int): Bool = sum(18 * chunk + 16)

    def shiftedChunk(chunk: Int): Bits = {
      val upper = if (chunk == 3) sumTop(chunk)
      else Mux(laneEnd(chunk), sumTop(chunk), sumData(chunk + 1)(0))
      upper.asBits ## sumData(chunk)(15 downto 1)
    }

    val shifted = shiftedChunk(3) ## shiftedChunk(2) ##
      shiftedChunk(1) ## shiftedChunk(0)
    val data = Mux(amountZero, t, shifted)

    // The active lane's extra bit is the original fill on the amount-zero
    // bypass and the lane-ending top-cell sum after rounding otherwise.
    val top = Bits(4 bits)
    for (chunk <- 0 until 4)
      top(chunk) := Mux(amountZero, fill(chunk), sumTop(chunk))

    (data, top)
  }

  /**
   * RV64 pnclipp.* / pnclipup.*: clamp the 128-bit `rs2 @ rs1` source into one
   * 64-bit register.
   *
   * The one narrowing family with no shift at all -- no barrel, no rounding, no
   * amount -- so it shares nothing with shiftAndRound above and is much smaller
   * than it looks: each 2w-bit element is out of range exactly when its top w+1
   * bits disagree (signed) or any bit above the low w is set (unsigned), and
   * the clamp value follows from the source's sign. rs2 is the HIGH half, so
   * lane i for i < lanes/2 comes from rs1 (P-ext-proposal.adoc:21938).
   */
  def clipPair128(src: Bits, elemWidth: EW.C, unsigned: Bool): (Bits, Bool) = {
    require(src.getWidth == 128, "clipPair128() takes the 128-bit rs2 @ rs1 source")

    def geometry(w: Int): (Bits, Bool) = {
      val lanes = (0 until 64 / w).map { i =>
        val v = src(2 * w * i + 2 * w - 1 downto 2 * w * i)
        // Signed: in range iff bits [2w-1 : w-1] are all equal.
        val hiS = v(2 * w - 1 downto w - 1)
        val ovfS = !hiS.andR && hiS.orR
        val maxS = B((BigInt(1) << (w - 1)) - 1, w bits)
        val minS = B(BigInt(1) << (w - 1), w bits)
        val resS = Mux(ovfS, Mux(v.msb, minS, maxS), v(w - 1 downto 0))
        // Unsigned: in range iff bits [2w-1 : w] are all zero.
        val ovfU = v(2 * w - 1 downto w).orR
        val resU = Mux(ovfU, B((BigInt(1) << w) - 1, w bits), v(w - 1 downto 0))
        (Mux(unsigned, resU, resS), Mux(unsigned, ovfU, ovfS))
      }
      (lanes.map(_._1).reduceLeft((acc, next) => next ## acc),
        lanes.map(_._2).reduce(_ || _))
    }

    val (byteOut, byteSat) = geometry(8)
    val (halfOut, halfSat) = geometry(16)
    val (wordOut, wordSat) = geometry(32)

    (elemWidth.mux(EW.B -> byteOut, EW.H -> halfOut, default -> wordOut),
      elemWidth.mux(EW.B -> byteSat, EW.H -> halfSat, default -> wordSat))
  }

  /** Narrow the registered rounded lanes and optionally clamp them. */
  def finish(data: Bits, top: Bits, elemWidth: EW.C,
             signed: Bool, clip: Bool): (Bits, Bool) = {
    require(data.getWidth == 64, "finish() takes the rounded 64-bit lane data")
    require(top.getWidth == 4, "finish() takes one top bit per 16-bit chunk")

    def saturated(lane: Bits, laneTop: Bool, dstWidth: Int): Bool = {
      val srcWidth = lane.getWidth
      val signedHigh = laneTop.asBits ## lane(srcWidth - 1 downto dstWidth - 1)
      val signedSat = !(signedHigh.andR || !signedHigh.orR)
      val unsignedSat = lane(srcWidth - 1 downto dstWidth).orR
      Mux(signed, signedSat, unsignedSat)
    }

    val satW = saturated(data, top(3), 32)
    val satH0 = saturated(data(31 downto 0), top(1), 16)
    val satH1 = saturated(data(63 downto 32), top(3), 16)
    val satB = (0 until 4).map { chunk =>
      saturated(data(16 * chunk + 15 downto 16 * chunk), top(chunk), 8)
    }

    val plain = elemWidth.mux(
      EW.W -> data(31 downto 0),
      EW.H -> (data(47 downto 32) ## data(15 downto 0)),
      EW.B -> (data(55 downto 48) ## data(39 downto 32) ##
        data(23 downto 16) ## data(7 downto 0)))

    val satMask = elemWidth.mux(
      EW.W -> B(32 bits, default -> satW),
      EW.H -> (B(16 bits, default -> satH1) ## B(16 bits, default -> satH0)),
      EW.B -> (B(8 bits, default -> satB(3)) ## B(8 bits, default -> satB(2)) ##
        B(8 bits, default -> satB(1)) ## B(8 bits, default -> satB(0))))

    def clampValue(width: Int, negative: Bool): Bits = {
      val maxSigned = B((BigInt(1) << (width - 1)) - 1, width bits)
      val minSigned = B(BigInt(1) << (width - 1), width bits)
      val maxUnsigned = B((BigInt(1) << width) - 1, width bits)
      Mux(signed, Mux(negative, minSigned, maxSigned), maxUnsigned)
    }

    val clamped = elemWidth.mux(
      EW.W -> clampValue(32, top(3)),
      EW.H -> (clampValue(16, top(3)) ## clampValue(16, top(1))),
      EW.B -> (clampValue(8, top(3)) ## clampValue(8, top(2)) ##
        clampValue(8, top(1)) ## clampValue(8, top(0))))

    val injected = (plain & ~satMask) | (clamped & satMask)
    (Mux(clip, injected, plain), satMask.orR)
  }
}
