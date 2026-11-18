// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core._
import spinal.lib.traversableOnceBoolPimped
import vexiiriscv.execute.rvp.kinds.{RvpElemWidth => EW}


object RvpSaturatingAddOps {

  // One width-unified per-byte injection network, instantiated once per saturating
  // family. satHi/satLo are the overflow predicates at byte boundaries [7,15,23,31]
  // (sign bits for signed, guard carries for unsigned). A byte boundary is a lane MSB —
  // owns its lane's saturate decision and is that lane's top byte — exactly when ew does
  // not merge it into a wider lane; that mask == !prop(j), so the clamp lanes track the
  // shared adder's guards. Each byte routes to its lane owner and picks the
  // top-vs-interior clamp constant.
  private def injectUnified(ew: EW.C, satHi: Seq[Bool], satLo: Seq[Bool], passthrough: Bits,
                            hiTop: Bits, hiInt: Bits, loTop: Bits, loInt: Bits): (Bits, Bool) = {
    require(Seq(hiTop, hiInt, loTop, loInt).forall(_.getWidth == 8),
      "saturating injection patterns must be bytes")
    val half = ew === EW.H
    val word = ew === EW.W
    val byte = !(half || word)
    val active = Seq(byte, !word, byte, True) // boundary j is a lane MSB (== !prop(j))
    // Owner lane's sat bit for byte j (the ew fan-out): lowest active boundary >= j.
    def hiEff(j: Int): Bool = j match {
      case 0 => Mux(word, satHi(3), Mux(half, satHi(1), satHi(0)))
      case 1 => Mux(word, satHi(3), satHi(1))
      case 2 => Mux(half || word, satHi(3), satHi(2))
      case _ => satHi(3)
    }
    def loEff(j: Int): Bool = j match {
      case 0 => Mux(word, satLo(3), Mux(half, satLo(1), satLo(0)))
      case 1 => Mux(word, satLo(3), satLo(1))
      case 2 => Mux(half || word, satLo(3), satLo(2))
      case _ => satLo(3)
    }
    val outBytes = for (j <- 0 until 4) yield {
      val top = active(j) // top byte of its lane -> *_TOP constant, interior byte -> *_INT
      val hiPat = Mux(top, hiTop, hiInt)
      val loPat = Mux(top, loTop, loInt)
      val pass = passthrough(8 * j + 7 downto 8 * j)
      Mux(hiEff(j), hiPat, Mux(loEff(j), loPat, pass))
    }
    val rd = outBytes.reverse.reduce(_ ## _)
    val sat = (0 until 4).map(j => active(j) && (satHi(j) || satLo(j))).reduce(_ || _)
    (rd, sat)
  }

  private def signedSatShift1Lanes(a: Bits, w: Int): Seq[(Bits, Bool)] = {
    val maxValue = (BigInt(1) << (w - 1)) - 1
    val minValue = BigInt(1) << (w - 1)
    for (i <- 0 until 32 / w) yield {
      val high = w * i + w - 1
      val lane = a(high downto w * i)
      val overflows = lane(w - 1) ^ lane(w - 2)
      val wrapped = lane(w - 2 downto 0) ## B(0, 1 bits)
      val result = Mux(overflows,
        Mux(lane(w - 1),
          B(minValue, w bits),
          B(maxValue, w bits)
        ),
        wrapped
      )
      (result, overflows)
    }
  }

  def signedSatShift1(a: Bits, ew: EW.C): (Bits, Bool) = {
    val hLanes = signedSatShift1Lanes(a, 16)
    val wLanes = signedSatShift1Lanes(a, 32)
    val hRes = hLanes.map(_._1).reverse.reduce(_ ## _)
    val wRes = wLanes.map(_._1).reverse.reduce(_ ## _)
    val hSat = hLanes.map(_._2).orR
    val wSat = wLanes.map(_._2).orR
    (Mux(ew === EW.W,
      wRes,
      hRes
    ), Mux(ew === EW.W,
      wSat,
      hSat
    )
    )
  }

  private def signedSatAbsLane(rs1: Bits, w: Int): (Bits, Bool) = {

    val lanes = for (i <- 0 until 32 / w) yield {

      val minValue = BigInt(-1) << (w - 1) // detect
      val clampValue = (BigInt(1) << (w - 1)) - 1 //output
      val byte = rs1(w * i + w - 1 downto w * i).asSInt
      val smallerZero = byte < 0
      val clamp = byte === S(minValue, w bits)
      val psabsb_byteresult = Mux(clamp,
        S(clampValue, w bits),
        Mux(smallerZero,
          -byte,
          byte
        )
      )
      (psabsb_byteresult, clamp)
    }

    val psabsb = lanes.map(_._1.asBits).reverse.reduce(_ ## _)
    val vxsat = lanes.map(_._2).orR

    (psabsb, vxsat)
  }

  def signedSatAbs(rs1: Bits, width: EW.C): (Bits, Bool) = {
    val (bRes, bSat) = signedSatAbsLane(rs1, 8)
    val (hRes, hSat) = signedSatAbsLane(rs1, 16)
    val half = width === EW.H
    (Mux(half, hRes, bRes), Mux(half, hSat, bSat))
  }

  // One detection and injection network serves signed/unsigned add/sub, the
  // mixed add-sub cross forms, and SSH1's final signed add. `subLo` applies to
  // the low halfword, `subHi` to the high halfword; ordinary operations drive
  // both from DO_SUB.
  def satAddSub(rd: Bits, a: Bits, b: Bits, carries: Seq[Bool],
                ew: EW.C, signed: Bool, subLo: Bool, subHi: Bool): (Bits, Bool) = {
    require(carries.length == 4, "saturating add/sub needs four lane carries")
    val P = Seq(7, 15, 23, 31)
    def subAt(j: Int): Bool = if (j < 2) subLo else subHi

    val satHi = P.zipWithIndex.map { case (p, j) =>
      Mux(signed,
        Mux(subAt(j), !a(p) && b(p) && rd(p), !a(p) && !b(p) && rd(p)),
        !subAt(j) && carries(j))
    }
    val satLo = P.zipWithIndex.map { case (p, j) =>
      Mux(signed,
        Mux(subAt(j), a(p) && !b(p) && !rd(p), a(p) && b(p) && !rd(p)),
        subAt(j) && carries(j))
    }

    injectUnified(
      ew, satHi, satLo, rd,
      hiTop = Mux(signed, B(0x7f, 8 bits), B(0xff, 8 bits)),
      hiInt = B(0xff, 8 bits),
      loTop = Mux(signed, B(0x80, 8 bits), B(0x00, 8 bits)),
      loInt = B(0x00, 8 bits)
    )
  }

}
