// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core._
import vexiiriscv.execute.rvp.kinds.{RvpMulNet, RvpMulRes}
import vexiiriscv.execute.rvp.kinds.RvpMulNet._
import vexiiriscv.execute.rvp.kinds.RvpMulRes._

object RvpMulOps {


  private def ext(v: UInt, signed: Bool): SInt = {
    ((signed && v.msb) ## v).asSInt.resize(17)
  }

  def slotA(rs1: UInt, slot: Int, net: RvpMulNet.C, selA: Bool, sgnA: Bool): SInt = {
    val signed = slot match {
      case 0 => (net === RvpMulNet.S_WH || net === RvpMulNet.S_WW) ? False | sgnA
      case 1 => sgnA
      case 2 => (net === RvpMulNet.S_WW) ? False | sgnA
      case 3 => sgnA
      case _ => SpinalError(s"bad slot $slot")
    }

    slot match {
      case 0 => net.mux(
        S_HH -> (selA ? ext(rs1(31 downto 16), signed) | ext(rs1(15 downto 0), signed)),
        S_WH -> ext(rs1(15 downto 0), signed),
        S_WW -> ext(rs1(15 downto 0), signed),
        P_HH -> ext(rs1(15 downto 0), signed),
        P_BB -> (selA ? ext(rs1(15 downto 8), signed) | ext(rs1(7 downto 0), signed)),
        P_HB -> ext(rs1(15 downto 0), signed),
        P_B4 -> ext(rs1(7 downto 0), signed)
      )
      case 1 => net.mux(
        S_HH -> S(0, 17 bits),
        S_WH -> ext(rs1(31 downto 16), signed),
        S_WW -> ext(rs1(31 downto 16), signed),
        P_HH -> ext(rs1(31 downto 16), signed),
        P_BB -> (selA ? ext(rs1(31 downto 24), signed) | ext(rs1(23 downto 16), signed)),
        P_HB -> ext(rs1(31 downto 16), signed),
        P_B4 -> ext(rs1(15 downto 8), signed)
      )
      case 2 => net.mux(
        S_HH -> S(0, 17 bits),
        S_WH -> S(0, 17 bits),
        S_WW -> ext(rs1(15 downto 0), signed),
        P_HH -> S(0, 17 bits),
        P_BB -> S(0, 17 bits),
        P_HB -> S(0, 17 bits),
        P_B4 -> ext(rs1(23 downto 16), signed)
      )
      case 3 => net.mux(
        S_HH -> S(0, 17 bits),
        S_WH -> S(0, 17 bits),
        S_WW -> ext(rs1(31 downto 16), signed),
        P_HH -> S(0, 17 bits),
        P_BB -> S(0, 17 bits),
        P_HB -> S(0, 17 bits),
        P_B4 -> ext(rs1(31 downto 24), signed)
      )
      case _ => SpinalError(s"bad slot $slot")
    }
  }

  def slotB(rs2: UInt, slot: Int, net: RvpMulNet.C, selB: Bool, cross: Bool, sgnB: Bool): SInt = {
    val signed = slot match {
      case 0 => (net === S_WW) ? False | sgnB
      case 1 => (net === S_WW) ? False | sgnB
      case 2 => sgnB
      case 3 => sgnB
      case _ => SpinalError(s"bad slot $slot")
    }

    slot match {
      case 0 => net.mux(
        S_HH -> (selB ? ext(rs2(31 downto 16), signed) | ext(rs2(15 downto 0), signed)),
        S_WH -> (selB ? ext(rs2(31 downto 16), signed) | ext(rs2(15 downto 0), signed)),
        S_WW -> ext(rs2(15 downto 0), signed),
        P_HH -> (cross ? ext(rs2(31 downto 16), signed) | ext(rs2(15 downto 0), signed)),
        P_BB -> (selB ? ext(rs2(15 downto 8), signed) | ext(rs2(7 downto 0), signed)),
        P_HB -> (selB ? ext(rs2(15 downto 8), signed) | ext(rs2(7 downto 0), signed)),
        P_B4 -> ext(rs2(7 downto 0), signed)
      )
      case 1 => net.mux(
        S_HH -> S(0, 17 bits),
        S_WH -> (selB ? ext(rs2(31 downto 16), signed) | ext(rs2(15 downto 0), signed)),
        S_WW -> ext(rs2(15 downto 0), signed),
        P_HH -> (cross ? ext(rs2(15 downto 0), signed) | ext(rs2(31 downto 16), signed)),
        P_BB -> (selB ? ext(rs2(31 downto 24), signed) | ext(rs2(23 downto 16), signed)),
        P_HB -> (selB ? ext(rs2(31 downto 24), signed) | ext(rs2(23 downto 16), signed)),
        P_B4 -> ext(rs2(15 downto 8), signed)
      )
      case 2 => net.mux(
        S_HH -> S(0, 17 bits),
        S_WH -> S(0, 17 bits),
        S_WW -> ext(rs2(31 downto 16), signed),
        P_HH -> S(0, 17 bits),
        P_BB -> S(0, 17 bits),
        P_HB -> S(0, 17 bits),
        P_B4 -> ext(rs2(23 downto 16), signed)
      )
      case 3 => net.mux(
        S_HH -> S(0, 17 bits),
        S_WH -> S(0, 17 bits),
        S_WW -> ext(rs2(31 downto 16), signed),
        P_HH -> S(0, 17 bits),
        P_BB -> S(0, 17 bits),
        P_HB -> S(0, 17 bits),
        P_B4 -> ext(rs2(31 downto 24), signed)
      )
      case _ => SpinalError(s"bad slot $slot")
    }
  }

  def slot(a: SInt, b: SInt, c: SInt): SInt = {
    assert(a.getWidth == 17 && b.getWidth == 17)
    a * b + c
  }

  def roundConst(res: RvpMulRes.C, neg1: Bool, rnd: Bool, slot: Int): SInt = {
    slot match {
      case 0 => (rnd ? res.mux(
        LANE_HI -> S(BigInt(1) << 15, 34 bits),
        LANE_Q -> S(BigInt(1) << 14, 34 bits),
        SUMQ -> S(BigInt(1) << 14, 34 bits),
        SUM15 -> S(BigInt(1) << 14, 34 bits),
        SUM31 -> S(BigInt(1) << 30, 34 bits),
        SUM32 -> S(BigInt(1) << 31, 34 bits),
        default -> S(0, 34 bits)
      ) | S(0, 34 bits)) + (neg1 ? S(1, 34 bits) | S(0, 34 bits))
      case 1 => rnd ? res.mux(
        LANE_HI -> S(BigInt(1) << 15, 34 bits),
        LANE_Q -> S(BigInt(1) << 14, 34 bits),
        SUMQ -> S(BigInt(1) << 14, 34 bits),
        default -> S(0, 34 bits)
      ) | S(0, 34 bits)
      case 2 => S(0, 34 bits)
      case 3 => S(0, 34 bits)
      case _ => SpinalError(s"bad slot $slot")
    }
  }

  def align(p: SInt, slot: Int, net: RvpMulNet.C, res: RvpMulRes.C, neg1: Bool): SInt = {
    assert(p.getWidth == 34)

    val p64 = p.resize(64)

    slot match {
      case 0 => net.mux(
        S_HH -> p64,
        S_WH -> p64,
        S_WW -> p64,
        P_HH -> ((res === SUMQ) ? (p >> 15).resize(64) | p64),
        P_BB -> S(0, 64 bits),
        P_HB -> S(0, 64 bits),
        P_B4 -> p64
      )
      case 1 => net.mux(
        S_HH -> S(0, 64 bits),
        S_WH -> (p64 |<< 16),
        S_WW -> (p64 |<< 16),
        P_HH -> ((res === SUMQ) ?
          (p >> 15).resize(64) |
          (neg1 ?
            ~p64 |
            p64
            )
          ),

        P_BB -> S(0, 64 bits),
        P_HB -> S(0, 64 bits),
        P_B4 -> p64
      )
      case 2 => net.mux(
        S_HH -> S(0, 64 bits),
        S_WH -> S(0, 64 bits),
        S_WW -> (p64 |<< 16),
        P_HH -> S(0, 64 bits),
        P_BB -> S(0, 64 bits),
        P_HB -> S(0, 64 bits),
        P_B4 -> p64
      )
      case 3 => net.mux(
        S_HH -> S(0, 64 bits),
        S_WH -> S(0, 64 bits),
        S_WW -> (p64 |<< 32),
        P_HH -> S(0, 64 bits),
        P_BB -> S(0, 64 bits),
        P_HB -> S(0, 64 bits),
        P_B4 -> p64
      )
      case _ => SpinalError(s"wrong slot $slot")
    }
  }

  def readout(s: SInt, p0: SInt, p1: SInt, res: RvpMulRes.C): Bits = {
    assert(s.getWidth == 64 && p0.getWidth == 34 && p1.getWidth == 34)
    val sb = s.asBits
    res.mux(
      LANE_LO -> p1(15 downto 0) ## p0(15 downto 0),
      LANE_HI -> p1(31 downto 16) ## p0(31 downto 16),
      LANE_MIX -> p1(23 downto 8) ## p0(23 downto 8),
      LANE_Q -> p1(30 downto 15) ## p0(30 downto 15),
      SUM0 -> sb(31 downto 0),
      WIDE_H -> p0(31 downto 0).asBits,
      WIDE_B -> p1(15 downto 0).asBits ## p0(15 downto 0).asBits,
      SUM15 -> sb(46 downto 15),
      SUM16 -> sb(47 downto 16),
      SUM31 -> sb(62 downto 31),
      SUM32 -> sb(63 downto 32),
      SUMQ -> sb(31 downto 0)
    )
  }

}
