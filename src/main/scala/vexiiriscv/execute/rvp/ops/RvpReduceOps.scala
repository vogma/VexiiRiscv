// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.ops

import spinal.core._
import spinal.lib._
import vexiiriscv.execute.rvp.kinds.{RvpElemWidth => EW}

// Cross-lane reduction sum (predsum.* / predsumu.*)
object RvpReduceOps {

  def reduceSum(rs1: UInt, half: Bool, signed: Bool): Bits = {

    def ext(v: UInt): SInt = {
      val prefix = Mux(signed, v.msb, False)
      (prefix.asBits ## v.asBits).asSInt
    }


    val byteSum = (0 until 4).map(i => ext(rs1(8 * i + 7 downto 8 * i))).reduceBalancedTree(_ +^ _)
    val halfwordSum = (0 until 2).map(i => ext(rs1(16 * i + 15 downto 16 * i))).reduceBalancedTree(_ +^ _)

    val narrowSum = Mux(half, halfwordSum, byteSum.resize(18 bits))
    narrowSum.resize(32 bits).asBits
  }

  /** XLEN-wide reduction over one whole register (predsum.* at XLEN=64).
    *
    * `reduceSum` above serves the RV32 forms: a 32-bit source and two lane
    * widths (the `.d*` forms run it twice through the packed ALU's two-pass
    * sequencer, once per pair word). At XLEN=64 the source is the full
    * register, the accumulator is 64 bits, and `predsum.ws` / `predsumu.ws`
    * add a third lane geometry -- so all three trees are built and `ew` picks
    * one. The extra `ext` bit makes an unsigned element a non-negative SInt,
    * which is why one signed tree serves both signednesses.
    */
  def reduceWideSum(src: UInt, ew: EW.C, signed: Bool): Bits = {
    val w = src.getWidth

    def ext(v: UInt): SInt = (Mux(signed, v.msb, False).asBits ## v.asBits).asSInt

    def tree(elemW: Int): SInt =
      (0 until w / elemW)
        .map(i => ext(src(elemW * i + elemW - 1 downto elemW * i)))
        .reduceBalancedTree(_ +^ _)

    val sum = ew.mux(
      EW.B -> tree(8).resize(w bits),
      EW.H -> tree(16).resize(w bits),
      default -> tree(32).resize(w bits)
    )
    sum.asBits
  }

}
