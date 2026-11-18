// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.kinds

import spinal.core.SpinalEnum

// Operation family for the packed ALU, set at decode and selecting the writeback
// form in RvpPackedAluPlugin. All families share the one segmented adder, they only
// differ in how they read out its result (raw sum, carry bits, or operand select).
//   ADDSUB          add / subtract              -> raw lane sum
//   SLTU / SLT      set-less-than u / signed    -> per-lane 0x00/0xFF mask
//   CMPEQ           set-equal                   -> per-lane 0x00/0xFF mask
//   MINU/MIN/MAXU/MAX  min / max, u / signed    -> per-lane operand select
object RvpAluKind extends SpinalEnum {
  val ADDSUB, SLTU, WIDEN, SLT, CMPEQ, MINU, MIN, MAXU, MAX, AVGU, AVGS, ABSD, SABS, ABSDU, ABSDSUMU, REDSUMS, REDSUMU, SADD, SADDU, SSUB, SSUBU, SATIW, USATIW, SCROSS, SSH1 = newElement()
}
