// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.kinds

import spinal.core.SpinalEnum

object RvpMulRes extends SpinalEnum {
  val SUM0, SUM15, SUM16, SUM31, SUM32, SUMQ, LANE_LO, LANE_HI, LANE_MIX, LANE_Q, WIDE_H, WIDE_B = newElement()
}
