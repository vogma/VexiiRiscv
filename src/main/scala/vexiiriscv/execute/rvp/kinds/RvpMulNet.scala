// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.kinds

import spinal.core.SpinalEnum

object RvpMulNet extends SpinalEnum {
  val S_HH, S_WH, S_WW, P_HH, P_BB, P_HB, P_B4 = newElement()
}
