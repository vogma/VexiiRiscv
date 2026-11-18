// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.kinds

import spinal.core.SpinalEnum

object RvpImmediateKind extends SpinalEnum {
  val LI_B, LI_H, LUI_H = newElement()
}
