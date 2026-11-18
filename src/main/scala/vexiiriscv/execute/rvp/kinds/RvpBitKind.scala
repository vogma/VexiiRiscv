// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.kinds

import spinal.core.SpinalEnum

object RvpBitKind extends SpinalEnum {
  val ABS, CLS, REV, PACK, SEXT, SEXT_W_H, SEXT_W_B = newElement()
}
