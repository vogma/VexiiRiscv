// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.kinds

import spinal.core.SpinalEnum

object RvpPairKind extends SpinalEnum {
  val PAIRE_B, PAIRE_H, PAIREO_B, PAIREO_H, PAIROE_B, PAIROE_H, PAIRO_B, PAIRO_H, ZIP8, ZIP16 = newElement()
}
