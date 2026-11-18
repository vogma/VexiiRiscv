// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.kinds

import spinal.core.SpinalEnum

// Compact post-stage selector derived from RvpAluKind in Execute(0). Multiple
// instruction kinds that share one physical result network also share one
// readout value, keeping the wider decode enum out of Execute(1)'s mux trees.
object RvpReadout extends SpinalEnum {
  val ADDSUB, CMP, MINMAX, AVG, ABSD, ABSDSUMU, REDSUM, SATAS, SATI, SABS, WIDEN = newElement()
}
