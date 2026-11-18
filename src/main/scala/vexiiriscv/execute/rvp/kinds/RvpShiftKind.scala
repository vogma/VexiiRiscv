// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.kinds

import spinal.core.SpinalEnum

// Operation family for the packed-shift datapath, set at decode and selecting the
// per-lane shift in RvpPackedShiftPlugin.
//   SLL   logical left       -> zero fill, over-shift saturates to 0
//   SRL   logical right      -> zero fill, over-shift saturates to 0
//   SRA   arithmetic right   -> sign fill, over-shift saturates to all-sign
//   SRAR  round-half-up arithmetic right (psrari.h / srari)
object RvpShiftKind extends SpinalEnum {
  val SLL, SRL, SRA, SRAR = newElement()
}
