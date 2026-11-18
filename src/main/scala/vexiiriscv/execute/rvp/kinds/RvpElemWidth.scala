// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.kinds

import spinal.core.SpinalEnum

// Element width for the packed-ALU datapath: byte / halfword / word.
object RvpElemWidth extends SpinalEnum {
  val B, H, W = newElement()
}
