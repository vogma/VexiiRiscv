// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.core._
import vexiiriscv.execute.ExecuteLaneService

/**
 * Two-pass sequencer for a datapath shared between the two words of a double-wide
 * (pair-source + pair-destination) op.
 *
 * A double-wide op needs the same lane geometry applied twice, once to the low word and
 * once to the high word. This holds the op in its own Execute stage for one extra cycle and runs the single
 * datapath twice:
 *
 * pass 1 (`first`) : datapath <- high word, result -> a hold register
 * pass 2           : datapath <- low  word, result -> the stage payload
 *
 * The pass-1 result needs an explicit `Reg` written on that cycle, which the caller then drives into
 * its high-half payload on the firing cycle.
 *
 * @param twoPass true while an op needing the second pass sits in the caller's stage
 * @param moving  the stage's `up.isMoving` -- the op leaves this cycle (fired or cancelled)
 * @param enabled false on a no-pairs build: `first`/`second` are constant False, no state, no freeze
 */
case class RvpDoubleWidePass(lane: ExecuteLaneService, twoPass: Bool, moving: Bool, enabled: Boolean = true) extends Area {
  val second: Bool = if (enabled) RegInit(False) else False
  val first: Bool = if (enabled) twoPass && !second else False
  if (enabled) {
    lane.freezeWhen(first)
    when(moving) {
      second := False
    } elsewhen (first) {
      second := True
    }
  }
}
