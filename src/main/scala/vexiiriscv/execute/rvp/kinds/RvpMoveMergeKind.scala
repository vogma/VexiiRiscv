// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp.kinds

import spinal.core.SpinalEnum

object RvpMoveMergeKind extends SpinalEnum {
  val MVM, MVMN, MERGE = newElement()
}
