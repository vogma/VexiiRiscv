// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.lib.misc.plugin.Hostable
import vexiiriscv.execute.{ExecuteLaneService, LaneLayer}

import scala.collection.mutable.ArrayBuffer

object RvpPluginSuite {
  /**
   * @param pairs RV32 only: keep the architectural register-pair ops (pair source, pair
   *              destination, destination-pair read). `false` is the no-pairs
   *              configuration: every op whose
   *              manifest row has pair_source, pair_dest or reads_rd_pair is not decoded,
   *              RvpNarrowPlugin (all of whose RV32 ops read a pair) and RvpPairWritePlugin
   *              (odd-half write port, pair PHYS remap, pair hazard term) are not installed,
   *              and the surviving plugins elaborate without their HIGH-pass datapaths.
   *              Ignored at XLEN=64, which has no pair ops.
   */
  def install(plugins: ArrayBuffer[Hostable], layer: LaneLayer,
              lane: ExecuteLaneService, intWritebackAt: Int, xlen: Int,
              pairs: Boolean = true): Unit = {
    require(xlen == 32 || xlen == 64, s"RVP requires XLEN 32 or 64, got $xlen")

    if (xlen == 32) {
      plugins += new RvpPackedAluPlugin(layer = layer, intWritebackAt = intWritebackAt, xlen = xlen, pairs = pairs)
      plugins += new RvpPackedShiftPlugin(layer = layer, xlen = xlen, pairs = pairs)
      plugins += new RvpSaturatingShiftPlugin(layer = layer, intWritebackAt = intWritebackAt, xlen = xlen, pairs = pairs)
      plugins += new RvpMiscPlugin(layer = layer, xlen = xlen, pairs = pairs)
      if (pairs) plugins += new RvpNarrowPlugin(layer = layer, xlen = xlen)
      plugins += new RvpMulPlugin(layer = layer, xlen = xlen, pairs = pairs)
      if (pairs) plugins += new RvpPairWritePlugin(lane, writeAt = intWritebackAt)
      plugins += new RvpVxsatPlugin(lane)
    } else {
      plugins += new RvpPackedAluPlugin(layer = layer, intWritebackAt = intWritebackAt, xlen = xlen)
      plugins += new RvpPackedShiftPlugin(layer = layer, xlen = xlen)
      plugins += new RvpSaturatingShiftPlugin(layer = layer, intWritebackAt = intWritebackAt, xlen = xlen)
      plugins += new RvpMiscPlugin(layer = layer, xlen = xlen)
      plugins += new RvpNarrowPlugin(layer = layer, xlen = xlen)
      plugins += new RvpMulPlugin(layer = layer, xlen = xlen)
      plugins += new RvpVxsatPlugin(lane)
    }
    // Only on request: an extra fiber changes signal declaration order in the generated
    // Verilog (not its logic), which would change the hash of otherwise identical RTL.
    if (sys.env.contains("RVP_INVENTORY_DUMP"))
      plugins += new RvpInventoryPlugin(lane, pairs = pairs || xlen == 64)
  }
}
