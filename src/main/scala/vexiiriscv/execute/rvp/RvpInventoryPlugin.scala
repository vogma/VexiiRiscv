// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.ExecuteLaneService
import vexiiriscv.riscv.{Riscv, SingleDecoding}

import java.io.PrintWriter

/**
 * Elaboration-time decode inventory: dumps what the decoder actually accepts, as a
 * cross-check of the configured instruction selection. Generates no hardware.
 *
 * When the environment variable RVP_INVENTORY_DUMP names a file, every micro-op the
 * execute lane decodes is written there as `mask<TAB>match<TAB>name`, one per line, after
 * all plugins have registered their ops (`lane.getUops()` waits for the lane's uop lock).
 * Diffing that list against the expected instruction selection verifies a no-pairs
 * build by what the decoder actually accepts and not by the Scala guards.
 */
class RvpInventoryPlugin(val lane: ExecuteLaneService, val pairs: Boolean) extends FiberPlugin {
  val logic = during build new Area {
    sys.env.get("RVP_INVENTORY_DUMP").foreach { path =>
      val pw = new PrintWriter(path)
      pw.println(s"# xlen=${Riscv.XLEN.get} pairs=$pairs lane=${lane.laneName}")
      pw.println("mask\tmatch\tname")
      for (uop <- lane.getUops()) uop match {
        case sd: SingleDecoding =>
          for (key <- sd.keys)
            pw.println(f"0x${key.careAbout.toLong}%08x\t0x${key.value.toLong}%08x\t${sd.getName("?")}")
        case other =>
          pw.println(s"?\t?\t$other")
      }
      pw.close()
    }
  }
}
