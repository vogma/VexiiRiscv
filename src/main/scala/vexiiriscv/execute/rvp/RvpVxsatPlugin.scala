// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.traversableOnceBoolPimped
import vexiiriscv.Global
import vexiiriscv.execute.{CsrService, ExecuteLaneService, LaneLayer}
import vexiiriscv.riscv.CSR

import scala.collection.mutable.ArrayBuffer

object RvpVxsatPlugin extends AreaObject

class RvpVxsatPlugin(val lane: ExecuteLaneService) extends FiberPlugin {
  val elaborationLock = Retainer()

  case class Port(at: Int) {
    val pulse = Bool()
  }

  val ports = ArrayBuffer[Port]()

  def createPort(at: Int): Port = ports.addRet(Port(at))

  val api = during build new Area {
    val vxsat = Reg(Bool()) init False
  }

  val logic = during setup new Area {
    val csrService = host[CsrService]
    val buildBefore = retains(lane.pipelineLock, csrService.csrLock)
    awaitBuild()
    elaborationLock.await()
    assert(Global.HART_COUNT.get == 1)

    csrService.readWrite(CSR.VXSAT, 0 -> api.vxsat)

    val committed = ports.map { p =>
      val c = lane.execute(p.at)
      p.pulse & c.isValid & c.isReady & !c.isCancel & c.up(Global.COMMIT)
    }


    buildBefore.release()
    csrService.waitElaborationDone()
    when(!lane.isFreezed()) {
      api.vxsat setWhen committed.orR
    }
  }
}
