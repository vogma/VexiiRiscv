// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.pipeline.Payload
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.Global.HART_ID
import vexiiriscv.decode.Decode.UOP_ID
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.execute.ExecuteLaneService
import vexiiriscv.execute.rvp.RvpPairWritePlugin.{PAIR_RD, PAIR_RD_SRC, PAIR_RS1, PAIR_RS2}
import vexiiriscv.regfile.RegfileService
import vexiiriscv.riscv.{IntRegFile, RD, RDS, RS1, RS2, RS3, Riscv}
import vexiiriscv.schedule.DispatchPlugin

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object RvpPairWritePlugin extends AreaObject {
  val PAIR_RS1 = Payload(Bool())
  val PAIR_RS2 = Payload(Bool())
  val PAIR_RD = Payload(Bool())
  val PAIR_RD_SRC = Payload(Bool())
}

class RvpPairWritePlugin(val lane: ExecuteLaneService, val writeAt: Int) extends FiberPlugin {

  val elaborationLock = Retainer()

  val ports = ArrayBuffer[(Int, Flow[Bits])]()

  def createPort(at: Int): Flow[Bits] = {
    assert(at <= writeAt, s"RvpPairWritePlugin.createPort beyond writeback range ($at > $writeAt)")
    val port = Flow(Bits(Riscv.XLEN bits))
    ports += (at -> port)
    port
  }

  val logic = during setup new Area {
    val rfp = host.find[RegfileService](_.rfSpec == IntRegFile)
    val decoder = host[DecoderService]
    val buildBefore = retains(lane.pipelineLock, rfp.elaborationLock)
    val decoderRetainer = retains(decoder.decodingLock)
    val dp = host[DispatchPlugin]
    val dispatchRetainer = retains(dp.elaborationLock)
    awaitBuild()

    assert(Global.HART_COUNT.get == 1)
    assert(Riscv.XLEN.get == 32)
    // Single-issue only. DispatchPlugin:422 compares a candidate's rdKeys.PHYS against
    // an older candidate's source PHYS, and a pair op carries only the even half.
    assert(Decode.LANES.get == 1, "Rvp extension currently only supports single-issue configurations")

    dp.addPairRd(lane, PAIR_RD)
    dispatchRetainer.release()

    decoder.addMicroOpDecodingDefault(PAIR_RS1, False)
    decoder.addMicroOpDecodingDefault(PAIR_RS2, False)
    decoder.addMicroOpDecodingDefault(PAIR_RD, False)
    decoder.addMicroOpDecodingDefault(PAIR_RD_SRC, False)

    lane.setDecodingDefault(PAIR_RD_SRC, False)
    lane.setDecodingDefault(PAIR_RD, False)
    lane.setDecodingDefault(PAIR_RS2, False)

    decoder.addDecodingLogic { ctx =>
      val inst = ctx.node(Decode.INSTRUCTION)

      val rdKeys = Decode.rfaKeys.get(RD)
      val rs1Keys = Decode.rfaKeys.get(RS1)
      val rs2Keys = Decode.rfaKeys.get(RS2)
      val rs3Keys = Decode.rfaKeys.get(RS3)
      val rdsKeys = Decode.rfaKeys.get(RDS)

      val rdP = inst(11 downto 8).asUInt
      val rs1P = inst(19 downto 16).asUInt

      when(ctx.node(PAIR_RD)) {
        ctx.node(rdKeys.PHYS) := rdP @@ False
        ctx.node(rdKeys.ENABLE) clearWhen (rdP === 0)
      }

      when(ctx.node(PAIR_RS1)) {
        ctx.node(rs1Keys.PHYS) := rs1P @@ False
        ctx.node(rdsKeys.PHYS) :=
          (rs1P === 0) ? U(0, 5 bits) | (rs1P @@ U"1")
      }

      val rs2P = inst(24 downto 21).asUInt
      when(ctx.node(PAIR_RS2)) {
        ctx.node(rs2Keys.PHYS) := rs2P @@ False
        ctx.node(rs3Keys.PHYS) := (rs2P === 0) ? U(0, 5 bits) | (rs2P @@ U"1")
      }

      when(ctx.node(PAIR_RD_SRC)) {
        ctx.node(rdsKeys.PHYS) := rdP @@ False
        ctx.node(rs3Keys.PHYS) := (rdP === 0) ? U(0, 5 bits) | (rdP @@ U"1")
      }
    }


    decoderRetainer.release()

    elaborationLock.await()

    val rfa = Decode.rfaKeys.get(RD)
    val HI = Payload(Bits(Riscv.XLEN bits))

    val sorted = ports.groupByLinked(_._1).values.toList.sortBy(_.head._1)
    val chain = for (group <- sorted) yield new lane.Execute(group.head._1) {
      val hits = B(group.map(_._2.valid))
      val muxed = OHMux.or(hits, group.map(_._2.payload))
      bypass(HI) := (if (group == sorted.head) muxed else up(HI) | muxed)
    }

    val seq = new lane.Execute(writeAt) {
      val pair = isValid && PAIR_RD

      // `second` must track this plugin's own freeze request, not lane.isFreezed(). The
      // freeze is global (ExecutePipelinePlugin drives ctrls.last.down.ready off the OR of
      // every request), so any other stall in the cycle before the op reaches writeAt would
      // present the op with `second` already set and the high half would never be written.
      val second = RegNext(lane.isFreezed()) init (False)
      val request = pair && !second
      lane.freezeWhen(request)

      val hi = rfp.newWrite(withReady = true, sharingKey = lane.laneName, priority = -1)
      val rd = rfa.PHYS(log2Up(IntRegFile.sizeArch) - 1 downto 0)

      hi.valid := request && !isCancel && up(rfa.ENABLE) && Global.COMMIT
      hi.address := HART_ID @@ (rd | U(1, rd.getWidth bits))
      hi.data := HI
      hi.hartId := HART_ID
      hi.uopId := UOP_ID

      assert(!(hi.valid && !hi.ready), "RVP pair write: high half lost arbitration")
      val hiDone = RegNext(hi.valid) init (False)
      assert(!(hiDone && isValid && isCancel), "RVP pair write: uop cancelled after high half")
    }

    buildBefore.release()


  }

}
