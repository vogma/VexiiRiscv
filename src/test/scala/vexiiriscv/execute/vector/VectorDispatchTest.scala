package vexiiriscv.execute.vector

import java.io.File
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.pipeline.Payload
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.{Global, ParamSimple}
import vexiiriscv.decode.Decode
import vexiiriscv.execute.ExecuteLanePlugin
import vexiiriscv.execute.lsu.{AguPlugin, LsuPlugin}
import vexiiriscv.misc.{InflightService, TrapReason}
import vexiiriscv.riscv.{Const, IntRegFile, RS1, Rvv}
import vexiiriscv.riscv.Riscv._
import vexiiriscv.schedule.DispatchPlugin
import vexiiriscv.tester.{TestBench, TestBenchDut, TestOptions}

case class SequenceDescriptor() extends Bundle {
  val index = UInt(5 bits)
  val last, access = Bool()
}

// Test-only sequencer: rd micro-ops per instruction (rd = 0: one finish micro-op), element i at rs1 + i * size,
// store bytes 0x40 + i. Loads expect memory bytes equal to their address bits 7..0.
class VectorSequenceDriver extends FiberPlugin {
  val logic = during setup new Area {
    val lsu = host[LsuPlugin]
    val dp = host[DispatchPlugin]
    val lanes = host.list[ExecuteLanePlugin]
    val lock = retains(lsu.vectorLsuLock, dp.elaborationLock)
    val pipelineLock = retains(lanes.map(_.pipelineLock))
    awaitBuild()
    val port = lsu.newVectorLsuPort()
    dp.sequence(for (w <- Seq(8, 16, 32); op <- Seq("vle", "vse")) yield Rvv.all(s"$op$w.v"))
    lock.release()
    val seq = dp.logic.sequencer.port

    val layer = lsu.layer
    val lane = layer.lane
    val DESCRIPTOR = Payload(SequenceDescriptor())
    val ADDRESS = Payload(UInt(XLEN bits))

    val dispatch = new Area {
      val count = seq.uop(Const.rdRange).asUInt
      val next = Reg(UInt(5 bits)) init(0)
      val index = seq.first ? U(0, 5 bits) | next
      seq.last := count <= (index +^ 1)
      seq.ready := !seq.first || port.storeBufferEmpty
      when(seq.fire) { next := index + 1 }
      val descriptor = SequenceDescriptor()
      descriptor.index := index
      descriptor.last := seq.last
      descriptor.access := count =/= 0
      lane.ctrl(0).up(DESCRIPTOR) := descriptor
    }

    def sizeOf(uop: Bits): UInt = uop(Const.funct3Range).mux(
      B"000" -> U(0, LSU_SIZE_WIDTH bits), B"101" -> U(1, LSU_SIZE_WIDTH bits), default -> U(2, LSU_SIZE_WIDTH bits))

    val e0 = new layer.Execute(0) {
      val d = apply(DESCRIPTOR)
      val size = sizeOf(Decode.UOP)
      port.cmd.valid := isValid && AguPlugin.VECTOR
      port.cmd.address := (up(lane(IntRegFile, RS1)).asUInt + (d.index << size).resize(XLEN)).resized
      port.cmd.size := size
      port.cmd.storeData := (U(0x40, 8 bits) + d.index.resize(8)).asBits #* (LSLEN / 8)
      port.cmd.access := d.access
      port.cmd.last := d.last
      ADDRESS := port.cmd.address
    }

    val checks = new layer.Execute(lsu.timingParameter.ctrlAt) {
      val d = apply(DESCRIPTOR)
      val rsp = port.rsp
      val expected = Reg(UInt(5 bits)) init(0)
      when(rsp.valid) {
        assert(rsp.last === d.last && rsp.access === d.access, "descriptor does not match its micro-op")
        when(rsp.trap.valid) {
          expected := 0
        } otherwise {
          assert(d.index === expected, "micro-ops out of order")
          expected := d.last ? U(0, 5 bits) | d.index + 1
        }
        when(rsp.access && rsp.load && !rsp.trap.valid) {
          val size = sizeOf(Decode.UOP)
          for (byte <- 0 until 4) when(size >= U(log2Up(byte + 1))) {
            assert(rsp.data(byte * 8, 8 bits) === (apply(ADDRESS) + byte).resize(8).asBits, "unexpected load data")
          }
        }
      }
    }

    // While a sequence is open, every in-flight uop belongs to it; a younger one may only pair with the last micro-op.
    val order = new Area {
      val inflight = host.list[InflightService].map(_.hasInflight(0)).orR
      val open = RegInit(False)
      val uopId = Reg(Decode.UOP_ID())
      val insertId = lane.ctrl(0)(Decode.UOP_ID)
      when(seq.fire) {
        assert(!seq.first || !inflight, "first micro-op with older uops in flight")
        assert(seq.first || insertId === uopId, "micro-op of another instruction")
        assert(!seq.first || port.storeBufferEmpty, "first micro-op with a busy store buffer")
        assert(!lane.ctrl(0)(Global.TRAP), "decode trap issued as a micro-op")
        uopId := insertId
        open := !seq.last
      }
      when(!seq.valid) { open := False }
      for (l <- lanes; at <- 1 to 4) when(open && l.ctrl(at).isValid) {
        assert(l.ctrl(at)(Decode.UOP_ID) === uopId, "younger uop issued inside a sequence")
      }
      for (l <- lanes if l != lane) when(open && !(seq.fire && seq.last) && l.ctrl(0).isValid) {
        assert(False, "younger uop issued beside a micro-op")
      }
    }

    val fires, firsts, overlaps, pairs, storeBufferWaits, halts = Reg(UInt(32 bits)) init(0)
    val loads, stores, finishes, lasts, redo, refill, exceptions, midTraps = Reg(UInt(32 bits)) init(0)
    when(seq.fire) {
      fires := fires + 1
      when(seq.first) { firsts := firsts + 1 }
      when(!seq.first && order.inflight) { overlaps := overlaps + 1 }
      when(seq.last && lanes.filter(_ != lane).map(_.ctrl(0).isValid).orR) { pairs := pairs + 1 }
    }
    when(seq.valid && seq.first && !port.storeBufferEmpty) { storeBufferWaits := storeBufferWaits + 1 }
    when(order.open && dp.api.haltDispatch) { halts := halts + 1 }
    val response = port.rsp
    when(response.valid) {
      when(response.trap.valid) {
        when(checks.d.index =/= 0) { midTraps := midTraps + 1 }
        when(response.trap.exception) { exceptions := exceptions + 1 }
        .elsewhen(response.trap.code === TrapReason.REDO) { redo := redo + 1 }
        .elsewhen(response.trap.code === TrapReason.MMU_REFILL) { refill := refill + 1 }
      } otherwise {
        when(!response.access) { finishes := finishes + 1 }
        .elsewhen(response.load) { loads := loads + 1 }
        .otherwise { stores := stores + 1 }
        when(response.last) { lasts := lasts + 1 }
      }
    }
    val counters = Seq("fires" -> fires, "firsts" -> firsts, "overlaps" -> overlaps, "pairs" -> pairs,
      "storeBufferWaits" -> storeBufferWaits, "halts" -> halts, "loads" -> loads, "stores" -> stores,
      "finishes" -> finishes, "lasts" -> lasts, "redo" -> redo, "refill" -> refill, "exceptions" -> exceptions,
      "midTraps" -> midTraps)
    counters.foreach(_._2.simPublic())
    pipelineLock.release()
  }
}

// VEXII_VECTOR_DISPATCH_DIR holds dispatch-rv{32,64}.elf and the core arguments rv{32,64}.args; without it the tests are canceled.
class VectorDispatchTest extends AnyFunSuite {
  def fixture(name: String): File = {
    val dir = sys.env.get("VEXII_VECTOR_DISPATCH_DIR")
    assume(dir.nonEmpty, "VEXII_VECTOR_DISPATCH_DIR is not set")
    new File(dir.get, name)
  }

  def fixtureParam(xlen: Int): ParamSimple = {
    val param = new ParamSimple
    val config = scala.io.Source.fromFile(fixture(s"rv$xlen.args"))
    val args = try config.mkString.split("\\s+").filter(_.nonEmpty) finally config.close()
    val parser = new scopt.OptionParser[Unit]("VectorDispatchTest") { param.addOptions(this) }
    assert(parser.parse(args, ()).nonEmpty)
    param
  }

  for (xlen <- Seq(32, 64)) test(s"sequenced vector dispatch RV$xlen") {
    val param = fixtureParam(xlen)
    val compiled = SimConfig.compile {
      val plugins = TestBench.paramToPlugins(param)
      plugins += new VectorSequenceDriver
      new TestBenchDut(Seq(plugins))
    }
    val options = new TestOptions
    options.withRvls = false
    options.withRvlsCheck = false
    options.withStdIn = false
    options.addElf(fixture(s"dispatch-rv$xlen.elf"))
    options.setFailAfter(5000000)
    var counts = Map.empty[String, Long]
    compiled.doSimUntilVoid { dut =>
      val driver = dut.cores.head.host[VectorSequenceDriver].logic
      onSimEnd {
        counts = driver.counters.map { case (name, reg) => name -> reg.toLong }.toMap
        println(driver.counters.map(_._1).map(name => s"$name=${counts(name)}").mkString(" "))
      }
      options.test(dut, f => f)
    }
    // Hand-counted from dispatch.S: completed vector instructions, finish micro-ops, misaligned elements.
    // Page faults reach the LSU as MMU_REFILL; the walker raises them.
    assert(counts("lasts") == 18)
    assert(counts("finishes") == 2)
    assert(counts("exceptions") == 2)
    assert(counts("refill") >= 2 && counts("redo") > 0 && counts("midTraps") > 0)
    assert(counts("overlaps") > 0 && counts("pairs") > 0 && counts("storeBufferWaits") > 0 && counts("halts") > 0)
  }
}
