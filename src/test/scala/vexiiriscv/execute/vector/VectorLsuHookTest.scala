package vexiiriscv.execute.vector

import java.io.File
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.{Global, ParamSimple}
import vexiiriscv.decode.Decode
import vexiiriscv.execute.lsu.{AguPlugin, LsuPlugin}
import vexiiriscv.misc.TrapReason
import vexiiriscv.riscv.{Const, IntRegFile, RS1}
import vexiiriscv.riscv.Riscv._
import vexiiriscv.tester.{TestBench, TestBenchDut, TestOptions}

// A single-element fixture: deliberately use a different address and data from the scalar AGU/register file.
class VectorLsuHookDriver extends FiberPlugin {
  val logic = during setup new Area {
    val lsu = host[LsuPlugin]
    val lock = retains(lsu.vectorLsuLock)
    val pipelineLock = retains(lsu.layer.lane.pipelineLock)
    awaitBuild()
    val port = lsu.newVectorLsuPort()
    lock.release()
    val layer = lsu.layer
    val e0 = new layer.Execute(0) {
      port.cmd.valid := isValid && AguPlugin.VECTOR
      port.cmd.address := (up(lsu.layer.lane(IntRegFile, RS1)).asUInt + 16).resized
      if (XLEN.get == 64) {
        when(Decode.UOP(Const.rdRange).asUInt === 29) { port.cmd.address(40) := True }
        when(Decode.UOP(Const.rdRange).asUInt === 28) { port.cmd.address(40) := False }
      }
      port.cmd.size := Decode.UOP(Const.funct3Range).mux(
        B"000" -> U(0, LSU_SIZE_WIDTH bits), B"101" -> U(1, LSU_SIZE_WIDTH bits), default -> U(2, LSU_SIZE_WIDTH bits))
      port.cmd.storeData := B(BigInt("89abcdef", 16), LSLEN bits)
      port.cmd.access := Decode.UOP(Const.rdRange).asUInt =/= 31
      port.cmd.last := Decode.UOP(Const.rdRange).asUInt =/= 30
    }
    // The LSU builds the stage areas after the port has been registered.
    lsu.logic.get
    val ctrl = lsu.logic.onCtrl
    val response = port.rsp
    when(response.valid) {
      assert(!ctrl.down(Global.COMMIT) || response.last && !response.trap.valid)
      assert(!lsu.logic.iwb.valid)
      when(response.access && response.load && !response.trap.valid) {
        val width = ctrl(Decode.UOP)(Const.funct3Range)
        when(width === B"000") { assert(response.data(7 downto 0) === B"8'xef") }
        when(width === B"101") { assert(response.data(15 downto 0) === B"16'xcdef") }
        when(width === B"110") { assert(response.data(31 downto 0) === B"32'x89abcdef") }
      }
      when(!response.access) {
        assert(!response.trap.valid && ctrl.down(Global.COMMIT))
        assert(!ctrl(lsu.logic.l1.SEL) || !ctrl(lsu.logic.FROM_LSU))
      }
    }
    val loads, stores, finishes, nonlast, redo, refill, exceptions = Reg(UInt(16 bits)) init(0)
    when(response.valid) {
      when(response.trap.valid) {
        when(response.trap.exception) { exceptions := exceptions + 1 }
        .elsewhen(response.trap.code === TrapReason.REDO) { redo := redo + 1 }
        .elsewhen(response.trap.code === TrapReason.MMU_REFILL) { refill := refill + 1 }
      } otherwise {
        when(!response.access) { finishes := finishes + 1 }
        .elsewhen(response.load) { loads := loads + 1 }
        .otherwise { stores := stores + 1 }
        when(!response.last) { nonlast := nonlast + 1 }
      }
    }
    Seq(loads, stores, finishes, nonlast, redo, refill, exceptions).foreach(_.simPublic())
    pipelineLock.release()
  }
}

// VEXII_LSU_HOOK_DIR holds hooks-rv{32,64}.elf, unattached.elf and the core arguments rv{32,64}.args; without it the tests are canceled.
class VectorLsuHookTest extends AnyFunSuite {
  def fixture(name: String): File = {
    val dir = sys.env.get("VEXII_LSU_HOOK_DIR")
    assume(dir.nonEmpty, "VEXII_LSU_HOOK_DIR is not set")
    new File(dir.get, name)
  }

  def fixtureParam(xlen: Int): ParamSimple = {
    val param = new ParamSimple
    val config = scala.io.Source.fromFile(fixture(s"rv$xlen.args"))
    val args = try config.mkString.split("\\s+").filter(_.nonEmpty) finally config.close()
    val parser = new scopt.OptionParser[Unit]("VectorLsuHookTest") { param.addOptions(this) }
    assert(parser.parse(args, ()).nonEmpty)
    param
  }

  for (xlen <- Seq(32, 64)) test(s"shared LSU vector element hooks RV$xlen") {
    val param = fixtureParam(xlen)
    val compiled = SimConfig.compile {
      val plugins = TestBench.paramToPlugins(param)
      plugins += new VectorLsuHookDriver
      new TestBenchDut(Seq(plugins))
    }
    val options = new TestOptions
    options.withRvls = false
    options.withRvlsCheck = false
    options.withStdIn = false
    options.traceRvlsLog = true
    options.addElf(fixture(s"hooks-rv$xlen.elf"))
    options.setFailAfter(5000000)
    var counts = Seq.empty[Int]
    compiled.doSimUntilVoid { dut =>
      val driver = dut.cores.head.host[VectorLsuHookDriver].logic
      onSimEnd {
        println(s"loads=${driver.loads.toInt} stores=${driver.stores.toInt} finishes=${driver.finishes.toInt} nonlast=${driver.nonlast.toInt} redo=${driver.redo.toInt} refill=${driver.refill.toInt} exceptions=${driver.exceptions.toInt}")
        counts = Seq(driver.loads, driver.stores, driver.finishes, driver.nonlast, driver.redo, driver.refill, driver.exceptions).map(_.toInt)
      }
      options.test(dut, f => f)
    }
    assert(counts.take(4) == Seq(if (xlen == 32) 8 else 9, 8, 2, 1))
    assert(counts(4) > 0 && counts(5) >= 3)
    assert(counts(6) == (if (xlen == 32) 4 else 5))
  }

  test("unattached vector LSU rejects memory instructions") {
    val param = fixtureParam(32)
    val compiled = SimConfig.compile(TestBench.makeDut(param, 1))
    val options = new TestOptions
    options.withRvls = false
    options.withRvlsCheck = false
    options.withStdIn = false
    options.addElf(fixture("unattached.elf"))
    options.setFailAfter(5000000)
    options.test(compiled)
  }
}
