package vexiiriscv.execute.vector

import java.io.{File, PrintWriter}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.fiber.Fiber
import spinal.core.sim._
import vexiiriscv.ParamSimple
import vexiiriscv.misc.PrivilegedPlugin
import vexiiriscv.tester.{TestBench, TestOptions}

// The fixture enables external interrupts and waits for a handler after each element-unit instruction.
class VectorInterruptLatencyTest extends AnyFunSuite {
  for (xlen <- Seq(32, 64)) test(s"interrupt during element arithmetic RV$xlen") {
    val dir = sys.env.get("VEXII_VECTOR_LATENCY_DIR")
    assume(dir.nonEmpty, "VEXII_VECTOR_LATENCY_DIR is not set")
    def file(name: String) = new File(dir.get, name)
    val config = scala.io.Source.fromFile(file(s"rv$xlen.args"))
    val args = try config.mkString.split("\\s+").filter(_.nonEmpty) finally config.close()
    val param = new ParamSimple
    assert(new scopt.OptionParser[Unit]("latency") { param.addOptions(this) }.parse(args, ()).nonEmpty)
    val compiled = SimConfig.compile {
      val dut = TestBench.makeDut(param, 1)
      dut.rework {
        Fiber patch {
          val unit = dut.cores.head.host[VectorPlugin].logic.element
          unit.io.cmd.valid.simPublic()
          unit.io.cmd.op.simPublic()
          unit.io.cmd.sew.simPublic()
          unit.io.cmd.vl.simPublic()
          unit.io.done.simPublic()
        }
      }
      dut
    }
    val options = new TestOptions
    assert(options.withRvls, "the vector RVLS library must be available in the fixture overlay")
    options.withStdIn = false
    options.addElf(file(s"latency-irq-rv$xlen.elf"))
    options.setFailAfter(50000000)
    options.testName = Some(s"latency-irq-rv$xlen")
    var rows = 0
    val out = new PrintWriter(file(s"interrupt-rv$xlen.csv"))
    out.println("index,op,sew,vl,command_to_done,request_to_done,request_to_ack")
    try compiled.doSimUntilVoid(seed = 2) { dut =>
      options.test(dut, f => f)
      val unit = dut.cores.head.host[VectorPlugin].logic.element
      val irq = dut.cores.head.host[PrivilegedPlugin].hart(0).int.m.external
      val cd = dut.cores.head.clockDomain
      var cycle, start, request, done = -1L
      var op, sew, vl = 0
      var pending = false
      cd.onSamplings {
        cycle += 1
        if (pending && !irq.toBoolean) {
          assert(done >= request, "handler acknowledged before arithmetic completed")
          out.println(s"$rows,$op,$sew,$vl,${done-start},${done-request},${cycle-request}")
          out.flush()
          rows += 1
          pending = false
        }
        if (start >= 0 && cycle == start + 1) {
          assert(!pending)
          irq #= true
          pending = true
          request = cycle
        }
        if (unit.io.cmd.valid.toBoolean) {
          assert(!pending, "previous interrupt has not been acknowledged")
          start = cycle
          done = -1
          op = unit.io.cmd.op.toInt
          sew = 8 << unit.io.cmd.sew.toInt
          vl = unit.io.cmd.vl.toInt
        }
        if (unit.io.done.toBoolean) done = cycle
      }
    } finally out.close()
    val sites = sys.env.get("VEXII_VECTOR_LATENCY_SITES").map(_.toInt).getOrElse(104)
    assert(rows == sites, s"expected every arithmetic site, got $rows")
  }
}
