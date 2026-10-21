package vexiiriscv.execute.vector

import java.io.File
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import vexiiriscv.ParamSimple
import vexiiriscv.tester.{TestBench, TestOptions}

// Check interrupt inhibition on every cycle of a load retry and fault-only-first trim.
class VectorSystemTest extends AnyFunSuite {
  for (xlen <- Seq(32, 64)) test(s"interrupt hold across translated loads RV$xlen") {
    val dir = sys.env.get("VEXII_VECTOR_SYSTEM_DIR")
    assume(dir.nonEmpty, "VEXII_VECTOR_SYSTEM_DIR is not set")
    def file(name: String) = new File(dir.get, name)
    val config = scala.io.Source.fromFile(file(s"rv$xlen.args"))
    val args = try config.mkString.split("\\s+").filter(_.nonEmpty) finally config.close()
    val param = new ParamSimple
    assert(new scopt.OptionParser[Unit]("system") { param.addOptions(this) }.parse(args, ()).nonEmpty)
    val compiled = SimConfig.compile(TestBench.makeDut(param, 1))
    val options = new TestOptions
    assert(options.withRvls)
    options.withStdIn = false
    options.vectorSystemTrace = true
    options.vectorIrqInject = true
    options.addElf(file(s"irq-mem-rv$xlen.elf"))
    options.setFailAfter(50000000)
    options.testName = Some(s"system-rv$xlen")
    var held, armed, trimmed = 0
    compiled.doSimUntilVoid(seed = 2) { dut =>
      options.test(dut, f => f)
      val v = dut.vectorSystem.get.head.get
      dut.cores.head.clockDomain.onSamplings {
        if(v.resume.toBoolean) {
          assert(!v.allowInterrupts.toBoolean, "interrupts allowed while the load is being refetched")
          if(v.enabled.toBoolean && v.timerEnabled.toBoolean) {
            held += 1
            if(v.armed.toBoolean) armed += 1
            if(v.trim.toBoolean) trimmed += 1
          }
        }
      }
    }
    assert(held > 0 && armed > 0 && trimmed > 0, s"missing pending interrupt coverage: $held/$armed/$trimmed")
  }
}
