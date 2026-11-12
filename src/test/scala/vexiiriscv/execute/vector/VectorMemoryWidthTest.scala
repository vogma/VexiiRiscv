package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import vexiiriscv.{ParamSimple, VexiiRiscv}
import vexiiriscv.execute.lsu.LsuL1Plugin

// --vector-memory-width=128: unsupported configurations are rejected with their reason, supported ones elaborate
// with a 128-bit L1 CPU word.
class VectorMemoryWidthTest extends AnyFunSuite {
  val isa64 = "--with-isa=i,m,a,f,d,c,zicsr,zifencei,zicntr,zihpm,zba,zbb,zbs,zca,zcb,zcd,zaamo,zalrsc,zicbop,zihintntl,zihintpause,zimop,zcmop"
  val rv64 = Seq("--xlen=64", "--dual-issue", "--max-ipc", "--with-mul", "--with-div", "--allow-bypass-from=0", "--relaxed-branch",
    "--fetch-l1", "--fetch-l1-ways=4", "--fetch-l1-mem-data-width-min=64", "--lsu-l1", "--lsu-l1-ways=4",
    "--lsu-l1-mem-data-width-min=64", "--with-lsu-bypass", "--with-btb", "--with-ras", "--with-gshare")
  val v256d128 = rv64 ++ Seq(isa64 + ",zve64x,u,s", "--vlen=256", "--vector-dlen=128")
  val vmw = Seq("--vector-memory-width=128")
  val stress = Seq("--lsu-l1-sets=16", "--lsu-l1-ways=1", "--lsu-l1-refill-count=1", "--lsu-l1-writeback-count=1",
    "--lsu-l1-store-buffer-slots=1", "--lsu-l1-store-buffer-ops=8")
  val overlap = Seq("--vector-decoupled", "--vector-beat-slides", "--vector-beat-compress", "--vector-overlap", "--vector-nofence",
    "--vector-queue=3", "--vector-trap-e0", "--vector-beat-mul", "--vector-chaining", "--vector-class-overlap", "--vector-memory-overlap")

  def param(args: Seq[String]): ParamSimple = {
    val p = new ParamSimple
    assert(new scopt.OptionParser[Unit]("vmw") { p.addOptions(this) }.parse(args, ()).nonEmpty, args.mkString(" "))
    p
  }

  def rejects(args: Seq[String], reason: String): Unit = {
    val e = intercept[Throwable](param(args).plugins())
    assert(String.valueOf(e.getMessage).contains(reason), s"expected '$reason', got '${e.getMessage}'")
  }

  test("unsupported configurations are rejected") {
    rejects(v256d128 :+ "--vector-memory-width=64", "--vector-memory-width must be 0 or 128")
    rejects(rv64 ++ Seq(isa64 + ",zve64x,u,s", "--vlen=256", "--vector-dlen=64") ++ vmw, "needs --vector-dlen=128")
    rejects(v256d128 ++ vmw :+ "--no-vector-beat-memory", "needs vector beat memory")
    rejects(v256d128 ++ vmw ++ Seq("--pmp-size=4", "--pmp-granularity=4"), "PMP granularity of at least 8 bytes")
    rejects(Seq("--xlen=32", "--lsu-l1", "--with-mul", "--with-div",
      "--with-isa=i,m,a,f,c,zicsr,zifencei,zicntr,zihpm,zaamo,zalrsc,zve32x,u,s", "--vlen=128", "--vector-dlen=64") ++ vmw,
      "needs RV64 without Q")
    rejects(rv64 ++ Seq(isa64 + ",u,s") ++ vmw, "--vector-memory-width needs a vector extension")
  }

  test("the option sets the L1 CPU word, and only then") {
    assert(param(v256d128).lsuCpuDataWidth == 64)
    assert(param(v256d128 ++ vmw).lsuCpuDataWidth == 128)
    assert(param(v256d128 ++ vmw).lsuMemDataWidth >= 128)
    assert(param(v256d128 :+ "--lsu-l1-mem-data-width-min=128").lsuCpuDataWidth == 64)
  }

  for ((name, args) <- Seq(
    "RV64 256/128" -> (v256d128 ++ vmw),
    "RV64 256/128 with PMP granularity 8" -> (v256d128 ++ vmw ++ Seq("--pmp-size=16", "--pmp-granularity=8")),
    "RV64 256/128 with a store buffer" -> (v256d128 ++ vmw ++ stress),
    "RV64 256/128 with memory overlap" -> (v256d128 ++ vmw ++ overlap),
    "RV64 Zve32x 128/128" -> (rv64 ++ Seq(isa64 + ",zve32x,u,s", "--vlen=128", "--vector-dlen=128") ++ vmw)
  )) test(s"elaborates: $name") {
    val p = param(args)
    SpinalConfig(targetDirectory = s"simWorkspace/vector-memory-width/${name.replaceAll("[^A-Za-z0-9]+", "-")}").generateVerilog {
      val plugins = p.plugins()
      val l1 = plugins.collectFirst { case l: LsuL1Plugin => l }.get
      assert(l1.cpuDataWidth == 128 && l1.memDataWidth >= 128, s"L1 ${l1.cpuDataWidth}/${l1.memDataWidth}")
      ParamSimple.setPma(plugins, ParamSimple.defaultPma)
      VexiiRiscv(plugins)
    }
  }
}
