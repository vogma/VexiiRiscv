package vexiiriscv.tester

import java.io.{File, PrintWriter}
import java.util.concurrent.Executors
import org.apache.commons.io.FileUtils
import org.yaml.snakeyaml.Yaml
import spinal.core.sim._
import spinal.lib.misc.test.AsyncJob
import vexiiriscv.ParamSimple
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.io.Source

/** Compile once and run a fresh list of self-checking ELFs with RVLS. */
object TestList {
  case class Entry(name: String, elf: File, options: Seq[String])
  // List-only option: log LSU events of the entry to lsu-events.log (LsuEventMonitor)
  val LSU_EVENTS = "--with-lsu-events"

  def read(file: File): Seq[Entry] = {
    val source = Source.fromFile(file)
    try source.getLines().filter(l => l.trim.nonEmpty && !l.trim.startsWith("#")).map { line =>
      val fields = if (line.trim.startsWith("{")) {
        val row = new Yaml().load(line).asInstanceOf[java.util.Map[String, Object]]
        val options = Option(row.get("options")).map(_.asInstanceOf[java.util.List[String]].asScala.toSeq).getOrElse(Seq.empty)
        Seq(row.get("name").toString, row.get("elf").toString) ++ options
      } else line.trim.split("\\s+").toSeq
      require(fields.size >= 2, s"Invalid list entry: $line")
      require(fields.head.split("/", -1).forall(p => p.matches("[A-Za-z0-9_.-]+") && p != "." && p != ".."),
        s"Invalid test name: ${fields.head}")
      Entry(fields.head, new File(fields(1)).getAbsoluteFile, fields.drop(2))
    }.toVector finally source.close()
  }

  def write(file: File, value: String): Unit = {
    val writer = new PrintWriter(file)
    try writer.print(value) finally writer.close()
  }

  def quote(value: String): String = "\"" + value.flatMap {
    case '"' => "\\\""
    case '\\' => "\\\\"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case c if c < ' ' => f"\\u${c.toInt}%04x"
    case c => c.toString
  } + "\""

  def run(args: Array[String]): Int = {
    val param = new ParamSimple
    var list: File = null
    var output: File = null
    var jobs = 1
    var defaults = Seq.empty[String]
    val parser = new scopt.OptionParser[Unit]("TestList") {
      help("help")
      opt[File]("list").required().action((v, _) => list = v)
      opt[File]("out").required().action((v, _) => output = v)
      opt[Int]("jobs").validate(v => if (v > 0) success else failure("jobs must be positive")).action((v, _) => jobs = v)
      opt[String]("default-opts").action((v, _) => defaults = v.trim.split("\\s+").filter(_.nonEmpty).toSeq)
      param.addOptions(this)
    }
    if (parser.parse(args, ()).isEmpty) return 2
    output = output.getAbsoluteFile
    require(!output.exists(), s"refusing to reuse output directory $output")
    val entries = read(list)
    require(entries.nonEmpty, "empty test list")
    require(entries.map(_.name).distinct.size == entries.size, "duplicate test names")
    val rvlsAvailable = new File("ext/rvls/build/apps/rvls.so").isFile
    require(rvlsAvailable || entries.forall(_.options.contains("--no-rvls-check")),
      "RVLS is not available: ext/rvls/build/apps/rvls.so")

    val tests = entries.map { entry0 =>
      val entry: Entry = entry0.copy(options = entry0.options.filter(_ != LSU_EVENTS))
      val options = defaults ++ entry.options
      require(!options.contains("--no-probe"), "--no-probe disables retirement checking and is forbidden")
      require(!options.contains("--pass-after"), "--pass-after disables self-checking and is forbidden")
      val test = new TestOptions
      def parseOptions(values: Seq[String]): Unit = {
        val testParser = new scopt.OptionParser[Unit]("TestList entry") { test.addOptions(this) }
        require(testParser.parse(values, ()).nonEmpty, s"invalid options for ${entry.name}")
      }
      parseOptions(defaults)
      parseOptions(entry.options)
      test.addElf(entry.elf)
      test.testName = Some(entry.name)
      test.withStdIn = false
      if (test.failAfter.isEmpty) test.setFailAfter(100000000L)
      require(!entry0.options.contains(LSU_EVENTS) || !test.dualSim, s"$LSU_EVENTS needs a single simulation")
      entry0 -> test
    }

    FileUtils.forceMkdir(output)
    val simConfig = SpinalSimConfig()
    simConfig.workspacePath(new File(output, "simWorkspace").getAbsolutePath)
    simConfig.setTestPath(new File(output, "tests/$TEST").getAbsolutePath)
    if (tests.exists(_._2.traceWave)) simConfig.withFstWave
    val withEvents = entries.exists(_.options.contains(LSU_EVENTS))
    val compiled = simConfig.compile {
      val dut = TestBench.makeDut(param, 1)
      if (withEvents) LsuEventMonitor.publish(dut)
      dut
    }
    implicit val executor: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(jobs))
    val hostOutput = Console.out
    val tasks = tests.map { case (entry, test) =>
      val directory = new File(output, "tests/" + entry.name)
      new AsyncJob(toStdout = false, logsPath = directory)({
        write(new File(directory, "args"), (defaults ++ entry.options).mkString(" ") + "\n")
        val events = entry.options.contains(LSU_EVENTS)
        var status = "PASS"
        var reason = ""
        try {
          if (!entry.elf.isFile) throw new java.io.FileNotFoundException(s"missing ELF: ${entry.elf}")
          if (events) compiled.doSimUntilVoid(name = test.getTestName(), seed = test.seed) { dut =>
            disableSimWave()
            test.test(dut, f => f)
            LsuEventMonitor.attach(dut, new File(directory, "lsu-events.log"))
          } else test.test(compiled)
        } catch {
          case error: Throwable =>
            status = "FAIL"
            val message = Option(error.getMessage).getOrElse(error.toString)
            reason = if (message.toLowerCase.contains("timeout")) "timeout" else message
            error.printStackTrace(Console.err)
        }
        write(new File(directory, "meta.json"),
          s"""{"status":${quote(status)},"reason":${quote(reason)},"rvls":${test.withRvls && test.withRvlsCheck && test.withProbe}}""" + "\n")
        write(new File(directory, status), "")
        hostOutput.println(s"$status ${entry.name}")
      })
    }
    tasks.foreach(_.join())
    executor.asInstanceOf[scala.concurrent.ExecutionContextExecutorService].shutdown()
    if (entries.forall(e => new File(output, "tests/" + e.name + "/PASS").isFile)) 0 else 1
  }

  def main(args: Array[String]): Unit = {
    val result = try run(args) catch {
      case error: Throwable =>
        error.printStackTrace(Console.err)
        2
    }
    System.exit(result)
  }
}
