package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

// Byte-enabled vector storage with synchronous read-first ports and a separate v0 mask shadow.
case class VectorRegFileParam(vlen: Int = 128,
                             dlen: Int = 64,
                             readPorts: Int = 3,
                             ramStyle: String = "distributed") {
  assert(isPow2(vlen) && isPow2(dlen), "VLEN and DLEN must be powers of two")
  assert(dlen >= 32 && dlen <= vlen, "need 32 <= DLEN <= VLEN")
  assert(readPorts >= 1)
  assert(List("distributed", "block", "registers").contains(ramStyle))

  val regCount = 32
  val beatsPerReg = vlen / dlen
  val beatWidth = log2Up(beatsPerReg)
  val wordCount = regCount * beatsPerReg
  val addressWidth = log2Up(wordCount)
  val beatBytes = dlen / 8

  def wordOf(vreg: Int, beat: Int): Int = vreg * beatsPerReg + beat
}

case class VrfReadPort(p: VectorRegFileParam) extends Bundle with IMasterSlave {
  val cmd = Flow(UInt(p.addressWidth bits))
  val rsp = Bits(p.dlen bits)

  override def asMaster(): Unit = {
    master(cmd)
    in(rsp)
  }
}

case class VrfWriteCmd(p: VectorRegFileParam) extends Bundle {
  val address = UInt(p.addressWidth bits)
  val data = Bits(p.dlen bits)
  val mask = Bits(p.beatBytes bits)
}

class VectorRegFile(val p: VectorRegFileParam) extends Component {
  import p._

  val io = new Bundle {
    val reads = Vec.fill(readPorts)(slave(VrfReadPort(p)))
    val write = slave(Flow(VrfWriteCmd(p)))
    val v0 = out Bits (vlen bits)
    val initHold = out Bool()
    val writeEvent = master(Flow(VrfWriteCmd(p)))
  }

  val initializer = new Area {
    val counter = Reg(UInt(addressWidth + 1 bits)) init(0)
    val done = counter.msb
    when(!done) {
      counter := counter + 1
    }
    io.initHold := !done
  }

  // The clear owns the write port until every word (including the v0 shadow) has been zeroed.
  val writePort = Flow(VrfWriteCmd(p))
  writePort := io.write
  when(io.initHold) {
    writePort.valid := True
    writePort.address := initializer.counter.resized
    writePort.data := 0
    writePort.mask.setAll()
  }
  io.writeEvent := writePort

  val storage = new Area {
    val ram = Mem.fill(wordCount)(Bits(dlen bits))
    ram.addAttribute("ram_style", ramStyle)
    ram.write(
      address = writePort.address,
      data = writePort.data,
      enable = writePort.valid,
      mask = writePort.mask
    )

    val reads = for (port <- io.reads) yield new Area {
      port.rsp := ram.readSync(
        address = port.cmd.payload,
        enable = port.cmd.valid && !io.initHold,
        readUnderWrite = readFirst
      )
    }
  }

  // Shadow copy of v0 (words 0 until beatsPerReg), kept in flops.
  val v0Shadow = new Area {
    val value = Reg(Bits(vlen bits))
    val hitsV0 = writePort.valid && (writePort.address >> beatWidth) === 0
    val beat = if (beatWidth == 0) U(0, 1 bits) else writePort.address.resize(beatWidth)

    for (b <- 0 until beatsPerReg; byte <- 0 until beatBytes) {
      when(hitsV0 && beat === b && writePort.mask(byte)) {
        value(b * dlen + byte * 8, 8 bits) := writePort.data(byte * 8, 8 bits)
      }
    }
    io.v0 := value
  }
}

// Register inputs and read responses to measure register-to-register paths through the storage.
class VectorRegFileOoc(p: VectorRegFileParam) extends Component {
  val io = new Bundle {
    val reads = Vec.fill(p.readPorts)(slave(VrfReadPort(p)))
    val write = slave(Flow(VrfWriteCmd(p)))
    val v0 = out Bits (p.vlen bits)
    val initHold = out Bool()
  }
  setDefinitionName("VectorRegFileOoc")

  val vrf = new VectorRegFile(p)
  vrf.io.write << io.write.stage()
  for ((outer, inner) <- io.reads.zip(vrf.io.reads)) {
    inner.cmd << outer.cmd.stage()
    outer.rsp := RegNext(inner.rsp)
  }
  io.v0 := vrf.io.v0
  io.initHold := vrf.io.initHold
}

// Arguments: outDir vlen dlen name:readPorts:ramStyle [name:readPorts:ramStyle ...].
object VectorRegFileGen extends App {
  assert(args.length >= 4, "usage: outDir vlen dlen name:readPorts:ramStyle ...")
  val outDir = args(0)
  val vlen = args(1).toInt
  val dlen = args(2).toInt
  for (variant <- args.drop(3)) {
    val Array(name, readPorts, ramStyle) = variant.split(":")
    val p = VectorRegFileParam(vlen, dlen, readPorts.toInt, ramStyle)
    SpinalConfig(
      mode = Verilog,
      targetDirectory = s"$outDir/$name/rtl"
    ).generate(new VectorRegFileOoc(p))
  }
}
