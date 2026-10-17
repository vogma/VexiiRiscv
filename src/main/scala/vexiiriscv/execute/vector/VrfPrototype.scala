package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

/*
 * Phase 0b feasibility prototype of the vector register file (VRF).
 * Design note: docs/design/vrf.md in the vexii-rvv top-level repository.
 *
 * This file is standalone on purpose. Nothing in Param.scala or in any plugin
 * references it (Phase 0b exit criterion 3, AGENTS.md rule 8). It exists to be
 * simulated (VrfPrototypeTest) and synthesized out of context (VrfPrototypeGen,
 * scripts/vrf-ooc.sh).
 *
 * Organisation:
 * - 32 vector registers of VLEN bits, stored as 32 * VLEN/DLEN words of DLEN
 *   bits. Word address = vreg * (VLEN/DLEN) + beat, so the beats of one
 *   register, and the registers of one group, are consecutive words.
 * - One write port with a byte enable per byte of the word. Undisturbed
 *   elements (prestart, inactive, tail) are simply not enabled.
 * - readPorts synchronous read ports: address in cycle t, data in cycle t+1.
 *   A read of the word written in the same cycle returns the old content
 *   (read-first). When the command is not valid, rsp keeps its last value.
 * - A v0 shadow register of VLEN bits, updated by every write that targets
 *   v0 with the same byte enables, so the mask needs no read port.
 * - No reset: the storage and the shadow power up undefined, as a LUTRAM or
 *   BRAM would.
 */
case class VrfPrototypeParam(vlen: Int = 128,
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

case class VrfReadPort(p: VrfPrototypeParam) extends Bundle with IMasterSlave {
  val cmd = Flow(UInt(p.addressWidth bits))
  val rsp = Bits(p.dlen bits)

  override def asMaster(): Unit = {
    master(cmd)
    in(rsp)
  }
}

case class VrfWriteCmd(p: VrfPrototypeParam) extends Bundle {
  val address = UInt(p.addressWidth bits)
  val data = Bits(p.dlen bits)
  val mask = Bits(p.beatBytes bits)
}

class VrfPrototype(val p: VrfPrototypeParam) extends Component {
  import p._

  val io = new Bundle {
    val reads = Vec.fill(readPorts)(slave(VrfReadPort(p)))
    val write = slave(Flow(VrfWriteCmd(p)))
    val v0 = out Bits (vlen bits)
  }

  val storage = new Area {
    val ram = Mem.fill(wordCount)(Bits(dlen bits))
    ram.addAttribute("ram_style", ramStyle)
    ram.write(
      address = io.write.address,
      data = io.write.data,
      enable = io.write.valid,
      mask = io.write.mask
    )

    val reads = for (port <- io.reads) yield new Area {
      port.rsp := ram.readSync(
        address = port.cmd.payload,
        enable = port.cmd.valid,
        readUnderWrite = readFirst
      )
    }
  }

  // Shadow copy of v0 (words 0 until beatsPerReg), kept in flops.
  val v0Shadow = new Area {
    val value = Reg(Bits(vlen bits))
    val hitsV0 = io.write.valid && (io.write.address >> beatWidth) === 0
    val beat = if (beatWidth == 0) U(0, 1 bits) else io.write.address.resize(beatWidth)

    for (b <- 0 until beatsPerReg; byte <- 0 until beatBytes) {
      when(hitsV0 && beat === b && io.write.mask(byte)) {
        value(b * dlen + byte * 8, 8 bits) := io.write.data(byte * 8, 8 bits)
      }
    }
    io.v0 := value
  }
}

/**
 * Out-of-context harness: registers every input and every read response, so
 * that the timing report measures register-to-register paths through the VRF
 * instead of unconstrained I/O. The v0 output is already a flop.
 */
class VrfPrototypeOoc(p: VrfPrototypeParam) extends Component {
  val io = new Bundle {
    val reads = Vec.fill(p.readPorts)(slave(VrfReadPort(p)))
    val write = slave(Flow(VrfWriteCmd(p)))
    val v0 = out Bits (p.vlen bits)
  }
  setDefinitionName("VrfPrototypeOoc")

  val vrf = new VrfPrototype(p)
  vrf.io.write << io.write.stage()
  for ((outer, inner) <- io.reads.zip(vrf.io.reads)) {
    inner.cmd << outer.cmd.stage()
    outer.rsp := RegNext(inner.rsp)
  }
  io.v0 := vrf.io.v0
}

/**
 * Verilog generator for scripts/vrf-ooc.sh.
 * Arguments: outDir vlen dlen name:readPorts:ramStyle [name:readPorts:ramStyle ...]
 * Each variant is written to outDir/name/rtl/VrfPrototypeOoc.v.
 */
object VrfPrototypeGen extends App {
  assert(args.length >= 4, "usage: outDir vlen dlen name:readPorts:ramStyle ...")
  val outDir = args(0)
  val vlen = args(1).toInt
  val dlen = args(2).toInt
  for (variant <- args.drop(3)) {
    val Array(name, readPorts, ramStyle) = variant.split(":")
    val p = VrfPrototypeParam(vlen, dlen, readPorts.toInt, ramStyle)
    SpinalConfig(
      mode = Verilog,
      targetDirectory = s"$outDir/$name/rtl"
    ).generate(new VrfPrototypeOoc(p))
  }
}
