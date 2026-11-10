package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

// Merge the coincident older write into the synchronous read-first response.
class VectorReadForwarding(p: VectorRegFileParam) extends Component {
  val io = new Bundle {
    val reads = Vec.fill(3)(slave(VrfReadPort(p)))
    val raw = in Vec(Bits(p.dlen bits), 3)
    val write = slave(Flow(VrfWriteCmd(p)))
    val older = in Bool()
  }
  val data = RegNext(io.write.data)
  val mask = RegNext(io.write.mask)
  for (i <- 0 until 3) {
    val hit = RegNext(io.reads(i).cmd.valid && io.write.valid && io.older &&
      io.reads(i).cmd.payload === io.write.address) init(False)
    for (b <- 0 until p.beatBytes) {
      io.reads(i).rsp(b * 8, 8 bits) := (hit && mask(b)) ? data(b * 8, 8 bits) | io.raw(i)(b * 8, 8 bits)
    }
  }
}
