package vexiiriscv.execute.vector

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

object VectorAluOp {
  val ADD = 0
  val SUB = 1
  val RSUB = 2
  val AND = 3
  val OR = 4
  val XOR = 5
  val MOVE = 6
  val MIN = 7
  val MAX = 8
  val MERGE = 9 // vs1 where the v0 bit is set, else vs2
  val SLL = 10
  val SRL = 11  // shifts in the sign bit when signed
  // Compares of vs2 with vs1, unsigned or signed; ADD and SUB give the carry or borrow out under MASK_DST
  val SEQ = 12
  val SNE = 13
  val SLT = 14
  val SLE = 15
  val SGT = 16
  // Mask logicals; the N forms invert the result, ANDN and ORN invert vs1
  val NAND = 17
  val ANDN = 18
  val NOR = 19
  val ORN = 20
  val XNOR = 21
  def width = 5
}

// Step function of a command: which word of each operand a step reads and which destination word it writes
object VectorShape {
  val SINGLE = 0       // vd, vs2 and vs1 at SEW, step k on word k of each group
  val SCALAR_READ = 1  // element 0 of vs2 to the scalar result, also when vl is zero
  val SCALAR_WRITE = 2 // the scalar to element 0 of vd
  val EXT2 = 3         // vs2 at SEW/2: step k reads half k mod 2 of word k/2
  val EXT4 = 4         // vs2 at SEW/4: step k reads quarter k mod 4 of word k/4
  val MASK_DST = 5     // one result bit per vs2/vs1 element, gathered into mask word k*SEW/64 of vd
  val MASK_MM = 6      // vd, vs2 and vs1 are masks: step k on word k, bits below vl only
  def width = 4
}

case class VectorArithmeticCmd(p: VectorRegFileParam, xlen: Int) extends Bundle {
  val vd, vs1, vs2 = UInt(5 bits)
  val vl = UInt(log2Up(p.vlen) + 1 bits)
  val vstart = UInt(log2Up(p.vlen) bits)
  val sew = UInt(2 bits)
  val shape = UInt(VectorShape.width bits)
  val op = UInt(VectorAluOp.width bits)
  val scalar = Bits(xlen bits)
  val scalarOperand, unmasked = Bool()
  val maskData = Bool() // v0 is a carry or merge select; every body element is written
  val signed, saturate = Bool()
  val vxrm = Bits(2 bits)
}

// One step per cycle: issue synchronous reads, compute in X1 and X2, then write the destination word with byte enables.
class VectorArithmetic(val p: VectorRegFileParam, val xlen: Int) extends Component {
  require(p.dlen == 64 && p.readPorts >= 3)
  require(xlen == 32 || xlen == 64)
  val io = new Bundle {
    val cmd = slave(Flow(VectorArithmeticCmd(p, xlen)))
    val reads = Vec.fill(3)(master(VrfReadPort(p)))
    val write = master(Flow(VrfWriteCmd(p)))
    val v0 = in Bits(p.vlen bits)
    val busy, done = out Bool()
    val saturated = out Bool() // an active element saturated; holds from done until the next command
    val result = out Bits(xlen bits)
  }

  val command = Reg(VectorArithmeticCmd(p, xlen))
  val scalarRead = command.shape === VectorShape.SCALAR_READ
  val scalarWrite = command.shape === VectorShape.SCALAR_WRITE
  val ext2 = command.shape === VectorShape.EXT2
  val ext4 = command.shape === VectorShape.EXT4
  val maskDst = command.shape === VectorShape.MASK_DST
  val maskMm = command.shape === VectorShape.MASK_MM
  val subtract = command.op === VectorAluOp.SUB || command.op === VectorAluOp.RSUB ||
    command.op === VectorAluOp.MIN || command.op === VectorAluOp.MAX ||
    command.op === VectorAluOp.SLT || command.op === VectorAluOp.SLE || command.op === VectorAluOp.SGT
  val shift = command.op === VectorAluOp.SLL || command.op === VectorAluOp.SRL
  val running = RegInit(False)
  val busy = RegInit(False)
  val saturated = RegInit(False)
  val step = Reg(UInt(log2Up(p.beatsPerReg * 8) bits)) init(0)
  val result = Reg(Bits(xlen bits))
  io.result := result
  io.busy := busy
  io.saturated := saturated

  def word(reg: UInt, index: UInt): UInt = ((reg.resize(p.addressWidth) << p.beatWidth) + index).resize(p.addressWidth)
  // Most significant byte of the element holding byte j
  def topOf(j: Int): UInt = command.sew.mux(U(0) -> U(j, 3 bits), U(1) -> U(j | 1, 3 bits), default -> U(j | 3, 3 bits))

  when(io.cmd.valid) {
    command := io.cmd.payload
    running := True
    busy := True
    saturated := False
    step := 0
  }

  val issue = new Area {
    val enables, selects = Bits(8 bits) // selects: v0 bit of the element holding each byte
    for (j <- 0 until 8) {
      val element = (((step.resize(log2Up(p.vlen) + 3) << 3) + j) >> command.sew).resize(command.vl.getWidth)
      selects(j) := io.v0(element.resize(log2Up(p.vlen)))
      enables(j) := element >= command.vstart.resize(element.getWidth) && element < command.vl &&
        (command.unmasked || command.maskData || selects(j)) &&
        (!scalarWrite || element === 0)
    }
    val wide = step.resize(log2Up(p.vlen) + 7)
    val nextElement = (maskMm ? ((wide + 1) << 6) | (((wide + 1) << 3) >> command.sew)).resize(log2Up(p.vlen) + 7)
    // Mask shapes issue every step up to the last one, so the old destination word is always read
    val active = running && (enables.orR || scalarRead || (maskDst || maskMm) && command.vl =/= 0)
    val last = running && (scalarRead || scalarWrite || nextElement >= command.vl)
    // MASK_DST: position of this step's result bits in the destination mask word, and that word
    val offset = ((wide << 3) >> command.sew).resize(6)
    val nextOffset = (((wide + 1) << 3) >> command.sew).resize(6)
    val maskWord = (step >> 3) >> command.sew
    val wordLast = !maskDst || last || nextOffset === 0
    // MASK_MM: body bits of this word; mask/mask commands start at element 0, E0 traps a nonzero vstart
    val rest = command.vl.resize(wide.getWidth) - (wide << 6).resize(wide.getWidth)
    val bodyBits = (rest >= 64) ? U(64, 7 bits) | rest.resize(7)

    // Ports A, B and C; scalar reads always use vs2, including when vl is zero
    io.reads(0).cmd.valid := active && (scalarRead || !scalarWrite && command.op =/= VectorAluOp.MOVE)
    io.reads(0).cmd.payload := word(command.vs2, ext4 ? (step >> 2) | (ext2 ? (step >> 1) | step))
    io.reads(1).cmd.valid := active && !command.scalarOperand && !scalarRead && !ext2 && !ext4
    io.reads(1).cmd.payload := word(command.vs1, step)
    // The old destination word, whose bits outside the result are kept (norm:vreg_mask_tail_agn, P3)
    io.reads(2).cmd.valid := active && (maskMm || maskDst && offset === 0)
    io.reads(2).cmd.payload := word(command.vd, maskDst ? maskWord.resize(step.getWidth) | step)

    when(running) {
      step := step + 1
      when(last) { running := False }
    }
  }

  // Adder, logic, overflow flags, min/max/merge select and extension on the read data
  val x1 = new Area {
    val valid = RegNext(issue.active) init(False)
    val last = RegNext(issue.last) init(False)
    val address = RegNext(word(command.vd, maskDst ? issue.maskWord.resize(step.getWidth) | step))
    val mask = RegNext(issue.enables)
    val selects = RegNext(issue.selects)
    val part = RegNext(step(1 downto 0))
    val offset = RegNext(issue.offset)
    val wordLast = RegNext(issue.wordLast)
    val bodyBits = RegNext(issue.bodyBits)
    val old = io.reads(2).rsp

    val scalarBeat = command.sew.mux(
      U(0) -> (command.scalar(7 downto 0) #* 8),
      U(1) -> (command.scalar(15 downto 0) #* 4),
      default -> (command.scalar(31 downto 0) #* 2)
    )
    val a = io.reads(0).rsp
    val b = command.scalarOperand ? scalarBeat | io.reads(1).rsp
    val left = (command.op === VectorAluOp.RSUB) ? b | a
    val right = (command.op === VectorAluOp.RSUB) ? a | b
    val sum = Bits(64 bits)
    val carries = Vec(Bool(), 9)
    // Per byte, meaningful at the most significant byte of each element: the result does not fit SEW bits
    val overflow = Bits(8 bits)
    // Unsigned or signed vs2 < vs1, meaningful at the most significant byte of each element
    val less = Bits(8 bits)
    carries(0) := subtract
    for (j <- 0 until 8) {
      val boundary = if (j % 4 == 0) True else if (j % 2 == 0) command.sew =/= 2 else command.sew === 0
      // Carry-in, or borrow-in for subtraction, from v0 (norm:vadc_vsbc_op)
      val carry = boundary ? (subtract ^ (command.maskData && selects(j))) | carries(j)
      val rhs = right(j * 8, 8 bits) ^ B(8 bits, default -> subtract)
      val value = left(j * 8, 8 bits).asUInt.resize(9) + rhs.asUInt.resize(9) + carry.asUInt
      sum(j * 8, 8 bits) := value(7 downto 0).asBits
      carries(j + 1) := value.msb
      val leftSign = left(j * 8 + 7)
      overflow(j) := command.signed ? (leftSign === rhs.msb && value(7) =/= leftSign) | (value.msb ^ subtract)
      less(j) := command.signed ? (value(7) ^ overflow(j)) | overflow(j)
    }
    val selected = Bits(64 bits)
    for (j <- 0 until 8) {
      val takeA = (command.op === VectorAluOp.MIN) === less(topOf(j))
      val takeB = (command.op === VectorAluOp.MERGE) ? selects(j) | !takeA
      selected(j * 8, 8 bits) := takeB ? b(j * 8, 8 bits) | a(j * 8, 8 bits)
    }

    // One result bit per element of the word, element e in bit e (norm:vmseq_vmsne_vmsltu_vmslt_vmsleu_vmsle_vmsgtu_vmsgt_op)
    val equal = Bits(8 bits)
    for (j <- 0 until 8) equal(j) := a(j * 8, 8 bits) === b(j * 8, 8 bits)
    val outcome = Bits(8 bits) // meaningful at the most significant byte of each element
    for (j <- 0 until 8) {
      val same = command.sew.mux(U(0) -> equal(j), U(1) -> equal(j & 6, 2 bits).andR, default -> equal(j & 4, 4 bits).andR)
      outcome(j) := command.op.mux(
        U(VectorAluOp.SEQ) -> same,
        U(VectorAluOp.SNE) -> !same,
        U(VectorAluOp.SLT) -> less(j),
        U(VectorAluOp.SLE) -> (less(j) || same),
        U(VectorAluOp.SGT) -> !(less(j) || same),
        default -> overflow(j) // unsigned carry or borrow out (norm:vmadc_vmsbc_op_masked, norm:vmsbc_borrow_neg)
      )
    }
    val bits, actives = Bits(8 bits)
    for (e <- 0 until 8) {
      def at(f: Bits, s: Int, top: Boolean): Bool = if (e < (8 >> s)) f((e << s) + (if (top) (1 << s) - 1 else 0)) else False
      bits(e) := command.sew.mux(U(0) -> at(outcome, 0, true), U(1) -> at(outcome, 1, true), default -> at(outcome, 2, true))
      actives(e) := command.sew.mux(U(0) -> at(mask, 0, false), U(1) -> at(mask, 1, false), default -> at(mask, 2, false))
    }

    // Sign or zero extension of the selected part of vs2 (norm:vsext_vzext_op)
    val source = ext4 ? a.subdivideIn(16 bits)(part).resize(32) | a.subdivideIn(32 bits)(part(0 downto 0))
    def extend(fromBytes: Int, toBytes: Int): Bits = {
      val out = Bits(64 bits)
      for (j <- 0 until 8) {
        val (e, k) = (j / toBytes, j % toBytes)
        out(j * 8, 8 bits) := (if (k < fromBytes) source((e * fromBytes + k) * 8, 8 bits)
          else B(8 bits, default -> (command.signed && source((e + 1) * fromBytes * 8 - 1))))
      }
      out
    }
    val extended = (command.sew === 1) ? extend(1, 2) | (ext4 ? extend(1, 4) | extend(2, 4))
    val data = command.op.mux(
      U(VectorAluOp.AND) -> (a & b),
      U(VectorAluOp.OR) -> (a | b),
      U(VectorAluOp.XOR) -> (a ^ b),
      U(VectorAluOp.NAND) -> ~(a & b),
      U(VectorAluOp.ANDN) -> (a & ~b),
      U(VectorAluOp.NOR) -> ~(a | b),
      U(VectorAluOp.ORN) -> (a | ~b),
      U(VectorAluOp.XNOR) -> ~(a ^ b),
      U(VectorAluOp.MOVE) -> b,
      U(VectorAluOp.MIN) -> selected,
      U(VectorAluOp.MAX) -> selected,
      U(VectorAluOp.MERGE) -> selected,
      U(VectorAluOp.SLL) -> a,
      U(VectorAluOp.SRL) -> a,
      default -> sum
    )
    val output = (ext2 || ext4) ? extended | data
    val negative = Bits(8 bits)
    for (j <- 0 until 8) negative(j) := a(j * 8 + 7)

    when(valid && scalarRead) {
      result := command.sew.mux(
        U(0) -> a(7 downto 0).asSInt.resize(xlen).asBits,
        U(1) -> a(15 downto 0).asSInt.resize(xlen).asBits,
        default -> a(31 downto 0).asSInt.resize(xlen).asBits
      )
    }
  }

  // Shifts and saturation of whole elements (norm:vsaddu_vsadd_vssubu_vssub_op_overflow_vxsat_op_vsaddsub), mask merging
  val x2 = new Area {
    val valid = RegNext(x1.valid) init(False)
    val last = RegNext(x1.last) init(False)
    val address = RegNext(x1.address)
    val mask = RegNext(x1.mask)
    val offset = RegNext(x1.offset)
    val wordLast = RegNext(x1.wordLast)
    val bodyBits = RegNext(x1.bodyBits)
    val old = RegNext(x1.old)
    val bits = RegNext(x1.bits)
    val actives = RegNext(x1.actives)
    val data = RegNext(x1.output)
    val amounts = RegNext(x1.b)
    val overflow = RegNext(x1.overflow)
    val negative = RegNext(x1.negative)

    // Segmented shifter, one stage per amount bit; only the low lg2(SEW) bits count (norm:vsll_vsrl_vsra_shamt)
    val left = command.op === VectorAluOp.SLL
    def lane(i: Int, w: Int) = i - i % w
    val fill = Bits(64 bits)
    for (i <- 0 until 64) fill(i) := command.signed && command.sew.mux(
      U(0) -> data(lane(i, 8) + 7), U(1) -> data(lane(i, 16) + 15), default -> data(lane(i, 32) + 31))
    val stages = ArrayBuffer(data)
    for (k <- 0 until 5) {
      val s = 1 << k
      val in = stages.last
      val out = Bits(64 bits)
      for (i <- 0 until 64) {
        def bit(w: Int): Bool = {
          val pos = i % w
          if (s >= w) in(i)
          else {
            val l = if (pos >= s) in(i - s) else False
            val r = if (pos + s < w) in(i + s) else fill(i)
            amounts(lane(i, w) + k) ? (left ? l | r) | in(i)
          }
        }
        out(i) := command.sew.mux(U(0) -> bit(8), U(1) -> bit(16), default -> bit(32))
      }
      stages += out
    }
    val base = shift ? stages.last | data

    val clipped = Bits(8 bits)
    val value = Bits(64 bits)
    for (j <- 0 until 8) {
      val top = topOf(j)
      val upper = top === U(j, 3 bits)
      val signedLimit = negative(top) ? (upper ? B"x80" | B"x00") | (upper ? B"x7f" | B"xff")
      clipped(j) := command.saturate && overflow(top)
      value(j * 8, 8 bits) := clipped(j) ? (command.signed ? signedLimit | (subtract ? B"x00" | B"xff")) | base(j * 8, 8 bits)
    }
    // Only active elements set vxsat; their bytes are the enabled ones
    when(valid && (clipped & mask).orR) { saturated := True }

    // MASK_DST: the accumulator starts from the old word at a word's first step and takes active bits only
    val accumulator = Reg(Bits(64 bits))
    val start = (offset === 0) ? old | accumulator
    val placed = (bits.resize(64) << offset).resize(64)
    val enabled = (actives.resize(64) << offset).resize(64)
    val gathered = (start & ~enabled) | (placed & enabled)
    when(valid && maskDst) { accumulator := gathered }
    // MASK_MM: result bits below vl, the old word above
    val body = Bits(64 bits)
    for (i <- 0 until 64) body(i) := U(i, 7 bits) < bodyBits
    val merged = (data & body) | (old & ~body)
    val output = maskDst ? gathered | (maskMm ? merged | value)
  }

  val writeback = new Area {
    val valid = RegNext(x2.valid && !scalarRead && x2.wordLast) init(False)
    val last = RegNext(x2.last) init(False)
    val address = RegNext(x2.address)
    val mask = RegNext((maskDst || maskMm) ? B(0xff, 8 bits) | x2.mask)
    val data = RegNext(x2.output)
    io.write.valid := valid
    io.write.address := address
    io.write.mask := mask
    io.write.data := data
  }

  // The VRF holds every written word when done rises
  io.done := RegNext(writeback.last) init(False)
  when(io.done) { busy := False }
}
