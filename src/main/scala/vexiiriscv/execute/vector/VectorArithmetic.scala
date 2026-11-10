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
  // Fixed point, rounded by vxrm: averaging (vs2 +/- vs1) >> 1, and scaling right shifts that shift in the sign bit when signed
  val AADD = 22
  val ASUB = 23
  val SSRL = 24
  val INDEX = 25 // the element index, without operands (norm:vid_op)
  val MSBF = 26
  val MSIF = 27
  val MSOF = 28
  def width = 5
}

object VectorFixedPoint {
  // Increment after a right shift from the kept lsb, the first dropped bit and the OR of the others (norm:vxrm_op)
  def roundIncrement(lsb: Bool, half: Bool, sticky: Bool, vxrm: Bits): Bool = vxrm.mux(
    B"00" -> half,                         // rnu
    B"01" -> (half && (sticky || lsb)),    // rne
    B"10" -> False,                        // rdn
    B"11" -> (!lsb && (half || sticky))    // rod
  )
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
  val WHOLE = 7        // word k of vs2 to word k of vd for nreg registers, whatever vl, vstart, SEW and v0
  val WIDE_VV = 8      // vd at 2*SEW: step k writes word k from half k mod 2 of word k/2 of vs2 and vs1
  val WIDE_WV = 9      // vd and vs2 at 2*SEW: step k on word k of both, half k mod 2 of word k/2 of vs1
  val NARROW = 10      // vs2 at 2*SEW: step k reads its word k and half k mod 2 of word k/2 of vs1, writes half k mod 2 of word k/2 of vd
  // Reductions into element 0 of vd: step k folds word k of vs2 (WREDUCE: half k mod 2 of word k/2, extended to 2*SEW)
  // into an accumulator that starts from element 0 of vs1, then the accumulator lanes fold into lane 0
  val REDUCE = 11
  val WREDUCE = 12
  val EXT8 = 13        // vs2 at SEW/8: step k reads byte k mod 8 of word k/8
  // Permutes, with the beat slides option: step k writes word k from vs2 bytes at a constant distance, or from one element
  val SLIDE = 14       // vslideup, vslidedown, vslide1up, vslide1down (norm:vslideup_op, norm:vslidedown_op)
  val SPLAT = 15       // vrgather.vx/.vi: element idx of vs2 to every element (norm:vrgather-vx_vrgather-vi_op)
  def width = 4
}

case class VectorArithmeticCmd(p: VectorRegFileParam, xlen: Int, slides: Boolean = false, beatCompress: Boolean = false, beatMul: Boolean = false) extends Bundle {
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
  val nreg = UInt(4 bits) // WHOLE: 1, 2, 4 or 8 registers
  val vlmax = slides generate UInt(log2Up(p.vlen) + 1 bits)
  val slide = slides generate Bits(2 bits) // SLIDE: bit 0 down, bit 1 by one element with the scalar inserted
  val compress = beatCompress generate Bool() // vcompress.vm: pack the vs2 elements selected by the vs1 mask into vd
  val multiply, signedVs1, negate = beatMul generate Bool()
  val mulOp = beatMul generate UInt(VectorElementOp.width bits)
}

// Shape and op properties of a command, registered with it so the datapath does not decode them
case class VectorArithmeticFlags() extends Bundle {
  val scalarRead, scalarWrite, ext2, ext4, ext8, maskDst, maskMm, maskScan, whole = Bool()
  val wideVv, widening, narrow, wreduce, reduce = Bool()
  val index, subtract, average, rounding, shift, left, rsub, min, merge = Bool()
  val dstSew, laneSew = UInt(2 bits)
}

object VectorArithmeticFlags {
  def apply(c: VectorArithmeticCmd, elen: Int): VectorArithmeticFlags = {
    val f = VectorArithmeticFlags()
    def op(ops: Int*): Bool = ops.map(c.op === _).orR
    f.scalarRead := c.shape === VectorShape.SCALAR_READ
    f.scalarWrite := c.shape === VectorShape.SCALAR_WRITE
    f.ext2 := c.shape === VectorShape.EXT2
    f.ext4 := c.shape === VectorShape.EXT4
    f.ext8 := (if (elen == 64) c.shape === VectorShape.EXT8 else False)
    f.maskDst := c.shape === VectorShape.MASK_DST
    f.maskMm := c.shape === VectorShape.MASK_MM
    f.maskScan := op(VectorAluOp.MSBF, VectorAluOp.MSIF, VectorAluOp.MSOF)
    f.whole := c.shape === VectorShape.WHOLE
    f.wideVv := c.shape === VectorShape.WIDE_VV
    f.widening := f.wideVv || c.shape === VectorShape.WIDE_WV
    f.narrow := c.shape === VectorShape.NARROW
    f.wreduce := c.shape === VectorShape.WREDUCE
    f.reduce := f.wreduce || c.shape === VectorShape.REDUCE
    f.dstSew := c.sew + U(f.widening || f.wreduce)
    f.laneSew := c.sew + U(f.widening || f.narrow || f.wreduce)
    f.index := c.op === VectorAluOp.INDEX
    f.subtract := op(VectorAluOp.SUB, VectorAluOp.RSUB, VectorAluOp.MIN, VectorAluOp.MAX,
      VectorAluOp.SLT, VectorAluOp.SLE, VectorAluOp.SGT, VectorAluOp.ASUB)
    f.average := op(VectorAluOp.AADD, VectorAluOp.ASUB)
    f.rounding := f.average || c.op === VectorAluOp.SSRL
    f.shift := op(VectorAluOp.SLL, VectorAluOp.SRL) || f.rounding
    f.left := c.op === VectorAluOp.SLL
    f.rsub := c.op === VectorAluOp.RSUB
    f.min := c.op === VectorAluOp.MIN
    f.merge := c.op === VectorAluOp.MERGE
    f
  }
}

// One step per cycle: issue synchronous reads, compute in X1 and X2, then write the destination word with byte enables.
// ELEN 64 adds 64-bit lanes (sew 3). No element crosses a 64-bit sub-word, so a DLEN-128 word is two copies of the
// lane logic, joined only by the mask, reduction, narrowing, permute and compress structures. With overlap a command may start while the previous one's
// steps are still in X1 and later; each stage then works from its own copy of the command.
class VectorArithmetic(val p: VectorRegFileParam, val xlen: Int, val elen: Int = 32, val slides: Boolean = false,
                        val beatCompress: Boolean = false, val overlap: Boolean = false, val tagWidth: Int = 3,
                        val beatMul: Boolean = false, val chaining: Boolean = false,
                        val classOverlap: Boolean = false) extends Component {
  require((p.dlen == 64 || p.dlen == 128) && p.readPorts >= 3)
  require(!classOverlap || beatMul && overlap)
  require(!beatCompress || slides)
  require(!chaining || overlap)
  val wordBytes = p.beatBytes
  val subWords = p.dlen / 64
  val byteBits = log2Up(wordBytes)
  val maskBits = log2Up(p.dlen)
  def ones(n: Int): Bits = B((BigInt(1) << n) - 1, n bits)
  def rep(v: Bits, n: Int): Bits = if (n == 1) v else v #* n
  require(xlen == 32 || xlen == 64)
  require(elen == 32 || elen == 64)
  val io = new Bundle {
    val cmd = slave(Flow(VectorArithmeticCmd(p, xlen, slides, beatCompress, beatMul)))
    val reads = Vec.fill(3)(master(VrfReadPort(p)))
    val write = master(Flow(VrfWriteCmd(p)))
    val candidates = chaining generate Vec.fill(3)(master(Flow(UInt(p.addressWidth bits))))
    val sourceReady = chaining generate (in Bool())
    val readTag = chaining generate (out UInt(tagWidth bits))
    val progress = chaining generate master(Flow(VectorWordProgress(p, tagWidth)))
    val v0 = in Bits(p.vlen bits)
    val busy, done = out Bool()
    val saturated = out Bool() // an active element saturated; holds from done until the next command
    val result = out Bits(xlen bits)
    val ready = overlap generate (out Bool()) // a command may start this cycle
    val tag = overlap generate (in UInt(tagWidth bits)) // taken with the command
    val writeTag, doneTag = overlap generate (out UInt(tagWidth bits)) // the command of io.write and of io.done
    val tailMultiply = classOverlap generate (out Bool()) // a multiply command is issuing; an ordinary one must wait
  }

  def issueFire: Bool = if (chaining) io.sourceReady else True
  def accepted(v: Bool): Bool = if (chaining) v && io.sourceReady else v
  val requests = if (chaining) io.candidates else io.reads.map(_.cmd)
  val command = Reg(VectorArithmeticCmd(p, xlen, slides, beatCompress, beatMul))
  val mulIssueTag = beatMul && overlap generate (RegNextWhen(io.tag, io.cmd.valid) init(0))
  val mulPass = beatMul generate (RegInit(False))
  val mulSplit = beatMul generate (if (elen == 64) command.multiply && command.sew === 3 else False)
  val flags = Reg(VectorArithmeticFlags())
  val scalarRead = flags.scalarRead
  val scalarWrite = flags.scalarWrite
  val ext2 = flags.ext2
  val ext4 = flags.ext4
  val ext8 = flags.ext8
  val maskDst = flags.maskDst
  val maskMm = flags.maskMm
  val maskScan = flags.maskScan
  val whole = flags.whole
  val wideVv = flags.wideVv
  val widening = flags.widening
  val narrow = flags.narrow
  val wreduce = flags.wreduce
  val reduce = flags.reduce
  // log2 bytes of a destination element (of a source element for reductions), and of the adder and shifter lanes
  val dstSew = flags.dstSew
  val laneSew = flags.laneSew
  val index = flags.index
  val subtract = flags.subtract
  val average = flags.average
  val rounding = flags.rounding
  val shift = flags.shift
  val running = RegInit(False)
  val busy = RegInit(False)
  val saturated = RegInit(False)
  val step = Reg(UInt(log2Up(p.beatsPerReg * 16) bits)) init(0)
  // Reductions: after the last step, one fold per halving of the accumulator down to one lane
  val folding = RegInit(False)
  val fold = Reg(UInt(2 bits)) init(0)
  val result = Reg(Bits(xlen bits))
  io.result := result
  io.busy := busy
  if (!overlap) io.saturated := saturated
  // The command of a step one stage later
  def stage[T <: Data](that: T): T = if (overlap) RegNext(that) else that

  def word(reg: UInt, index: UInt): UInt = ((reg.resize(p.addressWidth) << p.beatWidth) + index).resize(p.addressWidth)
  // Most significant byte of the lane holding byte j
  def topOf(j: Int, laneSew: UInt = laneSew): UInt = if (elen == 64)
    laneSew.mux(U(0) -> U(j, byteBits bits), U(1) -> U(j | 1, byteBits bits), U(2) -> U(j | 3, byteBits bits), default -> U(j | 7, byteBits bits))
  else laneSew.mux(U(0) -> U(j, byteBits bits), U(1) -> U(j | 1, byteBits bits), default -> U(j | 3, byteBits bits))
  // Byte j is the least significant byte of its lane
  def boundary(j: Int, laneSew: UInt): Bool = { val k = j % 8; if (elen == 64) {
    if (k == 0) True else if (k == 4) laneSew =/= 3 else if (k % 2 == 0) laneSew <= 1 else laneSew === 0
  } else if (k % 4 == 0) True else if (k % 2 == 0) laneSew =/= 2 else laneSew === 0 }
  // a + b per lane on one carry chain, with a guard bit below each byte: (1, 0) passes the carry of the byte
  // below, (c, c) starts a lane with carry-in c. Returns the sum and the carry out of each byte.
  def laneAdd(a: Bits, b: Bits, carryIn: Bits, laneSew: UInt): (Bits, Bits) = if (subWords == 1) laneAdd64(a, b, carryIn, laneSew) else {
    val parts = (0 until subWords).map(s => laneAdd64(a(s * 64, 64 bits), b(s * 64, 64 bits), carryIn(s * 8, 8 bits), laneSew))
    (Cat(parts.map(_._1)), Cat(parts.map(_._2)))
  }
  // One 64-bit sub-word of laneAdd
  def laneAdd64(a: Bits, b: Bits, carryIn: Bits, laneSew: UInt): (Bits, Bits) = {
    def spread(bytes: Bits, guards: Bits): UInt = Cat((0 until 8).map(j => bytes(j * 8, 8 bits) ## guards(j))).asUInt
    val guardA, guardB = Bits(8 bits)
    for (j <- 0 until 8) {
      guardA(j) := !boundary(j, laneSew) || carryIn(j)
      guardB(j) := boundary(j, laneSew) && carryIn(j)
    }
    val total = spread(a, guardA).resize(73) + spread(b, guardB).resize(73)
    val sumOut = Bits(64 bits)
    val carryOut = Bits(8 bits)
    for (j <- 0 until 8) {
      sumOut(j * 8, 8 bits) := total(j * 9 + 1, 8 bits).asBits
      // A passing guard sums to the inverted carry, a starting one to the carry itself
      carryOut(j) := (if (j == 7) total(72) else boundary(j + 1, laneSew) === total((j + 1) * 9))
    }
    (sumOut, carryOut)
  }

  def startCommand(): Unit = {
    when(io.cmd.valid) {
      command := io.cmd.payload
      flags := VectorArithmeticFlags(io.cmd.payload, elen)
      running := True
      if (!overlap) {
        busy := True
        saturated := False
      }
      step := 0
      if (beatMul) mulPass := False
      folding := False
      fold := 0
    }
  }
  if (!overlap) startCommand()

  val issue = new Area {
    val advance = beatMul generate (!mulSplit || mulPass)
    val enables, selects = Bits(wordBytes bits) // selects: v0 bit of the element holding each byte
    // Narrowing writes half step mod 2 of destination word step / 2
    val dstWord = narrow ? (step >> 1) | step
    for (j <- 0 until wordBytes) {
      val element = (((dstWord.resize(log2Up(p.vlen) + byteBits) << byteBits) + j) >> dstSew).resize(command.vl.getWidth)
      selects(j) := io.v0(element.resize(log2Up(p.vlen)))
      enables(j) := whole || element >= command.vstart.resize(element.getWidth) && element < command.vl &&
        (command.unmasked || command.maskData || selects(j)) &&
        (!scalarWrite || element === 0) && (!narrow || step(0) === Bool(j >= wordBytes / 2))
    }
    val wide = step.resize(log2Up(p.vlen) + 7)
    val nextElement = (maskMm ? ((wide + 1) << maskBits) | (((wide + 1) << byteBits) >> laneSew)).resize(log2Up(p.vlen) + 7)
    // Mask shapes issue every step up to the last one, so the old destination word is always read; reductions
    // always fold vs1 and write, unless vl is zero (norm:vreduction_vl_0)
    val candidateActive = running && (enables.orR || scalarRead || (maskDst || maskMm || reduce) && command.vl =/= 0)
    val active = if (chaining) candidateActive && issueFire else candidateActive
    if (!chaining) active.setName("issue_active")
    val wholeLast = (wide + 1) === (command.nreg.resize(wide.getWidth) << p.beatWidth)
    val stepsDone = nextElement >= command.vl
    val foldLast = fold === (U(maskBits - 4, 2 bits) - laneSew.resize(2))
    // A 64-bit accumulator is one lane: the last step ends the reduction
    val reduceLast = if (elen == 64 && subWords == 1) (laneSew === 3) ? stepsDone | (folding && foldLast) else folding && foldLast
    val last = (if (beatMul) accepted(running) && advance else accepted(running)) && (scalarRead || scalarWrite || (whole ? wholeLast |
      (reduce ? (command.vl === 0 || reduceLast) | stepsDone)))
    // MASK_DST: position of this step's result bits in the destination mask word, and that word
    val offset = ((wide << byteBits) >> command.sew).resize(maskBits)
    val nextOffset = (((wide + 1) << byteBits) >> command.sew).resize(maskBits)
    val maskWord = (step >> 3) >> command.sew
    val wordLast = reduce ? last | (!maskDst || last || nextOffset === 0)
    // MASK_MM: body bits of this word; mask/mask commands start at element 0, E0 traps a nonzero vstart
    val rest = command.vl.resize(wide.getWidth) - (wide << maskBits).resize(wide.getWidth)
    val bodyBits = (rest >= p.dlen) ? U(p.dlen, maskBits + 1 bits) | rest.resize(maskBits + 1)

    // Ports A, B and C; scalar reads always use vs2, including when vl is zero
    requests(0).valid := candidateActive && !folding && (scalarRead || !scalarWrite && command.op =/= VectorAluOp.MOVE && !index)
    requests(0).payload := word(command.vs2, ext8 ? (step >> 3) | (ext4 ? (step >> 2) | ((ext2 || wideVv || wreduce) ? (step >> 1) | step)))
    requests(1).valid := candidateActive && !command.scalarOperand && !scalarRead && !ext2 && !ext4 && !ext8 && !whole && !index && !maskScan &&
      (!reduce || step === 0 && !folding)
    requests(1).payload := word(command.vs1, reduce ? U(0, step.getWidth bits) | ((widening || narrow) ? (step >> 1) | step))
    // The old destination word, whose bits outside the result are kept (norm:vreg_mask_tail_agn, P3)
    val readAccumulator = beatMul generate (command.multiply && (command.mulOp === VectorElementOp.MACC ||
      command.mulOp === VectorElementOp.MADD || command.mulOp === VectorElementOp.WMACC))
    requests(2).valid := candidateActive && (if (beatMul) maskMm || maskDst && offset === 0 || readAccumulator else maskMm || maskDst && offset === 0)
    requests(2).payload := word(command.vd, maskDst ? maskWord.resize(step.getWidth) | step)

    when(accepted(running)) {
      when(if (beatMul) !folding && advance else !folding) { step := step + 1 }
      if (beatMul) mulPass := mulSplit && !mulPass
      when(reduce && stepsDone) { folding := True }
      when(folding) { fold := fold + 1 }
      when(last) { running := False }
    }
  }

  // A new command overrides the end of the previous one in issue
  if (overlap) startCommand()
  if (overlap) io.ready := (if (beatCompress) !running || issue.last && !command.compress else !running || issue.last)
  // Multiply steps write at issue + 4, ordinary ones at issue + 3: one idle issue cycle after a multiply keeps
  // every write, completion and progress token in issue order.
  val classOrder = classOverlap generate new Area {
    io.tailMultiply := running && command.multiply
    val multiplied = RegNext(accepted(running) && command.multiply) init(False)
    assert(!(io.cmd.valid && !io.cmd.multiply && running && command.multiply), "vector ordinary command during a multiply issue")
    assert(!(multiplied && accepted(running) && !command.multiply), "vector ordinary step right after a multiply step")
  }

  // SLIDE and SPLAT: step k reads the vs2 words holding source bytes 8k + D .. 8k + D + 7 on ports A and B, D the
  // signed byte distance; SPLAT reads the word of element idx in every step. Source bytes outside VLMAX elements read zero.
  val permute = slides generate new Area {
    val w = log2Up(p.vlen) + 1
    // The slide amount or index, clamped to VLMAX: any larger value acts like VLMAX (norm:vslidedown_op_src)
    val amount = Reg(UInt(w bits))
    val slide, splat, down, one = Reg(Bool()) init(False)
    when(io.cmd.valid) {
      val c = io.cmd.payload
      val width = xlen max w
      amount := (c.scalar.asUInt.resize(width) >= c.vlmax.resize(width)) ? c.vlmax | c.scalar.asUInt.resize(w)
      slide := c.shape === VectorShape.SLIDE
      splat := c.shape === VectorShape.SPLAT
      down := c.slide(0)
      one := c.slide(1)
    }
    val active = slide || splat
    val groupBytes = (command.vlmax.resize(w + 3) << command.sew).resize(w + 3)
    val distance = ((one ? U(1, w bits) | amount).resize(w + 3) << command.sew).resize(w + 3).asSInt.resize(w + 5)
    val stepBytes = (step.resize(w + 3) << byteBits).resize(w + 4).asSInt.resize(w + 5)
    val source = (splat ? S(0, w + 5 bits) | stepBytes) + ((down || splat) ? distance | -distance)
    val wordA = source >> byteBits
    val wordB = wordA + S(1, wordA.getWidth bits)
    val rotate = source(byteBits - 1 downto 0).asUInt
    def inGroup(byte: SInt): Bool = !byte.msb && byte.asUInt.resize(w + 5) < groupBytes.resize(w + 5)
    val readA = inGroup((wordA << byteBits).resize(w + 5))
    val readB = !splat && rotate =/= 0 && inGroup((wordB << byteBits).resize(w + 5))
    val zeros, inserts = Bits(wordBytes bits)
    for (j <- 0 until wordBytes) {
      val element = (((step.resize(log2Up(p.vlen) + byteBits) << byteBits) + j) >> command.sew).resize(command.vl.getWidth)
      zeros(j) := !inGroup(source + S(j, w + 5 bits))
      // vslide1up writes the scalar to element 0, vslide1down to element vl - 1 (norm:vslide1down-vx_op)
      inserts(j) := slide && one && (down ? (element + 1 === command.vl) | element === 0)
      // vslideup leaves the elements below OFFSET undisturbed (norm:vslideup_op)
      when(slide && !down && !one && element < amount) { issue.enables(j) := False }
    }
    when(active) {
      requests(0).valid := issue.candidateActive && readA
      requests(0).payload := word(command.vs2, wordA.asUInt.resize(step.getWidth))
      requests(1).valid := issue.candidateActive && readB
      requests(1).payload := word(command.vs2, wordB.asUInt.resize(step.getWidth))
    }
    // Reads stay inside the vs2 group
    val groupWords = ((groupBytes + (wordBytes - 1)) >> byteBits).resize(p.addressWidth + 1)
    for (port <- 0 to 1) assert(!(active && requests(port).valid &&
      (requests(port).payload - word(command.vs2, U(0, 1 bits))).resize(p.addressWidth + 1) >= groupWords),
      "vector permute read outside the vs2 group")
    val x1Active = RegNext(issue.active && active) init(False)
    val x1Splat = RegNext(splat)
    val x1Rotate = RegNext(rotate)
    val x1Zeros = RegNext(zeros)
    val x1Inserts = RegNext(inserts)
  }

  // Adder, logic, overflow flags, min/max/merge select and extension on the read data
  val x1 = new Area {
    val command = stage(VectorArithmetic.this.command)
    val flags = stage(VectorArithmetic.this.flags)
    val (reduce, wreduce, wideVv, widening, narrow) = (flags.reduce, flags.wreduce, flags.wideVv, flags.widening, flags.narrow)
    val (ext2, ext4, ext8, whole, maskScan) = (flags.ext2, flags.ext4, flags.ext8, flags.whole, flags.maskScan)
    val (scalarRead, subtract, average, laneSew) = (flags.scalarRead, flags.subtract, flags.average, flags.laneSew)
    val begin = overlap generate (RegNext(accepted(running) && step === 0 && !folding) init(False))
    val valid = RegNext(if (beatMul) issue.active && !VectorArithmetic.this.command.multiply else issue.active) init(False)
    val last = RegNext(if (beatMul) issue.last && !VectorArithmetic.this.command.multiply else issue.last) init(False)
    // Registered from issue, with the issuing command
    val address = RegNext(word(VectorArithmetic.this.command.vd, VectorArithmetic.this.maskDst ? issue.maskWord.resize(step.getWidth) |
      (VectorArithmetic.this.reduce ? U(0, step.getWidth bits) | issue.dstWord)))
    val mask = RegNext(issue.enables)
    val selects = RegNext(issue.selects)
    val part = RegNext(step(log2Up(elen / 16) downto 0))
    val position = RegNext(step)
    val offset = RegNext(issue.offset)
    val wordLast = RegNext(issue.wordLast)
    val bodyBits = RegNext(issue.bodyBits)
    val first = RegNext(step === 0 && !folding)
    val folds = RegNext(folding)
    val foldIndex = RegNext(fold)
    val old = io.reads(2).rsp

    // A 64-bit lane takes the scalar sign-extended from XLEN (norm:vop-vx_rs1_sext_sewbits)
    val scalarBeat = if (elen == 64) command.sew.mux(
      U(0) -> (command.scalar(7 downto 0) #* wordBytes),
      U(1) -> (command.scalar(15 downto 0) #* (wordBytes / 2)),
      U(2) -> (command.scalar(31 downto 0) #* (wordBytes / 4)),
      default -> rep(command.scalar.asSInt.resize(64).asBits, subWords)
    ) else command.sew.mux(
      U(0) -> (command.scalar(7 downto 0) #* wordBytes),
      U(1) -> (command.scalar(15 downto 0) #* (wordBytes / 2)),
      default -> (command.scalar(31 downto 0) #* (wordBytes / 4))
    )
    // Sign or zero extension of the elements in half part(0) of a word to twice their width
    def extend(source: Bits, fromBytes: Int, toBytes: Int): Bits = {
      val out = Bits(p.dlen bits)
      for (j <- 0 until wordBytes) {
        val (e, k) = (j / toBytes, j % toBytes)
        out(j * 8, 8 bits) := (if (k < fromBytes) source((e * fromBytes + k) * 8, 8 bits)
          else B(8 bits, default -> (command.signed && source((e + 1) * fromBytes * 8 - 1))))
      }
      out
    }
    def widen(v: Bits): Bits = {
      val half = v.subdivideIn(p.dlen / 2 bits)(part(0 downto 0))
      if (elen == 64) command.sew.mux(U(0) -> extend(half, 1, 2), U(1) -> extend(half, 2, 4), default -> extend(half, 4, 8))
      else (command.sew === 0) ? extend(half, 1, 2) | extend(half, 2, 4)
    }
    // Reduction identity of every lane: 0 for sum, or, xor and maxu, all ones for and and minu, INT_MAX for min,
    // INT_MIN for max (norm:vredsum_vredmaxu_vredmax_vredminu_vredmin_vredand_vredor_vredxor_op)
    val identity = Bits(p.dlen bits)
    for (j <- 0 until wordBytes) {
      val top = topOf(j, laneSew) === U(j, byteBits bits)
      identity(j * 8, 8 bits) := command.op.mux(
        U(VectorAluOp.AND) -> B"xff",
        U(VectorAluOp.MIN) -> (command.signed ? (top ? B"x7f" | B"xff") | B"xff"),
        U(VectorAluOp.MAX) -> ((command.signed && top) ? B"x80" | B"x00"),
        default -> B"x00"
      )
    }
    // Reductions: a = the active elements of the step (identity elsewhere, norm:vreduction_scalar_disregard_maskval),
    // or the upper part of the accumulator during folds; b = the accumulator, at step 0 vs1[0] and identities
    val partial = Reg(Bits(p.dlen bits))
    val elements = wreduce ? widen(io.reads(0).rsp) | io.reads(0).rsp
    // Fold k moves the upper half of the remaining dlen >> k accumulator bits onto the lower half
    val halves = (3 until maskBits).reverse.map(1 << _)
    val upperPart = foldIndex.mux(halves.init.zipWithIndex.map { case (h, k) => U(k) -> (partial |>> h) } :+
      (default -> (partial |>> halves.last)): _*)
    val reduceA, reduceB = Bits(p.dlen bits)
    for (j <- 0 until wordBytes) {
      reduceA(j * 8, 8 bits) := folds ? upperPart(j * 8, 8 bits) | (mask(j) ? elements(j * 8, 8 bits) | identity(j * 8, 8 bits))
      val scalarByte = (U(j, byteBits bits) >> laneSew) === 0
      reduceB(j * 8, 8 bits) := first ? (scalarByte ? io.reads(1).rsp(j * 8, 8 bits) | identity(j * 8, 8 bits)) | partial(j * 8, 8 bits)
    }
    // Widening extends the SEW operands to 2*SEW (norm:vwaddu_vwadd_vwsubu_vwsub_op); narrowing places each
    // SEW shift amount in the low bits of its 2*SEW lane
    val a = reduce ? reduceA | (wideVv ? widen(io.reads(0).rsp) | io.reads(0).rsp)
    val narrowB = command.scalarOperand ? scalarBeat | io.reads(1).rsp
    val b = reduce ? reduceB | ((widening || narrow) ? widen(narrowB) | narrowB)
    val left = flags.rsub ? b | a
    val right = flags.rsub ? a | b
    val rhs = right ^ B(p.dlen bits, default -> subtract)
    // Carry-in, or borrow-in for subtraction, of each lane from v0 (norm:vadc_vsbc_op)
    val carryIn = Bits(wordBytes bits)
    for (j <- 0 until wordBytes) carryIn(j) := subtract ^ (command.maskData && selects(j))
    val (sum, carries) = laneAdd(left, rhs, carryIn, laneSew)
    // Per byte, meaningful at the most significant byte of each element: the result does not fit SEW bits
    val overflow = Bits(wordBytes bits)
    // Unsigned or signed vs2 < vs1, meaningful at the most significant byte of each element
    val less = Bits(wordBytes bits)
    // Bit SEW of the exact sum or difference, meaningful at the most significant byte of each element
    val upper = Bits(wordBytes bits)
    for (j <- 0 until wordBytes) {
      val leftSign = left(j * 8 + 7)
      val rhsSign = rhs(j * 8 + 7)
      val sign = sum(j * 8 + 7)
      overflow(j) := command.signed ? (leftSign === rhsSign && sign =/= leftSign) | (carries(j) ^ subtract)
      less(j) := command.signed ? (sign ^ overflow(j)) | overflow(j)
      upper(j) := command.signed ? (leftSign ^ rhsSign ^ carries(j)) | (carries(j) ^ subtract)
    }
    // Averaging shifts every element right by one; a 64-bit lane reads the amount of its low word
    def perSubWord(hex: String): Bits = B(BigInt(hex * subWords, 16), p.dlen bits)
    val one = command.sew.mux(U(0) -> perSubWord("0101010101010101"),
      U(1) -> perSubWord("0001000100010001"), default -> perSubWord("0000000100000001"))
    val amounts = average ? one | b
    val selected = Bits(p.dlen bits)
    for (j <- 0 until wordBytes) {
      val takeA = flags.min === less(topOf(j, laneSew))
      val takeB = flags.merge ? selects(j) | !takeA
      selected(j * 8, 8 bits) := takeB ? b(j * 8, 8 bits) | a(j * 8, 8 bits)
    }

    // One result bit per element of the word, element e in bit e (norm:vmseq_vmsne_vmsltu_vmslt_vmsleu_vmsle_vmsgtu_vmsgt_op)
    val equal = Bits(wordBytes bits)
    for (j <- 0 until wordBytes) equal(j) := a(j * 8, 8 bits) === b(j * 8, 8 bits)
    val outcome = Bits(wordBytes bits) // meaningful at the most significant byte of each element
    for (j <- 0 until wordBytes) {
      val same = if (elen == 64) command.sew.mux(U(0) -> equal(j), U(1) -> equal(j & ~1, 2 bits).andR, U(2) -> equal(j & ~3, 4 bits).andR,
        default -> (if (subWords == 1) equal.andR else equal(j & ~7, 8 bits).andR))
      else command.sew.mux(U(0) -> equal(j), U(1) -> equal(j & ~1, 2 bits).andR, default -> equal(j & ~3, 4 bits).andR)
      outcome(j) := command.op.mux(
        U(VectorAluOp.SEQ) -> same,
        U(VectorAluOp.SNE) -> !same,
        U(VectorAluOp.SLT) -> less(j),
        U(VectorAluOp.SLE) -> (less(j) || same),
        U(VectorAluOp.SGT) -> !(less(j) || same),
        default -> overflow(j) // unsigned carry or borrow out (norm:vmadc_vmsbc_op_masked, norm:vmsbc_borrow_neg)
      )
    }
    val bits, actives = Bits(wordBytes bits)
    for (e <- 0 until wordBytes) {
      def at(f: Bits, s: Int, top: Boolean): Bool = if (e < (wordBytes >> s)) f((e << s) + (if (top) (1 << s) - 1 else 0)) else False
      if (elen == 64) {
        bits(e) := command.sew.mux(U(0) -> at(outcome, 0, true), U(1) -> at(outcome, 1, true), U(2) -> at(outcome, 2, true),
          default -> at(outcome, 3, true))
        actives(e) := command.sew.mux(U(0) -> at(mask, 0, false), U(1) -> at(mask, 1, false), U(2) -> at(mask, 2, false),
          default -> at(mask, 3, false))
      } else {
        bits(e) := command.sew.mux(U(0) -> at(outcome, 0, true), U(1) -> at(outcome, 1, true), default -> at(outcome, 2, true))
        actives(e) := command.sew.mux(U(0) -> at(mask, 0, false), U(1) -> at(mask, 1, false), default -> at(mask, 2, false))
      }
    }

    // Sign or zero extension of the selected part of vs2 (norm:vsext_vzext_op)
    val source = if (elen == 64) ext8 ? a.subdivideIn(p.dlen / 8 bits)(part).resize(p.dlen / 2) |
        (ext4 ? a.subdivideIn(p.dlen / 4 bits)(part(1 downto 0)).resize(p.dlen / 2) | a.subdivideIn(p.dlen / 2 bits)(part(0 downto 0)))
      else ext4 ? a.subdivideIn(p.dlen / 4 bits)(part).resize(p.dlen / 2) | a.subdivideIn(p.dlen / 2 bits)(part(0 downto 0))
    val extended = if (elen == 64) command.sew.mux(
      U(1) -> extend(source, 1, 2),
      U(2) -> (ext4 ? extend(source, 1, 4) | extend(source, 2, 4)),
      default -> (ext8 ? extend(source, 1, 8) | (ext4 ? extend(source, 2, 8) | extend(source, 4, 8))))
      else (command.sew === 1) ? extend(source, 1, 2) | (ext4 ? extend(source, 1, 4) | extend(source, 2, 4))
    // Element indices of the word, truncated to SEW (norm:vid_op_overflow)
    def indices(bytes: Int): Bits = if (bytes == wordBytes) position.resize(64).asBits
      else Cat((0 until wordBytes / bytes).map(e => (position @@ U(e, log2Up(wordBytes / bytes) bits)).resize(bytes * 8)))
    val ids = if (elen == 64) command.sew.mux(U(0) -> indices(1), U(1) -> indices(2), U(2) -> indices(4), default -> indices(8))
      else command.sew.mux(U(0) -> indices(1), U(1) -> indices(2), default -> indices(4))
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
      U(VectorAluOp.SSRL) -> a,
      U(VectorAluOp.INDEX) -> ids,
      default -> sum
    )
    // SLIDE and SPLAT: bytes rotate .. rotate + 7 of {B, A}, out-of-range bytes zero, then the scalar or the replicated element
    val permuted = slides generate {
      val rotated = ((io.reads(1).rsp ## io.reads(0).rsp) >> (permute.x1Rotate @@ U(0, 3 bits))).resize(p.dlen)
      val clean = Bits(p.dlen bits)
      for (j <- 0 until wordBytes) clean(j * 8, 8 bits) := permute.x1Inserts(j) ? scalarBeat(j * 8, 8 bits) |
        (permute.x1Zeros(j) ? B(0, 8 bits) | rotated(j * 8, 8 bits))
      val splatted = if (elen == 64) command.sew.mux(U(0) -> (clean(7 downto 0) #* wordBytes), U(1) -> (clean(15 downto 0) #* (wordBytes / 2)),
        U(2) -> (clean(31 downto 0) #* (wordBytes / 4)), default -> (if (subWords == 1) clean else clean(63 downto 0) #* subWords))
        else command.sew.mux(U(0) -> (clean(7 downto 0) #* wordBytes), U(1) -> (clean(15 downto 0) #* (wordBytes / 2)),
          default -> (clean(31 downto 0) #* (wordBytes / 4)))
      permute.x1Splat ? splatted | clean
    }
    // Mask prefix operations: one recurrence per mask word, counting only active source bits
    val scanFound = Reg(Bool())
    val scanEn = Bits(p.dlen bits)
    val maskWord = (io.v0 >> (position << maskBits)).resize(p.dlen)
    for (i <- 0 until p.dlen) scanEn(i) := U(i, maskBits + 1 bits) < bodyBits && (command.unmasked || maskWord(i))
    val scanSource = a & scanEn
    val before = (scanSource.asUInt - 1).asBits
    val scanBits = command.op.mux(
      U(VectorAluOp.MSBF) -> (before & ~scanSource),
      U(VectorAluOp.MSIF) -> (before ^ scanSource),
      default -> (scanSource & ~before)
    )
    val scanOutput = (!first && scanFound) ? B(0, p.dlen bits) | scanBits
    when(valid && maskScan) { scanFound := (!first && scanFound) || scanSource.orR }
    val output = {
      // vcompress passes the raw vs2 word on to X2
      val raw = if (beatCompress) whole || command.compress else whole
      val base = maskScan ? scanOutput | (raw ? a | ((ext2 || ext4 || ext8) ? extended | data))
      if (slides) permute.x1Active ? permuted | base else base
    }
    when(valid && reduce) { partial := data }
    val negative = Bits(wordBytes bits)
    for (j <- 0 until wordBytes) negative(j) := a(j * 8 + 7)

    when(valid && scalarRead) {
      // A 64-bit element gives its low XLEN bits (norm:vmv-x-s_op)
      result := (if (elen == 64) command.sew.mux(
        U(0) -> a(7 downto 0).asSInt.resize(xlen).asBits,
        U(1) -> a(15 downto 0).asSInt.resize(xlen).asBits,
        U(2) -> a(31 downto 0).asSInt.resize(xlen).asBits,
        default -> a(xlen - 1 downto 0)
      ) else command.sew.mux(
        U(0) -> a(7 downto 0).asSInt.resize(xlen).asBits,
        U(1) -> a(15 downto 0).asSInt.resize(xlen).asBits,
        default -> a(31 downto 0).asSInt.resize(xlen).asBits
      ))
    }
  }

  val multiplier = beatMul generate new Area {
    val core = new VectorBeatMultiplier(p.dlen, elen)
    core.io.valid := RegNext(accepted(running) && command.multiply) init(False)
    core.io.pass := RegNext(mulPass)
    core.io.sew := x1.command.sew
    core.io.wide := x1.flags.widening
    core.io.half := x1.part(0)
    core.io.op := x1.command.mulOp
    core.io.signedA := x1.command.signed
    core.io.signedB := x1.command.signedVs1
    core.io.negate := x1.command.negate
    core.io.vxrm := x1.command.vxrm
    core.io.a := io.reads(0).rsp
    core.io.b := x1.command.scalarOperand ? x1.scalarBeat | io.reads(1).rsp
    core.io.c := io.reads(2).rsp
    val address = RegNext(RegNext(x1.address))
    val mask = RegNext(RegNext(x1.mask))
    val last = RegNext(RegNext(RegNext(issue.last && command.multiply) init(False)) init(False)) init(False)
    val begin = RegNext(RegNext(RegNext(accepted(running) && command.multiply && step === 0 && issue.advance) init(False)) init(False)) init(False)
    val tag = overlap generate RegNext(RegNext(RegNext(mulIssueTag)))
    val saturating = core.io.outputValid && (core.io.saturated & mask).orR
    val seen = RegInit(False)
    val sofar = !begin && seen || saturating
    when(core.io.outputValid) { seen := sofar }
    val total = RegNextWhen(sofar, last) init(False)
    if (!overlap) when(saturating) { saturated := True }
    assert(!(last && !core.io.outputValid), "vector multiply completed before its final pass")
  }

  // Shifts with rounding (norm:vssrl_vssra_op), saturation (norm:vsaddu_vsadd_vssubu_vssub_op_overflow_vxsat_op_vsaddsub), mask merging
  val x2 = new Area {
    val command = stage(x1.command)
    val flags = stage(x1.flags)
    val (average, rounding, shift, laneSew, subtract) = (flags.average, flags.rounding, flags.shift, flags.laneSew, flags.subtract)
    val (narrow, maskDst, maskMm, maskScan) = (flags.narrow, flags.maskDst, flags.maskMm, flags.maskScan)
    val (scalarRead, reduce) = (flags.scalarRead, flags.reduce)
    val begin = overlap generate (RegNext(x1.begin) init(False))
    val valid = RegNext(x1.valid) init(False)
    val last = RegNext(x1.last) init(False)
    val address = RegNext(x1.address)
    val mask = RegNext(x1.mask)
    val offset = RegNext(x1.offset)
    val wordLast = RegNext(x1.wordLast)
    val bodyBits = RegNext(x1.bodyBits)
    val scanEn = RegNext(x1.scanEn)
    val old = RegNext(x1.old)
    val bits = RegNext(x1.bits)
    val actives = RegNext(x1.actives)
    val data = RegNext(x1.output)
    val amounts = RegNext(x1.amounts)
    val overflow = RegNext(x1.overflow)
    val upper = RegNext(x1.upper)
    val negative = RegNext(x1.negative)

    // Segmented shifter, one stage per amount bit; only the low lg2 of the lane width count
    // (norm:vsll_vsrl_vsra_shamt, norm:vnsrl_vnsra_shamt, norm:vnclipu_vnclip_shamt)
    val left = flags.left
    def lane(i: Int, w: Int) = i - i % w
    // Averaging shifts in bit SEW of the exact result (norm:vaaddu_vaadd_vasubu_vasub_op)
    val fill = Bits(p.dlen bits)
    for (i <- 0 until p.dlen) fill(i) := average ? upper(topOf(i / 8, laneSew)) | command.signed && (if (elen == 64) laneSew.mux(
      U(0) -> data(lane(i, 8) + 7), U(1) -> data(lane(i, 16) + 15), U(2) -> data(lane(i, 32) + 31), default -> data(lane(i, 64) + 63))
      else laneSew.mux(U(0) -> data(lane(i, 8) + 7), U(1) -> data(lane(i, 16) + 15), default -> data(lane(i, 32) + 31)))
    val stages = ArrayBuffer(data)
    for (k <- 0 until log2Up(elen)) {
      val s = 1 << k
      val in = stages.last
      val out = Bits(p.dlen bits)
      for (i <- 0 until p.dlen) {
        def bit(w: Int): Bool = {
          val pos = i % w
          if (s >= w) in(i)
          else {
            val l = if (pos >= s) in(i - s) else False
            val r = if (pos + s < w) in(i + s) else fill(i)
            amounts(lane(i, w) + k) ? (left ? l | r) | in(i)
          }
        }
        out(i) := (if (elen == 64) laneSew.mux(U(0) -> bit(8), U(1) -> bit(16), U(2) -> bit(32), default -> bit(64))
          else laneSew.mux(U(0) -> bit(8), U(1) -> bit(16), default -> bit(32)))
      }
      stages += out
    }
    // Rounding increment of each element from the bits the shift drops, added at the element's least significant byte
    def increments(w: Int): Bits = {
      val out = B(0, wordBytes bits)
      for (l <- 0 until p.dlen / w) {
        val d = amounts(l * w, log2Up(w) bits).asUInt
        val v = data(l * w, w bits)
        val lsb, half, sticky = Bits(w bits)
        for (i <- 0 until w) {
          lsb(i) := v(i) && d === i
          half(i) := (if (i + 1 < w) v(i) && d === i + 1 else False)
          sticky(i) := (if (i + 2 < w) v(i) && d > i + 1 else False)
        }
        out(l * w / 8) := VectorFixedPoint.roundIncrement(lsb.orR, half.orR, sticky.orR, command.vxrm)
      }
      out
    }
    val increment = rounding ? (if (elen == 64) laneSew.mux(U(0) -> increments(8), U(1) -> increments(16), U(2) -> increments(32),
      default -> increments(64)) else laneSew.mux(U(0) -> increments(8), U(1) -> increments(16), default -> increments(32))) | B(0, wordBytes bits)
    val rounded = laneAdd(stages.last, B(0, p.dlen bits), increment, laneSew)._1
    val base = shift ? rounded | data

    val clipped = Bits(wordBytes bits)
    val value = Bits(p.dlen bits)
    for (j <- 0 until wordBytes) {
      val top = topOf(j, laneSew)
      val upper = top === U(j, byteBits bits)
      val signedLimit = negative(top) ? (upper ? B"x80" | B"x00") | (upper ? B"x7f" | B"xff")
      clipped(j) := command.saturate && overflow(top)
      value(j * 8, 8 bits) := clipped(j) ? (command.signed ? signedLimit | (subtract ? B"x00" | B"xff")) | base(j * 8, 8 bits)
    }
    // NARROW: the low SEW bits of each rounded 2*SEW lane, or the SEW limit when vnclip(u) saturates
    // (norm:vnclipu_overflow, norm:vnclip_overflow); clips holds the saturation of each packed byte
    def pack(w: Int): (Bits, Bits) = {
      val packed = Bits(p.dlen / 2 bits)
      val clips = Bits(wordBytes / 2 bits)
      for (l <- 0 until p.dlen / 2 / w) {
        val v = rounded(l * 2 * w, 2 * w bits)
        val high = v(2 * w - 1 downto w - 1)
        val clip = command.saturate && (command.signed ? !(high.andR || !high.orR) | high(w downto 1).orR)
        val limit = command.signed ? (v.msb ## B(w - 1 bits, default -> !v.msb)) | B(w bits, default -> True)
        packed(l * w, w bits) := clip ? limit | v(w - 1 downto 0)
        for (k <- 0 until w / 8) clips(l * w / 8 + k) := clip
      }
      (packed, clips)
    }
    val (packed8, clips8) = pack(8)
    val (packed16, clips16) = pack(16)
    val (packed, narrowClips) = if (elen == 64) {
      val (packed32, clips32) = pack(32)
      (command.sew.mux(U(0) -> packed8, U(1) -> packed16, default -> packed32),
        command.sew.mux(U(0) -> clips8, U(1) -> clips16, default -> clips32))
    } else ((command.sew === 0) ? packed8 | packed16, (command.sew === 0) ? clips8 | clips16)
    // Only active elements set vxsat; their bytes are the enabled ones, in the written half for narrowing
    def saturating = valid && ((narrow ? (narrowClips ## narrowClips) | clipped) & mask).orR
    if (!overlap) when(saturating) { saturated := True }
    // Per command: the saturation of its steps so far, and of the whole command at its last step
    val sticky = overlap generate new Area {
      val seen = RegInit(False)
      val sofar = !begin && seen || saturating
      seen := sofar
      val total = RegNextWhen(sofar, last) init(False)
    }

    // MASK_DST: the accumulator starts from the old word at a word's first step and takes active bits only
    val accumulator = Reg(Bits(p.dlen bits))
    val start = (offset === 0) ? old | accumulator
    val placed = (bits.resize(p.dlen) << offset).resize(p.dlen)
    val enabled = (actives.resize(p.dlen) << offset).resize(p.dlen)
    val gathered = (start & ~enabled) | (placed & enabled)
    when(valid && maskDst) { accumulator := gathered }
    // MASK_MM: result bits below vl, the old word above
    val body = Bits(p.dlen bits)
    for (i <- 0 until p.dlen) body(i) := U(i, maskBits + 1 bits) < bodyBits
    val maskEnable = maskScan ? scanEn | body
    val merged = (data & maskEnable) | (old & ~maskEnable)
    val output = maskDst ? gathered | (maskMm ? merged | (narrow ? (packed ## packed) | value))
  }

  // vcompress.vm, one vs2 word per step (norm:vcompress_op): X1 selects the elements from the vs1 mask word, X2 places
  // them at the fill position of a one-word circular buffer and writes vd word by word; the tail stays undisturbed
  val packTags = beatCompress && overlap generate new Area {
    val issue = RegNextWhen(io.tag, io.cmd.valid) init(0)
    val x1 = RegNext(issue) init(0)
    val x2 = RegNext(x1) init(0)
  }
  val pack = beatCompress generate new Area {
    val active = command.compress
    val issueCompress = active
    val x1Compress = x1.command.compress
    val x2Compress = x2.command.compress
    val token = overlap generate (x2Compress && (x2.valid || x2.last))
    val context = if (overlap) x2.command else command
    // Port B reads the vs1 mask word holding the step's elements
    when(issueCompress) { requests(1).payload := word(command.vs1, ((step >> 3) >> command.sew).resize(step.getWidth)) }

    val mask = io.reads(1).rsp
    // The vs1 mask bits of the step's wordBytes >> sew elements
    def windowOf(sew: Int): Bits = {
      val n = wordBytes >> sew
      if (n == 1) mask(x1.offset).asBits.resize(wordBytes)
      else {
        val w = mask.subdivideIn(n bits)(x1.offset(maskBits - 1 downto log2Up(n)))
        if (n == wordBytes) w else w.resize(wordBytes)
      }
    }
    val windowSew = if (overlap) x1.command.sew else command.sew
    val window = if (elen == 64) windowSew.mux(U(0) -> windowOf(0), U(1) -> windowOf(1), U(2) -> windowOf(2), default -> windowOf(3))
      else windowSew.mux(U(0) -> windowOf(0), U(1) -> windowOf(1), default -> windowOf(2))
    // Selected elements of the word: mask bit set and below vl
    val selected = window & x1.actives
    val count = CountOne(selected).resize(byteBits + 1)
    // sources(s): the element index of the (s+1)-th selected element
    val sources = Vec(UInt(byteBits bits), wordBytes)
    for (s <- 0 until wordBytes) sources(s) := (0 until wordBytes).map(e => {
      val rank = if (e == 0) U(0, byteBits + 1 bits) else CountOne(selected(e - 1 downto 0)).resize(byteBits + 1)
      (selected(e) && rank === s) ? U(e, byteBits bits) | U(0, byteBits bits)
    }).reduce(_ | _)
    val x2Count = RegNext(count)
    val x2Sources = RegNext(sources)
    val x2Position = RegNext(x1.position)

    // fill: pending bytes below it in buffer; out: the vd word they belong to
    val fill = Reg(UInt(byteBits bits)) init(0)
    val buffer = Reg(Bits(p.dlen bits))
    val out = Reg(UInt(step.getWidth bits)) init(0)
    val flushPending = RegInit(False)
    // The first accepted token calculates from empty state, even after an older residual word.
    val first = overlap generate (token && x2.begin)
    val effectiveFill = if (overlap) first ? U(0, byteBits bits) | fill else fill
    val effectiveOut = if (overlap) first ? U(0, step.getWidth bits) | out else out
    val bytes = (x2Count << context.sew).resize(byteBits + 1)
    val total = effectiveFill.resize(byteBits + 1) + bytes
    val complete = total(byteBits)
    val nextFill = total(byteBits - 1 downto 0)
    val merged, nextBuffer = Bits(p.dlen bits)
    val belowFill, belowNext = Bits(wordBytes bits)
    for (j <- 0 until wordBytes) {
      val distance = U(j, byteBits bits) - effectiveFill
      val slot = distance >> context.sew
      val byteInElement = U(j, byteBits bits) & ~(U(wordBytes - 1, byteBits bits) << context.sew).resize(byteBits)
      val sourceByte = ((x2Sources(slot) << context.sew).resize(byteBits) | byteInElement)
      val newByte = x2.data.subdivideIn(8 bits)(sourceByte)
      val hit = distance.resize(byteBits + 1) < bytes
      belowFill(j) := U(j, byteBits bits) < effectiveFill
      belowNext(j) := U(j, byteBits bits) < nextFill
      merged(j * 8, 8 bits) := belowFill(j) ? buffer(j * 8, 8 bits) | newByte
      nextBuffer(j * 8, 8 bits) := hit ? newByte | buffer(j * 8, 8 bits)
    }
    val stepValid = x2.valid && (if (overlap) x2Compress else active)
    // The last step writes the partial word unless it completes a word with bytes left over: those take one more cycle
    val extraFlush = stepValid && x2.last && complete && nextFill =/= 0
    val write = flushPending || stepValid && (complete || x2.last && nextFill =/= 0)
    val residualMask = if (overlap) Vec((0 until wordBytes).map(j => U(j, byteBits bits) < fill)).asBits else belowFill
    val writeMask = flushPending ? residualMask | (complete ? ones(wordBytes) | belowNext)
    val writeData = flushPending ? buffer | merged
    val groupWords = if (overlap) (((context.vlmax.resize(log2Up(p.vlen) + 4) << context.sew) + (wordBytes - 1)) >> byteBits).resize(p.addressWidth + 1)
      else permute.groupWords
    val flushAddress = overlap generate Reg(UInt(p.addressWidth bits))
    val flushBase = overlap generate Reg(UInt(p.addressWidth bits))
    val flushExtent = overlap generate Reg(UInt(p.addressWidth + 1 bits))
    val flushTag = overlap generate Reg(UInt(tagWidth bits))
    val writeAddress = if (overlap) flushPending ? flushAddress | word(context.vd, effectiveOut) else word(command.vd, out)
    val tag = overlap generate (flushPending ? flushTag | packTags.x2)
    val last = if (overlap) flushPending || token && x2.last && !extraFlush
      else flushPending || x2.last && !extraFlush
    when(stepValid) {
      fill := nextFill
      buffer := nextBuffer
      out := effectiveOut + U(complete)
    }
    flushPending := extraFlush
    if (overlap) {
      when(first && !stepValid) { fill := 0; out := 0 }
      when(extraFlush) {
        flushAddress := word(context.vd, effectiveOut + 1)
        flushBase := word(context.vd, U(0, 1 bits))
        flushExtent := groupWords
        flushTag := packTags.x2
      }
      val owner = Reg(UInt(tagWidth bits))
      val packing = RegInit(False)
      when(first) { owner := packTags.x2; packing := !x2.last }
      when(token && x2.last) { packing := False }
      assert(!(token && !first && (!packing || owner =/= packTags.x2)), "vector compress token without its first token")
      assert(!(first && (packing || flushPending)), "vector compress starts before its owner drains")
      assert(!(flushPending && (x2.valid || x2.last)), "vector compress flush with an X2 token")
      assert(!(flushPending && flushTag =/= owner), "vector compress flush lost its owner")
      assert(!(flushPending && (writeAddress - flushBase).resize(p.addressWidth + 1) >= flushExtent), "vector compress flush outside destination")
      val reserved = RegNext(accepted(running) && issueCompress && issue.last) init(False)
      assert(!(running && issueCompress && issue.last && io.ready), "vector compress final issue admits a command")
      assert(!(reserved && running), "vector compress reserved issue slot occupied")
    } else {
      when(io.cmd.valid) { fill := 0; out := 0; flushPending := False }
      assert(!(flushPending && x2.valid), "vector compress flush with a step in X2")
    }
    assert(!(stepValid && effectiveOut > x2Position), "vector compress output word ahead of its step")
    assert(!((if (overlap) stepValid else active && write) && effectiveOut.resize(p.addressWidth + 1) >= groupWords), "vector compress write outside the vd group")
    val finished = !overlap generate RegInit(False)
    if (!overlap) {
      when(io.done) { finished := True }
      when(io.cmd.valid) { finished := False }
      assert(!(active && (io.done || finished) && io.write.valid), "vector compress write at or after done")
    }
  }

  val writeback = new Area {
    def packed[T <: Data](compressed: => T, other: T): T = if (beatCompress) pack.active ? compressed | other else other
    def multiplied[T <: Data](value: => T, other: T): T = if (beatMul) multiplier.core.io.outputValid ? value | other else other
    // Reductions write element 0 only; the rest of vd is tail (norm:vreduction_tail_policy)
    lazy val elementZero = Vec((0 until wordBytes).map(j => (U(j, byteBits bits) >> x2.laneSew) === 0)).asBits
    // Keep the legacy construction order when the complete-token path is disabled.
    if (beatCompress && overlap) elementZero.getBitsWidth
    val selected = beatCompress && overlap generate new Area {
      val alu = !x2.command.compress && (x2.valid || x2.last)
      val mul = if (beatMul) multiplier.core.io.outputValid || multiplier.last else False
      val compress = pack.token
      val flush = pack.flushPending
      assert(CountOne(flush ## compress ## alu ## mul) <= 1, "vector writeback producers collide")
      val packedToken = flush || compress
      val valid = multiplied(multiplier.mask.orR, packedToken ? pack.write | (alu && x2.valid && !x2.scalarRead && x2.wordLast))
      val last = if (beatMul) multiplier.last || (packedToken ? pack.last | (alu && x2.last))
        else packedToken ? pack.last | (alu && x2.last)
      val address = multiplied(multiplier.address, packedToken ? pack.writeAddress | x2.address)
      val mask = multiplied(multiplier.mask, packedToken ? pack.writeMask |
        ((x2.maskDst || x2.maskMm) ? ones(wordBytes) | (x2.reduce ? elementZero | x2.mask)))
      val data = multiplied(multiplier.core.io.result, packedToken ? pack.writeData | x2.output)
      val tag = multiplied(multiplier.tag, packedToken ? pack.tag | packTags.x2)
      val saturated = multiplied(multiplier.sofar, !packedToken && x2.sticky.sofar)
    }
    val valid = RegNext(if (beatCompress && overlap) selected.valid else multiplied(multiplier.mask.orR, packed(pack.write, x2.valid && !x2.scalarRead && x2.wordLast))) init(False)
    val last = RegNext(if (beatCompress && overlap) selected.last else if (beatMul) multiplier.last || packed(pack.last, x2.last) else packed(pack.last, x2.last)) init(False)
    val address = RegNext(if (beatCompress && overlap) selected.address else multiplied(multiplier.address, packed(pack.writeAddress, x2.address)))
    val mask = RegNext(if (beatCompress && overlap) selected.mask else multiplied(multiplier.mask, packed(pack.writeMask, (x2.maskDst || x2.maskMm) ? ones(wordBytes) | (x2.reduce ? elementZero | x2.mask))))
    val data = RegNext(if (beatCompress && overlap) selected.data else multiplied(multiplier.core.io.result, packed(pack.writeData, x2.output)))
    val tag = beatCompress && overlap generate (RegNext(selected.tag) init(0))
    val saturated = beatCompress && overlap generate (RegNext(selected.saturated) init(False))
    val multiply = beatMul generate (RegNext(multiplier.last) init(False))
    val mulTag = beatMul && overlap generate RegNext(multiplier.tag)
    if (beatMul) {
      val active = multiplier.core.io.outputValid
      assert(!(active && (x2.valid || x2.last)), "vector multiply and ALU pipelines collide")
    }
    io.write.valid := valid
    io.write.address := address
    io.write.mask := mask
    io.write.data := data
  }

  // The VRF holds every written word when done rises
  io.done := RegNext(writeback.last) init(False)
  if (!overlap) when(io.done) { busy := False }

  // Commands in the unit, from their command to their done; tags and saturation travel with the steps
  val lifecycle = overlap generate new Area {
    val inUnit = Reg(UInt(3 bits)) init(0)
    val inUnitNext = inUnit + U(io.cmd.valid).resized - U(io.done).resized
    inUnit := inUnitNext
    busy := inUnitNext =/= 0
    val issueTag = if (beatCompress) packTags.issue else if (beatMul) mulIssueTag else RegNextWhen(io.tag, io.cmd.valid) init(0)
    val x1Tag = RegNext(issueTag) init(0)
    val x2Tag = RegNext(x1Tag) init(0)
    val writeTag = RegNext(x2Tag) init(0)
    val selectedTag = if (beatCompress) writeback.tag else if (beatMul) writeback.multiply ? writeback.mulTag | writeTag else writeTag
    if (chaining) io.readTag := issueTag
    io.writeTag := (if (beatCompress) writeback.tag else if (beatMul) (RegNext(multiplier.core.io.outputValid) init(False)) ? writeback.mulTag | writeTag else writeTag)
    io.doneTag := RegNextWhen(selectedTag, writeback.last) init(0)
    io.saturated := RegNextWhen(if (beatCompress) writeback.saturated else if (beatMul) writeback.multiply ? multiplier.total | x2.sticky.total else x2.sticky.total, writeback.last) init(False)
    if (beatCompress) {
      val live = Vec.fill(1 << tagWidth)(RegInit(False))
      for (i <- 0 until (1 << tagWidth)) {
        when(io.done && io.doneTag === i) { live(i) := False }
        when(io.cmd.valid && io.tag === i) { live(i) := True }
      }
      assert(!(io.cmd.valid && live(io.tag)), "vector beat reused a live tag")
      assert(!(io.write.valid && !live(io.writeTag)), "vector beat write without a live owner")
      assert(!(io.done && !live(io.doneTag)), "vector beat completion without a live owner")
      assert(!(io.done && io.write.valid && io.writeTag === io.doneTag), "vector beat write at owner completion")
    }
    assert(!(inUnit === 0 && io.done), "vector beat unit done without a command")
    assert(!(io.cmd.valid && !io.ready), "vector beat command while the unit is not ready")
  }
  if (chaining) {
    for (i <- 0 until 3) {
      io.reads(i).cmd.valid := requests(i).valid && issueFire
      io.reads(i).cmd.payload := requests(i).payload
    }
    // A token also advances for disabled steps; narrowing releases only its final half.
    val finalWord = accepted(running) && !scalarRead && issue.wordLast &&
      (!narrow || step(0) || issue.last) && (if (beatCompress) !command.compress else True)
    val aluFinal = if (beatMul) finalWord && !command.multiply else finalWord
    val aluToken = RegNext(RegNext(RegNext(aluFinal) init(False)) init(False)) init(False)
    val mulToken = if (beatMul) RegNext(RegNext(RegNext(RegNext(finalWord && command.multiply && issue.advance) init(False)) init(False)) init(False)) init(False) else False
    io.progress.valid := aluToken || mulToken
    if (classOverlap) assert(!(aluToken && mulToken), "vector progress tokens of both latency classes")
    io.progress.address := writeback.address
    io.progress.tag := io.writeTag
    assert(!(running && !issueFire && (issue.last || io.reads.map(_.cmd.valid).orR)), "stalled vector step issued")
  }

}
