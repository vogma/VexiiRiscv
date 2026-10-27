package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class VectorArithmeticHarness(p: VectorRegFileParam, xlen: Int, elen: Int = 32) extends Component {
  val io = new Bundle {
    val cmd = slave(Flow(VectorArithmeticCmd(p, xlen)))
    val debug = in Bool()
    val access = slave(Flow(VrfWriteCmd(p)))
    val readAddress = in UInt(p.addressWidth bits)
    val readData = out Bits(64 bits)
    val initHold, done, busy, saturated = out Bool()
    val result = out Bits(xlen bits)
    val write = master(Flow(VrfWriteCmd(p)))
    val readValid = out Bits(3 bits)
    val readAddresses = out Vec(UInt(p.addressWidth bits), 3)
  }
  val vrf = new VectorRegFile(p)
  val arithmetic = new VectorArithmetic(p, xlen, elen)
  arithmetic.io.cmd := io.cmd
  arithmetic.io.v0 := vrf.io.v0
  vrf.io.write := arithmetic.io.write
  when(io.debug) { vrf.io.write := io.access }
  for (i <- 0 until 3) {
    vrf.io.reads(i).cmd := arithmetic.io.reads(i).cmd
    arithmetic.io.reads(i).rsp := vrf.io.reads(i).rsp
    io.readValid(i) := arithmetic.io.reads(i).cmd.valid
    io.readAddresses(i) := arithmetic.io.reads(i).cmd.payload
  }
  when(io.debug) {
    vrf.io.reads(0).cmd.valid := True
    vrf.io.reads(0).cmd.payload := io.readAddress
  }
  io.readData := vrf.io.reads(0).rsp
  io.initHold := vrf.io.initHold
  io.done := arithmetic.io.done
  io.busy := arithmetic.io.busy
  io.saturated := arithmetic.io.saturated
  io.result := arithmetic.io.result
  io.write := arithmetic.io.write
}

class VectorArithmeticTest extends AnyFunSuite {
  // name, ALU operation, saturating
  val ops = Seq(
    ("vadd", VectorAluOp.ADD, false), ("vsub", VectorAluOp.SUB, false), ("vrsub", VectorAluOp.RSUB, false),
    ("vand", VectorAluOp.AND, false), ("vor", VectorAluOp.OR, false), ("vxor", VectorAluOp.XOR, false),
    ("vmv", VectorAluOp.MOVE, false), ("vsaddu", VectorAluOp.ADD, true), ("vsadd", VectorAluOp.ADD, true),
    ("vssubu", VectorAluOp.SUB, true), ("vssub", VectorAluOp.SUB, true),
    ("vminu", VectorAluOp.MIN, false), ("vmin", VectorAluOp.MIN, false), ("vmaxu", VectorAluOp.MAX, false),
    ("vmax", VectorAluOp.MAX, false), ("vsll", VectorAluOp.SLL, false), ("vsrl", VectorAluOp.SRL, false),
    ("vsra", VectorAluOp.SRL, false), ("vadc", VectorAluOp.ADD, false), ("vsbc", VectorAluOp.SUB, false),
    ("vmerge", VectorAluOp.MERGE, false), ("vzext2", VectorAluOp.ADD, false), ("vsext2", VectorAluOp.ADD, false),
    ("vzext4", VectorAluOp.ADD, false), ("vsext4", VectorAluOp.ADD, false), ("vzext8", VectorAluOp.ADD, false),
    ("vsext8", VectorAluOp.ADD, false),
    ("vmseq", VectorAluOp.SEQ, false), ("vmsne", VectorAluOp.SNE, false), ("vmsltu", VectorAluOp.SLT, false),
    ("vmslt", VectorAluOp.SLT, false), ("vmsleu", VectorAluOp.SLE, false), ("vmsle", VectorAluOp.SLE, false),
    ("vmsgtu", VectorAluOp.SGT, false), ("vmsgt", VectorAluOp.SGT, false), ("vmadc", VectorAluOp.ADD, false),
    ("vmadcm", VectorAluOp.ADD, false), ("vmsbc", VectorAluOp.SUB, false), ("vmsbcm", VectorAluOp.SUB, false),
    ("vmand", VectorAluOp.AND, false), ("vmnand", VectorAluOp.NAND, false), ("vmandn", VectorAluOp.ANDN, false),
    ("vmxor", VectorAluOp.XOR, false), ("vmor", VectorAluOp.OR, false), ("vmnor", VectorAluOp.NOR, false),
    ("vmorn", VectorAluOp.ORN, false), ("vmxnor", VectorAluOp.XNOR, false),
    ("vaaddu", VectorAluOp.AADD, false), ("vaadd", VectorAluOp.AADD, false), ("vasubu", VectorAluOp.ASUB, false),
    ("vasub", VectorAluOp.ASUB, false), ("vssrl", VectorAluOp.SSRL, false), ("vssra", VectorAluOp.SSRL, false),
    ("vid", VectorAluOp.INDEX, false), ("vmv1r", VectorAluOp.ADD, false), ("vmv2r", VectorAluOp.ADD, false),
    ("vmv4r", VectorAluOp.ADD, false), ("vmv8r", VectorAluOp.ADD, false),
    ("vwaddu", VectorAluOp.ADD, false), ("vwadd", VectorAluOp.ADD, false), ("vwsubu", VectorAluOp.SUB, false),
    ("vwsub", VectorAluOp.SUB, false), ("vwadduw", VectorAluOp.ADD, false), ("vwaddw", VectorAluOp.ADD, false),
    ("vwsubuw", VectorAluOp.SUB, false), ("vwsubw", VectorAluOp.SUB, false), ("vnsrl", VectorAluOp.SRL, false),
    ("vnsra", VectorAluOp.SRL, false), ("vnclipu", VectorAluOp.SSRL, true), ("vnclip", VectorAluOp.SSRL, true),
    ("vredsum", VectorAluOp.ADD, false), ("vredand", VectorAluOp.AND, false), ("vredor", VectorAluOp.OR, false),
    ("vredxor", VectorAluOp.XOR, false), ("vredminu", VectorAluOp.MIN, false), ("vredmin", VectorAluOp.MIN, false),
    ("vredmaxu", VectorAluOp.MAX, false), ("vredmax", VectorAluOp.MAX, false), ("vwredsumu", VectorAluOp.ADD, false),
    ("vwredsum", VectorAluOp.ADD, false))
  val compares = Seq("vmseq", "vmsne", "vmsltu", "vmslt", "vmsleu", "vmsle", "vmsgtu", "vmsgt")
  val carries = Seq("vmadc", "vmadcm", "vmsbc", "vmsbcm")
  val logicals = Seq("vmand", "vmnand", "vmandn", "vmxor", "vmor", "vmnor", "vmorn", "vmxnor")
  val roundings = Seq("vaaddu", "vaadd", "vasubu", "vasub", "vssrl", "vssra", "vnclipu", "vnclip")
  val reductions = Seq("vredsum", "vredand", "vredor", "vredxor", "vredminu", "vredmin", "vredmaxu", "vredmax", "vwredsumu", "vwredsum")
  // 1: vd at 2*SEW from SEW sources, 2: vd and vs2 at 2*SEW, 3: vs2 at 2*SEW narrowed to SEW, 0: neither
  def wideKind(name: String) =
    if (reductions.contains(name)) 0
    else if (Seq("vwaddu", "vwadd", "vwsubu", "vwsub").contains(name)) 1
    else if (name.startsWith("vw")) 2 else if (name.startsWith("vn")) 3 else 0
  // Widening and narrowing need SEW <= ELEN/2 and LMUL <= 4 (norm:vwop_vd_eew_emul, norm:vnarrowing_eew_emul)
  // Widening reductions need SEW <= ELEN/2 at any LMUL (norm:vwredsum_op)
  def sizeOk(name: String, sew: Int, lmul: Int, elen: Int) = {
    val wideSew = log2Up(elen / 8) - 1
    if (name.startsWith("vwred")) sew <= wideSew else wideKind(name) == 0 || sew <= wideSew && lmul <= 2
  }
  // Operations whose signedness the command must carry; the others get a random value
  def isSigned(name: String) = Seq("vsadd", "vssub", "vmin", "vmax", "vsra", "vsext2", "vsext4", "vsext8", "vmslt", "vmsle", "vmsgt",
    "vaadd", "vasub", "vssra", "vwadd", "vwsub", "vwaddw", "vwsubw", "vnsra", "vnclip", "vredmin", "vredmax", "vwredsum").contains(name)
  def signedMatters(name: String) = isSigned(name) || carries.contains(name) ||
    Seq("vsaddu", "vssubu", "vminu", "vmaxu", "vsrl", "vzext2", "vzext4", "vzext8", "vmsltu", "vmsleu", "vmsgtu", "vaaddu", "vasubu",
      "vssrl", "vwaddu", "vwsubu", "vwadduw", "vwsubuw", "vnsrl", "vnclipu", "vredminu", "vredmaxu", "vwredsumu").contains(name)
  // v0 as carry or merge select: encoded masked, every body element written
  def withV0(name: String) = Seq("vadc", "vsbc", "vmerge", "vmadcm", "vmsbcm").contains(name)
  // Encoded unmasked
  def unmaskedOnly(name: String) = Seq("vmadc", "vmsbc").contains(name) || logicals.contains(name)
  // log2 of the extension factor, 0 for single width
  def extLog2(name: String) = if (name.endsWith("ext2")) 1 else if (name.endsWith("ext4")) 2 else if (name.endsWith("ext8")) 3 else 0
  // NREG of a whole-register move, 0 otherwise
  def nregOf(name: String) = if (name.matches("vmv[1248]r")) name(3).asDigit else 0
  def shapeOf(name: String) =
    if (nregOf(name) > 0) VectorShape.WHOLE
    else if (compares.contains(name) || carries.contains(name)) VectorShape.MASK_DST
    else if (logicals.contains(name)) VectorShape.MASK_MM
    else if (reductions.contains(name)) if (name.startsWith("vw")) VectorShape.WREDUCE else VectorShape.REDUCE
    else if (wideKind(name) != 0) Seq(VectorShape.WIDE_VV, VectorShape.WIDE_WV, VectorShape.NARROW)(wideKind(name) - 1)
    else extLog2(name) match {
      case 1 => VectorShape.EXT2; case 2 => VectorShape.EXT4; case 3 => VectorShape.EXT8; case _ => VectorShape.SINGLE
    }

  def run(vlen: Int, xlen: Int, elen: Int = 32): Unit = {
    val p = VectorRegFileParam(vlen = vlen)
    val maxSew = log2Up(elen / 8)
    SimConfig.compile(new VectorArithmeticHarness(p, xlen, elen)).doSim { dut =>
      val random = new Random(901 + xlen + vlen + (if (elen == 64) 64 else 0))
      val bytes = Array.fill(32, vlen / 8)(random.nextInt(256))
      val xmask = (BigInt(1) << xlen) - 1
      def word(reg: Int, offset: Int): BigInt =
        (0 until 8).map(b => BigInt(bytes(reg)(offset + b)) << (b * 8)).reduce(_ | _)
      def tick(): Unit = {
        dut.clockDomain.risingEdge(); sleep(5)
        dut.clockDomain.fallingEdge(); sleep(5)
      }
      dut.clockDomain.fallingEdge()
      dut.io.cmd.valid #= false
      dut.io.cmd.vd #= 0; dut.io.cmd.vs1 #= 0; dut.io.cmd.vs2 #= 0
      dut.io.cmd.vl #= 0; dut.io.cmd.vstart #= 0; dut.io.cmd.sew #= 0; dut.io.cmd.op #= 0
      dut.io.cmd.shape #= VectorShape.SINGLE; dut.io.cmd.vxrm #= 0; dut.io.cmd.nreg #= 1
      dut.io.cmd.scalar #= 0; dut.io.cmd.scalarOperand #= false; dut.io.cmd.unmasked #= true
      dut.io.cmd.signed #= false; dut.io.cmd.saturate #= false; dut.io.cmd.maskData #= false
      dut.io.debug #= true
      dut.io.access.valid #= false
      dut.io.access.address #= 0; dut.io.access.data #= 0; dut.io.access.mask #= 0
      dut.io.readAddress #= 0
      dut.clockDomain.deassertReset(); sleep(1)
      dut.clockDomain.assertReset(); sleep(1); tick()
      dut.clockDomain.deassertReset()
      for (_ <- 0 until p.wordCount) tick()
      assert(!dut.io.initHold.toBoolean)
      // Distinct data in every byte exposes addressing, overlap and lane carry errors.
      for (a <- 0 until p.wordCount) {
        dut.io.access.valid #= true
        dut.io.access.address #= a
        dut.io.access.mask #= 255
        dut.io.access.data #= word(a / p.beatsPerReg, (a % p.beatsPerReg) * 8)
        tick()
      }
      dut.io.access.valid #= false
      var cases = 0
      var writes = 0
      var sourceInVd = 0
      val saturations = scala.collection.mutable.Map[(String, Boolean), Int]().withDefaultValue(0)
      // Mask results seen per operation: active bits 0 and 1, and equal active operands of a compare
      val outcomes = scala.collection.mutable.Map[(String, String), Int]().withDefaultValue(0)
      // Rounding increments seen per operation and vxrm
      val increments = scala.collection.mutable.Map[(String, Int, Int), Int]().withDefaultValue(0)
      // Operations run with 64-bit destination or adder lanes
      val lanes64 = scala.collection.mutable.Set[String]()
      def execute(name: String, sew: Int, lmul: Int, vl: Int, start: Int, masked0: Boolean,
                  scalarOperand0: Boolean, shape0: Int = VectorShape.SINGLE, vxrm0: Int = -1): Unit = {
        val (_, op, saturate) = ops.find(_._1 == name).get
        val signed = if (signedMatters(name)) isSigned(name) else random.nextBoolean()
        val ext = extLog2(name)
        val shape = if (shapeOf(name) != VectorShape.SINGLE) shapeOf(name) else shape0
        val maskDst = shape == VectorShape.MASK_DST
        val maskMm = shape == VectorShape.MASK_MM
        val nreg = nregOf(name)
        val whole = nreg > 0
        val index = name == "vid"
        val masked = (masked0 || withV0(name)) && !unmaskedOnly(name)
        val scalarOperand = scalarOperand0 && ext == 0 && !maskMm && !whole && !index && !reductions.contains(name)
        val scalarRead = shape == VectorShape.SCALAR_READ
        val scalarWrite = shape == VectorShape.SCALAR_WRITE
        val width = 8 << sew
        val size = width / 8
        val mask = (BigInt(1) << width) - 1
        // Any aligned groups; same-EEW overlap is always legal, and vd = v0 only unmasked
        val wk = wideKind(name)
        if (sew == 3 || sew == 2 && (wk != 0 || name.startsWith("vwred"))) lanes64 += name
        val group = if (whole) nreg else 1 << scala.math.max(lmul, 0)
        def pick(): Int = random.nextInt(32 / group) * group
        // Widening and narrowing groups: SEW operands span narrowGroup, 2*SEW ones wideGroup (norm:vreg_overlap_legal)
        val narrowGroup = group
        val wideGroup = 1 << scala.math.max(lmul + 1, 0)
        def pickIn(g: Int): Int = random.nextInt(32 / g) * g
        def disjoint(a: Int, ga: Int, b: Int, gb: Int) = a + ga <= b || b + gb <= a
        val wideRegs = if (wk == 0) (0, 0, 0) else {
          val vdGroup = if (wk == 3) narrowGroup else wideGroup
          val vs2Group = if (wk == 1) narrowGroup else wideGroup
          val d = Iterator.continually(pickIn(vdGroup)).find(r => !masked || r != 0).get
          // A SEW source of a widening form: in the highest part of vd when its EMUL >= 1, or outside vd
          def narrowSource(): Int = if (lmul >= 0 && random.nextInt(3) == 0) d + vdGroup - narrowGroup
            else Iterator.continually(pickIn(narrowGroup)).find(disjoint(_, narrowGroup, d, vdGroup)).get
          val v2 = wk match {
            case 1 => narrowSource()
            case _ if d % vs2Group == 0 && random.nextInt(3) == 0 => d // same EEW, or a narrowing vd at the base of vs2
            case _ => Iterator.continually(pickIn(vs2Group)).find(disjoint(_, vs2Group, d, vdGroup)).get
          }
          val v1 = if (wk == 3 && random.nextInt(3) == 0) d else if (wk == 3) pickIn(narrowGroup) else narrowSource()
          if (v2 == d || !disjoint(v2, vs2Group, d, vdGroup) || !disjoint(v1, narrowGroup, d, vdGroup)) outcomes((name, "overlap")) += 1
          (d, v2, v1)
        }
        val reduction = reductions.contains(name)
        var vd = if (wk != 0) wideRegs._1 else pick()
        while (masked && vd == 0 && !maskDst && !whole && !reduction) vd = pick()
        val vs1Pick = if (wk != 0) wideRegs._3 else if (scalarRead || scalarWrite) 11 else if (maskMm) random.nextInt(32) else pick()
        // An extension source group overlaps vd only in its highest part, and only with EMUL >= 1 (norm:vreg_overlap_legal)
        val sourceGroup = 1 << scala.math.max(lmul - ext, 0)
        def legalSource(r: Int) = r + sourceGroup <= vd || r >= vd + group || lmul - ext >= 0 && r == vd + group - sourceGroup
        val vs2 = if (wk != 0) wideRegs._2 else if (scalarRead) 13 else if (maskMm) random.nextInt(32) else if (whole && random.nextInt(4) == 0) vd
          else if (ext == 0) pick() else {
          if (lmul - ext >= 0 && random.nextInt(3) == 0) vd + group - sourceGroup
          else Iterator.continually(random.nextInt(32 / sourceGroup) * sourceGroup).find(legalSource).get
        }
        if (ext != 0 && vs2 < vd + group && vs2 >= vd) sourceInVd += 1
        // A reduction's vd and vs1 are single registers that may overlap anything, v0 included (norm:vreduction_vd_overlap_vs)
        val reduceVs1 = if (!reduction) 0 else random.nextInt(4) match {
          case 0 => vs2 + random.nextInt(group)
          case 1 => 0
          case _ => random.nextInt(32)
        }
        if (reduction) vd = random.nextInt(4) match {
          case 0 => vs2 + random.nextInt(group)
          case 1 => reduceVs1
          case 2 => 0
          case _ => random.nextInt(32)
        }
        if (reduction && (vd >= vs2 && vd < vs2 + group || vd == reduceVs1)) outcomes((name, "overlap")) += 1
        val vs1 = if (reduction) reduceVs1 else vs1Pick
        // A mask vd is one register anywhere: on a source base, or outside both source groups (norm:vreg_overlap_legal)
        if (maskMm) vd = random.nextInt(32)
        if (maskDst) {
          def outside(r: Int, base: Int) = r < base || r >= base + group
          vd = random.nextInt(4) match {
            case 0 => vs2
            case 1 if !scalarOperand => vs1
            case _ => Iterator.continually(random.nextInt(32)).find(r => outside(r, vs2) && (scalarOperand || outside(r, vs1))).get
          }
        }
        def element(reg: Int, i: Int, size: Int = size): BigInt = {
          val index = i * size
          val r = reg + index / (vlen / 8)
          val offset = index % (vlen / 8)
          (0 until size).map(b => BigInt(bytes(r)(offset + b)) << (8 * b)).reduce(_ | _)
        }
        // Equal scalar and vector elements are rare with random data
        val scalar = if (vl > 0 && ext == 0 && !maskMm && !whole && random.nextInt(4) == 0) (BigInt(xlen, random) >> width << width) |
            element(vs2, random.nextInt(vl)) & xmask
          else BigInt(xlen, random)
        def sextFrom(v: BigInt, bits: Int): BigInt = if (v.testBit(bits - 1)) v - (BigInt(1) << bits) else v
        def sext(v: BigInt): BigInt = sextFrom(v, width)
        // A scalar narrower than SEW is sign-extended (norm:vop-vx_rs1_sext_sewbits)
        val scalarValue = sextFrom(scalar, xlen) & mask
        val vxrm = if (vxrm0 >= 0) vxrm0 else random.nextInt(4)
        // v >> d plus the rounding increment of the dropped bits (norm:vxrm_op)
        def roundoff(v: BigInt, d: Int): BigInt = {
          val low = v & ((BigInt(1) << d) - 1)
          val r = if (d == 0) 0 else vxrm match {
            case 0 => if (v.testBit(d - 1)) 1 else 0
            case 1 => if (v.testBit(d - 1) && (low != (BigInt(1) << (d - 1)) || v.testBit(d))) 1 else 0
            case 2 => 0
            case 3 => if (!v.testBit(d) && low != 0) 1 else 0
          }
          increments((name, vxrm, r)) += 1
          (v >> d) + r
        }
        // Clamp to the SEW range (norm:vsaddu_vsadd_vssubu_vssub_op_overflow_vxsat_op_vsaddsub)
        def clamp(v: BigInt, low: BigInt, high: BigInt): (BigInt, Boolean) =
          if (v > high) (high, true) else if (v < low) (low, true) else (v, false)
        val expectedResult = if (scalarRead) sext(element(vs2, 0)) & xmask else BigInt(0)
        var expectedSaturation = false
        val expected = ArrayBuffer[(Int, BigInt, Int)]()
        val expectedReads = ArrayBuffer[Int]()
        def maskBit(i: Int) = ((bytes(0)(i / 8) >> (i % 8)) & 1) != 0
        def bitOf(reg: Int, i: Int) = ((bytes(reg)(i / 8) >> (i % 8)) & 1) != 0
        if (whole) {
          // Every byte of NREG registers, whatever vl, vstart, SEW and v0 (norm:vmv-nr-r_op)
          if (vs2 == vd) outcomes((name, "same")) += 1
          for (k <- 0 until nreg * p.beatsPerReg) {
            val (r, o) = (k / p.beatsPerReg, k % p.beatsPerReg * 8)
            for (b <- 0 until 8) bytes(vd + r)(o + b) = bytes(vs2 + r)(o + b)
            expected += ((vd * p.beatsPerReg + k, word(vd + r, o), 255))
            expectedReads += k
          }
        } else if (maskDst || maskMm) {
          // Mask results: bits below vl, active ones only for compares; every other bit of vd is kept
          val results = for (i <- 0 until vl if maskMm || i >= start && (!masked || withV0(name) || maskBit(i))) yield {
            lazy val a = element(vs2, i)
            lazy val b = if (scalarOperand) scalarValue else element(vs1, i)
            val c = if (withV0(name) && maskBit(i)) 1 else 0
            val (x, y) = (bitOf(vs2, i), bitOf(vs1, i))
            val bit = name match {
              case "vmseq" => a == b
              case "vmsne" => a != b
              case "vmsltu" => a < b
              case "vmslt" => sext(a) < sext(b)
              case "vmsleu" => a <= b
              case "vmsle" => sext(a) <= sext(b)
              case "vmsgtu" => a > b
              case "vmsgt" => sext(a) > sext(b)
              case "vmadc" | "vmadcm" => (a + b + c) > mask
              case "vmsbc" | "vmsbcm" => a - b - c < 0
              case "vmand" => x && y
              case "vmnand" => !(x && y)
              case "vmandn" => x && !y
              case "vmxor" => x != y
              case "vmor" => x || y
              case "vmnor" => !(x || y)
              case "vmorn" => x || !y
              case "vmxnor" => x == y
            }
            if (maskDst) {
              outcomes((name, bit.toString)) += 1
              if (compares.contains(name) && a == b) outcomes((name, "equal")) += 1
            }
            i -> bit
          }
          for ((i, bit) <- results) {
            val byte = bytes(vd)(i / 8)
            bytes(vd)(i / 8) = if (bit) byte | (1 << (i % 8)) else byte & ~(1 << (i % 8))
          }
          if (vl > 0) for (w <- 0 to (vl - 1) / 64) {
            expected += ((vd * p.beatsPerReg + w, word(vd, w * 8), 255))
          }
          val steps = if (vl == 0) 0 else if (maskMm) (vl - 1) / 64 + 1 else (vl - 1) * size / 8 + 1
          expectedReads ++= 0 until steps
        } else if (reduction) {
          // vs1[0] combined with the active elements of vs2, written to element 0 of vd only if vl > 0
          // (norm:vreduction_scalar_def, norm:vreduction_vl_0, norm:vwredsumu_vwredsum_op_overflow)
          val wide = name.startsWith("vw")
          val dsize = if (wide) 2 * size else size
          val dmask = (BigInt(1) << (8 * dsize)) - 1
          if (vl > 0) {
            var acc = element(vs1, 0, dsize)
            for (i <- 0 until vl if !masked || maskBit(i)) {
              val e = if (wide && signed) sext(element(vs2, i)) & dmask else element(vs2, i)
              acc = name match {
                case "vredand" => acc & e
                case "vredor" => acc | e
                case "vredxor" => acc ^ e
                case "vredminu" => acc.min(e)
                case "vredmaxu" => acc.max(e)
                case "vredmin" => if (sext(e) < sext(acc)) e else acc
                case "vredmax" => if (sext(e) > sext(acc)) e else acc
                case _ => (acc + e) & dmask
              }
            }
            if (masked && (0 until vl).forall(i => !maskBit(i))) outcomes((name, "inactive")) += 1
            for (b <- 0 until dsize) bytes(vd)(b) = ((acc >> (8 * b)) & 255).toInt
            expected += ((vd * p.beatsPerReg, acc, (1 << dsize) - 1))
            expectedReads ++= (0 to (vl - 1) * dsize / 8).map(k => if (wide) k / 2 else k)
          }
        } else if (wk != 0) {
          // Each active element reads its sources and writes its destination bytes, in element order
          val dsize = if (wk == 3) size else 2 * size
          val updates = ArrayBuffer[(Int, BigInt)]()
          for (i <- start until vl if !masked || maskBit(i)) {
            def ext(v: BigInt) = if (signed) sext(v) else v
            val b = if (scalarOperand) scalarValue else element(vs1, i)
            val (value, saturated) = if (wk != 3) {
              val a = if (wk == 1) ext(element(vs2, i)) else element(vs2, i, 2 * size)
              (if (op == VectorAluOp.ADD) a + ext(b) else a - ext(b), false)
            } else {
              val wideValue = element(vs2, i, 2 * size)
              val a = if (signed) sextFrom(wideValue, 2 * width) else wideValue
              val amount = (b & (2 * width - 1)).toInt
              if (!saturate) (a >> amount, false)
              else if (signed) clamp(roundoff(a, amount), -(BigInt(1) << (width - 1)), (BigInt(1) << (width - 1)) - 1)
              else clamp(roundoff(a, amount), 0, mask)
            }
            expectedSaturation |= saturated
            updates += i -> (value & ((BigInt(1) << (8 * dsize)) - 1))
          }
          // One write per step: a destination word, or for narrowing one half of it
          val perStep = updates.groupBy(u => u._1 * (if (wk == 3) 2 * size else dsize) / 8).toSeq.sortBy(_._1)
          for ((k, entries) <- perStep) {
            var byteMask = 0
            var data = BigInt(0)
            for ((i, value) <- entries; b <- 0 until dsize) {
              val byte = i * dsize + b
              val valueByte = ((value >> (8 * b)) & 255).toInt
              byteMask |= 1 << (byte % 8)
              data |= BigInt(valueByte) << (8 * (byte % 8))
              bytes(vd + byte / (vlen / 8))(byte % (vlen / 8)) = valueByte
            }
            expected += ((vd * p.beatsPerReg + (if (wk == 3) k / 2 else k), data, byteMask))
            expectedReads += (if (wk == 1) k / 2 else k)
          }
        } else if (!scalarRead) {
          val updates = ArrayBuffer[(Int, BigInt)]()
          for (i <- 0 until vl if i >= start && (!scalarWrite || i == 0) && (!masked || withV0(name) || maskBit(i))) {
            val a = element(vs2, i, size >> ext)
            val b = if (scalarOperand) scalarValue else element(vs1, i)
            val half = BigInt(1) << (width - 1)
            val amount = (b & (width - 1)).toInt
            val m = if (maskBit(i)) 1 else 0
            val (value, saturated) = name match {
              case "vadd" => (a + b, false)
              case "vsub" => (a - b, false)
              case "vrsub" => (b - a, false)
              case "vand" => (a & b, false)
              case "vor" => (a | b, false)
              case "vxor" => (a ^ b, false)
              case "vmv" => (b, false)
              case "vsaddu" => clamp(a + b, 0, mask)
              case "vsadd" => clamp(sext(a) + sext(b), -half, half - 1)
              case "vssubu" => clamp(a - b, 0, mask)
              case "vssub" => clamp(sext(a) - sext(b), -half, half - 1)
              case "vminu" => (a.min(b), false)
              case "vmin" => (sext(a).min(sext(b)), false)
              case "vmaxu" => (a.max(b), false)
              case "vmax" => (sext(a).max(sext(b)), false)
              case "vsll" => (a << amount, false)
              case "vsrl" => (a >> amount, false)
              case "vsra" => (sext(a) >> amount, false)
              case "vadc" => (a + b + m, false)
              case "vsbc" => (a - b - m, false)
              case "vmerge" => (if (m == 1) b else a, false)
              case "vzext2" | "vzext4" | "vzext8" => (a, false)
              case "vsext2" | "vsext4" | "vsext8" => (sextFrom(a, width >> ext), false)
              // Exact sums and differences, then a rounded shift by 1; vasub wraps (norm:vasub_vasubu_op_overflow)
              case "vaaddu" => (roundoff(a + b, 1), false)
              case "vaadd" => (roundoff(sext(a) + sext(b), 1), false)
              case "vasubu" => (roundoff(a - b, 1), false)
              case "vasub" =>
                val v = roundoff(sext(a) - sext(b), 1)
                if (v >= half) outcomes((name, "wrap")) += 1
                (v, false)
              case "vssrl" => (roundoff(a, amount), false)
              case "vssra" => (roundoff(sext(a), amount), false)
              // Masking does not change the index (norm:vid_op_mask)
              case "vid" => (BigInt(i), false)
            }
            expectedSaturation |= saturated
            updates += i -> (value & mask)
          }
          val perBeat = updates.groupBy(_._1 * size / 8).toSeq.sortBy(_._1)
          for ((beat, entries) <- perBeat) {
            val address = vd * p.beatsPerReg + beat
            var byteMask = 0
            var data = BigInt(0)
            for ((i, value) <- entries; b <- 0 until size) {
              val byte = i * size + b
              val pos = byte % 8
              val valueByte = ((value >> (8 * b)) & 255).toInt
              byteMask |= 1 << pos
              data |= BigInt(valueByte) << (8 * pos)
              bytes(vd + byte / (vlen / 8))(byte % (vlen / 8)) = valueByte
            }
            expected += ((address, data, byteMask))
            if (!index) expectedReads += beat >> ext
          }
        } else expectedReads += 0
        if (saturate) saturations((name, expectedSaturation)) += 1
        dut.io.debug #= false
        dut.io.cmd.valid #= true
        val cmd = dut.io.cmd
        cmd.vd #= vd; cmd.vs1 #= vs1; cmd.vs2 #= vs2
        cmd.vl #= vl; cmd.vstart #= start; cmd.sew #= sew; cmd.op #= op
        cmd.shape #= shape; cmd.vxrm #= vxrm; cmd.nreg #= scala.math.max(nreg, 1)
        cmd.scalar #= scalar; cmd.scalarOperand #= scalarOperand
        cmd.unmasked #= !masked; cmd.signed #= signed; cmd.saturate #= saturate; cmd.maskData #= withV0(name)
        tick()
        cmd.valid #= false
        var cycles = 0
        var writeIndex = 0
        val actualReads = ArrayBuffer[Int]()
        val readCycles = ArrayBuffer[Int]()
        val writeCycles = ArrayBuffer[Int]()
        while (!dut.io.done.toBoolean && cycles < 100) {
          sleep(1)
          assert(dut.io.busy.toBoolean, s"busy before done case $cases")
          val rv = dut.io.readValid.toInt
          val readVs2 = scalarRead || !scalarWrite && op != VectorAluOp.MOVE && !index
          // A reduction reads vs1 at its first step only
          val readVs1 = !scalarOperand && !scalarRead && ext == 0 && !whole && !index && (!reduction || actualReads.isEmpty)
          if (rv != 0) {
            val port = if (readVs2) 0 else 1
            val base = if (readVs2) vs2 else vs1
            val beat = dut.io.readAddresses(port).toInt - base * p.beatsPerReg
            // Port C reads the old mask word: every step of MASK_MM, the first step of each word of MASK_DST
            val wordStep = beat * (8 >> sew) % 64 == 0
            val readVd = maskMm || maskDst && wordStep
            val expectedValid = (if (readVs2) 1 else 0) | (if (readVs1) 2 else 0) | (if (readVd) 4 else 0)
            assert(rv == expectedValid, s"read ports case $cases")
            actualReads += beat
            readCycles += cycles
            if (readVs2 && readVs1) assert(dut.io.readAddresses(1).toInt == vs1 * p.beatsPerReg +
              (if (reduction) 0 else if (wk >= 2) beat / 2 else beat))
            val vdWord = if (maskDst) beat * (8 >> sew) / 64 else beat
            if (readVd) assert(dut.io.readAddresses(2).toInt == vd * p.beatsPerReg + vdWord, s"old mask word case $cases")
          }
          if (dut.io.write.valid.toBoolean) {
            assert(writeIndex < expected.size, s"unexpected write in case $cases")
            val (address, data, byteMask) = expected(writeIndex)
            assert(dut.io.write.address.toInt == address, s"address case $cases")
            assert(dut.io.write.mask.toInt == byteMask, s"byte enables case $cases")
            val bitMask = (0 until 8).filter(b => (byteMask & (1 << b)) != 0).map(b => BigInt(255) << (8 * b)).reduce(_ | _)
            assert((dut.io.write.data.toBigInt & bitMask) == data, s"data case $cases $name sew $sew lmul $lmul vd $vd vs2 $vs2")
            if ((!masked || whole) && !maskDst && writeCycles.nonEmpty) assert(cycles == writeCycles.last + 1, "active beats must stream every cycle")
            writeCycles += cycles
            writeIndex += 1; writes += 1
          }
          tick(); cycles += 1
        }
        assert(dut.io.done.toBoolean, s"timeout case $cases")
        assert(writeIndex == expected.size)
        // Each write lands three cycles after its reads; done follows the last write
        // MASK_DST writes a mask word after the last step that produces its bits
        val writingSteps = actualReads.indices.filter(k => !maskDst || k == actualReads.size - 1 || (k + 1) * (8 >> sew) % 64 == 0)
        // A reduction writes after its last step and one fold per halving of the 64-bit accumulator down to one lane
        val folds = 3 - sew - (if (name.startsWith("vwred")) 1 else 0)
        if (!scalarRead && readCycles.nonEmpty)
          assert(writeCycles.toSeq == (if (reduction) Seq(readCycles.last + folds + 3) else writingSteps.map(readCycles(_) + 3)),
            s"write latency case $cases")
        if (index) assert(actualReads.isEmpty, s"vid reads case $cases")
        assert(dut.io.saturated.toBoolean == expectedSaturation, s"saturation case $cases $name sew $sew vl $vl")
        if (scalarRead) assert(dut.io.result.toBigInt == expectedResult)
        if (!scalarOperand || scalarRead || op != VectorAluOp.MOVE)
          assert(actualReads.toSeq == expectedReads.toSeq, s"read issue/skip case $cases: $actualReads vs $expectedReads")
        sleep(1); tick()
        assert(!dut.io.busy.toBoolean, s"busy after done case $cases")
        // Check all physical words, including untouched bytes and registers.
        dut.io.debug #= true
        for (a <- 0 until p.wordCount) {
          dut.io.readAddress #= a; tick()
          assert(dut.io.readData.toBigInt == word(a / p.beatsPerReg, (a % p.beatsPerReg) * 8), s"readback case $cases word $a")
        }
        cases += 1
      }
      for (sew <- 0 to maxSew; lmul <- -3 to 3 if (8 << sew) <= elen * scala.math.pow(2, lmul)) {
        val vlmax = ((vlen / (8 << sew)) * scala.math.pow(2, lmul)).toInt
        // Extensions need a source EEW of at least 8 bits
        for ((name, _, _) <- ops if sew >= extLog2(name) && sizeOk(name, sew, lmul, elen); scalar <- Seq(false, true);
             vl <- Seq(0, 1, vlmax - 1, vlmax).distinct;
             masked <- Seq(false, true))
          execute(name, sew, lmul, vl, 0, masked, scalar)
        for (start <- Seq(1, vlmax - 1, vlmax, scala.math.min(vlen - 1, vlmax + 3)).distinct.filter(_ < vlen))
          execute("vadd", sew, lmul, vlmax, start, true, false)
      }
      // Operands at the limits: random elements rarely reach them exactly, nor equal each other or shift by 0
      for (sew <- 0 to maxSew; (name, _, saturate) <- ops if sew >= extLog2(name) && sizeOk(name, sew, 1, elen) && name != "vmv" &&
           name != "vid" && nregOf(name) == 0;
           _ <- 0 until (if (saturate || compares.contains(name) || carries.contains(name) || roundings.contains(name)) 4 else 2)) {
        val size = 1 << sew
        // vasub wraps only for INT_MAX - INT_MIN (norm:vasub_vasubu_op_overflow)
        val extremes = name == "vasub" && random.nextBoolean()
        for (reg <- 0 until 32; e <- 0 until vlen / 8 / size) {
          val top = BigInt(1) << (8 * size - 1)
          val limit = if (extremes) Seq(top - 1, top) else Seq(BigInt(0), BigInt(1), top - 1, top, 2 * top - 1)
          val v = limit(random.nextInt(limit.size))
          for (b <- 0 until size) bytes(reg)(e * size + b) = ((v >> (8 * b)) & 255).toInt
        }
        dut.io.debug #= true
        for (a <- 0 until p.wordCount) {
          dut.io.access.valid #= true
          dut.io.access.address #= a
          dut.io.access.mask #= 255
          dut.io.access.data #= word(a / p.beatsPerReg, (a % p.beatsPerReg) * 8)
          tick()
        }
        dut.io.access.valid #= false
        // INT_MAX - INT_MIN is odd, so rnu rounds it up past INT_MAX
        execute(name, sew, random.nextInt(2), vlen / (8 << sew), 0, random.nextBoolean(), random.nextInt(4) == 0 && !extremes,
          vxrm0 = if (extremes) 0 else -1)
      }
      for (sew <- 0 to maxSew; vl <- Seq(0, 1, 3); read <- Seq(false, true))
        execute("vmv", sew, 3, vl, 0, false, true, if (read) VectorShape.SCALAR_READ else VectorShape.SCALAR_WRITE)
      for ((name, _, saturate) <- ops if saturate; outcome <- Seq(false, true))
        assert(saturations((name, outcome)) > 0, s"$name never had saturated = $outcome")
      for (name <- compares ++ carries; outcome <- Seq("true", "false") ++ (if (compares.contains(name)) Seq("equal") else Nil))
        assert(outcomes((name, outcome)) > 0, s"$name never had an active result $outcome")
      // Round to nearest up, nearest even and odd both increment and keep; down never increments
      for (name <- roundings; vxrm <- 0 to 3; r <- 0 to 1)
        assert((increments((name, vxrm, r)) > 0) == (vxrm != 2 || r == 0), s"$name vxrm $vxrm increment $r")
      assert(outcomes(("vasub", "wrap")) > 0, "vasub never wrapped")
      for (n <- Seq(1, 2, 4, 8)) assert(outcomes((s"vmv${n}r", "same")) > 0, s"vmv${n}r never had vd = vs2")
      for ((name, _, _) <- ops if wideKind(name) != 0 || reductions.contains(name))
        assert(outcomes((name, "overlap")) > 0, s"$name never overlapped vd")
      for (name <- reductions) assert(outcomes((name, "inactive")) > 0, s"$name never had only inactive elements")
      if (elen == 64) assert(lanes64 == ops.map(_._1).toSet, s"never with 64-bit lanes: ${ops.map(_._1).toSet -- lanes64}")
      println(s"VectorArithmetic VLEN=$vlen XLEN=$xlen ELEN=$elen: $cases cases, $writes writes, saturation outcomes " +
        saturations.toSeq.sorted.map { case ((n, o), c) => s"$n/$o=$c" }.mkString(" ") +
        s", $sourceInVd extensions with vs2 in the highest part of vd, mask outcomes " +
        outcomes.toSeq.sorted.map { case ((n, o), c) => s"$n/$o=$c" }.mkString(" ") + ", rounding increments " +
        increments.toSeq.sorted.map { case ((n, m, r), c) => s"$n/$m/$r=$c" }.mkString(" ") + ", full physical readback after every case")
    }
  }
  test("64-bit beats and byte preservation on RV32") { run(128, 32) }
  test("scalar truncation and sign extension on RV64") { run(128, 64) }
  test("larger vector register addressing") { run(256, 64) }
  test("SEW 64 with sign-extended scalars on RV32") { run(128, 32, 64) }
  test("SEW 64 and larger vector registers on RV64") { run(256, 64, 64) }
}
