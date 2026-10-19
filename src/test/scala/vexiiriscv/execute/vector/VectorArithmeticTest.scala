package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class VectorArithmeticHarness(p: VectorRegFileParam, xlen: Int) extends Component {
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
  val arithmetic = new VectorArithmetic(p, xlen)
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
    ("vzext4", VectorAluOp.ADD, false), ("vsext4", VectorAluOp.ADD, false),
    ("vmseq", VectorAluOp.SEQ, false), ("vmsne", VectorAluOp.SNE, false), ("vmsltu", VectorAluOp.SLT, false),
    ("vmslt", VectorAluOp.SLT, false), ("vmsleu", VectorAluOp.SLE, false), ("vmsle", VectorAluOp.SLE, false),
    ("vmsgtu", VectorAluOp.SGT, false), ("vmsgt", VectorAluOp.SGT, false), ("vmadc", VectorAluOp.ADD, false),
    ("vmadcm", VectorAluOp.ADD, false), ("vmsbc", VectorAluOp.SUB, false), ("vmsbcm", VectorAluOp.SUB, false),
    ("vmand", VectorAluOp.AND, false), ("vmnand", VectorAluOp.NAND, false), ("vmandn", VectorAluOp.ANDN, false),
    ("vmxor", VectorAluOp.XOR, false), ("vmor", VectorAluOp.OR, false), ("vmnor", VectorAluOp.NOR, false),
    ("vmorn", VectorAluOp.ORN, false), ("vmxnor", VectorAluOp.XNOR, false))
  val compares = Seq("vmseq", "vmsne", "vmsltu", "vmslt", "vmsleu", "vmsle", "vmsgtu", "vmsgt")
  val carries = Seq("vmadc", "vmadcm", "vmsbc", "vmsbcm")
  val logicals = Seq("vmand", "vmnand", "vmandn", "vmxor", "vmor", "vmnor", "vmorn", "vmxnor")
  // Operations whose signedness the command must carry; the others get a random value
  def isSigned(name: String) = Seq("vsadd", "vssub", "vmin", "vmax", "vsra", "vsext2", "vsext4", "vmslt", "vmsle", "vmsgt").contains(name)
  def signedMatters(name: String) = isSigned(name) || carries.contains(name) ||
    Seq("vsaddu", "vssubu", "vminu", "vmaxu", "vsrl", "vzext2", "vzext4", "vmsltu", "vmsleu", "vmsgtu").contains(name)
  // v0 as carry or merge select: encoded masked, every body element written
  def withV0(name: String) = Seq("vadc", "vsbc", "vmerge", "vmadcm", "vmsbcm").contains(name)
  // Encoded unmasked
  def unmaskedOnly(name: String) = Seq("vmadc", "vmsbc").contains(name) || logicals.contains(name)
  // log2 of the extension factor, 0 for single width
  def extLog2(name: String) = if (name.endsWith("ext2")) 1 else if (name.endsWith("ext4")) 2 else 0
  def shapeOf(name: String) =
    if (compares.contains(name) || carries.contains(name)) VectorShape.MASK_DST
    else if (logicals.contains(name)) VectorShape.MASK_MM
    else extLog2(name) match { case 1 => VectorShape.EXT2; case 2 => VectorShape.EXT4; case _ => VectorShape.SINGLE }

  def run(vlen: Int, xlen: Int): Unit = {
    val p = VectorRegFileParam(vlen = vlen)
    SimConfig.compile(new VectorArithmeticHarness(p, xlen)).doSim { dut =>
      val random = new Random(901 + xlen + vlen)
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
      dut.io.cmd.shape #= VectorShape.SINGLE; dut.io.cmd.vxrm #= 0
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
      def execute(name: String, sew: Int, lmul: Int, vl: Int, start: Int, masked0: Boolean,
                  scalarOperand0: Boolean, shape0: Int = VectorShape.SINGLE): Unit = {
        val (_, op, saturate) = ops.find(_._1 == name).get
        val signed = if (signedMatters(name)) isSigned(name) else random.nextBoolean()
        val ext = extLog2(name)
        val shape = if (shapeOf(name) != VectorShape.SINGLE) shapeOf(name) else shape0
        val maskDst = shape == VectorShape.MASK_DST
        val maskMm = shape == VectorShape.MASK_MM
        val masked = (masked0 || withV0(name)) && !unmaskedOnly(name)
        val scalarOperand = scalarOperand0 && ext == 0 && !maskMm
        val scalarRead = shape == VectorShape.SCALAR_READ
        val scalarWrite = shape == VectorShape.SCALAR_WRITE
        val width = 8 << sew
        val size = width / 8
        val mask = (BigInt(1) << width) - 1
        // Any aligned groups; same-EEW overlap is always legal, and vd = v0 only unmasked
        val group = 1 << scala.math.max(lmul, 0)
        def pick(): Int = random.nextInt(32 / group) * group
        var vd = pick()
        while (masked && vd == 0 && !maskDst) vd = pick()
        val vs1 = if (scalarRead || scalarWrite) 11 else if (maskMm) random.nextInt(32) else pick()
        // An extension source group overlaps vd only in its highest part, and only with EMUL >= 1 (norm:vreg_overlap_legal)
        val sourceGroup = 1 << scala.math.max(lmul - ext, 0)
        def legalSource(r: Int) = r + sourceGroup <= vd || r >= vd + group || lmul - ext >= 0 && r == vd + group - sourceGroup
        val vs2 = if (scalarRead) 13 else if (maskMm) random.nextInt(32) else if (ext == 0) pick() else {
          if (lmul - ext >= 0 && random.nextInt(3) == 0) vd + group - sourceGroup
          else Iterator.continually(random.nextInt(32 / sourceGroup) * sourceGroup).find(legalSource).get
        }
        if (ext != 0 && vs2 < vd + group && vs2 >= vd) sourceInVd += 1
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
        val scalar = if (vl > 0 && ext == 0 && !maskMm && random.nextInt(4) == 0) (BigInt(xlen, random) >> width << width) | element(vs2, random.nextInt(vl))
          else BigInt(xlen, random)
        def sextFrom(v: BigInt, bits: Int): BigInt = if (v.testBit(bits - 1)) v - (BigInt(1) << bits) else v
        def sext(v: BigInt): BigInt = sextFrom(v, width)
        // Clamp to the SEW range (norm:vsaddu_vsadd_vssubu_vssub_op_overflow_vxsat_op_vsaddsub)
        def clamp(v: BigInt, low: BigInt, high: BigInt): (BigInt, Boolean) =
          if (v > high) (high, true) else if (v < low) (low, true) else (v, false)
        val expectedResult = if (scalarRead) sext(element(vs2, 0)) & xmask else BigInt(0)
        var expectedSaturation = false
        val expected = ArrayBuffer[(Int, BigInt, Int)]()
        val expectedReads = ArrayBuffer[Int]()
        def maskBit(i: Int) = ((bytes(0)(i / 8) >> (i % 8)) & 1) != 0
        def bitOf(reg: Int, i: Int) = ((bytes(reg)(i / 8) >> (i % 8)) & 1) != 0
        if (maskDst || maskMm) {
          // Mask results: bits below vl, active ones only for compares; every other bit of vd is kept
          val results = for (i <- 0 until vl if maskMm || i >= start && (!masked || withV0(name) || maskBit(i))) yield {
            lazy val a = element(vs2, i)
            lazy val b = if (scalarOperand) scalar & mask else element(vs1, i)
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
        } else if (!scalarRead) {
          val updates = ArrayBuffer[(Int, BigInt)]()
          for (i <- 0 until vl if i >= start && (!scalarWrite || i == 0) && (!masked || withV0(name) || maskBit(i))) {
            val a = element(vs2, i, size >> ext)
            val b = if (scalarOperand) scalar & mask else element(vs1, i)
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
              case "vzext2" | "vzext4" => (a, false)
              case "vsext2" | "vsext4" => (sextFrom(a, width >> ext), false)
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
            expectedReads += beat >> ext
          }
        } else expectedReads += 0
        if (saturate) saturations((name, expectedSaturation)) += 1
        dut.io.debug #= false
        dut.io.cmd.valid #= true
        val cmd = dut.io.cmd
        cmd.vd #= vd; cmd.vs1 #= vs1; cmd.vs2 #= vs2
        cmd.vl #= vl; cmd.vstart #= start; cmd.sew #= sew; cmd.op #= op
        cmd.shape #= shape; cmd.vxrm #= random.nextInt(4)
        cmd.scalar #= scalar; cmd.scalarOperand #= scalarOperand
        cmd.unmasked #= !masked; cmd.signed #= signed; cmd.saturate #= saturate; cmd.maskData #= withV0(name)
        tick()
        cmd.valid #= false
        var cycles = 0
        var writeIndex = 0
        val actualReads = ArrayBuffer[Int]()
        val readCycles = ArrayBuffer[Int]()
        val writeCycles = ArrayBuffer[Int]()
        while (!dut.io.done.toBoolean && cycles < 80) {
          sleep(1)
          assert(dut.io.busy.toBoolean, s"busy before done case $cases")
          val rv = dut.io.readValid.toInt
          val readVs2 = scalarRead || !scalarWrite && op != VectorAluOp.MOVE
          val readVs1 = !scalarOperand && !scalarRead && ext == 0
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
            if (readVs2 && readVs1) assert(dut.io.readAddresses(1).toInt == vs1 * p.beatsPerReg + beat)
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
            if (!masked && !maskDst && writeCycles.nonEmpty) assert(cycles == writeCycles.last + 1, "active beats must stream every cycle")
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
        if (!scalarRead && readCycles.nonEmpty)
          assert(writeCycles.toSeq == writingSteps.map(readCycles(_) + 3), s"write latency case $cases")
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
      for (sew <- 0 to 2; lmul <- -2 to 3 if (8 << sew) <= 32 * scala.math.pow(2, lmul)) {
        val vlmax = ((vlen / (8 << sew)) * scala.math.pow(2, lmul)).toInt
        // Extensions need a source EEW of at least 8 bits
        for ((name, _, _) <- ops if sew >= extLog2(name); scalar <- Seq(false, true); vl <- Seq(0, 1, vlmax - 1, vlmax).distinct;
             masked <- Seq(false, true))
          execute(name, sew, lmul, vl, 0, masked, scalar)
        for (start <- Seq(1, vlmax - 1, vlmax, scala.math.min(vlen - 1, vlmax + 3)).distinct.filter(_ < vlen))
          execute("vadd", sew, lmul, vlmax, start, true, false)
      }
      // Operands at the limits: random elements rarely reach them exactly, nor equal each other or shift by 0
      for (sew <- 0 to 2; (name, _, saturate) <- ops if sew >= extLog2(name) && name != "vmv";
           _ <- 0 until (if (saturate || compares.contains(name) || carries.contains(name)) 4 else 2)) {
        val size = 1 << sew
        for (reg <- 0 until 32; e <- 0 until vlen / 8 / size) {
          val limit = Seq(0, 1, (1 << (8 * size - 1)) - 1, 1 << (8 * size - 1), (1L << (8 * size)) - 1).map(BigInt(_))
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
        execute(name, sew, random.nextInt(2), vlen / (8 << sew), 0, random.nextBoolean(), random.nextInt(4) == 0)
      }
      for (sew <- 0 to 2; vl <- Seq(0, 1, 3); read <- Seq(false, true))
        execute("vmv", sew, 3, vl, 0, false, true, if (read) VectorShape.SCALAR_READ else VectorShape.SCALAR_WRITE)
      for ((name, _, saturate) <- ops if saturate; outcome <- Seq(false, true))
        assert(saturations((name, outcome)) > 0, s"$name never had saturated = $outcome")
      for (name <- compares ++ carries; outcome <- Seq("true", "false") ++ (if (compares.contains(name)) Seq("equal") else Nil))
        assert(outcomes((name, outcome)) > 0, s"$name never had an active result $outcome")
      println(s"VectorArithmetic VLEN=$vlen XLEN=$xlen: $cases cases, $writes writes, saturation outcomes " +
        saturations.toSeq.sorted.map { case ((n, o), c) => s"$n/$o=$c" }.mkString(" ") +
        s", $sourceInVd extensions with vs2 in the highest part of vd, mask outcomes " +
        outcomes.toSeq.sorted.map { case ((n, o), c) => s"$n/$o=$c" }.mkString(" ") + ", full physical readback after every case")
    }
  }
  test("64-bit beats and byte preservation on RV32") { run(128, 32) }
  test("scalar truncation and sign extension on RV64") { run(128, 64) }
  test("larger vector register addressing") { run(256, 64) }
}
