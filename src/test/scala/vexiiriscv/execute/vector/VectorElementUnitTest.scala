package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class VectorElementHarness(p: VectorRegFileParam, xlen: Int, elen: Int = 32) extends Component {
  val io = new Bundle {
    val cmd = slave(Flow(VectorElementCmd(p, xlen)))
    val debug = in Bool()
    val access = slave(Flow(VrfWriteCmd(p)))
    val readAddress = in UInt(p.addressWidth bits)
    val readData = out Bits(64 bits)
    val initHold, done, busy, saturated = out Bool()
    val write = master(Flow(VrfWriteCmd(p)))
    val divCommand = out Bool()
    val readValid = out Bits(3 bits)
    val readAddresses = out Vec(UInt(p.addressWidth bits), 3)
  }
  val vrf = new VectorRegFile(p)
  val unit = new VectorElementUnit(p, xlen, elen)
  unit.io.cmd := io.cmd
  unit.io.v0 := vrf.io.v0
  vrf.io.write := unit.io.write
  when(io.debug) { vrf.io.write := io.access }
  for (i <- 0 until 3) {
    vrf.io.reads(i).cmd := unit.io.reads(i).cmd
    unit.io.reads(i).rsp := vrf.io.reads(i).rsp
    io.readValid(i) := unit.io.reads(i).cmd.valid
    io.readAddresses(i) := unit.io.reads(i).cmd.payload
  }
  when(io.debug) {
    vrf.io.reads(0).cmd.valid := True
    vrf.io.reads(0).cmd.payload := io.readAddress
  }
  val divFire = unit.rework { unit.division.core.io.cmd.fire }
  io.divCommand := divFire.pull()
  io.readData := vrf.io.reads(0).rsp
  io.initHold := vrf.io.initHold
  io.done := unit.io.done
  io.busy := unit.io.busy
  io.saturated := unit.io.saturated
  io.write := unit.io.write
}

class VectorElementUnitTest extends AnyFunSuite {
  import VectorElementOp._
  // name, operation, vs2 signed, vs1 or scalar signed, product subtracted, forms
  val ops = Seq(
    ("vmul", MUL, false, false, false, "vv vx"), ("vmulh", MULH, true, true, false, "vv vx"),
    ("vmulhu", MULH, false, false, false, "vv vx"), ("vmulhsu", MULH, true, false, false, "vv vx"),
    ("vmacc", MACC, false, false, false, "vv vx"), ("vnmsac", MACC, false, false, true, "vv vx"),
    ("vmadd", MADD, false, false, false, "vv vx"), ("vnmsub", MADD, false, false, true, "vv vx"),
    ("vwmul", WMUL, true, true, false, "vv vx"), ("vwmulu", WMUL, false, false, false, "vv vx"),
    ("vwmulsu", WMUL, true, false, false, "vv vx"), ("vwmacc", WMACC, true, true, false, "vv vx"),
    ("vwmaccu", WMACC, false, false, false, "vv vx"), ("vwmaccsu", WMACC, false, true, false, "vv vx"),
    ("vwmaccus", WMACC, true, false, false, "vx"), ("vsmul", SMUL, true, true, false, "vv vx"),
    ("vdiv", DIV, true, true, false, "vv vx"), ("vdivu", DIV, false, false, false, "vv vx"),
    ("vrem", REM, true, true, false, "vv vx"), ("vremu", REM, false, false, false, "vv vx"))
  def wide(name: String) = name.startsWith("vw")
  def readsVd(name: String) = Seq("vmacc", "vnmsac", "vmadd", "vnmsub").contains(name) || name.startsWith("vwmacc")

  def run(vlen: Int, xlen: Int, exhaustive: Boolean, elen: Int = 32): Unit = {
    val p = VectorRegFileParam(vlen = vlen)
    SimConfig.compile(new VectorElementHarness(p, xlen, elen)).doSim { dut =>
      val random = new Random(1301 + xlen + vlen + (if (elen == 64) 64 else 0))
      val vlenb = vlen / 8
      val bytes = Array.fill(32, vlenb)(random.nextInt(256))
      def word(reg: Int, offset: Int): BigInt =
        (0 until 8).map(b => BigInt(bytes(reg)(offset + b)) << (b * 8)).reduce(_ | _)
      def tick(): Unit = {
        dut.clockDomain.risingEdge(); sleep(5)
        dut.clockDomain.fallingEdge(); sleep(5)
      }
      dut.clockDomain.fallingEdge()
      val cmd = dut.io.cmd
      cmd.valid #= false
      cmd.vd #= 0; cmd.vs1 #= 0; cmd.vs2 #= 0; cmd.vl #= 0; cmd.vlmax #= 0; cmd.sew #= 0; cmd.op #= 0; cmd.scalar #= 0
      cmd.scalarOperand #= false; cmd.unmasked #= true; cmd.signed #= false; cmd.signedVs1 #= false
      cmd.negate #= false; cmd.vxrm #= 0
      dut.io.debug #= true
      dut.io.access.valid #= false
      dut.io.access.address #= 0; dut.io.access.data #= 0; dut.io.access.mask #= 0
      dut.io.readAddress #= 0
      dut.clockDomain.deassertReset(); sleep(1)
      dut.clockDomain.assertReset(); sleep(1); tick()
      dut.clockDomain.deassertReset()
      for (_ <- 0 until p.wordCount) tick()
      assert(!dut.io.initHold.toBoolean)
      def upload(regs: Seq[Int]): Unit = {
        dut.io.debug #= true
        for (r <- regs; k <- 0 until p.beatsPerReg) {
          dut.io.access.valid #= true
          dut.io.access.address #= r * p.beatsPerReg + k
          dut.io.access.mask #= 255
          dut.io.access.data #= word(r, k * 8)
          tick()
        }
        dut.io.access.valid #= false
      }
      upload(0 until 32)

      var cases, writes, overlaps, passCases, wide64 = 0
      val saturations = scala.collection.mutable.Map[Boolean, Int]().withDefaultValue(0)
      val increments = scala.collection.mutable.Map[(Int, Int), Int]().withDefaultValue(0)
      def sext(v: BigInt, bits: Int): BigInt = if (v.testBit(bits - 1)) v - (BigInt(1) << bits) else v
      def element(reg: Int, i: Int, size: Int): BigInt = {
        val index = i * size
        val r = reg + index / vlenb
        val offset = index % vlenb
        (0 until size).map(b => BigInt(bytes(r)(offset + b)) << (8 * b)).reduce(_ | _)
      }
      def maskBit(i: Int) = ((bytes(0)(i / 8) >> (i % 8)) & 1) != 0

      // One instruction against the element model; writes, read ports and latency are checked cycle by cycle
      def execute(name: String, vector: Boolean, sew: Int, lmul: Int, vl: Int, masked: Boolean, vd: Int, vs2: Int, vs1: Int,
                  scalar: BigInt, vxrm: Int, readback: Boolean): Unit = {
        val (_, op, signedVs2, signedVs1, negate, _) = ops.find(_._1 == name).get
        val w = 8 << sew
        val size = w / 8
        val dsize = if (wide(name)) 2 * size else size
        val mask = (BigInt(1) << w) - 1
        var expectedSaturation = false
        val expected = ArrayBuffer[(Int, BigInt, Int)]()
        val actives = ArrayBuffer[Int]()
        val divide = op == DIV || op == REM
        val divideLatencies = ArrayBuffer[Int]()
        // A SEW-64 multiply reads its element in each of three passes; an inactive element takes one cycle
        val passes = if (sew == 3 && op <= SMUL) 3 else 1
        val starts = (0 until vl).scanLeft(0)((c, i) => c + (if (!masked || maskBit(i)) passes else 1))
        if (passes == 3) passCases += 1
        if (wide(name) && sew == 2) wide64 += 1
        // Element by element, each reading its sources before writing vd, as the spec's loop
        for (i <- 0 until vl if !masked || maskBit(i)) {
          actives += i
          val a = element(vs2, i, size)
          val b = if (vector) element(vs1, i, size) else sext(scalar, xlen) & mask // norm:vop-vx_rs1_sext_sewbits
          val d = element(vd, i, dsize)
          val sa = if (signedVs2) sext(a, w) else a
          val sb = if (signedVs1) sext(b, w) else b
          if (divide) divideLatencies += scala.math.max(8, ((sa.abs.bitLength + 7) / 8) * 8) + 3
          val value = name match {
            case "vdiv" | "vdivu" | "vrem" | "vremu" =>
              val q = if (sb == 0) BigInt(-1) else sa / sb // truncates towards zero; narrow overflow wraps
              if (op == REM) { if (sb == 0) sa else sa - q * sb } else q
            case "vmul" => a * b
            case "vmulh" | "vmulhu" | "vmulhsu" => (sa * sb) >> w // norm:vmul_vmulh_vmulhu_vmulhsu_op
            case "vmacc" => b * a + d // norm:vmacc_vnmsac_vmadd_vnmsub_op
            case "vnmsac" => d - b * a
            case "vmadd" => b * d + a
            case "vnmsub" => a - b * d
            case "vwmul" | "vwmulu" | "vwmulsu" => sa * sb // norm:vwmul_vwmulu_vwmulsu_op
            case _ if name.startsWith("vwmacc") => sb * sa + d // norm:vwmaccu_vwmacc_vwmaccsu_vwmaccus_op
            case "vsmul" =>
              // clip(roundoff_signed(vs2 * vs1, SEW - 1)) (norm:vsmul_op, norm:vxrm_op)
              val v = sa * sb
              val dropped = w - 1
              val low = v & ((BigInt(1) << dropped) - 1)
              val r = vxrm match {
                case 0 => if (v.testBit(dropped - 1)) 1 else 0
                case 1 => if (v.testBit(dropped - 1) && (low != (BigInt(1) << (dropped - 1)) || v.testBit(dropped))) 1 else 0
                case 2 => 0
                case 3 => if (!v.testBit(dropped) && low != 0) 1 else 0
              }
              increments((vxrm, r)) += 1
              val rounded = (v >> dropped) + r
              val high = (BigInt(1) << (w - 1)) - 1
              if (rounded > high) { expectedSaturation = true; high } else rounded
          }
          val bits = value & ((BigInt(1) << (8 * dsize)) - 1)
          val at = i * dsize
          for (b <- 0 until dsize) bytes(vd + (at + b) / vlenb)((at + b) % vlenb) = ((bits >> (8 * b)) & 255).toInt
          val pos = at % 8
          val address = vd * p.beatsPerReg + at / 8
          val data = bits << (8 * pos)
          expected += ((address, data, ((1 << dsize) - 1) << pos))
        }
        if (name == "vsmul") saturations(expectedSaturation) += 1
        dut.io.debug #= false
        cmd.valid #= true
        cmd.vd #= vd; cmd.vs1 #= vs1; cmd.vs2 #= vs2; cmd.vl #= vl; cmd.sew #= sew; cmd.op #= op
        cmd.scalar #= scalar; cmd.scalarOperand #= !vector; cmd.unmasked #= !masked
        cmd.signed #= signedVs2; cmd.signedVs1 #= signedVs1; cmd.negate #= negate; cmd.vxrm #= vxrm
        tick()
        cmd.valid #= false
        var cycles = 0
        val readCycles, writeCycles = ArrayBuffer[Int]()
        var writeIndex, divCommands = 0
        while (!dut.io.done.toBoolean && cycles < 120 * vlen) {
          sleep(1)
          assert(dut.io.busy.toBoolean, s"busy before done case $cases")
          if (dut.io.divCommand.toBoolean) divCommands += 1
          val rv = dut.io.readValid.toInt
          if (rv != 0) {
            val k = readCycles.size / passes
            assert(k < actives.size, s"read for an inactive element case $cases")
            val i = actives(k)
            if (!divide) assert(cycles == starts(i) + readCycles.size % passes, s"read cycle case $cases $name element $i")
            val want = 1 | (if (vector) 2 else 0) | (if (readsVd(name)) 4 else 0)
            assert(rv == want, s"read ports case $cases $name")
            assert(dut.io.readAddresses(0).toInt == vs2 * p.beatsPerReg + i * size / 8, s"vs2 word case $cases")
            if (vector) assert(dut.io.readAddresses(1).toInt == vs1 * p.beatsPerReg + i * size / 8, s"vs1 word case $cases")
            if (readsVd(name)) assert(dut.io.readAddresses(2).toInt == vd * p.beatsPerReg + i * dsize / 8, s"vd word case $cases")
            readCycles += cycles
          }
          if (dut.io.write.valid.toBoolean) {
            assert(writeIndex < expected.size, s"unexpected write case $cases")
            val (address, data, byteMask) = expected(writeIndex)
            assert(dut.io.write.address.toInt == address, s"address case $cases $name")
            assert(dut.io.write.mask.toInt == byteMask, s"byte enables case $cases $name")
            val bitMask = (0 until 8).filter(b => (byteMask & (1 << b)) != 0).map(b => BigInt(255) << (8 * b)).reduce(_ | _)
            assert((dut.io.write.data.toBigInt & bitMask) == data,
              s"data case $cases $name sew $sew lmul $lmul element ${actives(writeIndex)} vd $vd vs2 $vs2 vs1 $vs1")
            writeCycles += cycles
            writeIndex += 1; writes += 1
          }
          tick(); cycles += 1
        }
        assert(dut.io.done.toBoolean, s"timeout case $cases")
        assert(writeIndex == expected.size, s"missing writes case $cases $name")
        assert(divCommands == (if (divide) actives.size else 0), s"divider commands case $cases")
        if (divide) {
          for (k <- actives.indices) {
            assert(writeCycles(k) == readCycles(k) + divideLatencies(k), s"divide latency case $cases element $k")
            if (k > 0) assert(readCycles(k) == writeCycles(k-1) + actives(k) - actives(k-1) - 1, s"divide issue case $cases")
          }
          if (actives.isEmpty) assert(cycles == scala.math.max(vl, 1) + 2, s"inactive divider cycles case $cases: $cycles")
        } else {
          assert(readCycles.size == actives.size * passes, s"reads per element case $cases $name")
          assert(writeCycles.toSeq == actives.map(i => starts(i) + passes + 4).toSeq, s"write latency case $cases $name")
          // done follows the last element's issue by six cycles: 3 * VLMAX + 6 for an all-active SEW-64 multiply
          assert(cycles == scala.math.max(starts(vl), 1) + 5, s"done cycle case $cases $name: $cycles")
        }
        assert(dut.io.saturated.toBoolean == expectedSaturation, s"saturation case $cases $name sew $sew")
        sleep(1); tick()
        assert(!dut.io.busy.toBoolean, s"busy after done case $cases")
        if (readback) {
          dut.io.debug #= true
          for (a <- 0 until p.wordCount) {
            dut.io.readAddress #= a; tick()
            assert(dut.io.readData.toBigInt == word(a / p.beatsPerReg, (a % p.beatsPerReg) * 8), s"readback case $cases word $a")
          }
        }
        cases += 1
      }

      if (exhaustive) {
        // Every SEW-8 operand pair of every multiply and divide, and of vsmul at each vxrm: vs2 in v8, vs1 in v16, vd in v24
        for ((name, _, _, _, _, forms) <- ops; vxrm <- if (name == "vsmul") 0 to 3 else Seq(0)) {
          val lmul = if (wide(name)) 2 else 3
          val vlmax = (vlen / 8) << lmul
          if (forms.contains("vv")) for (chunk <- 0 until 65536 / vlmax) {
            for (e <- 0 until vlmax) {
              val n = chunk * vlmax + e
              bytes(8 + e / vlenb)(e % vlenb) = n & 255
              bytes(16 + e / vlenb)(e % vlenb) = n >> 8
            }
            for (r <- 24 until 32; b <- 0 until vlenb) bytes(r)(b) = random.nextInt(256)
            upload((8 until 24) ++ (24 until 32))
            execute(name, true, 0, lmul, vlmax, false, 24, 8, 16, BigInt(xlen, random), vxrm, false)
          }
          if (!forms.contains("vv") || name.startsWith("vdiv") || name.startsWith("vrem")) for (s <- 0 until 256; chunk <- 0 until 256 / vlmax) {
            for (e <- 0 until vlmax) bytes(8 + e / vlenb)(e % vlenb) = chunk * vlmax + e
            upload(8 until 16)
            execute(name, false, 0, lmul, vlmax, false, 24, 8, 16, (BigInt(xlen - 8, random) << 8) | s, vxrm, false)
          }
        }
        println(s"VectorElementUnit exhaustive SEW 8: $cases instructions, $writes element writes, vsmul saturation $saturations")
      } else {

      // Random operands at every SEW and LMUL, with limit values in about half the elements
      // At ELEN 64, SEW-64 divides belong to the divider; vmulh* and vsmul at SEW 64 are reserved
      val sews = if (elen == 64) 0 to 3 else 0 to 2
      for ((name, op, _, _, _, forms) <- ops; form <- forms.split(" "); sew <- sews; lmul <- -3 to 3
           if (8 << sew) <= elen * scala.math.pow(2, lmul) && (!wide(name) || (16 << sew) <= elen && lmul <= 2) &&
             (sew < 3 || op == MUL || op == MACC || op == MADD)) {
        val w = 8 << sew
        for (reg <- 8 until 32; e <- 0 until vlenb / (w / 8) if random.nextBoolean()) {
          val v = Seq(BigInt(0), BigInt(1), (BigInt(1) << (w - 1)) - 1, BigInt(1) << (w - 1), (BigInt(1) << w) - 1)(random.nextInt(5))
          for (b <- 0 until w / 8) bytes(reg)(e * w / 8 + b) = ((v >> (8 * b)) & 255).toInt
        }
        for (b <- 0 until vlenb) bytes(0)(b) = random.nextInt(256)
        upload(0 until 32)
        val vlmax = ((vlen / w) * scala.math.pow(2, lmul)).toInt
        val group = 1 << scala.math.max(lmul, 0)
        val dgroup = if (wide(name)) 1 << scala.math.max(lmul + 1, 0) else group
        for (vl <- Seq(0, 1, vlmax - 1, vlmax).distinct; masked <- Seq(false, true)) {
          def pick(g: Int) = random.nextInt(32 / g) * g
          var vd = pick(dgroup)
          while (masked && vd == 0) vd = pick(dgroup)
          // Same-EEW sources overlap vd freely; a widening source only in the highest part of vd, with EMUL >= 1
          def source(): Int = if (!wide(name)) { if (random.nextInt(3) == 0) vd else pick(group) }
            else if (lmul >= 0 && random.nextInt(3) == 0) vd + dgroup - group
            else Iterator.continually(pick(group)).find(r => r + group <= vd || r >= vd + dgroup).get
          val (vs2, vs1) = (source(), source())
          if (vs2 >= vd && vs2 < vd + dgroup || form == "vv" && vs1 >= vd && vs1 < vd + dgroup) overlaps += 1
          execute(name, form == "vv", sew, lmul, vl, masked, vd, vs2, vs1, BigInt(xlen, random), random.nextInt(4), true)
        }
      }
      for (vxrm <- 0 to 3; r <- 0 to 1) assert((increments((vxrm, r)) > 0) == (vxrm != 2 || r == 0), s"vsmul vxrm $vxrm increment $r")
      assert((passCases > 0) == (elen == 64) && (wide64 > 0) == (elen == 64), s"SEW-64 cases $passCases, widening into 64 $wide64")
      println(s"VectorElementUnit VLEN=$vlen XLEN=$xlen ELEN=$elen: $cases cases ($passCases three-pass, $wide64 widening into 64), " +
        s"$writes element writes, $overlaps with a source in vd, " +
        s"vsmul saturation $saturations, rounding increments ${increments.toSeq.sorted.mkString(" ")}, full physical readback after every case")
      }
    }
  }
  test("every SEW-8 operand pair") { run(128, 32, true) }
  test("random operands and overlaps on RV32") { run(128, 32, false) }
  test("scalar truncation on RV64") { run(128, 64, false) }
  test("larger vector register addressing") { run(256, 64, false) }
  test("SEW-64 multiplies and widening into 64 on RV32") { run(128, 32, false, 64) }
  test("SEW-64 multiplies and widening into 64 on RV64") { run(128, 64, false, 64) }
  test("SEW-64 multiplies at VLEN 256") { run(256, 64, false, 64) }
}
