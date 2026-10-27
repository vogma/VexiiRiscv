package vexiiriscv.execute.vector

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.fetch.InitService
import vexiiriscv.Global
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.execute._
import vexiiriscv.execute.lsu.LsuPlugin
import vexiiriscv.misc.{InflightService, PrivilegedPlugin, TrapPlugin, TrapReason, TrapService}
import vexiiriscv.riscv.Riscv._
import vexiiriscv.riscv._
import vexiiriscv.schedule.{DispatchPlugin, ReschedulePlugin}

// EMUL class of a register operand, checked for group alignment at E0
object VectorEmul {
  val NONE = 0 // scalar, single register or no operand
  val L = 1    // EMUL = LMUL
  val L2 = 2   // EMUL = LMUL / 2
  val L4 = 3   // EMUL = LMUL / 4
  val N = 4    // EMUL = NREG of vmv<nr>r.v
  val W = 5    // EMUL = 2 * LMUL
  val INDEX16 = 6 // EEW 16 gather indices
  def width = 3
}

// Allowed overlap of vd with a source, checked at E0
object VectorOverlap {
  val ANY = 0
  val WIDEN = 1 // a smaller vs2 group overlaps vd only in its highest part; a fractional one not at all
  val BASE = 2  // a smaller vd (mask or narrowing) overlaps the source group only at its base
  val DISJOINT = 3 // no destination register may overlap the source
  def width = 2
}

// SEW range of an instruction, checked at E0
object VectorRange {
  val ANY = 0
  val EXT2 = 1 // SEW >= 16; the source EMUL is always legal for a legal vtype
  val EXT4 = 2 // SEW >= 32
  val WIDE = 3 // SEW <= ELEN / 2 and LMUL <= 4
  val WIDE_SCALAR = 4 // SEW <= ELEN / 2 for a single 2 * SEW element, whatever LMUL
  val NO64 = 5 // SEW < 64 (norm:Zve64_eew64_nsupport_vmulh, norm:Zve64_eew64_nsupport_vsmul)
  def width = 3
}

case class VectorMemoryDescriptor(indexWidth : Int, split : Boolean = false) extends Bundle {
  val index = UInt(indexWidth bits)
  val field = UInt(3 bits)
  val last, access = Bool()
  val half = split generate Bool() // upper word of an 8-byte element
}

/**
 * Vector instructions on one lane layer. Each one waits until every older instruction left the pipeline, is checked
 * against vtype/vstart at E0 and then does all its work there; vset* complete in that cycle.
 * Loads and stores instead issue one LSU micro-op per field of each active element and complete with the last one.
 * withMemory = false leaves the LSU element port to another sequencer. ELEN 64 adds SEW 64 and the EEW-64 memory forms;
 * with a 32-bit LSU each 8-byte element then takes two micro-ops, low word first.
 */
class VectorPlugin(val layer : LaneLayer, val dlen : Int = 64, var withMemory : Boolean = true, val elen : Int = 32) extends FiberPlugin with InitService {
  require(elen == 32 || elen == 64)
  override def initHold(): Bool = regFile.vrf.io.initHold

  // Initialization must be available before the trap controller finishes building.
  val regFile = during build new Area {
    val vrf = new VectorRegFile(VectorRegFileParam(vlen = host[VectorCsrService].vlen, dlen = dlen))
  }

  val SEL = Payload(Bool())
  val VECTOR = Payload(Bool()) // Decode stage, illegal while mstatus.VS is Off
  val VSET = Payload(Bool())
  val VSET_IMM = Payload(Bool()) // vsetivli: AVL from uimm, vtype from zimm10
  val VSET_REG = Payload(Bool()) // vsetvl: vtype from rs2
  val ARITH, SCALAR_OPERAND, IMMEDIATE, SIGNED, SATURATE, MASK_DATA = Payload(Bool())
  val ELEMENT, SIGNED_VS1, NEGATE = Payload(Bool()) // element unit; vs1 or scalar signed; product subtracted
  val ELEMENT_OP = Payload(UInt(VectorElementOp.width bits))
  val SHAPE = Payload(UInt(VectorShape.width bits))
  val ALU_OP = Payload(UInt(VectorAluOp.width bits))
  val RESULT = Payload(Bits(XLEN bits))

  // E0 legality checks, each enabled per uop
  val CHECK_VILL = Payload(Bool())
  val CHECK_VSTART = Payload(Bool())
  val EMUL_VD, EMUL_VS1, EMUL_VS2 = Payload(UInt(VectorEmul.width bits)) // register group aligned to its EMUL
  val MASK_NOT_V0 = Payload(Bool()) // vm = 0 with vd = v0 is reserved
  val OVERLAP_VS1, OVERLAP_VS2 = Payload(UInt(VectorOverlap.width bits))
  val RANGE = Payload(UInt(VectorRange.width bits))
  val MEM = Payload(Bool()) // Element micro-op through the shared LSU
  val UNSIGNED_IMMEDIATE = Payload(Bool()) // shift and clip amounts are zero-extended (norm:vsll_vsrl_vsra_shamt)

  val logic = during setup new Area {
    val elp = layer.lane
    val ifp = host.find[IntFormatPlugin](_.lane == elp)
    val dp = host[DispatchPlugin]
    val ds = host[DecoderService]
    val sp = host[ReschedulePlugin]
    val ts = host[TrapService]
    val vcs = host[VectorCsrService]
    val lsu = host[LsuPlugin]
    val ioRetainer = retains(elp.uopLock, ifp.elaborationLock, dp.elaborationLock, ds.elaborationLock, sp.elaborationLock, ts.trapLock)
    val lsuRetainer = withMemory generate retains(lsu.vectorLsuLock)
    val buildBefore = retains(elp.pipelineLock)
    awaitBuild()
    val lsuPort = withMemory generate lsu.newVectorLsuPort()
    if (withMemory) lsuRetainer.release()

    val checks = List(VSET, VSET_IMM, VSET_REG, CHECK_VILL, CHECK_VSTART, MASK_NOT_V0, MEM,
      ARITH, SCALAR_OPERAND, IMMEDIATE, SIGNED, SATURATE, MASK_DATA, ELEMENT, SIGNED_VS1, NEGATE)
    elp.setDecodingDefault(SEL, False)
    elp.setDecodingDefault(ALU_OP, U(VectorAluOp.ADD, VectorAluOp.width bits))
    elp.setDecodingDefault(SHAPE, U(VectorShape.SINGLE, VectorShape.width bits))
    elp.setDecodingDefault(ELEMENT_OP, U(VectorElementOp.MUL, VectorElementOp.width bits))
    for (key <- List(OVERLAP_VS1, OVERLAP_VS2)) elp.setDecodingDefault(key, U(VectorOverlap.ANY, VectorOverlap.width bits))
    elp.setDecodingDefault(RANGE, U(VectorRange.ANY, VectorRange.width bits))
    for (key <- List(EMUL_VD, EMUL_VS1, EMUL_VS2)) elp.setDecodingDefault(key, U(VectorEmul.NONE, VectorEmul.width bits))
    for (key <- checks) elp.setDecodingDefault(key, False)
    ds.addMicroOpDecodingDefault(VECTOR, False)
    if (elen == 64) elp.setDecodingDefault(UNSIGNED_IMMEDIATE, False)
    val shiftNames = Set("vsll", "vsrl", "vsra", "vssrl", "vssra", "vnsrl", "vnsra", "vnclipu", "vnclip")

    val wb = ifp.access(1)
    def add(op : MicroOp, decodings : (Payload[_ <: BaseType], Any)*) : UopLayerSpec = {
      val spec = layer.add(op)
      spec.addDecoding(SEL -> True)
      spec.addDecoding(decodings)
      ds.addMicroOpDecoding(op, VECTOR, True)
      dp.fenceOlder(op)
      spec.mayFlushUpTo(0)
      spec.dontFlushFrom(0)
      op.resources.foreach {
        case RfResource(IntRegFile, rs : RfRead) => spec.addRsSpec(rs, 0)
        case _ =>
      }
      if (op.resources.contains(IntRegFile -> RD)) ifp.addMicroOp(wb, spec) else spec.setCompletion(1)
      spec
    }

    add(Rvv.VSETVLI, VSET -> True)
    add(Rvv.VSETIVLI, VSET -> True, VSET_IMM -> True)
    add(Rvv.VSETVL, VSET -> True, VSET_REG -> True)

    def emul(cls : Int) = U(cls, VectorEmul.width bits)
    def shape(s : Int) = U(s, VectorShape.width bits)
    def alu(op : Int) = U(op, VectorAluOp.width bits)
    // Single-width forms: name, ALU operation, saturating, signed; the vvm/vxm/vim forms take v0 as carry or select
    val single = Seq(
      ("vadd", VectorAluOp.ADD, false, false), ("vsub", VectorAluOp.SUB, false, false), ("vrsub", VectorAluOp.RSUB, false, false),
      ("vand", VectorAluOp.AND, false, false), ("vor", VectorAluOp.OR, false, false), ("vxor", VectorAluOp.XOR, false, false),
      ("vmv.v", VectorAluOp.MOVE, false, false),
      ("vsaddu", VectorAluOp.ADD, true, false), ("vsadd", VectorAluOp.ADD, true, true),
      ("vssubu", VectorAluOp.SUB, true, false), ("vssub", VectorAluOp.SUB, true, true),
      ("vminu", VectorAluOp.MIN, false, false), ("vmin", VectorAluOp.MIN, false, true),
      ("vmaxu", VectorAluOp.MAX, false, false), ("vmax", VectorAluOp.MAX, false, true),
      ("vsll", VectorAluOp.SLL, false, false), ("vsrl", VectorAluOp.SRL, false, false), ("vsra", VectorAluOp.SRL, false, true),
      ("vadc", VectorAluOp.ADD, false, false), ("vsbc", VectorAluOp.SUB, false, false), ("vmerge", VectorAluOp.MERGE, false, false),
      ("vaaddu", VectorAluOp.AADD, false, false), ("vaadd", VectorAluOp.AADD, false, true),
      ("vasubu", VectorAluOp.ASUB, false, false), ("vasub", VectorAluOp.ASUB, false, true),
      ("vssrl", VectorAluOp.SSRL, false, false), ("vssra", VectorAluOp.SSRL, false, true))
    for ((name, op, saturate, signed) <- single; form <- Seq("vv", "vx", "vi", "vvm", "vxm", "vim")
         if Rvv.all.contains(s"$name.$form") || name == "vmv.v" && form.length == 2) {
      val mnemonic = if (name == "vmv.v") s"vmv.v.${form.last}" else s"$name.$form"
      val vector = form.startsWith("vv")
      val spec = add(Rvv.all(mnemonic), ARITH -> True, ALU_OP -> alu(op), SATURATE -> Bool(saturate), SIGNED -> Bool(signed),
        SCALAR_OPERAND -> Bool(!vector), IMMEDIATE -> Bool(form.startsWith("vi")), MASK_DATA -> Bool(form.length == 3),
        CHECK_VILL -> True, CHECK_VSTART -> True, EMUL_VD -> emul(VectorEmul.L),
        EMUL_VS1 -> emul(if (vector) VectorEmul.L else VectorEmul.NONE),
        EMUL_VS2 -> emul(if (name != "vmv.v") VectorEmul.L else VectorEmul.NONE), MASK_NOT_V0 -> Bool(name != "vmv.v"))
      if (elen == 64 && shiftNames(name) && form == "vi") spec.addDecoding(UNSIGNED_IMMEDIATE -> True)
    }
    // Extensions: vs2 at SEW / f and EMUL = LMUL / f (norm:vsext_vzext_vs_eew_emul)
    for (f <- Seq(2, 4); signed <- Seq(false, true)) {
      val (s, cls, range) = if (f == 2) (VectorShape.EXT2, VectorEmul.L2, VectorRange.EXT2) else (VectorShape.EXT4, VectorEmul.L4, VectorRange.EXT4)
      add(Rvv.all(s"v${if (signed) "s" else "z"}ext.vf$f"), ARITH -> True, SHAPE -> shape(s), SIGNED -> Bool(signed),
        CHECK_VILL -> True, CHECK_VSTART -> True, EMUL_VD -> emul(VectorEmul.L), EMUL_VS2 -> emul(cls), MASK_NOT_V0 -> True,
        OVERLAP_VS2 -> U(VectorOverlap.WIDEN, VectorOverlap.width bits), RANGE -> U(range, VectorRange.width bits))
    }
    // Mask results: compares, then carry and borrow out with v0 as carry or borrow in for the vvm/vxm/vim forms
    def overlap(o : Int) = U(o, VectorOverlap.width bits)
    val compares = Seq(
      ("vmseq", VectorAluOp.SEQ, false), ("vmsne", VectorAluOp.SNE, false), ("vmsltu", VectorAluOp.SLT, false),
      ("vmslt", VectorAluOp.SLT, true), ("vmsleu", VectorAluOp.SLE, false), ("vmsle", VectorAluOp.SLE, true),
      ("vmsgtu", VectorAluOp.SGT, false), ("vmsgt", VectorAluOp.SGT, true),
      ("vmadc", VectorAluOp.ADD, false), ("vmsbc", VectorAluOp.SUB, false))
    for ((name, op, signed) <- compares; form <- Seq("vv", "vx", "vi", "vvm", "vxm", "vim") if Rvv.all.contains(s"$name.$form")) {
      val vector = form.startsWith("vv")
      add(Rvv.all(s"$name.$form"), ARITH -> True, SHAPE -> shape(VectorShape.MASK_DST), ALU_OP -> alu(op), SIGNED -> Bool(signed),
        SCALAR_OPERAND -> Bool(!vector), IMMEDIATE -> Bool(form.startsWith("vi")), MASK_DATA -> Bool(form.length == 3),
        CHECK_VILL -> True, CHECK_VSTART -> True, EMUL_VS1 -> emul(if (vector) VectorEmul.L else VectorEmul.NONE),
        EMUL_VS2 -> emul(VectorEmul.L), OVERLAP_VS2 -> overlap(VectorOverlap.BASE),
        OVERLAP_VS1 -> overlap(if (vector) VectorOverlap.BASE else VectorOverlap.ANY))
    }
    // Mask logicals: single registers whatever LMUL, unmasked by encoding (norm:vmask_maskreg_def)
    val logicals = Seq(("vmand", VectorAluOp.AND), ("vmnand", VectorAluOp.NAND), ("vmandn", VectorAluOp.ANDN),
      ("vmxor", VectorAluOp.XOR), ("vmor", VectorAluOp.OR), ("vmnor", VectorAluOp.NOR), ("vmorn", VectorAluOp.ORN),
      ("vmxnor", VectorAluOp.XNOR))
    for ((name, op) <- logicals)
      add(Rvv.all(s"$name.mm"), ARITH -> True, SHAPE -> shape(VectorShape.MASK_MM), ALU_OP -> alu(op), CHECK_VILL -> True, CHECK_VSTART -> True)
    // Whole-register moves ignore vtype except vill (norm:vmv-nr-r_op); vid.v reads no register
    for ((_, op) <- Rvv.VMVR)
      add(op, ARITH -> True, SHAPE -> shape(VectorShape.WHOLE), CHECK_VILL -> True, CHECK_VSTART -> True,
        EMUL_VD -> emul(VectorEmul.N), EMUL_VS2 -> emul(VectorEmul.N))
    add(Rvv.all("vid.v"), ARITH -> True, ALU_OP -> alu(VectorAluOp.INDEX), CHECK_VILL -> True, CHECK_VSTART -> True,
      EMUL_VD -> emul(VectorEmul.L), MASK_NOT_V0 -> True)
    // Widening add/subtract: vd at 2 * SEW, vs2 at SEW or, for the .w forms, at 2 * SEW (norm:vwaddu_vwadd_vwsubu_vwsub_op)
    for ((name, op, signed) <- Seq(("vwaddu", VectorAluOp.ADD, false), ("vwadd", VectorAluOp.ADD, true),
         ("vwsubu", VectorAluOp.SUB, false), ("vwsub", VectorAluOp.SUB, true)); form <- Seq("vv", "vx", "wv", "wx")) {
      val vector = form.endsWith("v")
      val wideVs2 = form.startsWith("w")
      add(Rvv.all(s"$name.$form"), ARITH -> True, SHAPE -> shape(if (wideVs2) VectorShape.WIDE_WV else VectorShape.WIDE_VV),
        ALU_OP -> alu(op), SIGNED -> Bool(signed), SCALAR_OPERAND -> Bool(!vector), CHECK_VILL -> True, CHECK_VSTART -> True,
        EMUL_VD -> emul(VectorEmul.W), EMUL_VS1 -> emul(if (vector) VectorEmul.L else VectorEmul.NONE),
        EMUL_VS2 -> emul(if (wideVs2) VectorEmul.W else VectorEmul.L), MASK_NOT_V0 -> True,
        OVERLAP_VS2 -> overlap(if (wideVs2) VectorOverlap.ANY else VectorOverlap.WIDEN),
        OVERLAP_VS1 -> overlap(if (vector) VectorOverlap.WIDEN else VectorOverlap.ANY), RANGE -> U(VectorRange.WIDE, VectorRange.width bits))
    }
    // Narrowing shifts and clips: vs2 at 2 * SEW, shifted right by the low lg2(2 * SEW) bits of vs1, rs1 or uimm
    for ((name, op, signed, saturate) <- Seq(("vnsrl", VectorAluOp.SRL, false, false), ("vnsra", VectorAluOp.SRL, true, false),
         ("vnclipu", VectorAluOp.SSRL, false, true), ("vnclip", VectorAluOp.SSRL, true, true)); form <- Seq("wv", "wx", "wi")) {
      val vector = form == "wv"
      val spec = add(Rvv.all(s"$name.$form"), ARITH -> True, SHAPE -> shape(VectorShape.NARROW), ALU_OP -> alu(op),
        SIGNED -> Bool(signed), SATURATE -> Bool(saturate), SCALAR_OPERAND -> Bool(!vector), IMMEDIATE -> Bool(form == "wi"),
        CHECK_VILL -> True, CHECK_VSTART -> True, EMUL_VD -> emul(VectorEmul.L),
        EMUL_VS1 -> emul(if (vector) VectorEmul.L else VectorEmul.NONE), EMUL_VS2 -> emul(VectorEmul.W), MASK_NOT_V0 -> True,
        OVERLAP_VS2 -> overlap(VectorOverlap.BASE), RANGE -> U(VectorRange.WIDE, VectorRange.width bits))
      if (elen == 64 && form == "wi") spec.addDecoding(UNSIGNED_IMMEDIATE -> True)
    }
    // Element arithmetic: name, operation, vs2 signed, vs1 or scalar signed, product subtracted
    val elementOps = Seq(
      ("vmul", VectorElementOp.MUL, false, false, false), ("vmulh", VectorElementOp.MULH, true, true, false),
      ("vmulhu", VectorElementOp.MULH, false, false, false), ("vmulhsu", VectorElementOp.MULH, true, false, false),
      ("vmacc", VectorElementOp.MACC, false, false, false), ("vnmsac", VectorElementOp.MACC, false, false, true),
      ("vmadd", VectorElementOp.MADD, false, false, false), ("vnmsub", VectorElementOp.MADD, false, false, true),
      ("vwmul", VectorElementOp.WMUL, true, true, false), ("vwmulu", VectorElementOp.WMUL, false, false, false),
      ("vwmulsu", VectorElementOp.WMUL, true, false, false), ("vwmacc", VectorElementOp.WMACC, true, true, false),
      ("vwmaccu", VectorElementOp.WMACC, false, false, false), ("vwmaccsu", VectorElementOp.WMACC, false, true, false),
      ("vwmaccus", VectorElementOp.WMACC, true, false, false), ("vsmul", VectorElementOp.SMUL, true, true, false),
      ("vdiv", VectorElementOp.DIV, true, true, false), ("vdivu", VectorElementOp.DIV, false, false, false),
      ("vrem", VectorElementOp.REM, true, true, false), ("vremu", VectorElementOp.REM, false, false, false))
    for ((name, op, signed, signedVs1, negate) <- elementOps; form <- Seq("vv", "vx") if Rvv.all.contains(s"$name.$form")) {
      val vector = form == "vv"
      val wide = op == VectorElementOp.WMUL || op == VectorElementOp.WMACC
      add(Rvv.all(s"$name.$form"), ELEMENT -> True, ELEMENT_OP -> U(op, VectorElementOp.width bits), SIGNED -> Bool(signed),
        SIGNED_VS1 -> Bool(signedVs1), NEGATE -> Bool(negate), SCALAR_OPERAND -> Bool(!vector),
        CHECK_VILL -> True, CHECK_VSTART -> True, EMUL_VD -> emul(if (wide) VectorEmul.W else VectorEmul.L),
        EMUL_VS1 -> emul(if (vector) VectorEmul.L else VectorEmul.NONE), EMUL_VS2 -> emul(VectorEmul.L), MASK_NOT_V0 -> True,
        OVERLAP_VS2 -> overlap(if (wide) VectorOverlap.WIDEN else VectorOverlap.ANY),
        OVERLAP_VS1 -> overlap(if (wide && vector) VectorOverlap.WIDEN else VectorOverlap.ANY),
        RANGE -> U(if (wide) VectorRange.WIDE else if (elen == 64 && (op == VectorElementOp.MULH || op == VectorElementOp.SMUL))
          VectorRange.NO64 else VectorRange.ANY, VectorRange.width bits))
    }
    // Reductions into element 0 of vd: only vs2 is a group; vd = v0 is legal when masked (norm:vreduction_vd_overlap_vs)
    for ((name, op, signed) <- Seq(("vredsum", VectorAluOp.ADD, false), ("vredand", VectorAluOp.AND, false),
         ("vredor", VectorAluOp.OR, false), ("vredxor", VectorAluOp.XOR, false), ("vredminu", VectorAluOp.MIN, false),
         ("vredmin", VectorAluOp.MIN, true), ("vredmaxu", VectorAluOp.MAX, false), ("vredmax", VectorAluOp.MAX, true),
         ("vwredsumu", VectorAluOp.ADD, false), ("vwredsum", VectorAluOp.ADD, true))) {
      val wide = name.startsWith("vw")
      add(Rvv.all(s"$name.vs"), ARITH -> True, SHAPE -> shape(if (wide) VectorShape.WREDUCE else VectorShape.REDUCE),
        ALU_OP -> alu(op), SIGNED -> Bool(signed), CHECK_VILL -> True, CHECK_VSTART -> True, EMUL_VS2 -> emul(VectorEmul.L),
        RANGE -> U(if (wide) VectorRange.WIDE_SCALAR else VectorRange.ANY, VectorRange.width bits))
    }
    // Mask prefix results are single registers; iota writes an LMUL group (norm:viota_vreg_constr)
    for ((name, op) <- Seq(("vmsbf", VectorAluOp.MSBF), ("vmsif", VectorAluOp.MSIF), ("vmsof", VectorAluOp.MSOF)))
      add(Rvv.all(s"$name.m"), ARITH -> True, SHAPE -> shape(VectorShape.MASK_MM), ALU_OP -> alu(op),
        CHECK_VILL -> True, CHECK_VSTART -> True, MASK_NOT_V0 -> True, OVERLAP_VS2 -> overlap(VectorOverlap.DISJOINT))
    for ((name, op) <- Seq(("vcpop", VectorElementOp.CPOP), ("vfirst", VectorElementOp.FIRST), ("viota", VectorElementOp.IOTA))) {
      val iota = op == VectorElementOp.IOTA
      add(Rvv.all(s"$name.m"), ELEMENT -> True, ELEMENT_OP -> U(op, VectorElementOp.width bits),
        CHECK_VILL -> True, CHECK_VSTART -> True, MASK_NOT_V0 -> Bool(iota),
        EMUL_VD -> emul(if (iota) VectorEmul.L else VectorEmul.NONE),
        OVERLAP_VS2 -> overlap(if (iota) VectorOverlap.DISJOINT else VectorOverlap.ANY))
    }
    // Permutes share element writes; only downward slides allow source/destination overlap.
    for ((name, op) <- Seq(("vslideup", VectorElementOp.SLIDEUP), ("vslidedown", VectorElementOp.SLIDEDOWN),
         ("vslide1up", VectorElementOp.SLIDE1UP), ("vslide1down", VectorElementOp.SLIDE1DOWN),
         ("vrgather", VectorElementOp.GATHER), ("vrgatherei16", VectorElementOp.GATHEREI16),
         ("vcompress", VectorElementOp.COMPRESS)); form <- Seq("vv", "vx", "vi", "vm") if Rvv.all.contains(s"$name.$form")) {
      val gather = name.startsWith("vrgather")
      val compress = name == "vcompress"
      val vector = form == "vv"
      val disjoint = gather || compress || name == "vslideup" || name == "vslide1up"
      add(Rvv.all(s"$name.$form"), ELEMENT -> True, ELEMENT_OP -> U(op, VectorElementOp.width bits),
        SCALAR_OPERAND -> Bool(!vector && !compress), IMMEDIATE -> Bool(form == "vi"),
        CHECK_VILL -> True, CHECK_VSTART -> True, EMUL_VD -> emul(VectorEmul.L), EMUL_VS2 -> emul(VectorEmul.L),
        EMUL_VS1 -> emul(if (name == "vrgatherei16") VectorEmul.INDEX16 else if (vector) VectorEmul.L else VectorEmul.NONE),
        MASK_NOT_V0 -> True, OVERLAP_VS2 -> overlap(if (disjoint) VectorOverlap.DISJOINT else VectorOverlap.ANY),
        OVERLAP_VS1 -> overlap(if (vector || compress) VectorOverlap.DISJOINT else VectorOverlap.ANY))
    }
    add(Rvv.all("vmv.x.s"), ARITH -> True, SHAPE -> shape(VectorShape.SCALAR_READ), CHECK_VILL -> True, CHECK_VSTART -> True)
    add(Rvv.all("vmv.s.x"), ARITH -> True, ALU_OP -> alu(VectorAluOp.MOVE),
      SCALAR_OPERAND -> True, SHAPE -> shape(VectorShape.SCALAR_WRITE), CHECK_VILL -> True, CHECK_VSTART -> True)

    // The LSU frontend owns these uops (completion, flushes, rs1); masked loads must not overwrite v0
    val memoryForms = VectorMemoryOps(elen, XLEN.get)
    val memoryOps = memoryForms.all
    if (withMemory) {
      for ((op, load) <- memoryOps) layer.add(op).addDecoding(SEL -> True, MEM -> True,
        CHECK_VILL -> Bool(!memoryForms.whole.exists(_._1 == op)), MASK_NOT_V0 -> Bool(load))
      dp.sequence(memoryOps.map(_._1))
    }

    ds.addIllegalCheck(ctrl => ctrl(VECTOR) && !vcs.vectorEnable)

    val age = elp.getExecuteAge(0)
    val flushPort = sp.newFlushPort(age, laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
    val trapPort = ts.newTrap(age, Execute.LANE_AGE_WIDTH)
    ioRetainer.release()

    val vlen = vcs.vlen
    val vlWidth = vcs.vlWidth
    val vrf = regFile.vrf
    val arithmetic = new VectorArithmetic(vrf.p, XLEN, elen)
    val element = new VectorElementUnit(vrf.p, XLEN)
    vrf.io.write := arithmetic.io.write
    when(element.io.busy) { vrf.io.write := element.io.write }
    arithmetic.io.v0 := vrf.io.v0
    element.io.v0 := vrf.io.v0
    for (i <- 0 until 3) {
      arithmetic.io.reads(i).rsp := vrf.io.reads(i).rsp
      element.io.reads(i).rsp := vrf.io.reads(i).rsp
    }
    assert(!(arithmetic.io.busy && element.io.busy), "both vector units busy")
    val indexRead = Flow(UInt(vrf.p.addressWidth bits))
    if (!withMemory) indexRead.setIdle()
    vrf.io.reads(0).cmd := indexRead
    when(arithmetic.io.busy) { vrf.io.reads(0).cmd := arithmetic.io.reads(0).cmd }
    when(element.io.busy) { vrf.io.reads(0).cmd := element.io.reads(0).cmd }
    vrf.io.reads(1).cmd := arithmetic.io.reads(1).cmd
    when(element.io.busy) { vrf.io.reads(1).cmd := element.io.reads(1).cmd }
    // Port C belongs to a unit from its command until done, otherwise to the memory side
    val storeRead = Flow(UInt(vrf.p.addressWidth bits))
    if (!withMemory) storeRead.setIdle()
    vrf.io.reads(2).cmd := storeRead
    when(arithmetic.io.busy) { vrf.io.reads(2).cmd := arithmetic.io.reads(2).cmd }
    when(element.io.busy) { vrf.io.reads(2).cmd := element.io.reads(2).cmd }
    for (port <- vrf.io.reads.drop(3)) {
      port.cmd.valid := False
      port.cmd.payload.assignDontCare()
    }

    // log2(VLMAX) = log2(VLEN) - log2(SEW) + log2(LMUL), only meaningful for a legal vtype
    def vlmaxOf(vsew : UInt, vlmul : Bits) : UInt = {
      val shift = S(log2Up(vlen) - 3, 5 bits) + vlmul.asSInt.resize(5) - vsew.resize(5).asSInt
      (U(1, vlWidth bits) << shift.asUInt.resize(log2Up(vlWidth))).resize(vlWidth)
    }
    // Element size from the memory width code: 8, 16, 32 or, with ELEN 64, 64 bits
    def sizeOf(uop : Bits) : UInt = if (elen == 64)
      uop(Const.funct3Range).mux(B"000" -> U(0, 2 bits), B"101" -> U(1, 2 bits), B"111" -> U(3, 2 bits), default -> U(2, 2 bits))
    else uop(Const.funct3Range).mux(B"000" -> U(0, 2 bits), B"101" -> U(1, 2 bits), default -> U(2, 2 bits))
    // log2 of the memory EEW in bits
    def eewLog2Of(width : Bits) : SInt = if (elen == 64)
      width.mux(B"101" -> S(4, 5 bits), B"110" -> S(5, 5 bits), B"111" -> S(6, 5 bits), default -> S(3, 5 bits))
    else width.mux(B"101" -> S(4, 5 bits), B"110" -> S(5, 5 bits), default -> S(3, 5 bits))
    def indexed(uop : Bits) : Bool = uop(26)
    def strided(uop : Bits) : Bool = uop(27 downto 26) === B"10"
    def whole(uop : Bits) : Bool = uop(27 downto 26) === 0 && uop(Const.rs2Range) === 8
    def maskMemory(uop : Bits) : Bool = uop(27 downto 26) === 0 && uop(Const.rs2Range) === 11
    def faultOnlyFirst(uop : Bits) : Bool = uop(27 downto 26) === 0 && uop(Const.rs2Range) === 16
    def dataSize(uop : Bits) : UInt = indexed(uop) ? vcs.vtype.vsew.resize(2) | sizeOf(uop)
    // Fields per element minus one; whole-register transfers use nf as the register count
    def fieldsOf(uop : Bits) : UInt = whole(uop) ? U(0, 3 bits) | uop(31 downto 29).asUInt
    // log2 of the data EMUL: LMUL for indexed, EEW / SEW * LMUL otherwise (norm:vector_ls_indexed_op)
    def dataLog2Of(uop : Bits) : SInt = {
      val lmul = vcs.vtype.vlmul.asSInt.resize(5)
      val eewLog2 = eewLog2Of(uop(Const.funct3Range))
      indexed(uop) ? lmul | (eewLog2 - S(3, 5 bits) - vcs.vtype.vsew.resize(5).asSInt + lmul)
    }
    // Field f of a segment lives in register vd + f * max(EMUL, 1)
    def fieldReg(uop : Bits, field : UInt) : UInt = {
      val log2 = dataLog2Of(uop)
      val shift = (log2 <= S(0)) ? U(0, 2 bits) | log2.asUInt.resize(2)
      (uop(Const.rdRange).asUInt.resize(6) + (field.resize(6) << shift).resize(6)).resize(5)
    }
    // Byte offset of an element in its register group, and the VRF word holding it; a group has at most 8 registers
    def groupOffset(index : UInt, size : UInt) : UInt = (index.resize(log2Up(vlen)) << size).resize(log2Up(vlen))
    def wordOf(vreg : UInt, offset : UInt) : UInt =
      ((vreg.resize(vrf.p.addressWidth) << vrf.p.beatWidth) + (offset >> log2Up(vrf.p.beatBytes)).resized).resize(vrf.p.addressWidth)
    val split = withMemory && lsu.vectorSplit
    val DESCRIPTOR = Payload(VectorMemoryDescriptor(vcs.vstartWidth, split))

    // log2 EMUL of an operand class; classes without a group check as one register
    def emulOf(cls : UInt, lmul : SInt, nreg : SInt) : SInt = cls.mux(
      U(VectorEmul.L) -> lmul.resize(4),
      U(VectorEmul.L2) -> (lmul.resize(4) - 1),
      U(VectorEmul.L4) -> (lmul.resize(4) - 2),
      U(VectorEmul.N) -> nreg,
      U(VectorEmul.W) -> (lmul.resize(4) + 1),
      U(VectorEmul.INDEX16) -> (lmul.resize(4) + 1 - vcs.vtype.vsew.resize(4).asSInt),
      default -> S(0, 4 bits)
    )
    // A register group of 2^log2 registers starts at a multiple of 2^log2
    def aligned(reg : UInt, log2 : SInt) : Bool = {
      val mask = ((U(1, 4 bits) << log2.asUInt.resize(2)) - 1).resize(3)
      log2 <= S(0) || (reg(2 downto 0) & mask) === 0
    }
    // Registers occupied by a group: 2^log2, at least one
    def groupSize(log2 : SInt) : UInt = (log2 <= S(0)) ? U(1, 4 bits) | (U(1, 4 bits) << log2.asUInt.resize(2))
    // Overlap of two aligned groups; a misaligned group is illegal anyway
    def overlapped(a : UInt, aLog2 : SInt, b : UInt, bLog2 : SInt) : Bool = {
      val span = ((aLog2 > bLog2) ? groupSize(aLog2) | groupSize(bLog2)).resize(5)
      (a & ~(span - 1)) === (b & ~(span - 1))
    }

    val e0 = new layer.Execute(0) {
      val vtype = vcs.vtype
      val vd = Decode.UOP(Const.rdRange).asUInt
      val vs1 = Decode.UOP(Const.rs1Range).asUInt
      val vs2 = Decode.UOP(Const.rs2Range).asUInt
      val vm = Decode.UOP(25)

      val legality = new Area {
        val lmul = vtype.vlmul.asSInt
        // imm[2:0] = NREG - 1 is 0, 1, 3 or 7 (norm:vmv-nr-r_enc)
        val imm = Decode.UOP(17 downto 15)
        val nreg = imm(2) ? S(3, 4 bits) | (imm(1) ? S(2, 4 bits) | (imm(0) ? S(1, 4 bits) | S(0, 4 bits)))
        val width = Decode.UOP(Const.funct3Range)
        val eewLog2 = eewLog2Of(width)
        val emul = eewLog2 - S(3, 5 bits) - vtype.vsew.resize(5).asSInt + lmul.resize(5)
        val emulOk = emul >= S(-3) && emul <= S(3)

        val vill = CHECK_VILL && vtype.vill
        val vstart = CHECK_VSTART && vcs.vstart =/= 0
        val group = !aligned(vd, emulOf(EMUL_VD, lmul, nreg)) || !aligned(vs1, emulOf(EMUL_VS1, lmul, nreg)) || !aligned(vs2, emulOf(EMUL_VS2, lmul, nreg))
        val maskOverlap = MASK_NOT_V0 && !vm && vd === 0
        val wholeLog2 = Decode.UOP(31) ? S(3, 4 bits) | (Decode.UOP(30) ? S(2, 4 bits) | (Decode.UOP(29) ? S(1, 4 bits) | S(0, 4 bits)))
        val dataLog2 = dataLog2Of(Decode.UOP).resize(4)
        val indexLog2 = emul.resize(4)
        // Segments: nf groups of max(EMUL, 1) registers, at most 8 and within v31 (norm:vector_ls_seg_op)
        val segment = Decode.UOP(31 downto 29) =/= 0
        val span = ((Decode.UOP(31 downto 29).asUInt.resize(4) + 1) * groupSize(dataLog2)).resize(7)
        val segmentRange = span > 8 || vd.resize(7) + span > 32
        val segmentOverlap = vd.resize(7) < vs2.resize(7) + groupSize(indexLog2) && vs2.resize(7) < vd.resize(7) + span
        val memoryOverlap = overlapped(vd, dataLog2, vs2, indexLog2)
        val indexHighest = indexLog2 >= 0 && vs2.resize(6) + groupSize(indexLog2) === vd.resize(6) + groupSize(dataLog2)
        val badIndexOverlap = memoryOverlap && (
          (sizeOf(Decode.UOP) > vtype.vsew && vd =/= vs2) ||
          (sizeOf(Decode.UOP) < vtype.vsew && !indexHighest))
        val mem = MEM && (whole(Decode.UOP) ? !aligned(vd, wholeLog2) |
          (!maskMemory(Decode.UOP) && (!emulOk || !aligned(vd, dataLog2) ||
            segmentRange || (indexed(Decode.UOP) && (!aligned(vs2, indexLog2) ||
              (!Decode.UOP(5) && (segment ? segmentOverlap | badIndexOverlap)))))))
        // A smaller source may overlap vd only in its highest part, and only with EMUL >= 1 (norm:vreg_overlap_legal)
        val vdLog2 = emulOf(EMUL_VD, lmul, nreg)
        val vs2Log2 = emulOf(EMUL_VS2, lmul, nreg)
        def highest(vs : UInt, log2 : SInt) = log2 >= S(0) && vs.resize(6) + groupSize(log2) === vd.resize(6) + groupSize(vdLog2)
        val vs1Log2 = emulOf(EMUL_VS1, lmul, nreg)
        val widen = OVERLAP_VS2 === VectorOverlap.WIDEN && overlapped(vd, vdLog2, vs2, vs2Log2) && !highest(vs2, vs2Log2) ||
          OVERLAP_VS1 === VectorOverlap.WIDEN && overlapped(vd, vdLog2, vs1, vs1Log2) && !highest(vs1, vs1Log2)
        // A smaller vd may overlap a source group only at its base (norm:vreg_overlap_legal)
        val baseVs2 = OVERLAP_VS2 === VectorOverlap.BASE && overlapped(vd, vdLog2, vs2, vs2Log2) && vd =/= vs2
        val baseVs1 = OVERLAP_VS1 === VectorOverlap.BASE && overlapped(vd, vdLog2, vs1, vs1Log2) && vd =/= vs1
        val disjoint = OVERLAP_VS2 === VectorOverlap.DISJOINT && overlapped(vd, vdLog2, vs2, vs2Log2) ||
          OVERLAP_VS1 === VectorOverlap.DISJOINT && overlapped(vd, vdLog2, vs1, vs1Log2)
        val indexRange = EMUL_VS1 === VectorEmul.INDEX16 && (vs1Log2 < -3 || vs1Log2 > 3)
        val wideSew = log2Up(elen / 8)
        val range = if (elen == 64) RANGE.mux(
          U(VectorRange.EXT2) -> (vtype.vsew === 0),
          U(VectorRange.EXT4) -> (vtype.vsew < 2),
          U(VectorRange.WIDE) -> (vtype.vsew === wideSew || vtype.vlmul === B"011"),
          U(VectorRange.WIDE_SCALAR) -> (vtype.vsew === wideSew),
          U(VectorRange.NO64) -> (vtype.vsew === 3),
          default -> False
        ) else RANGE.mux(
          U(VectorRange.EXT2) -> (vtype.vsew === 0),
          U(VectorRange.EXT4) -> (vtype.vsew < 2),
          U(VectorRange.WIDE) -> (vtype.vsew === wideSew || vtype.vlmul === B"011"),
          U(VectorRange.WIDE_SCALAR) -> (vtype.vsew === wideSew),
          default -> False
        )
        val illegal = vill || vstart || group || maskOverlap || mem || widen || baseVs2 || baseVs1 || disjoint || indexRange || range
      }

      val sel = isValid && SEL
      val fire = sel && isReady && !legality.illegal

      flushPort.valid := sel && legality.illegal
      flushPort.hartId := Global.HART_ID
      flushPort.uopId := Decode.UOP_ID
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := False

      trapPort.valid := sel && legality.illegal
      trapPort.exception := True
      trapPort.code := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
      trapPort.tval := Decode.UOP.resized
      trapPort.tval2 := 0
      trapPort.arg := 0
      trapPort.laneAge := Execute.LANE_AGE
      trapPort.hartId := Global.HART_ID
      when(sel && legality.illegal) {
        bypass(Global.TRAP) := True
        bypass(Global.COMMIT) := False
      }

      // Every vector instruction that passed its checks ends in a commit or a memory trap: VS is Dirty from here
      when(sel && !legality.illegal) {
        vcs.setDirty := True
      }

      val vset = new Area {
        val rs1 = up(elp(IntRegFile, RS1)).asUInt
        val rs2 = up(elp(IntRegFile, RS2))
        val bits = Bits(XLEN bits)
        bits := VSET_REG ? rs2 | (VSET_IMM ? Decode.UOP(29 downto 20).resize(XLEN) | Decode.UOP(30 downto 20).resize(XLEN))
        val vsew = bits(5 downto 3).asUInt
        val vlmul = bits(2 downto 0)
        // SEW <= LMUL * ELEN; LMUL 100 is reserved (norm:vtype_lmul_fval_rsv)
        val lmulOk = vlmul.mux(
          B"100" -> False,
          B"101" -> (if (elen == 64) vsew === 0 else False),
          B"110" -> (if (elen == 64) vsew <= 1 else vsew === 0),
          B"111" -> (if (elen == 64) vsew <= 2 else vsew <= 1),
          default -> True
        )
        val typeOk = bits(XLEN - 1 downto 8) === 0 && vsew <= log2Up(elen / 8) && lmulOk
        val vlmax = vlmaxOf(vsew, vlmul)
        val oldVlmax = vtype.vill ? U(0, vlWidth bits) | vlmaxOf(vtype.vsew, vtype.vlmul)

        val rs1Zero = Decode.UOP(Const.rs1Range) === 0
        val rdZero = Decode.UOP(Const.rdRange) === 0
        val keepVl = !VSET_IMM && rs1Zero && rdZero // x0, x0: keep vl, reserved if VLMAX changes
        val toVlmax = !VSET_IMM && rs1Zero
        val vill = !typeOk || keepVl && vlmax =/= oldVlmax
        val avl = VSET_IMM ? Decode.UOP(Const.rs1Range).asUInt.resize(XLEN) | rs1
        val vl = vill ? U(0, vlWidth bits) | (keepVl ? vcs.vl | ((toVlmax || avl >= vlmax.resize(XLEN)) ? vlmax | avl.resize(vlWidth)))

        when(fire && VSET) {
          vcs.configWrite.valid := True
          vcs.configWrite.vl := vl
          vcs.configWrite.vtype.vill := vill
          vcs.configWrite.vtype.vma := !vill && bits(7)
          vcs.configWrite.vtype.vta := !vill && bits(6)
          vcs.configWrite.vtype.vsew := vill ? U(0, 3 bits) | vsew
          vcs.configWrite.vtype.vlmul := vill ? B(0, 3 bits) | vlmul
          vcs.vstartWrite.valid := True
          vcs.vstartWrite.payload := 0
        }
        RESULT := ELEMENT ? element.io.result | ((SHAPE === VectorShape.SCALAR_READ) ? arithmetic.io.result | vl.resize(XLEN).asBits)
      }

      val sequencer = new Area {
        val started, completed = RegInit(False)
        arithmetic.io.cmd.valid := sel && ARITH && !legality.illegal && !started
        arithmetic.io.cmd.vd := vd
        arithmetic.io.cmd.vs1 := vs1
        arithmetic.io.cmd.vs2 := vs2
        arithmetic.io.cmd.vl := vcs.vl
        arithmetic.io.cmd.vstart := vcs.vstart
        arithmetic.io.cmd.sew := vtype.vsew.resized
        arithmetic.io.cmd.shape := SHAPE
        arithmetic.io.cmd.op := ALU_OP
        arithmetic.io.cmd.scalar := IMMEDIATE ? (if (elen == 64) UNSIGNED_IMMEDIATE ? Decode.UOP(Const.rs1Range).resize(XLEN) |
          Decode.UOP(Const.rs1Range).asSInt.resize(XLEN).asBits else Decode.UOP(Const.rs1Range).asSInt.resize(XLEN).asBits) | up(elp(IntRegFile, RS1))
        arithmetic.io.cmd.scalarOperand := SCALAR_OPERAND
        arithmetic.io.cmd.unmasked := vm
        arithmetic.io.cmd.maskData := MASK_DATA
        arithmetic.io.cmd.signed := SIGNED
        arithmetic.io.cmd.saturate := SATURATE
        arithmetic.io.cmd.vxrm := vcs.vxrm
        arithmetic.io.cmd.nreg := Decode.UOP(17 downto 15).asUInt.resize(4) + 1
        element.io.cmd.valid := sel && ELEMENT && !legality.illegal && !started
        element.io.cmd.vd := vd
        element.io.cmd.vs1 := vs1
        element.io.cmd.vs2 := vs2
        element.io.cmd.vl := vcs.vl
        element.io.cmd.vlmax := vlmaxOf(vtype.vsew, vtype.vlmul)
        element.io.cmd.sew := vtype.vsew.resized
        element.io.cmd.op := ELEMENT_OP
        element.io.cmd.scalar := IMMEDIATE ? Decode.UOP(19 downto 15).resize(XLEN) | up(elp(IntRegFile, RS1))
        element.io.cmd.scalarOperand := SCALAR_OPERAND
        element.io.cmd.unmasked := vm
        element.io.cmd.signed := SIGNED
        element.io.cmd.signedVs1 := SIGNED_VS1
        element.io.cmd.negate := NEGATE
        element.io.cmd.vxrm := vcs.vxrm
        val unit = ARITH || ELEMENT
        elp.freezeWhen(sel && unit && !legality.illegal && !completed)
        when(arithmetic.io.cmd.valid || element.io.cmd.valid) { started := True }
        when(arithmetic.io.done || element.io.done) { completed := True }
        when(fire && unit) {
          started := False
          completed := False
          vcs.vstartWrite.valid := True
          vcs.vstartWrite.payload := 0
          when(ARITH ? arithmetic.io.saturated | element.io.saturated) { vcs.vxsatSet := True }
        }
      }
    }

    val e1 = new layer.Execute(1) {
      wb.valid := SEL && !MEM
      wb.payload := RESULT
    }

    // One LSU micro-op per field of each active element; the descriptor travels with it from the dispatch insert node.
    val memory = withMemory generate new Area {
      val port = lsuPort
      val seq = dp.logic.sequencer.port
      val walker = new VectorElementWalker(vlen, split)
      val resume = RegInit(False) // an element retries after REDO or a TLB refill
      val resumeField = Reg(UInt(3 bits)) init(0) // the field it retries from
      // Fault-only-first: the trap controller may turn the exception of an element past 0 into a trim,
      // after which the refetched instruction only writes vl := trimIndex
      val armed, trim = RegInit(False)
      val triggerPending = RegInit(False)
      val triggerAddress = Reg(UInt(XLEN bits))
      val trimIndex = Reg(UInt(vcs.vstartWidth bits)) init(0)
      val inflight = host.list[InflightService].map(_.hasInflight(0)).orR
      // Older instructions have left, so vl, vstart and v0 are final; the first micro-op uses that sample
      walker.io.start := seq.valid && seq.first && !inflight && !seq.fire
      walker.io.advance := seq.fire
      val count = UInt(vlWidth bits)
      val nreg = seq.uop(31 downto 29).asUInt.resize(vlWidth) + 1
      count := vcs.vl
      when(maskMemory(seq.uop)) { count := ((vcs.vl.resize(vlWidth + 1) + 7) >> 3).resized }
      when(whole(seq.uop)) { count := ((nreg << (log2Up(vlen) - 3)) >> sizeOf(seq.uop)).resized }
      when(trim) { count := 0 }
      walker.io.evl := count
      walker.io.vstart := vcs.vstart
      walker.io.unmasked := seq.uop(25)
      walker.io.v0 := vrf.io.v0
      walker.io.fields := fieldsOf(seq.uop)
      walker.io.startField := resumeField
      if (split) walker.io.wide := dataSize(seq.uop) === 3
      // A candidate leaves only by fire or cancel, so an uncancelled sample belongs to the current candidate
      val prepared = RegNext(walker.io.start && !seq.cancel) init(False)
      seq.ready := !seq.first || prepared && port.storeBufferEmpty
      seq.last := walker.io.last

      val descriptor = VectorMemoryDescriptor(vcs.vstartWidth, split)
      if (split) descriptor.half := walker.io.half
      descriptor.index := walker.io.index
      descriptor.field := walker.io.field
      descriptor.last := walker.io.last
      descriptor.access := walker.io.any // otherwise a single finish micro-op
      elp.ctrl(0).up(DESCRIPTOR) := descriptor

      // Store data: read the element's word one stage before the LSU captures it; the response holds over freezes
      val read = new layer.Execute(-1) {
        val index = apply(DESCRIPTOR).index
        val field = apply(DESCRIPTOR).field
        val size = dataSize(Decode.UOP)
        val offset = groupOffset(index, size)
        val fieldTerm = (field.resize(XLEN) << size).resize(XLEN)
        val term = UInt(XLEN bits)
        term := ((index * (fieldsOf(Decode.UOP).resize(4) + 1) + field).resize(XLEN) << size).resized
        when(strided(Decode.UOP)) { term := ((index * apply(elp(IntRegFile, RS2)).asUInt).resize(XLEN) + fieldTerm).resized }
        when(indexed(Decode.UOP)) { term := fieldTerm }
        val OFFSET = insert(term)
        indexRead.valid := !elp.isFreezed() && isValid && MEM && indexed(Decode.UOP)
        indexRead.payload := wordOf(Decode.UOP(Const.rs2Range).asUInt, groupOffset(index, sizeOf(Decode.UOP)))
        storeRead.valid := !elp.isFreezed()
        storeRead.payload := wordOf(fieldReg(Decode.UOP, field), offset)
      }

      val issue = new layer.Execute(0) {
        val d = apply(DESCRIPTOR)
        val size = dataSize(Decode.UOP)
        val offset = groupOffset(d.index, size)
        val illegal = e0.legality.illegal
        val word = vrf.io.reads(2).rsp
        port.cmd.valid := isValid && MEM
        val indexOffset = groupOffset(d.index, sizeOf(Decode.UOP))
        val indexWord = vrf.io.reads(0).rsp >> (indexOffset.resize(log2Up(vrf.p.beatBytes)) << 3)
        val indexElement = if (elen == 64 && XLEN.get == 64) sizeOf(Decode.UOP).mux(
          U(0) -> indexWord(7 downto 0).asUInt.resize(XLEN),
          U(1) -> indexWord(15 downto 0).asUInt.resize(XLEN),
          U(2) -> indexWord(31 downto 0).asUInt.resize(XLEN),
          default -> indexWord(63 downto 0).asUInt.resize(XLEN))
        else sizeOf(Decode.UOP).mux(
          U(0) -> indexWord(7 downto 0).asUInt.resize(XLEN),
          U(1) -> indexWord(15 downto 0).asUInt.resize(XLEN),
          default -> indexWord(31 downto 0).asUInt.resize(XLEN))
        if (split) {
          // The upper word of a split element is at A + 4 and in bytes 4-7 of its VRF word
          val upper = U(d.half) << 2
          port.cmd.address := (up(elp(IntRegFile, RS1)).asUInt + apply(read.OFFSET) + (indexed(Decode.UOP) ? indexElement | U(0, XLEN bits)) + upper).resized
          port.cmd.size := ((size === 3) ? U(2, 2 bits) | size).resized
          port.cmd.storeData := (word >> ((offset.resize(log2Up(vrf.p.beatBytes)) | upper.resized) << 3)).resize(LSLEN)
          port.cmd.wide := size === 3 && !d.half
        } else {
          port.cmd.address := (up(elp(IntRegFile, RS1)).asUInt + apply(read.OFFSET) + (indexed(Decode.UOP) ? indexElement | U(0, XLEN bits))).resized
          port.cmd.size := size.resized
          port.cmd.storeData := (word >> (offset.resize(log2Up(vrf.p.beatBytes)) << 3)).resize(LSLEN)
        }
        port.cmd.access := d.access && !illegal // an illegal micro-op only carries its trap
        port.cmd.last := d.last || illegal
        val ILLEGAL = insert(illegal)
      }

      val complete = new layer.Execute(lsu.timingParameter.ctrlAt) {
        val d = apply(DESCRIPTOR)
        val size = dataSize(Decode.UOP)
        val offset = groupOffset(d.index, size)
        val rsp = port.rsp
        val legal = !apply(issue.ILLEGAL)

        val write = Flow(VrfWriteCmd(vrf.p))
        write.valid := rsp.valid && rsp.access && rsp.load && !rsp.trap.valid
        write.address := wordOf(fieldReg(Decode.UOP, d.field), offset)
        write.data := size.mux(
          U(0) -> (rsp.data(7 downto 0) #* 8),
          U(1) -> (rsp.data(15 downto 0) #* 4),
          default -> (rsp.data(31 downto 0) #* 2)
        )
        val bytes = size.mux(U(0) -> B"0001", U(1) -> B"0011", default -> B"1111")
        write.mask := (bytes.resize(vrf.p.beatBytes) << offset.resize(log2Up(vrf.p.beatBytes))).resize(vrf.p.beatBytes)
        if (elen == 64) when(size === 3) { write.mask := B(0xff, vrf.p.beatBytes bits) }
        // 8-byte elements: one LSU word, or with a 32-bit LSU the held low word and the upper half's word
        if (elen == 64 && !split) when(size === 3) { write.data := rsp.data(63 downto 0) }
        val low = split generate Reg(Bits(32 bits))
        if (split) {
          when(rsp.valid && rsp.load && !d.half) { low := rsp.data(31 downto 0) }
          when(size === 3) {
            write.valid clearWhen(!d.half)
            write.data := rsp.data(31 downto 0) ## low
          }
        }

        // Any element trap restarts at that element (norm:vector_ls_vstart); the last one completes the instruction
        when(rsp.valid && legal) {
          val refill = !rsp.trap.exception && rsp.trap.code === TrapReason.MMU_REFILL
          val arm = rsp.trap.valid && faultOnlyFirst(Decode.UOP) && d.index =/= 0 && (rsp.trap.exception || refill)
          armed := arm
          when(rsp.trap.valid && !rsp.trap.exception && rsp.trap.code === TrapReason.DEBUG_TRIGGER) {
            triggerPending := True
            triggerAddress := lsu.logic.onCtrl(lsu.logic.vector.ELEMENT).address.resized
          }
          when(rsp.trap.valid) {
            vcs.vstartWrite.valid := True
            vcs.vstartWrite.payload := d.index
            trimIndex := d.index
            val retry = !rsp.trap.exception && (rsp.trap.code === TrapReason.REDO || refill)
            resume := retry || arm
            // A retry continues at the faulting field; an architectural trap restarts the element from its first field
            resumeField := retry ? d.field | U(0, 3 bits)
          } elsewhen(rsp.last) {
            vcs.vstartWrite.valid := True
            vcs.vstartWrite.payload := 0
            resume := False
            resumeField := 0
            // norm:vector_ff_op: vl := index of the faulting element, vtype unchanged
            when(trim) {
              vcs.configWrite.valid := True
              vcs.configWrite.vl := trimIndex.resized
              vcs.configWrite.vtype := vcs.vtype
              trim := False
            }
          }
        }
        when(write.valid) {
          vrf.io.write := write
        }
        assert(!(write.valid && (arithmetic.io.write.valid || element.io.write.valid)), "VRF write port requested by a vector unit and a load")
      }
    }
    buildBefore.release()

    // Interrupts and debug halts wait until the retried instruction commits or traps
    val interruptHold = withMemory generate new Area {
      val tp = host[TrapPlugin]
      val pp = host[PrivilegedPlugin]
      def clear() : Unit = {
        memory.resume := False
        memory.resumeField := 0
        memory.armed := False
        memory.trim := False
        memory.triggerPending := False
      }
      when(tp.api.harts(0).rvTrap) { clear() }
      if (pp.p.withDebug) {
        val fsm = tp.logic.harts(0).trap.fsm
        when(fsm.isActive(fsm.ENTER_DEBUG)) { clear() }
      }
      when(memory.resume) { pp.inhibateInterrupts(0) }
    }

    if (withMemory && host[PrivilegedPlugin].p.debugTriggers > 0) {
      val hart = host[TrapPlugin].logic.harts(0)
      hart.trap.fsm.TRAP_TVAL.whenIsActiveWithPriority(1) {
        when(memory.triggerPending) { hart.crsPorts.write.data := memory.triggerAddress.asBits }
      }
    }

    // Fault-only-first: an armed element's exception, from the LSU or the page walker, refetches the instruction
    val trimControl = withMemory generate new Area {
      val trap = host[TrapPlugin].logic.harts(0).trap
      val fsm = trap.fsm
      def convert() : Unit = {
        trap.pending.state.exception := False
        host[TrapPlugin].api.harts(0).redo := True
        memory.armed := False
        memory.trim := True
        fsm.goto(fsm.JUMP)
      }
      fsm.COMPUTE.whenIsActiveWithPriority(1) {
        when(memory.armed && trap.pending.state.exception) {
          fsm.jumpOffset := 0 // exception codes alias TrapReason.NEXT and others
          convert()
        }
      }
      if (fsm.ATS_RSP != null) fsm.ATS_RSP.whenIsActiveWithPriority(1) {
        val rsp = fsm.atsPorts.refill.rsp
        when(memory.armed && rsp.valid && (rsp.pageFault || rsp.accessFault)) {
          rsp.ready := True
          convert()
        }
      }
    }

    // Simulation taps for the probe
    val whitebox = new Area {
      val fire = CombInit(e0.fire).simPublic()
      val uopId = CombInit(e0(Decode.UOP_ID)).simPublic()
      val memoryIssue = CombInit(e0.fire && e0(MEM)).simPublic()
      val memoryComplete = (if (withMemory) CombInit(memory.port.rsp.valid) else False).simPublic()
      val memoryUopId = (if (withMemory) CombInit(memory.port.rsp.uopId) else U(0, Decode.UOP_ID_WIDTH bits)).simPublic()
      val configValid = CombInit(vcs.configWrite.valid).simPublic()
      val configVl = CombInit(vcs.configWrite.vl).simPublic()
      val configVtype = CombInit(vtypeBits(vcs.configWrite.vtype)).simPublic()
      val vstartValid = CombInit(vcs.vstartWrite.valid).simPublic()
      val vstartValue = CombInit(vcs.vstartWrite.payload).simPublic()
      val vrfWriteValid = CombInit(vrf.io.writeEvent.valid && !vrf.io.initHold).simPublic()
      val vrfWriteAddress = CombInit(vrf.io.writeEvent.address).simPublic()
      val vrfWriteData = CombInit(vrf.io.writeEvent.data).simPublic()
      val vrfWriteMask = CombInit(vrf.io.writeEvent.mask).simPublic()

      val vs = CombInit(vcs.vs).simPublic()
      val vstart = CombInit(vcs.vstart).simPublic()
      val vxrm = CombInit(vcs.vxrm).simPublic()
      val vxsat = CombInit(vcs.vxsat).simPublic()
      val vxsatSet = CombInit(vcs.vxsatSet).simPublic()

      val trap = host[TrapPlugin].logic.harts(0).trap
      val trapEpc = CombInit(trap.pending.pc).simPublic()
      val trapTval = CombInit(trap.fsm.buffer.trap.tval).simPublic()
      if (withMemory) when(memory.triggerPending) { trapTval := memory.triggerAddress.asBits.resized }
    }
  }

  // Architectural vtype value: vill at bit XLEN-1
  def vtypeBits(t : VType) : Bits = {
    val r = B(0, XLEN bits)
    r(XLEN - 1) := t.vill
    r(7) := t.vma
    r(6) := t.vta
    r(5 downto 3) := t.vsew.asBits
    r(2 downto 0) := t.vlmul
    r
  }
}
