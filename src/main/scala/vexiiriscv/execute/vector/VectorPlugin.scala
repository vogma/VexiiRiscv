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
import vexiiriscv.misc.{TrapPlugin, TrapService}
import vexiiriscv.riscv.Riscv._
import vexiiriscv.riscv._
import vexiiriscv.schedule.{DispatchPlugin, ReschedulePlugin}

/**
 * Vector instructions on one lane layer. Each one waits until every older instruction left the pipeline, is checked
 * against vtype/vstart at E0 and then does all its work there; vset* complete in that cycle.
 */
class VectorPlugin(val layer : LaneLayer, val dlen : Int = 64) extends FiberPlugin with InitService {
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
  val ARITH, SCALAR_OPERAND, IMMEDIATE, SCALAR_READ, SCALAR_WRITE = Payload(Bool())
  val ALU_OP = Payload(UInt(3 bits))
  val RESULT = Payload(Bits(XLEN bits))

  // E0 legality checks, each enabled per uop
  val CHECK_VILL = Payload(Bool())
  val CHECK_VSTART = Payload(Bool())
  val ALIGN_VD, ALIGN_VS1, ALIGN_VS2 = Payload(Bool()) // register group aligned to LMUL
  val MASK_NOT_V0 = Payload(Bool()) // vm = 0 with vd = v0 is reserved
  val MEM = Payload(Bool()) // EMUL = EEW / SEW * LMUL in [1/8, 8], vd/vs3 aligned to it

  val logic = during setup new Area {
    val elp = layer.lane
    val ifp = host.find[IntFormatPlugin](_.lane == elp)
    val dp = host[DispatchPlugin]
    val ds = host[DecoderService]
    val sp = host[ReschedulePlugin]
    val ts = host[TrapService]
    val vcs = host[VectorCsrService]
    val ioRetainer = retains(elp.uopLock, ifp.elaborationLock, dp.elaborationLock, ds.elaborationLock, sp.elaborationLock, ts.trapLock)
    val buildBefore = retains(elp.pipelineLock)
    awaitBuild()

    val checks = List(VSET, VSET_IMM, VSET_REG, CHECK_VILL, CHECK_VSTART, ALIGN_VD, ALIGN_VS1, ALIGN_VS2, MASK_NOT_V0, MEM,
      ARITH, SCALAR_OPERAND, IMMEDIATE, SCALAR_READ, SCALAR_WRITE)
    elp.setDecodingDefault(SEL, False)
    elp.setDecodingDefault(ALU_OP, U(VectorAluOp.ADD, 3 bits))
    for (key <- checks) elp.setDecodingDefault(key, False)
    ds.addMicroOpDecodingDefault(VECTOR, False)

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

    for ((name, alu) <- Seq("vadd" -> VectorAluOp.ADD, "vsub" -> VectorAluOp.SUB,
      "vrsub" -> VectorAluOp.RSUB, "vand" -> VectorAluOp.AND, "vor" -> VectorAluOp.OR, "vxor" -> VectorAluOp.XOR,
      "vmv.v" -> VectorAluOp.MOVE); form <- Seq("vv", "vx", "vi") if Rvv.all.contains(s"$name.$form") || name == "vmv.v") {
      val mnemonic = if (name == "vmv.v") s"vmv.v.${form.last}" else s"$name.$form"
      val vector = form == "vv"
      add(Rvv.all(mnemonic), ARITH -> True, ALU_OP -> U(alu, 3 bits),
        SCALAR_OPERAND -> Bool(!vector), IMMEDIATE -> Bool(form == "vi"),
        CHECK_VILL -> True, CHECK_VSTART -> True, ALIGN_VD -> True,
        ALIGN_VS1 -> Bool(vector), ALIGN_VS2 -> Bool(name != "vmv.v"), MASK_NOT_V0 -> Bool(name != "vmv.v"))
    }
    add(Rvv.all("vmv.x.s"), ARITH -> True, SCALAR_READ -> True, CHECK_VILL -> True, CHECK_VSTART -> True)
    add(Rvv.all("vmv.s.x"), ARITH -> True, ALU_OP -> U(VectorAluOp.MOVE, 3 bits),
      SCALAR_OPERAND -> True, SCALAR_WRITE -> True, CHECK_VILL -> True, CHECK_VSTART -> True)

    ds.addIllegalCheck(ctrl => ctrl(VECTOR) && !vcs.vectorEnable)

    val age = elp.getExecuteAge(0)
    val flushPort = sp.newFlushPort(age, laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)
    val trapPort = ts.newTrap(age, Execute.LANE_AGE_WIDTH)
    ioRetainer.release()

    val vlen = vcs.vlen
    val vlWidth = vcs.vlWidth
    val vrf = regFile.vrf
    val arithmetic = new VectorArithmetic(vrf.p, XLEN)
    vrf.io.write := arithmetic.io.write
    arithmetic.io.v0 := vrf.io.v0
    for (i <- 0 until 2) {
      vrf.io.reads(i).cmd := arithmetic.io.reads(i).cmd
      arithmetic.io.reads(i).rsp := vrf.io.reads(i).rsp
    }
    for (port <- vrf.io.reads.drop(2)) {
      port.cmd.valid := False
      port.cmd.payload.assignDontCare()
    }

    // log2(VLMAX) = log2(VLEN) - log2(SEW) + log2(LMUL), only meaningful for a legal vtype
    def vlmaxOf(vsew : UInt, vlmul : Bits) : UInt = {
      val shift = S(log2Up(vlen) - 3, 5 bits) + vlmul.asSInt.resize(5) - vsew.resize(5).asSInt
      (U(1, vlWidth bits) << shift.asUInt.resize(log2Up(vlWidth))).resize(vlWidth)
    }
    // A register group of 2^log2 registers starts at a multiple of 2^log2
    def aligned(reg : UInt, log2 : SInt) : Bool = {
      val mask = ((U(1, 4 bits) << log2.asUInt.resize(2)) - 1).resize(3)
      log2 <= S(0) || (reg(2 downto 0) & mask) === 0
    }

    val e0 = new layer.Execute(0) {
      val vtype = vcs.vtype
      val vd = Decode.UOP(Const.rdRange).asUInt
      val vs1 = Decode.UOP(Const.rs1Range).asUInt
      val vs2 = Decode.UOP(Const.rs2Range).asUInt
      val vm = Decode.UOP(25)

      val legality = new Area {
        val lmul = vtype.vlmul.asSInt
        val width = Decode.UOP(Const.funct3Range)
        val eewLog2 = width.mux(B"101" -> S(4, 5 bits), B"110" -> S(5, 5 bits), default -> S(3, 5 bits))
        val emul = eewLog2 - S(3, 5 bits) - vtype.vsew.resize(5).asSInt + lmul.resize(5)
        val emulOk = emul >= S(-3) && emul <= S(3)

        val vill = CHECK_VILL && vtype.vill
        val vstart = CHECK_VSTART && vcs.vstart =/= 0
        val group = ALIGN_VD && !aligned(vd, lmul) || ALIGN_VS1 && !aligned(vs1, lmul) || ALIGN_VS2 && !aligned(vs2, lmul)
        val maskOverlap = MASK_NOT_V0 && !vm && vd === 0
        val mem = MEM && (!emulOk || !aligned(vd, emul.resize(4)))
        val illegal = vill || vstart || group || maskOverlap || mem
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
        val lmulOk = vlmul.mux(
          B"100" -> False,
          B"101" -> False,
          B"110" -> (vsew === 0),
          B"111" -> (vsew <= 1),
          default -> True
        )
        val typeOk = bits(XLEN - 1 downto 8) === 0 && vsew <= 2 && lmulOk
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
        RESULT := SCALAR_READ ? arithmetic.io.result | vl.resize(XLEN).asBits
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
        arithmetic.io.cmd.op := ALU_OP
        arithmetic.io.cmd.scalar := IMMEDIATE ? Decode.UOP(Const.rs1Range).asSInt.resize(XLEN).asBits | up(elp(IntRegFile, RS1))
        arithmetic.io.cmd.scalarOperand := SCALAR_OPERAND
        arithmetic.io.cmd.unmasked := vm
        arithmetic.io.cmd.scalarRead := SCALAR_READ
        arithmetic.io.cmd.scalarWrite := SCALAR_WRITE
        elp.freezeWhen(sel && ARITH && !legality.illegal && !completed)
        when(arithmetic.io.cmd.valid) { started := True }
        when(arithmetic.io.done) { completed := True }
        when(fire && ARITH) {
          started := False
          completed := False
          vcs.vstartWrite.valid := True
          vcs.vstartWrite.payload := 0
        }
      }
    }

    val e1 = new layer.Execute(1) {
      wb.valid := SEL
      wb.payload := RESULT
    }
    buildBefore.release()

    // Simulation taps for the probe
    val whitebox = new Area {
      val fire = CombInit(e0.fire).simPublic()
      val uopId = CombInit(e0(Decode.UOP_ID)).simPublic()
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

      val trap = host[TrapPlugin].logic.harts(0).trap
      val trapEpc = CombInit(trap.pending.pc).simPublic()
      val trapTval = CombInit(trap.fsm.buffer.trap.tval).simPublic()
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
