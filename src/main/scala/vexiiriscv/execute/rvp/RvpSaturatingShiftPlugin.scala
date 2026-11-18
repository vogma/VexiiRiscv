// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.execute.rvp.RvpPairWritePlugin.{PAIR_RD, PAIR_RS1}
import vexiiriscv.execute.rvp.RvpSaturatingShiftPlugin.Rv64
import vexiiriscv.execute.rvp.kinds.{RvpElemWidth => EW}
import vexiiriscv.execute.rvp.ops.{RvpSatShiftOps, RvpShiftOps}
import vexiiriscv.execute.{ExecutionUnitElementSimple, LaneLayer}
import vexiiriscv.riscv.{IntRegFile, MicroOp, RDS, RS1, RS2, Riscv}

// Saturating / rounding shift family. A separate datapath from the segmented adder in RvpPackedAluPlugin.
object RvpSaturatingShiftPlugin {
  // Register forms (TypeR: RS1, RS2, RD). rs2[7:0] is the signed shift amount.
  val PSSHA_HS = IntRegFile.TypeR(M"1110100----------010-----0011011")
  val PSSHAR_HS = IntRegFile.TypeR(M"1111100----------010-----0011011")
  val PSSHL_HS = IntRegFile.TypeR(M"1010100----------010-----0011011")
  val PSSHLR_HS = IntRegFile.TypeR(M"1011100----------010-----0011011")

  val SSHA = IntRegFile.TypeR(M"1110101----------010-----0011011")
  val SSHAR = IntRegFile.TypeR(M"1111101----------010-----0011011")
  val SSHL = IntRegFile.TypeR(M"1010101----------010-----0011011")
  val SSHLR = IntRegFile.TypeR(M"1011101----------010-----0011011")
  val SSLAI = IntRegFile.TypeI(M"1101001----------010-----0011011")
  val PSSLAI_H = IntRegFile.TypeI(M"11010001---------010-----0011011")

  val PSSHA_DHS = IntRegFile.TypeR_RDS(M"0110100---------0110----00011011")
  val PSSHA_DWS = IntRegFile.TypeR_RDS(M"0110101---------0110----00011011")
  val PSSHAR_DHS = IntRegFile.TypeR_RDS(M"0111100---------0110----00011011")
  val PSSHAR_DWS = IntRegFile.TypeR_RDS(M"0111101---------0110----00011011")
  val PSSHL_DHS = IntRegFile.TypeR_RDS(M"0010100---------0110----00011011")
  val PSSHL_DWS = IntRegFile.TypeR_RDS(M"0010101---------0110----00011011")
  val PSSHLR_DHS = IntRegFile.TypeR_RDS(M"0011100---------0110----00011011")
  val PSSHLR_DWS = IntRegFile.TypeR_RDS(M"0011101---------0110----00011011")
  val PSSLAI_DH = IntRegFile.TypeI_RDS(M"01010001--------0110----00011011")
  val PSSLAI_DW = IntRegFile.TypeI_RDS(M"0101001---------0110----00011011")

  object Rv64 {
    val PSSHA_HS = IntRegFile.TypeR(M"1110100----------010-----0011011") // pssha.hs
    val PSSHA_WS = IntRegFile.TypeR(M"1110101----------010-----0011011") // pssha.ws (recycled: RV32 ssha)
    val PSSHAR_HS = IntRegFile.TypeR(M"1111100----------010-----0011011") // psshar.hs
    val PSSHAR_WS = IntRegFile.TypeR(M"1111101----------010-----0011011") // psshar.ws (recycled: RV32 sshar)
    val PSSHL_HS = IntRegFile.TypeR(M"1010100----------010-----0011011") // psshl.hs
    val PSSHL_WS = IntRegFile.TypeR(M"1010101----------010-----0011011") // psshl.ws (recycled: RV32 sshl)
    val PSSHLR_HS = IntRegFile.TypeR(M"1011100----------010-----0011011") // psshlr.hs
    val PSSHLR_WS = IntRegFile.TypeR(M"1011101----------010-----0011011") // psshlr.ws (recycled: RV32 sshlr)
    val PSSLAI_H = IntRegFile.TypeI(M"11010001---------010-----0011011") // psslai.h (uimm4)
    val PSSLAI_W = IntRegFile.TypeI(M"1101001----------010-----0011011") // psslai.w (uimm5, recycled: RV32 sslai)

    val SHA = IntRegFile.TypeR(M"1110111----------010-----0011011") // sha (RV64-only)
    val SHAR = IntRegFile.TypeR(M"1111111----------010-----0011011") // shar (RV64-only)
    val SHL = IntRegFile.TypeR(M"1010111----------010-----0011011") // shl (RV64-only)
    val SHLR = IntRegFile.TypeR(M"1011111----------010-----0011011") // shlr (RV64-only)
  }
}

class RvpSaturatingShiftPlugin(val layer: LaneLayer, val intWritebackAt: Int,
                               val xlen: Int, val pairs: Boolean = true) extends ExecutionUnitElementSimple(layer) {

  require(xlen == 32 || xlen == 64, s"RvpSaturatingShiftPlugin: XLEN 32 or 64, got $xlen")
  private val rv64 = xlen == 64
  // RV32 register-pair support; false = the no-pairs configuration: no
  // double-wide two-pass, no HIGH-word holds, no pair-write port.
  private val withPairs = pairs && !rv64

  val ELEM_W = Payload(EW()) // H = packed halfword lanes, W = 32-bit word lanes
  val SIGNED = Payload(Bool()) // arithmetic (signed) vs logical (unsigned) shift + clamp
  val ROUNDING = Payload(Bool()) // round-half-up on the right-shift path
  val SATURATED = Payload(Bool()) // data-dependent left-overflow pulse -> vxsat
  val IS_IMM = Payload(Bool())
  val DOUBLE_WIDE = Payload(Bool()) // pair source + pair dest: RDS supplies the high word
  // XLEN=64 only: sha/shar/shl/shlr shift the whole register and never saturate,
  // so they bypass the per-word banks entirely.
  val IS_XLEN = Payload(Bool())
  val SCALAR_MID = Payload(Bits(65 bits))
  val SCALAR_AMOUNT_HI = Payload(UInt(3 bits))
  val SCALAR_RIGHT = Payload(Bool())
  val SCALAR_FILL = Payload(Bool())
  val SCALAR_RD = Payload(Bits(xlen bits))

  val MID = Payload(RvpSatShiftOps.SatShiftMid()) // barrel-shift outputs, crosses 0 -> 1
  val MID_HI = Payload(RvpSatShiftOps.SatShiftMid())
  val RD = Payload(Bits(32 bits))
  val RD_HI = Payload(Bits(32 bits))

  val logic = during setup new Logic {

    // Register form: reads RS1 + RS2.
    def registerOpcode(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop
    }

    def registerSingleRsOpcode(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop
    }

    // Pair source + pair dest, amount in rs2.
    def registerOpcodeRDS(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop.spec.addRsSpec(RDS, executeAt = 0)
      uop
    }

    def registerOpcodeImmRDS(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RDS, executeAt = 0)
      uop
    }

    if (!rv64) el.setDecodingDefault(DOUBLE_WIDE, False) else el.setDecodingDefault(IS_XLEN, False)

    val decoderService = host[DecoderService]
    val decoderRetainer = retains(decoderService.decodingLock)

    awaitBuild()

    assert(Riscv.XLEN.get == xlen)

    assert(intWritebackAt >= 2)
    val writeBack = newWriteback(ifp, intWritebackAt)

    // Register a port at the retirement stage and retain the elaboration lock so RvpVxsatPlugin sees this before it reads its port list.
    val vxsat = host[RvpVxsatPlugin]
    val vxsatRetainer = retains(vxsat.elaborationLock)
    val vxsatPort = vxsat.createPort(at = intWritebackAt)
    vxsatRetainer.release()

    // Pair-destination high half. RvpPairWritePlugin freezes the lane for one cycle at its writeAt, so the vxsat pulse below still lands only once
    val pairWrite = withPairs generate host[RvpPairWritePlugin]
    val pairRetainer = withPairs generate retains(pairWrite.elaborationLock)
    val hiPort = withPairs generate pairWrite.createPort(at = intWritebackAt)
    if (withPairs) pairRetainer.release()

    if (!rv64) {
      // pssha.hs: packed halfword (EW.H), signed arithmetic, non-rounding.
      registerOpcode(RvpSaturatingShiftPlugin.PSSHA_HS).decode(ELEM_W -> EW.H, SIGNED -> True, IS_IMM -> False, ROUNDING -> False)
      registerOpcode(RvpSaturatingShiftPlugin.PSSHAR_HS).decode(ELEM_W -> EW.H, SIGNED -> True, IS_IMM -> False, ROUNDING -> True)
      registerOpcode(RvpSaturatingShiftPlugin.PSSHL_HS).decode(ELEM_W -> EW.H, SIGNED -> False, IS_IMM -> False, ROUNDING -> False)
      registerOpcode(RvpSaturatingShiftPlugin.PSSHLR_HS).decode(ELEM_W -> EW.H, SIGNED -> False, IS_IMM -> False, ROUNDING -> True)

      registerOpcode(RvpSaturatingShiftPlugin.SSHA).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> False, ROUNDING -> False)
      registerOpcode(RvpSaturatingShiftPlugin.SSHAR).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> False, ROUNDING -> True)
      registerOpcode(RvpSaturatingShiftPlugin.SSHL).decode(ELEM_W -> EW.W, SIGNED -> False, IS_IMM -> False, ROUNDING -> False)
      registerOpcode(RvpSaturatingShiftPlugin.SSHLR).decode(ELEM_W -> EW.W, SIGNED -> False, IS_IMM -> False, ROUNDING -> True)

      registerSingleRsOpcode(RvpSaturatingShiftPlugin.SSLAI).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> True, ROUNDING -> False)
      registerSingleRsOpcode(RvpSaturatingShiftPlugin.PSSLAI_H).decode(ELEM_W -> EW.H, SIGNED -> True, IS_IMM -> True, ROUNDING -> False)

      if (pairs) { // ---- register-pair ops, dropped by --rvp-no-pairs ----
      // Double-wide: identical payloads to the .hs / scalar forms above, plus DOUBLE_WIDE and PAIR_RD
      registerOpcodeRDS(RvpSaturatingShiftPlugin.PSSHA_DHS).decode(ELEM_W -> EW.H, SIGNED -> True, IS_IMM -> False, ROUNDING -> False, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpSaturatingShiftPlugin.PSSHA_DWS).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> False, ROUNDING -> False, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpSaturatingShiftPlugin.PSSHAR_DHS).decode(ELEM_W -> EW.H, SIGNED -> True, IS_IMM -> False, ROUNDING -> True, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpSaturatingShiftPlugin.PSSHAR_DWS).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> False, ROUNDING -> True, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpSaturatingShiftPlugin.PSSHL_DHS).decode(ELEM_W -> EW.H, SIGNED -> False, IS_IMM -> False, ROUNDING -> False, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpSaturatingShiftPlugin.PSSHL_DWS).decode(ELEM_W -> EW.W, SIGNED -> False, IS_IMM -> False, ROUNDING -> False, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpSaturatingShiftPlugin.PSSHLR_DHS).decode(ELEM_W -> EW.H, SIGNED -> False, IS_IMM -> False, ROUNDING -> True, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpSaturatingShiftPlugin.PSSHLR_DWS).decode(ELEM_W -> EW.W, SIGNED -> False, IS_IMM -> False, ROUNDING -> True, PAIR_RD -> True, DOUBLE_WIDE -> True)

      registerOpcodeImmRDS(RvpSaturatingShiftPlugin.PSSLAI_DH).decode(ELEM_W -> EW.H, SIGNED -> True, IS_IMM -> True, ROUNDING -> False, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpSaturatingShiftPlugin.PSSLAI_DW).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> True, ROUNDING -> False, PAIR_RD -> True, DOUBLE_WIDE -> True)

      for (enc <- List(
        RvpSaturatingShiftPlugin.PSSHA_DHS, RvpSaturatingShiftPlugin.PSSHA_DWS,
        RvpSaturatingShiftPlugin.PSSHAR_DHS, RvpSaturatingShiftPlugin.PSSHAR_DWS,
        RvpSaturatingShiftPlugin.PSSHL_DHS, RvpSaturatingShiftPlugin.PSSHL_DWS,
        RvpSaturatingShiftPlugin.PSSHLR_DHS, RvpSaturatingShiftPlugin.PSSHLR_DWS,
        RvpSaturatingShiftPlugin.PSSLAI_DH, RvpSaturatingShiftPlugin.PSSLAI_DW
      )) {
        decoderService.addMicroOpDecoding(enc, PAIR_RD, True)
        decoderService.addMicroOpDecoding(enc, PAIR_RS1, True)
      }
      } // pairs
    } else {
      // ---- RV64 -----------------------------------------------------------
      // EW.H is 4 halfword lanes and EW.W is 2 word lanes so the same per-word
      // network runs twice. sha/shar/shl/shlr are the odd ones out: one 64-bit
      // lane, no saturation, hence IS_XLEN.
      registerOpcode(Rv64.PSSHA_HS).decode(ELEM_W -> EW.H, SIGNED -> True, IS_IMM -> False, ROUNDING -> False)
      registerOpcode(Rv64.PSSHA_WS).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> False, ROUNDING -> False)
      registerOpcode(Rv64.PSSHAR_HS).decode(ELEM_W -> EW.H, SIGNED -> True, IS_IMM -> False, ROUNDING -> True)
      registerOpcode(Rv64.PSSHAR_WS).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> False, ROUNDING -> True)
      registerOpcode(Rv64.PSSHL_HS).decode(ELEM_W -> EW.H, SIGNED -> False, IS_IMM -> False, ROUNDING -> False)
      registerOpcode(Rv64.PSSHL_WS).decode(ELEM_W -> EW.W, SIGNED -> False, IS_IMM -> False, ROUNDING -> False)
      registerOpcode(Rv64.PSSHLR_HS).decode(ELEM_W -> EW.H, SIGNED -> False, IS_IMM -> False, ROUNDING -> True)
      registerOpcode(Rv64.PSSHLR_WS).decode(ELEM_W -> EW.W, SIGNED -> False, IS_IMM -> False, ROUNDING -> True)
      registerSingleRsOpcode(Rv64.PSSLAI_H).decode(ELEM_W -> EW.H, SIGNED -> True, IS_IMM -> True, ROUNDING -> False)
      registerSingleRsOpcode(Rv64.PSSLAI_W).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> True, ROUNDING -> False)

      registerOpcode(Rv64.SHA).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> False, ROUNDING -> False, IS_XLEN -> True)
      registerOpcode(Rv64.SHAR).decode(ELEM_W -> EW.W, SIGNED -> True, IS_IMM -> False, ROUNDING -> True, IS_XLEN -> True)
      registerOpcode(Rv64.SHL).decode(ELEM_W -> EW.W, SIGNED -> False, IS_IMM -> False, ROUNDING -> False, IS_XLEN -> True)
      registerOpcode(Rv64.SHLR).decode(ELEM_W -> EW.W, SIGNED -> False, IS_IMM -> False, ROUNDING -> True, IS_XLEN -> True)
    }

    decoderRetainer.release()
    uopRetainer.release()

    // Execute(0)'s frozen-cycle hold for the high word
    val midHi = withPairs generate Reg(RvpSatShiftOps.SatShiftMid())

    val shiftStage = (!rv64) generate new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asBits
      val rs2 = up(el(IntRegFile, RS2)).asBits
      // RDS = the odd half of the pair source; only the double-wide ops declare it here.
      val rds = if (withPairs) up(el(IntRegFile, RDS)).asBits else B(0, 32 bits)

      val immediate = Mux(this (ELEM_W) === EW.W,
        Decode.UOP(24 downto 20).asUInt,
        Decode.UOP(23 downto 20).asUInt
      )

      val amount = Mux(this (IS_IMM),
        immediate.resize(8).asBits,
        rs2(7 downto 0)
      )

      def doubleWide = if (withPairs) this (DOUBLE_WIDE) else False
      val pass = RvpDoubleWidePass(el, isValid && SEL && doubleWide, up.isMoving, enabled = withPairs)

      val shiftSrc = if (withPairs) pass.first ? rds | rs1 else rs1
      val mid = RvpSatShiftOps.satShiftShift(shiftSrc, amount, this (ELEM_W), this (SIGNED), this (ROUNDING))

      // The stage register is frozen on pass 1, so the high word needs an explicit hold
      if (withPairs) when(pass.first) {
        midHi := mid
      }

      this (MID) := mid
    }

    val clampStage = (!rv64) generate new el.Execute(id = 1) {

      def doubleWide = if (withPairs) this (DOUBLE_WIDE) else False
      val pass = RvpDoubleWidePass(el, isValid && SEL && doubleWide, up.isMoving, enabled = withPairs)

      val mMid = if (withPairs) pass.first ? midHi | this (MID) else this (MID)
      val (rd, saturated) = RvpSatShiftOps.satShiftClamp(
        mMid, this (ELEM_W), this (SIGNED), this (ROUNDING))

      val rdHiHold = if (withPairs) RegNextWhen(rd, pass.first) else B(0, 32 bits)
      val satHiHold = if (withPairs) RegNextWhen(saturated, pass.first) else False

      this (RD) := rd
      if (withPairs) this (RD_HI) := rdHiHold

      this (SATURATED) := SEL && (saturated || (doubleWide && satHiHold))
    }

    val writeBackStage = (!rv64) generate new el.Execute(id = intWritebackAt) {
      if (withPairs) {
        hiPort.valid := SEL && this (PAIR_RD)
        hiPort.payload := this (RD_HI)
      }

      writeBack.valid := SEL
      writeBack.payload := this (RD)
    }

    val shiftStage64 = rv64 generate new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asBits
      val rs2 = up(el(IntRegFile, RS2)).asBits

      // uimm5 for psslai.w, uimm4 for psslai.h -- the same split as RV32.
      val immediate = Mux(this (ELEM_W) === EW.W,
        Decode.UOP(24 downto 20).asUInt,
        Decode.UOP(23 downto 20).asUInt
      )
      val amount = Mux(this (IS_IMM), immediate.resize(8).asBits, rs2(7 downto 0))

      this (MID) := RvpSatShiftOps.satShiftShift(
        rs1(31 downto 0), amount, this (ELEM_W), this (SIGNED), this (ROUNDING))
      this (MID_HI) := RvpSatShiftOps.satShiftShift(
        rs1(63 downto 32), amount, this (ELEM_W), this (SIGNED), this (ROUNDING))

      val (scalarMid, scalarAmountHi, scalarRight, scalarFill) =
        RvpShiftOps.scalarVarShift64Low(
          rs1, rs2(7 downto 0), this (SIGNED))
      this (SCALAR_MID) := scalarMid
      this (SCALAR_AMOUNT_HI) := scalarAmountHi
      this (SCALAR_RIGHT) := scalarRight
      this (SCALAR_FILL) := scalarFill
    }

    val clampStage64 = rv64 generate new el.Execute(id = 1) {

      this (SCALAR_RD) := RvpShiftOps.scalarVarShift64High(
        this (SCALAR_MID), this (SCALAR_AMOUNT_HI),
        this (SCALAR_RIGHT), this (SCALAR_FILL), this (ROUNDING))

      val (rd, saturated) = RvpSatShiftOps.satShiftClamp(
        this (MID), this (ELEM_W), this (SIGNED), this (ROUNDING))
      val (rdHi, saturatedHi) = RvpSatShiftOps.satShiftClamp(
        this (MID_HI), this (ELEM_W), this (SIGNED), this (ROUNDING))
      this (RD) := rd
      this (RD_HI) := rdHi
      this (SATURATED) := SEL && !this (IS_XLEN) && (saturated || saturatedHi)
    }

    val writeBackStage64 = rv64 generate new el.Execute(id = intWritebackAt) {
      writeBack.valid := SEL
      writeBack.payload := Mux(this (IS_XLEN), this (SCALAR_RD),
        this (RD_HI) ## this (RD))
    }

    val vxSatCommit = new el.Execute(id = intWritebackAt) {
      vxsatPort.pulse := this (SATURATED)
    }
  }
}
