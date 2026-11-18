// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.execute.rvp.RvpPackedShiftPlugin.Rv64
import vexiiriscv.execute.rvp.RvpPairWritePlugin.{PAIR_RD, PAIR_RS1}
import vexiiriscv.execute.rvp.kinds.{RvpElemWidth, RvpShiftKind}
import vexiiriscv.execute.rvp.kinds.RvpShiftKind._
import vexiiriscv.execute.rvp.ops.RvpShiftOps
import vexiiriscv.execute.{ExecutionUnitElementSimple, LaneLayer}
import vexiiriscv.riscv.{IntRegFile, MicroOp, RDS, RS1, RS2, Riscv}

import scala.language.postfixOps

// Packed-shift instructions
object RvpPackedShiftPlugin {
  // Register forms (TypeR: RS1, RS2, RD).
  val PSLL_BS = IntRegFile.TypeR(M"1000110----------010-----0011011")
  val PSLL_HS = IntRegFile.TypeR(M"1000100----------010-----0011011")
  val PSRL_BS = IntRegFile.TypeR(M"1000110----------100-----0011011")
  val PSRL_HS = IntRegFile.TypeR(M"1000100----------100-----0011011")
  val PSRA_BS = IntRegFile.TypeR(M"1100110----------100-----0011011")
  val PSRA_HS = IntRegFile.TypeR(M"1100100----------100-----0011011")
  val PSLLI_B = IntRegFile.TypeR(M"100000001--------010-----0011011")
  val PSLLI_H = IntRegFile.TypeR(M"10000001---------010-----0011011")
  val PSRLI_B = IntRegFile.TypeR(M"100000001--------100-----0011011")
  val PSRLI_H = IntRegFile.TypeR(M"10000001---------100-----0011011")
  val PSRAI_B = IntRegFile.TypeR(M"110000001--------100-----0011011")
  val PSRAI_H = IntRegFile.TypeR(M"11000001---------100-----0011011")
  val PSRARI_H = IntRegFile.TypeR(M"11010001---------100-----0011011")
  val SRARI = IntRegFile.TypeR(M"1101001----------100-----0011011")
  val WSLL = IntRegFile.TypeR(M"0000111----------010----00011011")
  val WSLA = IntRegFile.TypeR(M"0100111----------010----00011011")
  val PWSLL_HS = IntRegFile.TypeR(M"0000101----------010----00011011")
  val PWSLL_BS = IntRegFile.TypeR(M"0000100----------010----00011011")
  val PWSLA_HS = IntRegFile.TypeR(M"0100101----------010----00011011")
  val PWSLA_BS = IntRegFile.TypeR(M"0100100----------010----00011011")
  val WSLLI = IntRegFile.TypeI(M"000001-----------010----00011011")
  val WSLAI = IntRegFile.TypeI(M"010001-----------010----00011011")
  val PWSLLI_H = IntRegFile.TypeI(M"0000001----------010----00011011")
  val PWSLLI_B = IntRegFile.TypeI(M"00000001---------010----00011011")
  val PWSLAI_H = IntRegFile.TypeI(M"0100001----------010----00011011")
  val PWSLAI_B = IntRegFile.TypeI(M"01000001---------010----00011011")
  val PSLL_DBS = IntRegFile.TypeR_RDS(M"0000110---------0110----00011011")
  val PSLL_DHS = IntRegFile.TypeR_RDS(M"0000100---------0110----00011011")
  val PSLL_DWS = IntRegFile.TypeR_RDS(M"0000101---------0110----00011011")
  val PSRL_DBS = IntRegFile.TypeR_RDS(M"0000110---------1110----00011011")
  val PSRL_DHS = IntRegFile.TypeR_RDS(M"0000100---------1110----00011011")
  val PSRL_DWS = IntRegFile.TypeR_RDS(M"0000101---------1110----00011011")
  val PSRA_DBS = IntRegFile.TypeR_RDS(M"0100110---------1110----00011011")
  val PSRA_DHS = IntRegFile.TypeR_RDS(M"0100100---------1110----00011011")
  val PSRA_DWS = IntRegFile.TypeR_RDS(M"0100101---------1110----00011011")
  // Double-wide immediate forms (TypeI_RDS: RS1, RDS, RD -- no RS2). The shamt field
  // narrows with the element: uimm3 [22:20] / uimm4 [23:20] / uimm5 [24:20], which is
  // also why the fixed prefix grows from 7 to 9 bits as the width shrinks.
  val PSLLI_DB = IntRegFile.TypeI_RDS(M"000000001-------0110----00011011")
  val PSLLI_DH = IntRegFile.TypeI_RDS(M"00000001--------0110----00011011")
  val PSLLI_DW = IntRegFile.TypeI_RDS(M"0000001---------0110----00011011")
  val PSRLI_DB = IntRegFile.TypeI_RDS(M"000000001-------1110----00011011")
  val PSRLI_DH = IntRegFile.TypeI_RDS(M"00000001--------1110----00011011")
  val PSRLI_DW = IntRegFile.TypeI_RDS(M"0000001---------1110----00011011")
  val PSRAI_DB = IntRegFile.TypeI_RDS(M"010000001-------1110----00011011")
  val PSRAI_DH = IntRegFile.TypeI_RDS(M"01000001--------1110----00011011")
  val PSRAI_DW = IntRegFile.TypeI_RDS(M"0100001---------1110----00011011")
  val PSRARI_DH = IntRegFile.TypeI_RDS(M"01010001--------1110----00011011")
  val PSRARI_DW = IntRegFile.TypeI_RDS(M"0101001---------1110----00011011")

  object Rv64 {
    val PSLL_BS = IntRegFile.TypeR(M"1000110----------010-----0011011") // psll.bs
    val PSLL_HS = IntRegFile.TypeR(M"1000100----------010-----0011011") // psll.hs
    val PSLL_WS = IntRegFile.TypeR(M"1000101----------010-----0011011") // psll.ws (RV64-only)
    val PSRL_BS = IntRegFile.TypeR(M"1000110----------100-----0011011") // psrl.bs
    val PSRL_HS = IntRegFile.TypeR(M"1000100----------100-----0011011") // psrl.hs
    val PSRL_WS = IntRegFile.TypeR(M"1000101----------100-----0011011") // psrl.ws (RV64-only)
    val PSRA_BS = IntRegFile.TypeR(M"1100110----------100-----0011011") // psra.bs
    val PSRA_HS = IntRegFile.TypeR(M"1100100----------100-----0011011") // psra.hs
    val PSRA_WS = IntRegFile.TypeR(M"1100101----------100-----0011011") // psra.ws (RV64-only)

    val PSLLI_B = IntRegFile.TypeI(M"100000001--------010-----0011011") // pslli.b (uimm3)
    val PSLLI_H = IntRegFile.TypeI(M"10000001---------010-----0011011") // pslli.h (uimm4)
    val PSLLI_W = IntRegFile.TypeI(M"1000001----------010-----0011011") // pslli.w (uimm5, RV64-only)
    val PSRLI_B = IntRegFile.TypeI(M"100000001--------100-----0011011") // psrli.b
    val PSRLI_H = IntRegFile.TypeI(M"10000001---------100-----0011011") // psrli.h
    val PSRLI_W = IntRegFile.TypeI(M"1000001----------100-----0011011") // psrli.w (RV64-only)
    val PSRAI_B = IntRegFile.TypeI(M"110000001--------100-----0011011") // psrai.b
    val PSRAI_H = IntRegFile.TypeI(M"11000001---------100-----0011011") // psrai.h
    val PSRAI_W = IntRegFile.TypeI(M"1100001----------100-----0011011") // psrai.w (RV64-only)

    val PSRARI_H = IntRegFile.TypeI(M"11010001---------100-----0011011") // psrari.h
    val PSRARI_W = IntRegFile.TypeI(M"1101001----------100-----0011011") // psrari.w (recycled: RV32 srari)
    val SRARI = IntRegFile.TypeI(M"110101-----------100-----0011011") // srari (uimm6)
  }
}

/** The draft-P packed-shift family.
 *
 * At XLEN=32 one 32-bit lane-segmented shifter (RvpShiftOps.packed) is
 * time-multiplexed over a register pair for the `.d*` forms, and a separate
 * 64-bit bank serves the widening `w*`/`pw*` ops
 */
class RvpPackedShiftPlugin(val layer: LaneLayer, val xlen: Int, val postAt: Int = 1, val pairs: Boolean = true) extends ExecutionUnitElementSimple(layer) {

  require(xlen == 32 || xlen == 64, s"RvpPackedShiftPlugin: XLEN 32 or 64, got $xlen")
  private val rv64 = xlen == 64
  private val withPairs = pairs && !rv64 // RV32 pair ops; false = no-pairs configuration: no w*/pw* bank, no two-pass, no pair-write port

  val ELEM_WIDTH = Payload(RvpElemWidth())
  val IS_IMM = Payload(Bool()) // amount from instruction field vs rs2
  val IS_SCALAR = Payload(Bool()) // srari: whole-word, no lane split
  val SHIFT_OP = Payload(RvpShiftKind())
  val RD = Payload(Bits(32 bits)) // registered packed/scalar shift result, crosses 0 -> postAt
  val RD_HI = Payload(Bits(32 bits))
  val WIDE_SIGNED = Payload(Bool())
  val DOUBLE_WIDE = Payload(Bool())

  val logic = during setup new Logic {

    // Register form: reads RS1 + RS2.
    def registerRegOpcode(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop
    }

    def registerOpcodeRs1(enc: MicroOp) = {
      val u = add(enc)
      u.spec.addRsSpec(RS1, executeAt = 0)
      u
    }

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

    // Widening and double-wide are pair concepts which do not occur in the 64-bit specificaiton, so their decode entries are not created at XLEN=64.
    if (!rv64) {
      el.setDecodingDefault(WIDE_SIGNED, False)
      el.setDecodingDefault(DOUBLE_WIDE, False)
    }

    val decoderService = host[DecoderService]
    val decoderRetainer = retains(decoderService.decodingLock)

    awaitBuild()

    assert(Riscv.XLEN.get == xlen)

    // The packed/scalar shift finishes in Execute(0), the writeback format mux lands at Execute(postAt)
    assert(postAt >= 1)
    val writeBack = newWriteback(ifp, postAt)

    val pairWrite = withPairs generate host[RvpPairWritePlugin]
    val pairRetainer = withPairs generate retains(pairWrite.elaborationLock)
    val hiPort = withPairs generate pairWrite.createPort(at = postAt) // must be <= writeAt
    if (withPairs) pairRetainer.release()

    if (!rv64) {
      registerRegOpcode(RvpPackedShiftPlugin.PSLL_BS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL)
      registerRegOpcode(RvpPackedShiftPlugin.PSLL_HS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL)
      registerRegOpcode(RvpPackedShiftPlugin.PSRL_BS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRL)
      registerRegOpcode(RvpPackedShiftPlugin.PSRL_HS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRL)
      registerRegOpcode(RvpPackedShiftPlugin.PSRA_BS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRA)
      registerRegOpcode(RvpPackedShiftPlugin.PSRA_HS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRA)
      registerRegOpcode(RvpPackedShiftPlugin.PSLLI_B).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL)
      registerRegOpcode(RvpPackedShiftPlugin.PSLLI_H).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL)
      registerRegOpcode(RvpPackedShiftPlugin.PSRLI_B).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRL)
      registerRegOpcode(RvpPackedShiftPlugin.PSRLI_H).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRL)
      registerRegOpcode(RvpPackedShiftPlugin.PSRAI_B).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRA)
      registerRegOpcode(RvpPackedShiftPlugin.PSRAI_H).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRA)
      registerRegOpcode(RvpPackedShiftPlugin.PSRARI_H).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRAR)
      registerRegOpcode(RvpPackedShiftPlugin.SRARI).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> True, IS_SCALAR -> True, SHIFT_OP -> SRAR)
      if (pairs) { // ---- register-pair ops, dropped by --rvp-no-pairs ----
      registerRegOpcode(RvpPackedShiftPlugin.WSLL).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> True, SHIFT_OP -> SLL, PAIR_RD -> True)
      registerOpcodeRs1(RvpPackedShiftPlugin.WSLLI).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> True, SHIFT_OP -> SLL, PAIR_RD -> True)
      registerOpcodeRs1(RvpPackedShiftPlugin.WSLAI).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> True, SHIFT_OP -> SLL, PAIR_RD -> True, WIDE_SIGNED -> True)
      registerRegOpcode(RvpPackedShiftPlugin.WSLA).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> True, SHIFT_OP -> SLL, PAIR_RD -> True, WIDE_SIGNED -> True)
      registerRegOpcode(RvpPackedShiftPlugin.PWSLL_HS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL, PAIR_RD -> True)
      registerRegOpcode(RvpPackedShiftPlugin.PWSLL_BS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL, PAIR_RD -> True)
      registerOpcodeRs1(RvpPackedShiftPlugin.PWSLLI_H).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL, PAIR_RD -> True)
      registerOpcodeRs1(RvpPackedShiftPlugin.PWSLLI_B).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL, PAIR_RD -> True)
      registerOpcodeRs1(RvpPackedShiftPlugin.PWSLAI_H).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL, WIDE_SIGNED -> True, PAIR_RD -> True)
      registerOpcodeRs1(RvpPackedShiftPlugin.PWSLAI_B).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL, WIDE_SIGNED -> True, PAIR_RD -> True)
      registerRegOpcode(RvpPackedShiftPlugin.PWSLA_BS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL, WIDE_SIGNED -> True, PAIR_RD -> True)
      registerRegOpcode(RvpPackedShiftPlugin.PWSLA_HS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL, WIDE_SIGNED -> True, PAIR_RD -> True)
      registerOpcodeRDS(RvpPackedShiftPlugin.PSLL_DBS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedShiftPlugin.PSLL_DHS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedShiftPlugin.PSLL_DWS).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedShiftPlugin.PSRL_DBS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedShiftPlugin.PSRL_DHS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedShiftPlugin.PSRL_DWS).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedShiftPlugin.PSRA_DBS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRA, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedShiftPlugin.PSRA_DHS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRA, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedShiftPlugin.PSRA_DWS).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRA, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSLLI_DB).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSLLI_DH).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSLLI_DW).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSRLI_DB).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSRLI_DH).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSRLI_DW).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRL, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSRAI_DB).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRA, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSRAI_DH).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRA, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSRAI_DW).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRA, PAIR_RD -> True, DOUBLE_WIDE -> True)

      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSRARI_DH).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRAR, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedShiftPlugin.PSRARI_DW).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRAR, PAIR_RD -> True, DOUBLE_WIDE -> True)

      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.WSLL, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.WSLLI, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.WSLA, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.WSLAI, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PWSLL_HS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PWSLA_BS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PWSLL_BS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PWSLA_HS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PWSLLI_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PWSLLI_B, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PWSLAI_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PWSLAI_B, PAIR_RD, True)

      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSLL_DBS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSLL_DBS, PAIR_RS1, True)

      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSLL_DWS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSLL_DWS, PAIR_RS1, True)

      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSLL_DHS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSLL_DHS, PAIR_RS1, True)


      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRL_DBS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRL_DBS, PAIR_RS1, True)

      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRL_DHS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRL_DHS, PAIR_RS1, True)

      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRL_DWS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRL_DWS, PAIR_RS1, True)

      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRA_DBS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRA_DBS, PAIR_RS1, True)

      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRA_DHS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRA_DHS, PAIR_RS1, True)

      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRA_DWS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedShiftPlugin.PSRA_DWS, PAIR_RS1, True)

      for (enc <- List(
        RvpPackedShiftPlugin.PSLLI_DB, RvpPackedShiftPlugin.PSLLI_DH, RvpPackedShiftPlugin.PSLLI_DW,
        RvpPackedShiftPlugin.PSRLI_DB, RvpPackedShiftPlugin.PSRLI_DH, RvpPackedShiftPlugin.PSRLI_DW,
        RvpPackedShiftPlugin.PSRAI_DB, RvpPackedShiftPlugin.PSRAI_DH, RvpPackedShiftPlugin.PSRAI_DW,
        RvpPackedShiftPlugin.PSRARI_DH, RvpPackedShiftPlugin.PSRARI_DW
      )) {
        decoderService.addMicroOpDecoding(enc, PAIR_RD, True)
        decoderService.addMicroOpDecoding(enc, PAIR_RS1, True)
      }
      } // pairs
    } else {
      registerRegOpcode(Rv64.PSLL_BS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL)
      registerRegOpcode(Rv64.PSLL_HS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL)
      registerRegOpcode(Rv64.PSLL_WS).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SLL)
      registerRegOpcode(Rv64.PSRL_BS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRL)
      registerRegOpcode(Rv64.PSRL_HS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRL)
      registerRegOpcode(Rv64.PSRL_WS).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRL)
      registerRegOpcode(Rv64.PSRA_BS).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRA)
      registerRegOpcode(Rv64.PSRA_HS).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRA)
      registerRegOpcode(Rv64.PSRA_WS).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> False, IS_SCALAR -> False, SHIFT_OP -> SRA)

      registerOpcodeRs1(Rv64.PSLLI_B).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL)
      registerOpcodeRs1(Rv64.PSLLI_H).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL)
      registerOpcodeRs1(Rv64.PSLLI_W).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SLL)
      registerOpcodeRs1(Rv64.PSRLI_B).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRL)
      registerOpcodeRs1(Rv64.PSRLI_H).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRL)
      registerOpcodeRs1(Rv64.PSRLI_W).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRL)
      registerOpcodeRs1(Rv64.PSRAI_B).decode(ELEM_WIDTH -> RvpElemWidth.B, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRA)
      registerOpcodeRs1(Rv64.PSRAI_H).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRA)
      registerOpcodeRs1(Rv64.PSRAI_W).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRA)

      registerOpcodeRs1(Rv64.PSRARI_H).decode(ELEM_WIDTH -> RvpElemWidth.H, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRAR)
      registerOpcodeRs1(Rv64.PSRARI_W).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> True, IS_SCALAR -> False, SHIFT_OP -> SRAR)
      registerOpcodeRs1(Rv64.SRARI).decode(ELEM_WIDTH -> RvpElemWidth.W, IS_IMM -> True, IS_SCALAR -> True, SHIFT_OP -> SRAR)
    }

    decoderRetainer.release()
    uopRetainer.release()

    val process = (!rv64) generate new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt
      val rds = if (withPairs) up(el(IntRegFile, RDS)).asUInt else U(0, 32 bits) // pair-source odd half (double-wide ops only)

      val elementWidth = this (ELEM_WIDTH)
      val half = elementWidth === RvpElemWidth.H
      val scalar = this (IS_SCALAR)

      val immediate = Mux(
        scalar,
        Decode.UOP(24 downto 20).asUInt,
        elementWidth.mux(
          RvpElemWidth.B -> Decode.UOP(22 downto 20).asUInt.resize(5),
          RvpElemWidth.H -> Decode.UOP(23 downto 20).asUInt.resize(5),
          RvpElemWidth.W -> Decode.UOP(24 downto 20).asUInt.resize(5)
        )
      )

      val packedWideShiftAmount = UInt(5 bits)

      when(this (IS_IMM)) {
        packedWideShiftAmount := Mux(
          half,
          Decode.UOP(24 downto 20).asUInt, // pwslli.h: uimm5
          Decode.UOP(23 downto 20).asUInt.resize(5) // pwslli.b: uimm4
        )
      } otherwise {
        packedWideShiftAmount := rs2(4 downto 0)
      }

      val shiftAmount = UInt(5 bits)
      val wideAmount = Mux(this (IS_IMM), Decode.UOP(25 downto 20).asUInt, rs2(5 downto 0))
      val wideBankAmount = Mux(scalar, wideAmount, packedWideShiftAmount.resize(6))
      val wideBank = if (withPairs) RvpShiftOps.wideLeftBank(rs1, wideBankAmount, elementWidth, scalar, this (WIDE_SIGNED)) else B(0, 64 bits) // pair-destination ops only
      val pairLo = wideBank(31 downto 0)
      val pairHi = wideBank(63 downto 32)

      // when the instruction does not carry an immediate value, use rs2 as shift amount
      when(!IS_IMM) {
        shiftAmount := up(el(IntRegFile, RS2)).asUInt(4 downto 0)
      } otherwise {
        shiftAmount := immediate
      }

      val doubleWide = if (withPairs) this (DOUBLE_WIDE) else False
      def pairRd = if (withPairs) this (PAIR_RD) else False
      val pass = RvpDoubleWidePass(el, isValid && SEL && doubleWide, up.isMoving, enabled = withPairs)

      // Run the low word first and the high word second
      val packedSrc = if (withPairs) pass.second ? rds | rs1 else rs1
      val packedResult = RvpShiftOps.packed(packedSrc, shiftAmount, elementWidth, this (SHIFT_OP))
      // Pass 1 shifted `rs1`
      val packedResultLo = if (withPairs) RegNextWhen(packedResult, pass.first) else packedResult

      // Register the shift result so the cross-producer writeback format mux (and its routing) moves into the Execute(postAt) stage below
      this (RD) := Mux(doubleWide,
        packedResultLo,
        Mux(pairRd,
          pairLo.asBits,
          packedResult
        )
      )
      if (withPairs) this (RD_HI) := Mux(doubleWide, packedResult, pairHi.asBits)
    }

    val writeBackStage = (!rv64) generate new el.Execute(id = postAt) {

      if (withPairs) {
        hiPort.valid := SEL && this (PAIR_RD)
        hiPort.payload := this (RD_HI)
      }

      writeBack.valid := SEL
      writeBack.payload := this (RD)
    }

    val process64 = rv64 generate new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt
      val elementWidth = this (ELEM_WIDTH)

      val immediate = elementWidth.mux(
        RvpElemWidth.B -> Decode.UOP(22 downto 20).asUInt.resize(5),
        RvpElemWidth.H -> Decode.UOP(23 downto 20).asUInt.resize(5),
        RvpElemWidth.W -> Decode.UOP(24 downto 20).asUInt.resize(5)
      )
      val shiftAmount = Mux(this (IS_IMM), immediate, rs2(4 downto 0))

      val op = this (SHIFT_OP)
      val lo = RvpShiftOps.packed(rs1(31 downto 0), shiftAmount, elementWidth, op)
      val hi = RvpShiftOps.packed(rs1(63 downto 32), shiftAmount, elementWidth, op)


      val scalar = RvpShiftOps.srari64(
        rs1.asBits, Decode.UOP(25 downto 20).asUInt)

      val isScalar = this (IS_SCALAR)
      this (RD) := Mux(isScalar, scalar(31 downto 0), lo)
      this (RD_HI) := Mux(isScalar, scalar(63 downto 32), hi)
    }

    val writeBackStage64 = rv64 generate new el.Execute(id = postAt) {
      writeBack.valid := SEL
      writeBack.payload := this (RD_HI) ## this (RD)
    }
  }
}
