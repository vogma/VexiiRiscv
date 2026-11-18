// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.core._
import spinal.lib.CountLeadingZeroes
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.execute.rvp.RvpMiscPlugin.Rv64
import vexiiriscv.execute.rvp.RvpPairPlugin.{WZIP16P, WZIP8P}
import vexiiriscv.execute.rvp.RvpPairWritePlugin.{PAIR_RD, PAIR_RS1, PAIR_RS2}
import vexiiriscv.execute.rvp.kinds.RvpBitKind._
import vexiiriscv.execute.rvp.kinds.RvpImmediateKind._
import vexiiriscv.execute.rvp.kinds.RvpMoveMergeKind._
import vexiiriscv.execute.rvp.kinds.RvpPairKind._
import vexiiriscv.execute.rvp.kinds.{RvpBitKind, RvpImmediateKind, RvpMoveMergeKind, RvpPairKind, RvpElemWidth => EW}
import vexiiriscv.execute.{ExecutionUnitElementSimple, LaneLayer}
import vexiiriscv.riscv.Riscv.XLEN
import vexiiriscv.riscv.{IntRegFile, MicroOp, RDS, RS1, RS2, RS3, Riscv}

object RvpLoadImmediatePlugin {
  val PLI_B = IntRegFile.TypeU(M"10110100--------0010-----0011011")
  val PLI_H = IntRegFile.TypeU(M"1011000----------010-----0011011")
  val PLUI_H = IntRegFile.TypeU(M"1111000----------010-----0011011")
  val PLI_DB = IntRegFile.TypeU(M"00110100--------0010----00011011")
  val PLI_DH = IntRegFile.TypeU(M"0011000----------010----00011011")
  val PLUI_DH = IntRegFile.TypeU(M"0111000----------010----00011011")
}

object RvpFunnelShiftPlugin {
  val SLX = IntRegFile.TypeR_RDS(M"1000111----------001-----0111011")
  val SRX = IntRegFile.TypeR_RDS(M"1010111----------001-----0111011")
}

object RvpMoveMergePlugin {
  val MVM = IntRegFile.TypeR_RDS(M"1010100----------001-----0111011")
  val MVMN = IntRegFile.TypeR_RDS(M"1010101----------001-----0111011")
  val MERGE = IntRegFile.TypeR_RDS(M"1010110----------001-----0111011")
}

object RvpScalarBitPlugin {
  val PABS = IntRegFile.TypeI(M"011000000111-----001-----0010011")
  val CLS = IntRegFile.TypeI(M"011000000011-----001-----0010011")
  val REV = IntRegFile.TypeI(M"011010011111-----101-----0010011")
  val PACK = IntRegFile.TypeR(M"0000100----------100-----0110011")
  val PSEXT_H_B = IntRegFile.TypeI(M"111000000100-----010-----0011011")
  val PSEXT_DH_B = IntRegFile.TypeI_RDS(M"011000000100----0110----00011011")
  val PSEXT_DW_B = IntRegFile.TypeI_RDS(M"011000100100----0110----00011011")
  val PSEXT_DW_H = IntRegFile.TypeI_RDS(M"011000100101----0110----00011011")
}

object RvpPairPlugin {
  val PPAIRE_B = IntRegFile.TypeR(M"1000000----------100-----0111011")
  val PPAIREO_B = IntRegFile.TypeR(M"1001000----------100-----0111011")
  val PPAIREO_H = IntRegFile.TypeR(M"1001001----------100-----0111011")
  val PPAIROE_B = IntRegFile.TypeR(M"1010000----------100-----0111011")
  val PPAIROE_H = IntRegFile.TypeR(M"1010001----------100-----0111011")
  val PPAIRO_B = IntRegFile.TypeR(M"1011000----------100-----0111011")
  val PPAIRO_H = IntRegFile.TypeR(M"1011001----------100-----0111011")
  val WZIP8P = IntRegFile.TypeR(M"0111100----------010----00011011")
  val WZIP16P = IntRegFile.TypeR(M"0111101----------010----00011011")
  val PPAIRE_DB = IntRegFile.TypeR_RDS_RS3(M"1000000----0----1110----00011011")
  val PPAIRE_DH = IntRegFile.TypeR_RDS_RS3(M"1000001----0----1110----00011011")
  val PPAIREO_DB = IntRegFile.TypeR_RDS_RS3(M"1001000----0----1110----00011011")
  val PPAIREO_DH = IntRegFile.TypeR_RDS_RS3(M"1001001----0----1110----00011011")
  val PPAIROE_DB = IntRegFile.TypeR_RDS_RS3(M"1010000----0----1110----00011011")
  val PPAIROE_DH = IntRegFile.TypeR_RDS_RS3(M"1010001----0----1110----00011011")
  val PPAIRO_DB = IntRegFile.TypeR_RDS_RS3(M"1011000----0----1110----00011011")
  val PPAIRO_DH = IntRegFile.TypeR_RDS_RS3(M"1011001----0----1110----00011011")
}

object RvpMiscFamily extends SpinalEnum {
  val LOAD_IMMEDIATE, FUNNEL_SHIFT, MOVE_MERGE, SCALAR_BIT, PAIR = newElement()
}

object RvpMiscPlugin {

  object Rv64 {
    val PLI_B = IntRegFile.TypeU(M"10110100--------0010-----0011011") // pli.b
    val PLI_H = IntRegFile.TypeU(M"1011000----------010-----0011011") // pli.h
    val PLI_W = IntRegFile.TypeU(M"1011001----------010-----0011011") // pli.w (RV64-only)
    val PLUI_H = IntRegFile.TypeU(M"1111000----------010-----0011011") // plui.h
    val PLUI_W = IntRegFile.TypeU(M"1111001----------010-----0011011") // plui.w (RV64-only)

    val SLX = IntRegFile.TypeR_RDS(M"1000111----------001-----0111011") // slx
    val SRX = IntRegFile.TypeR_RDS(M"1010111----------001-----0111011") // srx
    val MVM = IntRegFile.TypeR_RDS(M"1010100----------001-----0111011") // mvm
    val MVMN = IntRegFile.TypeR_RDS(M"1010101----------001-----0111011") // mvmn
    val MERGE = IntRegFile.TypeR_RDS(M"1010110----------001-----0111011") // merge

    val ABS = IntRegFile.TypeI(M"011000000111-----001-----0010011") // abs
    val ABSW = IntRegFile.TypeI(M"011000000111-----001-----0011011") // absw (RV64-only)
    val CLS = IntRegFile.TypeI(M"011000000011-----001-----0010011") // cls
    val CLSW = IntRegFile.TypeI(M"011000000011-----001-----0011011") // clsw (RV64-only)
    val REV = IntRegFile.TypeI(M"011010111111-----101-----0010011") // rev (rv64 uimm6 encoding)
    val REV16 = IntRegFile.TypeI(M"011010110000-----101-----0010011") // rev16 (RV64-only)
    val PACK = IntRegFile.TypeR(M"0000100----------100-----0110011") // pack
    val PSEXT_H_B = IntRegFile.TypeI(M"111000000100-----010-----0011011") // psext.h.b
    val PSEXT_W_B = IntRegFile.TypeI(M"111000100100-----010-----0011011") // psext.w.b (RV64-only)
    val PSEXT_W_H = IntRegFile.TypeI(M"111000100101-----010-----0011011") // psext.w.h (RV64-only)

    val PPAIRE_B = IntRegFile.TypeR(M"1000000----------100-----0111011") // ppaire.b
    val PPAIRE_H = IntRegFile.TypeR(M"1000001----------100-----0111011") // ppaire.h (RV64-only)
    val PPAIREO_B = IntRegFile.TypeR(M"1001000----------100-----0111011") // ppaireo.b
    val PPAIREO_H = IntRegFile.TypeR(M"1001001----------100-----0111011") // ppaireo.h
    val PPAIREO_W = IntRegFile.TypeR(M"1001011----------100-----0111011") // ppaireo.w (RV64-only)
    val PPAIROE_B = IntRegFile.TypeR(M"1010000----------100-----0111011") // ppairoe.b
    val PPAIROE_H = IntRegFile.TypeR(M"1010001----------100-----0111011") // ppairoe.h
    val PPAIROE_W = IntRegFile.TypeR(M"1010011----------100-----0111011") // ppairoe.w (RV64-only)
    val PPAIRO_B = IntRegFile.TypeR(M"1011000----------100-----0111011") // ppairo.b
    val PPAIRO_H = IntRegFile.TypeR(M"1011001----------100-----0111011") // ppairo.h
    val PPAIRO_W = IntRegFile.TypeR(M"1011011----------100-----0111011") // ppairo.w (RV64-only)

    val ZIP8P = IntRegFile.TypeR(M"1111000----------010-----0111011") // zip8p (RV64-only)
    val ZIP8HP = IntRegFile.TypeR(M"1111010----------010-----0111011") // zip8hp (RV64-only)
    val ZIP16P = IntRegFile.TypeR(M"1111001----------010-----0111011") // zip16p (RV64-only)
    val ZIP16HP = IntRegFile.TypeR(M"1111011----------010-----0111011") // zip16hp (RV64-only)
    val UNZIP8P = IntRegFile.TypeR(M"1110000----------010-----0111011") // unzip8p (RV64-only)
    val UNZIP8HP = IntRegFile.TypeR(M"1110010----------010-----0111011") // unzip8hp (RV64-only)
    val UNZIP16P = IntRegFile.TypeR(M"1110001----------010-----0111011") // unzip16p (RV64-only)
    val UNZIP16HP = IntRegFile.TypeR(M"1110011----------010-----0111011") // unzip16hp (RV64-only)
  }
}

/**
 * Five small RVP units behind one IntFormatPlugin writeback and one pair-high
 * contribution.  Their datapaths remain independent Areas and MISC_FAMILY only selects which
 * already-computed result drives the shared ports.
 */
class RvpMiscPlugin(val layer: LaneLayer, val xlen: Int, val pairs: Boolean = true) extends ExecutionUnitElementSimple(layer) {

  require(xlen == 32 || xlen == 64, s"RvpMiscPlugin: XLEN 32 or 64, got $xlen")
  private val rv64 = xlen == 64
  // RV32 register-pair support; false = the no-pairs configuration. The
  // single-register `ppair*` permutations and `pack` (Zbb zext.h owner) are NOT pair ops and
  // stay; only the `.d*` immediates/sign-extends, the `ppair*.d*` gathers and `wzip*p` go.
  private val withPairs = pairs && !rv64

  val MISC_FAMILY = Payload(RvpMiscFamily())
  val IMM_OP = Payload(RvpImmediateKind())
  val LEFT_SHIFT = Payload(Bool())
  val MOVE_OP = Payload(RvpMoveMergeKind())
  val BIT_OP = Payload(RvpBitKind())
  val PAIR_OP = Payload(RvpPairKind())

  // XLEN=64 only. The shared kind enums stay exactly as RV32. The
  // RV64 forms that RV32 has no name for are carried by these flags instead, so
  // no RV32 decode payload widens.
  //   ELEM_W  LOAD_IMMEDIATE: the lane the immediate fills. PAIR: the source
  //           sub-element of a ppair gather / the chunk of a zip.
  //   HI_1/2  ppair: take the odd (high) sub-element of rs1 / rs2.
  //   ZIP_FORM / IS_UNZIP / ZIP_HIGH  select zip vs ppair, its direction, and
  //           which 32-bit half of each source it reads.
  //   IS_W    absw / clsw: the base-ISA W convention
  //   IS_REV16  rev16 reverses 16-bit chunks where rev reverses bits.
  val ELEM_W = Payload(EW())
  val HI_1 = Payload(Bool())
  val HI_2 = Payload(Bool())
  val ZIP_FORM = Payload(Bool())
  val IS_UNZIP = Payload(Bool())
  val ZIP_HIGH = Payload(Bool())
  val IS_W = Payload(Bool())
  val IS_REV16 = Payload(Bool())

  val logic = during setup new Logic {

    def registerNoRs(enc: MicroOp) = add(enc)

    def registerRs1(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop
    }

    def registerRs1Rs2(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop
    }

    def registerRs1Rds(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RDS, executeAt = 0)
      uop
    }

    def registerRs1Rs2Rds(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop.spec.addRsSpec(RDS, executeAt = 0)
      uop
    }

    def registerPair(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0) // X[2*rs1_p]
      uop.spec.addRsSpec(RS2, executeAt = 0) // X[2*rs2_p]
      uop.spec.addRsSpec(RS3, executeAt = 0) // X[2*rs2_p+1], x0 when rs2_p == 0
      uop.spec.addRsSpec(RDS, executeAt = 0) // X[2*rs1_p+1], x0 when rs1_p == 0
      uop.decode(PAIR_RD -> True)
      for (p <- List(PAIR_RS1, PAIR_RS2, PAIR_RD))
        decoderService.addMicroOpDecoding(enc, p, True)
      uop
    }

    val decoderService = host[DecoderService]
    val decoderRetainer = retains(decoderService.decodingLock)

    // Only the XLEN=64 arm uses these, and only some of its uops set them.
    if (rv64) for (flag <- List(HI_1, HI_2, ZIP_FORM, IS_UNZIP, ZIP_HIGH, IS_W, IS_REV16))
      el.setDecodingDefault(flag, False)

    awaitBuild()
    assert(Riscv.XLEN.get == xlen)

    val writeback = newWriteback(ifp, 0)

    // No RV64 draft-P instruction writes a register pair, so the pair-high port
    // -- and with it every `.d*` form below -- is RV32-only.
    val pairWrite = withPairs generate host[RvpPairWritePlugin]
    val pairRetainer = withPairs generate retains(pairWrite.elaborationLock)
    val hiPort = withPairs generate pairWrite.createPort(at = 0) // must be <= writeAt
    if (withPairs) pairRetainer.release()

    val rv32Decode = (!rv64) generate new Area {
      registerNoRs(RvpLoadImmediatePlugin.PLI_H).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LI_H)
      registerNoRs(RvpLoadImmediatePlugin.PLI_B).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LI_B)
      registerNoRs(RvpLoadImmediatePlugin.PLUI_H).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LUI_H)
      if (pairs) { // ---- register-pair ops, dropped by --rvp-no-pairs ----
      registerNoRs(RvpLoadImmediatePlugin.PLI_DB).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LI_B, PAIR_RD -> True)
      registerNoRs(RvpLoadImmediatePlugin.PLI_DH).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LI_H, PAIR_RD -> True)
      registerNoRs(RvpLoadImmediatePlugin.PLUI_DH).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LUI_H, PAIR_RD -> True)
      } // pairs

      registerRs1Rs2Rds(RvpFunnelShiftPlugin.SLX).decode(MISC_FAMILY -> RvpMiscFamily.FUNNEL_SHIFT, LEFT_SHIFT -> True)
      registerRs1Rs2Rds(RvpFunnelShiftPlugin.SRX).decode(MISC_FAMILY -> RvpMiscFamily.FUNNEL_SHIFT, LEFT_SHIFT -> False)

      registerRs1Rs2Rds(RvpMoveMergePlugin.MVM).decode(MISC_FAMILY -> RvpMiscFamily.MOVE_MERGE, MOVE_OP -> MVM)
      registerRs1Rs2Rds(RvpMoveMergePlugin.MVMN).decode(MISC_FAMILY -> RvpMiscFamily.MOVE_MERGE, MOVE_OP -> MVMN)
      registerRs1Rs2Rds(RvpMoveMergePlugin.MERGE).decode(MISC_FAMILY -> RvpMiscFamily.MOVE_MERGE, MOVE_OP -> MERGE)

      registerRs1(RvpScalarBitPlugin.PABS).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> ABS)
      registerRs1(RvpScalarBitPlugin.CLS).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> CLS)
      registerRs1(RvpScalarBitPlugin.REV).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> REV)
      registerRs1Rs2(RvpScalarBitPlugin.PACK).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> PACK)
      registerRs1(RvpScalarBitPlugin.PSEXT_H_B).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> SEXT)
      if (pairs) { // ---- register-pair ops, dropped by --rvp-no-pairs ----
      registerRs1Rds(RvpScalarBitPlugin.PSEXT_DH_B).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> SEXT, PAIR_RD -> True)
      registerRs1Rds(RvpScalarBitPlugin.PSEXT_DW_B).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> SEXT_W_B, PAIR_RD -> True)
      registerRs1Rds(RvpScalarBitPlugin.PSEXT_DW_H).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> SEXT_W_H, PAIR_RD -> True)

      for (enc <- List(RvpScalarBitPlugin.PSEXT_DH_B, RvpScalarBitPlugin.PSEXT_DW_B, RvpScalarBitPlugin.PSEXT_DW_H)) {
        decoderService.addMicroOpDecoding(enc, PAIR_RD, True)
        decoderService.addMicroOpDecoding(enc, PAIR_RS1, True)
      }
      } // pairs

      registerRs1Rs2(RvpPairPlugin.PPAIRE_B).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIRE_B)
      registerRs1Rs2(RvpPairPlugin.PPAIREO_B).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIREO_B)
      registerRs1Rs2(RvpPairPlugin.PPAIREO_H).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIREO_H)
      registerRs1Rs2(RvpPairPlugin.PPAIROE_B).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIROE_B)
      registerRs1Rs2(RvpPairPlugin.PPAIROE_H).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIROE_H)
      registerRs1Rs2(RvpPairPlugin.PPAIRO_B).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIRO_B)
      registerRs1Rs2(RvpPairPlugin.PPAIRO_H).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIRO_H)
      if (pairs) { // ---- register-pair ops, dropped by --rvp-no-pairs ----
      registerPair(RvpPairPlugin.PPAIRE_DB).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIRE_B)
      registerPair(RvpPairPlugin.PPAIRE_DH).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIRE_H)
      registerPair(RvpPairPlugin.PPAIREO_DB).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIREO_B)
      registerPair(RvpPairPlugin.PPAIREO_DH).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIREO_H)
      registerPair(RvpPairPlugin.PPAIROE_DB).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIROE_B)
      registerPair(RvpPairPlugin.PPAIROE_DH).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIROE_H)
      registerPair(RvpPairPlugin.PPAIRO_DB).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIRO_B)
      registerPair(RvpPairPlugin.PPAIRO_DH).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> PAIRO_H)
      registerRs1Rs2(WZIP8P).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> ZIP8, PAIR_RD -> True)
      registerRs1Rs2(WZIP16P).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, PAIR_OP -> ZIP16, PAIR_RD -> True)

      decoderService.addMicroOpDecoding(WZIP8P, PAIR_RD, True)
      decoderService.addMicroOpDecoding(WZIP16P, PAIR_RD, True)
      } // pairs
    }

    // ---- RV64 decode --------------------------------------------------------
    val rv64Decode = rv64 generate new Area {
      registerNoRs(Rv64.PLI_B).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LI_B, ELEM_W -> EW.B)
      registerNoRs(Rv64.PLI_H).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LI_H, ELEM_W -> EW.H)
      registerNoRs(Rv64.PLI_W).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LI_H, ELEM_W -> EW.W)
      registerNoRs(Rv64.PLUI_H).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LUI_H, ELEM_W -> EW.H)
      registerNoRs(Rv64.PLUI_W).decode(MISC_FAMILY -> RvpMiscFamily.LOAD_IMMEDIATE, IMM_OP -> LUI_H, ELEM_W -> EW.W)

      registerRs1Rs2Rds(Rv64.SLX).decode(MISC_FAMILY -> RvpMiscFamily.FUNNEL_SHIFT, LEFT_SHIFT -> True)
      registerRs1Rs2Rds(Rv64.SRX).decode(MISC_FAMILY -> RvpMiscFamily.FUNNEL_SHIFT, LEFT_SHIFT -> False)

      registerRs1Rs2Rds(Rv64.MVM).decode(MISC_FAMILY -> RvpMiscFamily.MOVE_MERGE, MOVE_OP -> MVM)
      registerRs1Rs2Rds(Rv64.MVMN).decode(MISC_FAMILY -> RvpMiscFamily.MOVE_MERGE, MOVE_OP -> MVMN)
      registerRs1Rs2Rds(Rv64.MERGE).decode(MISC_FAMILY -> RvpMiscFamily.MOVE_MERGE, MOVE_OP -> MERGE)

      registerRs1(Rv64.ABS).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> ABS)
      registerRs1(Rv64.ABSW).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> ABS, IS_W -> True)
      registerRs1(Rv64.CLS).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> CLS)
      registerRs1(Rv64.CLSW).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> CLS, IS_W -> True)
      registerRs1(Rv64.REV).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> REV)
      registerRs1(Rv64.REV16).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> REV, IS_REV16 -> True)
      registerRs1Rs2(Rv64.PACK).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> PACK)
      registerRs1(Rv64.PSEXT_H_B).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> SEXT)
      registerRs1(Rv64.PSEXT_W_B).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> SEXT_W_B)
      registerRs1(Rv64.PSEXT_W_H).decode(MISC_FAMILY -> RvpMiscFamily.SCALAR_BIT, BIT_OP -> SEXT_W_H)

      registerRs1Rs2(Rv64.PPAIRE_B).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.B)
      registerRs1Rs2(Rv64.PPAIRE_H).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.H)
      registerRs1Rs2(Rv64.PPAIREO_B).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.B, HI_2 -> True)
      registerRs1Rs2(Rv64.PPAIREO_H).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.H, HI_2 -> True)
      registerRs1Rs2(Rv64.PPAIREO_W).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.W, HI_2 -> True)
      registerRs1Rs2(Rv64.PPAIROE_B).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.B, HI_1 -> True)
      registerRs1Rs2(Rv64.PPAIROE_H).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.H, HI_1 -> True)
      registerRs1Rs2(Rv64.PPAIROE_W).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.W, HI_1 -> True)
      registerRs1Rs2(Rv64.PPAIRO_B).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.B, HI_1 -> True, HI_2 -> True)
      registerRs1Rs2(Rv64.PPAIRO_H).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.H, HI_1 -> True, HI_2 -> True)
      registerRs1Rs2(Rv64.PPAIRO_W).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ELEM_W -> EW.W, HI_1 -> True, HI_2 -> True)

      registerRs1Rs2(Rv64.ZIP8P).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ZIP_FORM -> True, ELEM_W -> EW.B)
      registerRs1Rs2(Rv64.ZIP8HP).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ZIP_FORM -> True, ELEM_W -> EW.B, ZIP_HIGH -> True)
      registerRs1Rs2(Rv64.ZIP16P).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ZIP_FORM -> True, ELEM_W -> EW.H)
      registerRs1Rs2(Rv64.ZIP16HP).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ZIP_FORM -> True, ELEM_W -> EW.H, ZIP_HIGH -> True)
      registerRs1Rs2(Rv64.UNZIP8P).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ZIP_FORM -> True, IS_UNZIP -> True, ELEM_W -> EW.B)
      registerRs1Rs2(Rv64.UNZIP8HP).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ZIP_FORM -> True, IS_UNZIP -> True, ELEM_W -> EW.B, ZIP_HIGH -> True)
      registerRs1Rs2(Rv64.UNZIP16P).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ZIP_FORM -> True, IS_UNZIP -> True, ELEM_W -> EW.H)
      registerRs1Rs2(Rv64.UNZIP16HP).decode(MISC_FAMILY -> RvpMiscFamily.PAIR, ZIP_FORM -> True, IS_UNZIP -> True, ELEM_W -> EW.H, ZIP_HIGH -> True)
    }

    decoderRetainer.release()
    uopRetainer.release()

    val process = (!rv64) generate new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt
      val rds = up(el(IntRegFile, RDS)).asUInt // funnel shift, move/merge (and the pair gathers)
      // RS3 = the odd half of the second pair source; only `ppair*.d*` declare it.
      val rs3 = if (withPairs) up(el(IntRegFile, RS3)).asUInt else U(0, 32 bits)

      val family = this (MISC_FAMILY)
      val immOp = this (IMM_OP)
      val leftShift = this (LEFT_SHIFT)
      val moveOp = this (MOVE_OP)
      val bitOp = this (BIT_OP)
      val pairOp = this (PAIR_OP)

      val loadImmediate = new Area {
        val liByte = Decode.UOP(23 downto 16)
        val imm10 = Decode.UOP(24 downto 15)
        val liHalfword = imm10.asSInt.resize(16).asBits
        val luiHalfword = imm10 ## B"6'x0"

        val result = immOp.mux(
          LI_B -> (liByte ## liByte ## liByte ## liByte),
          LI_H -> (liHalfword ## liHalfword),
          LUI_H -> (luiHalfword ## luiHalfword)
        )
      }

      val funnelShift = new Area {
        val shamt = rs2(4 downto 0)
        val leftResult = ((rds ## rs1) << shamt)(63 downto 32)
        val rightResult = ((rs1 ## rds) >> shamt)(31 downto 0)
        val result = Mux(leftShift, leftResult, rightResult)
      }

      val moveMerge = new Area {
        val resultMvm = (rs2 & rs1) | (~rs2 & rds)
        val resultMvmN = (rs2 & rds) | (~rs2 & rs1)
        val resultMerge = (rds & rs2) | (~rds & rs1)

        val result = moveOp.mux(
          MVM -> resultMvm.asBits,
          MVMN -> resultMvmN.asBits,
          MERGE -> resultMerge.asBits
        )
      }

      val scalarBit = new Area {
        val sign = rs1.msb
        val packResult = rs2(15 downto 0) ## rs1(15 downto 0)
        // cls counts the bits below the sign that still equal it. XOR against the
        // sign turns that run into leading zeroes and the appended 1 caps the
        // count at XLEN-1 for the all-sign case, so the log-depth CLZ tree needs
        // neither a popcount nor a -1.
        val signDiff = rs1(XLEN.get - 2 downto 0).asBits ^ B(XLEN.get - 1 bits, default -> sign)
        val clsResult = CountLeadingZeroes(signDiff ## True).resize(32)
        val absResult = Mux(sign, ~rs1 + 1, rs1)
        val revResult = rs1.reversed

        def sextHB(src: UInt): Bits = {
          val lane0 = src(7 downto 0).asSInt.resize(16).asBits
          val lane1 = src(23 downto 16).asSInt.resize(16).asBits
          lane1 ## lane0
        }

        def sextWB(src: UInt): Bits = src(7 downto 0).asSInt.resize(32).asBits

        def sextWH(src: UInt): Bits = src(15 downto 0).asSInt.resize(32).asBits

        val resultHi = Mux(
          bitOp === SEXT_W_B,
          sextWB(rds),
          Mux(bitOp === SEXT_W_H, sextWH(rds), sextHB(rds))
        )

        val result = bitOp.mux(
          ABS -> absResult.asBits,
          CLS -> clsResult.asBits,
          REV -> revResult.asBits,
          PACK -> packResult,
          SEXT -> sextHB(rs1),
          SEXT_W_H -> sextWH(rs1),
          SEXT_W_B -> sextWB(rs1)
        )
      }

      val pair = new Area {
        def pairLanes(a: UInt, b: UInt, elementWidth: Int, hi1: Boolean, hi2: Boolean): Bits = {
          val half = elementWidth / 2
          val n = 32 / elementWidth
          Cat((0 until n).map { i =>
            def sub(x: UInt, hi: Boolean) =
              if (hi) x(elementWidth * i + elementWidth - 1 downto elementWidth * i + half)
              else x(elementWidth * i + half - 1 downto elementWidth * i)

            sub(b, hi2) ## sub(a, hi1)
          })
        }

        def permuteWord(a: UInt, b: UInt, zip8: Bits, zip16: Bits): Bits = pairOp.mux(
          PAIRE_B -> pairLanes(a, b, 16, false, false),
          PAIRE_H -> pairLanes(a, b, 32, false, false),
          PAIREO_B -> pairLanes(a, b, 16, false, true),
          PAIREO_H -> pairLanes(a, b, 32, false, true),
          PAIROE_B -> pairLanes(a, b, 16, true, false),
          PAIROE_H -> pairLanes(a, b, 32, true, false),
          PAIRO_B -> pairLanes(a, b, 16, true, true),
          PAIRO_H -> pairLanes(a, b, 32, true, true),
          ZIP8 -> zip8,
          ZIP16 -> zip16
        )

        val lo8Zip = rs2(15 downto 8) ## rs1(15 downto 8) ## rs2(7 downto 0) ## rs1(7 downto 0)
        val hi8Zip = rs2(31 downto 24) ## rs1(31 downto 24) ## rs2(23 downto 16) ## rs1(23 downto 16)
        val lo16Zip = rs2(15 downto 0) ## rs1(15 downto 0)
        val hi16Zip = rs2(31 downto 16) ## rs1(31 downto 16)

        val result = permuteWord(rs1, rs2, lo8Zip, lo16Zip)
        val resultHi = permuteWord(rds, rs3, hi8Zip, hi16Zip)
      }

      val result = family.mux(
        RvpMiscFamily.LOAD_IMMEDIATE -> loadImmediate.result,
        RvpMiscFamily.FUNNEL_SHIFT -> funnelShift.result,
        RvpMiscFamily.MOVE_MERGE -> moveMerge.result,
        RvpMiscFamily.SCALAR_BIT -> scalarBit.result,
        RvpMiscFamily.PAIR -> pair.result
      )

      // Only these three families ever assert PAIR_RD.  Funnel and move/merge have no high
      // result, so keeping them out of this mux avoids adding dead inputs to the pair port.
      val resultHi = withPairs generate Mux(
        family === RvpMiscFamily.LOAD_IMMEDIATE,
        loadImmediate.result,
        Mux(family === RvpMiscFamily.SCALAR_BIT, scalarBit.resultHi, pair.resultHi)
      )

      writeback.valid := SEL
      writeback.payload := result
      if (withPairs) { // pair-destination high half -> RvpPairWritePlugin
        hiPort.valid := SEL && this (PAIR_RD)
        hiPort.payload := resultHi
      }
    }

    // XLEN=64 datapath
    // The same five families. Everything here is
    // combinational lane bookkeeping except the funnel shift, which needs a
    // 128-bit barrel.
    val process64 = rv64 generate new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt
      val rds = up(el(IntRegFile, RDS)).asUInt

      val family = this (MISC_FAMILY)
      val immOp = this (IMM_OP)
      val leftShift = this (LEFT_SHIFT)
      val moveOp = this (MOVE_OP)
      val bitOp = this (BIT_OP)
      val elemW = this (ELEM_W)
      val isW = this (IS_W)
      val isRev16 = this (IS_REV16)
      val hi1 = this (HI_1)
      val hi2 = this (HI_2)
      val zipForm = this (ZIP_FORM)
      val isUnzip = this (IS_UNZIP)
      val zipHigh = this (ZIP_HIGH)

      /** `v` repeated until it fills the register. */
      def replicate(v: Bits): Bits = Cat(List.fill(xlen / v.getWidth)(v))

      /** Element `idx` of `x`, `ew` bits wide. */
      def elemOf(x: UInt, ew: Int, idx: Int): UInt = x(ew * idx + ew - 1 downto ew * idx)

      val loadImmediate = new Area {
        val liByte = Decode.UOP(23 downto 16)
        val imm10 = Decode.UOP(24 downto 15)
        val liHalfword = imm10.asSInt.resize(16).asBits
        val liWord = imm10.asSInt.resize(32).asBits
        val luiHalfword = imm10 ## B"6'x0"
        val luiWord = imm10 ## B"22'x0"
        val word = elemW === EW.W

        val result = immOp.mux(
          LI_B -> replicate(liByte),
          LI_H -> Mux(word, replicate(liWord), replicate(liHalfword)),
          LUI_H -> Mux(word, replicate(luiWord), replicate(luiHalfword))
        )
      }

      val funnelShift = new Area {
        // One 2*XLEN right shifter serves both directions:
        //   srx = ((rs1 @ rd) >> shamt)[XLEN-1:0]
        //   slx = ((rd @ rs1) << shamt)[2*XLEN-1:XLEN], and shifting a
        //         2*XLEN value left by s and keeping the top half is the same
        //         as shifting it right by XLEN-s and keeping the bottom one
        val shamt = rs2(log2Up(xlen) - 1 downto 0)
        val src = Mux(leftShift, rds ## rs1, rs1 ## rds).asUInt
        val amount = Mux(leftShift, U(xlen, log2Up(xlen) + 1 bits) - shamt.resize(log2Up(xlen) + 1),
          shamt.resize(log2Up(xlen) + 1))
        val result = (src >> amount)(xlen - 1 downto 0).asBits
      }

      val moveMerge = new Area {
        val resultMvm = (rs2 & rs1) | (~rs2 & rds)
        val resultMvmN = (rs2 & rds) | (~rs2 & rs1)
        val resultMerge = (rds & rs2) | (~rds & rs1)

        val result = moveOp.mux(
          MVM -> resultMvm.asBits,
          MVMN -> resultMvmN.asBits,
          MERGE -> resultMerge.asBits
        )
      }

      val scalarBit = new Area {
        val lowWord = rs1(31 downto 0)
        val packResult = rs2(xlen / 2 - 1 downto 0) ## rs1(xlen / 2 - 1 downto 0)

        // Count leading sign bits: XOR the bits below the sign against it so the
        // run becomes leading zeroes, and append a 1 to cap the count at w-1 for
        // the all-sign case
        def leadingSigns(v: UInt): UInt = {
          val w = v.getWidth
          val signDiff = v(w - 2 downto 0).asBits ^ B(w - 1 bits, default -> v.msb)
          CountLeadingZeroes(signDiff ## True)
        }

        val clsResult = Mux(isW, leadingSigns(lowWord).resize(xlen), leadingSigns(rs1).resize(xlen))

        // absw negates as a 32-bit value and sign-extends. abs does not extend.
        val absWord = Mux(lowWord.msb, ~lowWord + 1, lowWord)
        val absResult = Mux(isW, absWord.asSInt.resize(xlen).asUInt, Mux(rs1.msb, ~rs1 + 1, rs1))

        val revResult = Mux(isRev16,
          Cat((0 until xlen / 16).map(i => elemOf(rs1, 16, xlen / 16 - 1 - i))),
          rs1.reversed.asBits)

        // The source sub-element sits at the bottom of each destination lane.
        def sextLanes(srcWidth: Int, dstWidth: Int): Bits =
          Cat((0 until xlen / dstWidth).map(i =>
            rs1(dstWidth * i + srcWidth - 1 downto dstWidth * i).asSInt.resize(dstWidth).asBits))

        val result = bitOp.mux(
          ABS -> absResult.asBits,
          CLS -> clsResult.asBits,
          REV -> revResult,
          PACK -> packResult,
          SEXT -> sextLanes(8, 16),
          SEXT_W_H -> sextLanes(16, 32),
          SEXT_W_B -> sextLanes(8, 32)
        )
      }

      val pair = new Area {
        // ppair: `resultWidth` is twice the ELEM_W the mnemonic names, because
        // each result element pairs one sub-half of rs1 (low) with one of rs2
        // (high)
        // hi1/hi2 pick the odd sub-half of each. Same shape as the RV32
        // pairLanes above, over 4/2/1 lanes instead of 2/1.
        def pairLanes(resultWidth: Int): Bits = {
          val half = resultWidth / 2
          Cat((0 until xlen / resultWidth).map { i =>
            def sub(x: UInt, hi: Bool) = Mux(hi,
              x(resultWidth * i + resultWidth - 1 downto resultWidth * i + half),
              x(resultWidth * i + half - 1 downto resultWidth * i))

            sub(rs2, hi2) ## sub(rs1, hi1)
          })
        }

        // zip: the 32/ew chunks of the selected half alternate rs1, rs2.
        def zipLanes(ew: Int): Bits = {
          val n = 32 / ew
          Cat((0 until n).flatMap { i =>
            def chunk(x: UInt) = Mux(zipHigh, elemOf(x, ew, n + i), elemOf(x, ew, i))

            List(chunk(rs1), chunk(rs2))
          })
        }

        // unzip: every second chunk of rs1 fills the low half of the result and
        // the matching chunks of rs2 the high half.
        def unzipLanes(ew: Int): Bits = {
          val n = 32 / ew

          def chunks(x: UInt) = (0 until n).map(i =>
            Mux(zipHigh, elemOf(x, ew, 2 * i + 1), elemOf(x, ew, 2 * i)))

          Cat(chunks(rs1) ++ chunks(rs2))
        }

        val byte = elemW === EW.B
        val pairResult = elemW.mux(
          EW.B -> pairLanes(16),
          EW.H -> pairLanes(32),
          EW.W -> pairLanes(64)
        )
        val zipResult = Mux(isUnzip,
          Mux(byte, unzipLanes(8), unzipLanes(16)),
          Mux(byte, zipLanes(8), zipLanes(16)))

        val result = Mux(zipForm, zipResult, pairResult)
      }

      val result = family.mux(
        RvpMiscFamily.LOAD_IMMEDIATE -> loadImmediate.result,
        RvpMiscFamily.FUNNEL_SHIFT -> funnelShift.result,
        RvpMiscFamily.MOVE_MERGE -> moveMerge.result,
        RvpMiscFamily.SCALAR_BIT -> scalarBit.result,
        RvpMiscFamily.PAIR -> pair.result
      )

      writeback.valid := SEL
      writeback.payload := result
    }
  }
}
