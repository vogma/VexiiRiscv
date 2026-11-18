// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.execute.rvp.RvpNarrowPlugin._
import vexiiriscv.execute.rvp.RvpPairWritePlugin.PAIR_RS1
import vexiiriscv.execute.rvp.kinds.RvpElemWidth
import vexiiriscv.execute.rvp.ops.RvpNarrowOps
import vexiiriscv.execute.{ExecutionUnitElementSimple, LaneLayer}
import vexiiriscv.riscv._

import scala.language.postfixOps


object RvpNarrowPlugin {
  val NSRL = IntRegFile.TypeR_RDS(M"0000111---------1100-----0011011")
  val NSRA = IntRegFile.TypeR_RDS(M"0100111---------1100-----0011011")
  val NSRAR = IntRegFile.TypeR_RDS(M"0101111---------1100-----0011011")
  val PNSRL_HS = IntRegFile.TypeR_RDS(M"0000101---------1100-----0011011")
  val PNSRL_BS = IntRegFile.TypeR_RDS(M"0000100---------1100-----0011011")
  val PNSRA_BS = IntRegFile.TypeR_RDS(M"0100100---------1100-----0011011")
  val PNSRA_HS = IntRegFile.TypeR_RDS(M"0100101---------1100-----0011011")
  val PNSRAR_BS = IntRegFile.TypeR_RDS(M"0101100---------1100-----0011011")
  val PNSRAR_HS = IntRegFile.TypeR_RDS(M"0101101---------1100-----0011011")
  val NSRLI = IntRegFile.TypeI_RDS(M"000001----------1100-----0011011")
  val PNSRLI_B = IntRegFile.TypeI_RDS(M"00000001--------1100-----0011011")
  val PNSRLI_H = IntRegFile.TypeI_RDS(M"0000001---------1100-----0011011")
  val NSRAI = IntRegFile.TypeI_RDS(M"010001----------1100-----0011011")
  val PNSRAI_H = IntRegFile.TypeI_RDS(M"0100001---------1100-----0011011")
  val PNSRAI_B = IntRegFile.TypeI_RDS(M"01000001--------1100-----0011011")
  val NSRARI = IntRegFile.TypeI_RDS(M"010101----------1100-----0011011")
  val PNSRARI_H = IntRegFile.TypeI_RDS(M"0101001---------1100-----0011011")
  val PNSRARI_B = IntRegFile.TypeI_RDS(M"01010001--------1100-----0011011")
  val NCLIP = IntRegFile.TypeR_RDS(M"0110111---------1100-----0011011")
  val PNCLIP_BS = IntRegFile.TypeR_RDS(M"0110100---------1100-----0011011")
  val PNCLIP_HS = IntRegFile.TypeR_RDS(M"0110101---------1100-----0011011")
  val NCLIPI = IntRegFile.TypeI_RDS(M"011001----------1100-----0011011")
  val PNCLIPI_H = IntRegFile.TypeI_RDS(M"0110001---------1100-----0011011")
  val PNCLIPI_B = IntRegFile.TypeI_RDS(M"01100001--------1100-----0011011")
  val NCLIPR = IntRegFile.TypeR_RDS(M"0111111---------1100-----0011011")
  val PNCLIPR_BS = IntRegFile.TypeR_RDS(M"0111100---------1100-----0011011")
  val PNCLIPR_HS = IntRegFile.TypeR_RDS(M"0111101---------1100-----0011011")
  val NCLIPRI = IntRegFile.TypeI_RDS(M"011101----------1100-----0011011")
  val PNCLIPRI_H = IntRegFile.TypeI_RDS(M"0111001---------1100-----0011011")
  val PNCLIPRI_B = IntRegFile.TypeI_RDS(M"01110001--------1100-----0011011")
  val NCLIPU = IntRegFile.TypeR_RDS(M"0010111---------1100-----0011011")
  val PNCLIPU_BS = IntRegFile.TypeR_RDS(M"0010100---------1100-----0011011")
  val PNCLIPU_HS = IntRegFile.TypeR_RDS(M"0010101---------1100-----0011011")
  val NCLIPIU = IntRegFile.TypeI_RDS(M"001001----------1100-----0011011")
  val PNCLIPIU_H = IntRegFile.TypeI_RDS(M"0010001---------1100-----0011011")
  val PNCLIPIU_B = IntRegFile.TypeI_RDS(M"00100001--------1100-----0011011")
  val NCLIPRU = IntRegFile.TypeR_RDS(M"0011111---------1100-----0011011")
  val PNCLIPRU_BS = IntRegFile.TypeR_RDS(M"0011100---------1100-----0011011")
  val PNCLIPRU_HS = IntRegFile.TypeR_RDS(M"0011101---------1100-----0011011")
  val NCLIPRIU = IntRegFile.TypeI_RDS(M"001101----------1100-----0011011")
  val PNCLIPRIU_H = IntRegFile.TypeI_RDS(M"0011001---------1100-----0011011")
  val PNCLIPRIU_B = IntRegFile.TypeI_RDS(M"00110001--------1100-----0011011")

  object Rv64 {
    val PNCLIPP_B = IntRegFile.TypeR(M"1100000----------010-----0111011")  // pnclipp.b
    val PNCLIPP_H = IntRegFile.TypeR(M"1100001----------010-----0111011")  // pnclipp.h
    val PNCLIPP_W = IntRegFile.TypeR(M"1100011----------010-----0111011")  // pnclipp.w
    val PNCLIPUP_B = IntRegFile.TypeR(M"1000000----------010-----0111011")  // pnclipup.b
    val PNCLIPUP_H = IntRegFile.TypeR(M"1000001----------010-----0111011")  // pnclipup.h
    val PNCLIPUP_W = IntRegFile.TypeR(M"1000011----------010-----0111011")  // pnclipup.w
  }
}

class RvpNarrowPlugin(val layer: LaneLayer, val xlen: Int) extends ExecutionUnitElementSimple(layer) {

  require(xlen == 32 || xlen == 64, s"RvpNarrowPlugin: XLEN 32 or 64, got $xlen")
  private val rv64 = xlen == 64

  val EW = Payload(RvpElemWidth())
  val SIGNED = Payload(Bool())

  val ROUND = Payload(Bool())
  val CLIP = Payload(Bool())
  val IMM = Payload(Bool())

  val NARROW_DATA = Payload(Bits(64 bits))
  val NARROW_TOP = Payload(Bits(4 bits))
  val CLIP_RD = Payload(Bits(xlen bits)) // XLEN=64 clip result

  val SATURATED = Payload(Bool())


  val logic = during setup new Logic {

    def registerOpcodeRDS(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop.spec.addRsSpec(RDS, executeAt = 1) // scheduling optimization(?). This is the high half of the 64-bit operand but in the first cycle we only operate on the low half. So this may arrive at 1
      uop
    }

    // Two-register form. The XLEN=64 clips read rs1 and rs2 as the low and high halves of one 128-bit source.
    def registerOpcode(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop
    }

    // for immediates
    def registerOpcodeIRDS(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RDS, executeAt = 1)
      uop
    }


    el.setDecodingDefault(SIGNED, False)
    // The shift/round/clip selectors describe the RV32 narrowing-shift row, which has no RV64 member.
    if (!rv64) {
      el.setDecodingDefault(CLIP, False)
      el.setDecodingDefault(IMM, False)
      el.setDecodingDefault(ROUND, False)
    }


    val decoderService = host[DecoderService]
    val decoderRetainer = retains(decoderService.decodingLock)


    awaitBuild()

    assert(Riscv.XLEN.get == xlen)


    val writeBack = newWriteback(ifp, 2)

    val vxsat = host[RvpVxsatPlugin]
    val vxsatRetainer = retains(vxsat.elaborationLock)
    val vxsatPort = vxsat.createPort(at = 2)
    vxsatRetainer.release()

    if (!rv64) {
      decoderService.addMicroOpDecoding(NSRL, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NSRA, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NSRAR, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRL_HS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRLI_H, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRLI_B, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NSRLI, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRAR_HS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRL_BS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRA_BS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRA_HS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRAR_BS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NSRAI, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRAI_H, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRAI_B, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NSRARI, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRARI_H, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNSRARI_B, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NCLIP, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIP_BS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIP_HS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NCLIPI, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPI_H, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPI_B, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NCLIPR, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPR_BS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPR_HS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NCLIPRI, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPRI_H, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPRI_B, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NCLIPU, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPU_BS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPU_HS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NCLIPIU, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPIU_H, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPIU_B, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NCLIPRU, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPRU_BS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPRU_HS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(NCLIPRIU, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPRIU_H, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(PNCLIPRIU_B, PAIR_RS1, True)


      registerOpcodeRDS(RvpNarrowPlugin.NSRL).decode(EW -> RvpElemWidth.W)
      registerOpcodeRDS(RvpNarrowPlugin.NSRA).decode(EW -> RvpElemWidth.W, SIGNED -> True)
      registerOpcodeRDS(RvpNarrowPlugin.NSRAR).decode(EW -> RvpElemWidth.W, SIGNED -> True, ROUND -> True)
      registerOpcodeRDS(RvpNarrowPlugin.PNSRL_HS).decode(EW -> RvpElemWidth.H)
      registerOpcodeRDS(RvpNarrowPlugin.PNSRL_BS).decode(EW -> RvpElemWidth.B)
      registerOpcodeRDS(RvpNarrowPlugin.PNSRA_BS).decode(EW -> RvpElemWidth.B, SIGNED -> True)
      registerOpcodeRDS(RvpNarrowPlugin.PNSRA_HS).decode(EW -> RvpElemWidth.H, SIGNED -> True)
      registerOpcodeRDS(RvpNarrowPlugin.PNSRAR_BS).decode(EW -> RvpElemWidth.B, SIGNED -> True, ROUND -> True)
      registerOpcodeRDS(RvpNarrowPlugin.PNSRAR_HS).decode(EW -> RvpElemWidth.H, SIGNED -> True, ROUND -> True)
      registerOpcodeIRDS(RvpNarrowPlugin.PNSRLI_H).decode(EW -> RvpElemWidth.H, IMM -> True)
      registerOpcodeIRDS(RvpNarrowPlugin.PNSRLI_B).decode(EW -> RvpElemWidth.B, IMM -> True)
      registerOpcodeIRDS(RvpNarrowPlugin.NSRLI).decode(EW -> RvpElemWidth.W, IMM -> True)

      registerOpcodeIRDS(RvpNarrowPlugin.NSRAI).decode(EW -> RvpElemWidth.W, IMM -> True, SIGNED -> True)
      registerOpcodeIRDS(RvpNarrowPlugin.PNSRAI_H).decode(EW -> RvpElemWidth.H, IMM -> True, SIGNED -> True)
      registerOpcodeIRDS(RvpNarrowPlugin.PNSRAI_B).decode(EW -> RvpElemWidth.B, IMM -> True, SIGNED -> True)

      registerOpcodeIRDS(RvpNarrowPlugin.NSRARI).decode(EW -> RvpElemWidth.W, IMM -> True, SIGNED -> True, ROUND -> True)
      registerOpcodeIRDS(RvpNarrowPlugin.PNSRARI_H).decode(EW -> RvpElemWidth.H, IMM -> True, SIGNED -> True, ROUND -> True)
      registerOpcodeIRDS(RvpNarrowPlugin.PNSRARI_B).decode(EW -> RvpElemWidth.B, IMM -> True, SIGNED -> True, ROUND -> True)

      registerOpcodeRDS(RvpNarrowPlugin.NCLIP).decode(EW -> RvpElemWidth.W, SIGNED -> True, CLIP -> True)
      // NCLIP row (signed, truncating)
      registerOpcodeRDS(PNCLIP_BS).decode(EW -> RvpElemWidth.B, CLIP -> True, SIGNED -> True)
      registerOpcodeRDS(PNCLIP_HS).decode(EW -> RvpElemWidth.H, CLIP -> True, SIGNED -> True)
      registerOpcodeIRDS(NCLIPI).decode(EW -> RvpElemWidth.W, CLIP -> True, SIGNED -> True, IMM -> True)
      registerOpcodeIRDS(PNCLIPI_H).decode(EW -> RvpElemWidth.H, CLIP -> True, SIGNED -> True, IMM -> True)
      registerOpcodeIRDS(PNCLIPI_B).decode(EW -> RvpElemWidth.B, CLIP -> True, SIGNED -> True, IMM -> True)

      // NCLIPR row (signed, rounding)
      registerOpcodeRDS(NCLIPR).decode(EW -> RvpElemWidth.W, CLIP -> True, SIGNED -> True, ROUND -> True)
      registerOpcodeRDS(PNCLIPR_BS).decode(EW -> RvpElemWidth.B, CLIP -> True, SIGNED -> True, ROUND -> True)
      registerOpcodeRDS(PNCLIPR_HS).decode(EW -> RvpElemWidth.H, CLIP -> True, SIGNED -> True, ROUND -> True)
      registerOpcodeIRDS(NCLIPRI).decode(EW -> RvpElemWidth.W, CLIP -> True, SIGNED -> True, ROUND -> True, IMM -> True)
      registerOpcodeIRDS(PNCLIPRI_H).decode(EW -> RvpElemWidth.H, CLIP -> True, SIGNED -> True, ROUND -> True, IMM -> True)
      registerOpcodeIRDS(PNCLIPRI_B).decode(EW -> RvpElemWidth.B, CLIP -> True, SIGNED -> True, ROUND -> True, IMM -> True)

      // NCLIPU row (unsigned, truncating)
      registerOpcodeRDS(NCLIPU).decode(EW -> RvpElemWidth.W, CLIP -> True)
      registerOpcodeRDS(PNCLIPU_BS).decode(EW -> RvpElemWidth.B, CLIP -> True)
      registerOpcodeRDS(PNCLIPU_HS).decode(EW -> RvpElemWidth.H, CLIP -> True)
      registerOpcodeIRDS(NCLIPIU).decode(EW -> RvpElemWidth.W, CLIP -> True, IMM -> True)
      registerOpcodeIRDS(PNCLIPIU_H).decode(EW -> RvpElemWidth.H, CLIP -> True, IMM -> True)
      registerOpcodeIRDS(PNCLIPIU_B).decode(EW -> RvpElemWidth.B, CLIP -> True, IMM -> True)

      // NCLIPRU row (unsigned, rounding)
      registerOpcodeRDS(NCLIPRU).decode(EW -> RvpElemWidth.W, CLIP -> True, ROUND -> True)
      registerOpcodeRDS(PNCLIPRU_BS).decode(EW -> RvpElemWidth.B, CLIP -> True, ROUND -> True)
      registerOpcodeRDS(PNCLIPRU_HS).decode(EW -> RvpElemWidth.H, CLIP -> True, ROUND -> True)
      registerOpcodeIRDS(NCLIPRIU).decode(EW -> RvpElemWidth.W, CLIP -> True, ROUND -> True, IMM -> True)
      registerOpcodeIRDS(PNCLIPRIU_H).decode(EW -> RvpElemWidth.H, CLIP -> True, ROUND -> True, IMM -> True)
      registerOpcodeIRDS(PNCLIPRIU_B).decode(EW -> RvpElemWidth.B, CLIP -> True, ROUND -> True, IMM -> True)
    } else {
      // ---- RV64 ---------------------------------------------------------------
      registerOpcode(Rv64.PNCLIPP_B).decode(EW -> RvpElemWidth.B, SIGNED -> True)
      registerOpcode(Rv64.PNCLIPP_H).decode(EW -> RvpElemWidth.H, SIGNED -> True)
      registerOpcode(Rv64.PNCLIPP_W).decode(EW -> RvpElemWidth.W, SIGNED -> True)
      registerOpcode(Rv64.PNCLIPUP_B).decode(EW -> RvpElemWidth.B, SIGNED -> False)
      registerOpcode(Rv64.PNCLIPUP_H).decode(EW -> RvpElemWidth.H, SIGNED -> False)
      registerOpcode(Rv64.PNCLIPUP_W).decode(EW -> RvpElemWidth.W, SIGNED -> False)
    }

    decoderRetainer.release()
    uopRetainer.release()

    val stage2 = (!rv64) generate new el.Execute(id = 1) {
      val rds = up(el(IntRegFile, RDS)).asUInt
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt

      val src = (rds ## rs1).asUInt

      val uimm = EW.mux(
        RvpElemWidth.W -> Decode.UOP(25 downto 20).asUInt.resize(6),
        RvpElemWidth.H -> Decode.UOP(24 downto 20).asUInt.resize(6),
        RvpElemWidth.B -> Decode.UOP(23 downto 20).asUInt.resize(6))
      val shamt = this (IMM) ? uimm | ((EW === RvpElemWidth.W) ? rs2(5 downto 0) | rs2(4 downto 0).resized)

      val (narrowData, narrowTop) = RvpNarrowOps.shiftAndRound(
        src, shamt, this (EW), this (SIGNED), this (ROUND))
      this (NARROW_DATA) := narrowData
      this (NARROW_TOP) := narrowTop
    }

    val vxSatCommit = new el.Execute(id = 2) {
      vxsatPort.pulse := this (SATURATED)
    }

    val stage3 = (!rv64) generate new el.Execute(id = 2) {
      val (narrowResult, narrowSat) = RvpNarrowOps.finish(
        this (NARROW_DATA), this (NARROW_TOP), this (EW), this (SIGNED),
        this (CLIP))

      this (SATURATED) := SEL && narrowSat && this (CLIP)

      writeBack.valid   := SEL
      writeBack.payload := narrowResult
    }

    // ---- XLEN=64 datapath ---------------------------------------------------
    val clip64 = rv64 generate new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asBits
      val rs2 = up(el(IntRegFile, RS2)).asBits
      val (clipped, sat) = RvpNarrowOps.clipPair128(rs2 ## rs1, this (EW), !this (SIGNED))
      this (CLIP_RD) := clipped
      this (SATURATED) := SEL && sat
    }

    val writeBack64 = rv64 generate new el.Execute(id = 2) {
      writeBack.valid := SEL
      writeBack.payload := this (CLIP_RD)
    }
  }

}
