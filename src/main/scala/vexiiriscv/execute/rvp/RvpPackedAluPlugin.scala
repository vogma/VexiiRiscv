// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.execute.rvp.RvpPackedAluPlugin.Rv64
import vexiiriscv.execute.rvp.RvpPairWritePlugin.{PAIR_RD, PAIR_RD_SRC, PAIR_RS1, PAIR_RS2}
import vexiiriscv.execute.rvp.kinds.RvpAluKind
import vexiiriscv.execute.rvp.kinds.RvpAluKind._
import vexiiriscv.execute.rvp.kinds.RvpReadout
import vexiiriscv.execute.rvp.kinds.{RvpElemWidth => EW}
import vexiiriscv.execute.rvp.ops.{RvpAbsDiffOps, RvpAverageOps, RvpCompareOps, RvpMinMaxOps, RvpPackedLanes, RvpReduceOps, RvpSatWidthOps, RvpSaturatingAddOps}
import vexiiriscv.execute.{ExecutionUnitElementSimple, LaneLayer}
import vexiiriscv.riscv.{IntRegFile, MicroOp, RDS, RS1, RS2, RS3, Riscv}

import scala.language.postfixOps

object RvpPackedAluPlugin {
  val PADD_B = IntRegFile.TypeR(M"1000010----------000-----0111011")
  val PADD_H = IntRegFile.TypeR(M"1000000----------000-----0111011")
  val PSUB_B = IntRegFile.TypeR(M"1100010----------000-----0111011")
  val PSUB_H = IntRegFile.TypeR(M"1100000----------000-----0111011")
  val PADD_BS = IntRegFile.TypeR(M"1001110----------010-----0011011")
  val PADD_HS = IntRegFile.TypeR(M"1001100----------010-----0011011")
  val PMSLTU_B = IntRegFile.TypeR(M"1101110----------110-----0111011")
  val PMSLTU_H = IntRegFile.TypeR(M"1101100----------110-----0111011")
  val PMSEQ_B = IntRegFile.TypeR(M"1100010----------110-----0111011")
  val PMSEQ_H = IntRegFile.TypeR(M"1100000----------110-----0111011")
  val PMINU_B = IntRegFile.TypeR(M"1110110----------110-----0111011")
  val PMINU_H = IntRegFile.TypeR(M"1110100----------110-----0111011")
  val PMAXU_B = IntRegFile.TypeR(M"1111110----------110-----0111011")
  val PMAXU_H = IntRegFile.TypeR(M"1111100----------110-----0111011")
  val PMSLT_B = IntRegFile.TypeR(M"1101010----------110-----0111011")
  val PMSLT_H = IntRegFile.TypeR(M"1101000----------110-----0111011")
  val PMIN_B = IntRegFile.TypeR(M"1110010----------110-----0111011")
  val PMIN_H = IntRegFile.TypeR(M"1110000----------110-----0111011")
  val PMAX_B = IntRegFile.TypeR(M"1111010----------110-----0111011")
  val PMAX_H = IntRegFile.TypeR(M"1111000----------110-----0111011")
  val PAADDU_B = IntRegFile.TypeR(M"1011110----------000-----0111011")
  val PAADDU_H = IntRegFile.TypeR(M"1011100----------000-----0111011")
  val PAADD_B = IntRegFile.TypeR(M"1001110----------000-----0111011")
  val PAADD_H = IntRegFile.TypeR(M"1001100----------000-----0111011")
  val PASUB_B = IntRegFile.TypeR(M"1101110----------000-----0111011")
  val PASUB_H = IntRegFile.TypeR(M"1101100----------000-----0111011")
  val PASUBU_B = IntRegFile.TypeR(M"1111110----------000-----0111011")
  val PASUBU_H = IntRegFile.TypeR(M"1111100----------000-----0111011")
  val AADD = IntRegFile.TypeR(M"1001101----------000-----0111011")
  val AADDU = IntRegFile.TypeR(M"1011101----------000-----0111011")
  val ASUB = IntRegFile.TypeR(M"1101101----------000-----0111011")
  val ASUBU = IntRegFile.TypeR(M"1111101----------000-----0111011")
  val MSEQ = IntRegFile.TypeR(M"1100001----------110-----0111011")
  val MSLT = IntRegFile.TypeR(M"1101001----------110-----0111011")
  val MSLTU = IntRegFile.TypeR(M"1101101----------110-----0111011")
  val PABD_B = IntRegFile.TypeR(M"1100110----------000-----0111011")
  val PABD_H = IntRegFile.TypeR(M"1100100----------000-----0111011")
  val PABDU_B = IntRegFile.TypeR(M"1110110----------000-----0111011")
  val PABDU_H = IntRegFile.TypeR(M"1110100----------000-----0111011")
  val PABDSUMU_B = IntRegFile.TypeR(M"1011010----------001-----0111011")
  val PAS_HX = IntRegFile.TypeR(M"1000000----------110-----0111011")
  val PSA_HX = IntRegFile.TypeR(M"1000010----------110-----0111011")
  val PAAS_HX = IntRegFile.TypeR(M"1001100----------110-----0111011")
  val PASA_HX = IntRegFile.TypeR(M"1001110----------110-----0111011")
  val PSH1ADD_H = IntRegFile.TypeR(M"1010000----------010-----0111011")
  val PREDSUM_BS = IntRegFile.TypeR(M"1001110----------100-----0011011")
  val PREDSUM_HS = IntRegFile.TypeR(M"1001100----------100-----0011011")
  val PREDSUMU_BS = IntRegFile.TypeR(M"1011110----------100-----0011011")
  val PREDSUMU_HS = IntRegFile.TypeR(M"1011100----------100-----0011011")
  val PREDSUM_DHS = IntRegFile.TypeR_RDS(M"0001100---------0100-----0011011")
  val PREDSUM_DBS = IntRegFile.TypeR_RDS(M"0001110---------0100-----0011011")
  val PREDSUMU_DHS = IntRegFile.TypeR_RDS(M"0011100---------0100-----0011011")
  val PREDSUMU_DBS = IntRegFile.TypeR_RDS(M"0011110---------0100-----0011011")
  val PSADD_B = IntRegFile.TypeR(M"1001010----------000-----0111011")
  val PSADDU_B = IntRegFile.TypeR(M"1011010----------000-----0111011")
  val PSADD_H = IntRegFile.TypeR(M"1001000----------000-----0111011")
  val PSADDU_H = IntRegFile.TypeR(M"1011000----------000-----0111011")
  val PSSUB_B = IntRegFile.TypeR(M"1101010----------000-----0111011")
  val PSSUBU_B = IntRegFile.TypeR(M"1111010----------000-----0111011")
  val PSSUB_H = IntRegFile.TypeR(M"1101000----------000-----0111011")
  val PSSUBU_H = IntRegFile.TypeR(M"1111000----------000-----0111011")
  val SADD_W = IntRegFile.TypeR(M"1001001----------000-----0111011")
  val SADDU_W = IntRegFile.TypeR(M"1011001----------000-----0111011")
  val SSUB_W = IntRegFile.TypeR(M"1101001----------000-----0111011")
  val SSUBU_W = IntRegFile.TypeR(M"1111001----------000-----0111011")
  val SATI = IntRegFile.TypeI(M"1110001----------100-----0011011")
  val USATI = IntRegFile.TypeI(M"1010001----------100-----0011011")
  val PSATI_H = IntRegFile.TypeI(M"11100001---------100-----0011011")
  val PUSATI_H = IntRegFile.TypeI(M"10100001---------100-----0011011")
  val PSAS_HX = IntRegFile.TypeR(M"1001000----------110-----0111011")
  val PSSA_HX = IntRegFile.TypeR(M"1001010----------110-----0111011")
  val SSH1SADD = IntRegFile.TypeR(M"1011001----------010-----0111011")
  val SSH1SADD_H = IntRegFile.TypeR(M"1011000----------010-----0111011")
  val PSABS_B = IntRegFile.TypeI(M"111001000111-----010-----0011011")
  val PSABS_H = IntRegFile.TypeI(M"111000000111-----010-----0011011")
  val PABDSUMAU_B = IntRegFile.TypeR_RDS(M"1011110----------001-----0111011")

  // wide instructions
  val WADD = IntRegFile.TypeR(M"0000001----------010----10011011")
  val WADDU = IntRegFile.TypeR(M"0001001----------010----10011011")
  val WSUB = IntRegFile.TypeR(M"0100001----------010----10011011")
  val WSUBU = IntRegFile.TypeR(M"0101001----------010----10011011")
  val PWADD_H = IntRegFile.TypeR(M"0000000----------010----10011011")
  val PWADD_B = IntRegFile.TypeR(M"0000010----------010----10011011")
  val PWADDU_H = IntRegFile.TypeR(M"0001000----------010----10011011")
  val PWADDU_B = IntRegFile.TypeR(M"0001010----------010----10011011")
  val PWSUB_H = IntRegFile.TypeR(M"0100000----------010----10011011")
  val PWSUB_B = IntRegFile.TypeR(M"0100010----------010----10011011")
  val PWSUBU_H = IntRegFile.TypeR(M"0101000----------010----10011011")
  val PWSUBU_B = IntRegFile.TypeR(M"0101010----------010----10011011")

  val PADD_DHS = IntRegFile.TypeR_RDS(M"0001100---------0110----00011011")
  val PADD_DBS = IntRegFile.TypeR_RDS(M"0001110---------0110----00011011")
  val PADD_DWS = IntRegFile.TypeR_RDS(M"0001101---------0110----00011011")
  val PSATI_DH = IntRegFile.TypeI_RDS(M"01100001--------1110----00011011")
  val PSATI_DW = IntRegFile.TypeI_RDS(M"0110001---------1110----00011011")
  val PUSATI_DH = IntRegFile.TypeI_RDS(M"00100001--------1110----00011011")
  val PUSATI_DW = IntRegFile.TypeI_RDS(M"0010001---------1110----00011011")
  val PSABS_DB = IntRegFile.TypeI_RDS(M"011001000111----0110----00011011")
  val PSABS_DH = IntRegFile.TypeI_RDS(M"011000000111----0110----00011011")
  val PADD_DW = IntRegFile.TypeR_RDS_RS3(M"1000001----0----0110----00011011")
  val PADD_DH = IntRegFile.TypeR_RDS_RS3(M"1000000----0----0110----00011011")
  val PADD_DB = IntRegFile.TypeR_RDS_RS3(M"1000010----0----0110----00011011")
  val PSUB_DW = IntRegFile.TypeR_RDS_RS3(M"1100001----0----0110----00011011")
  val PSUB_DH = IntRegFile.TypeR_RDS_RS3(M"1100000----0----0110----00011011")
  val PSUB_DB = IntRegFile.TypeR_RDS_RS3(M"1100010----0----0110----00011011")
  val PSADD_DW = IntRegFile.TypeR_RDS_RS3(M"1001001----0----0110----00011011")
  val PSADD_DH = IntRegFile.TypeR_RDS_RS3(M"1001000----0----0110----00011011")
  val PSADD_DB = IntRegFile.TypeR_RDS_RS3(M"1001010----0----0110----00011011")
  val PSADDU_DW = IntRegFile.TypeR_RDS_RS3(M"1011001----0----0110----00011011")
  val PSADDU_DH = IntRegFile.TypeR_RDS_RS3(M"1011000----0----0110----00011011")
  val PSADDU_DB = IntRegFile.TypeR_RDS_RS3(M"1011010----0----0110----00011011")
  val PSSUB_DW = IntRegFile.TypeR_RDS_RS3(M"1101001----0----0110----00011011")
  val PSSUB_DH = IntRegFile.TypeR_RDS_RS3(M"1101000----0----0110----00011011")
  val PSSUB_DB = IntRegFile.TypeR_RDS_RS3(M"1101010----0----0110----00011011")
  val PSSUBU_DW = IntRegFile.TypeR_RDS_RS3(M"1111001----0----0110----00011011")
  val PSSUBU_DH = IntRegFile.TypeR_RDS_RS3(M"1111000----0----0110----00011011")
  val PSSUBU_DB = IntRegFile.TypeR_RDS_RS3(M"1111010----0----0110----00011011")
  val PMIN_DB = IntRegFile.TypeR_RDS_RS3(M"1110010----1----1110----00011011")
  val PMIN_DH = IntRegFile.TypeR_RDS_RS3(M"1110000----1----1110----00011011")
  val PMIN_DW = IntRegFile.TypeR_RDS_RS3(M"1110001----1----1110----00011011")
  val PMINU_DB = IntRegFile.TypeR_RDS_RS3(M"1110110----1----1110----00011011")
  val PMINU_DH = IntRegFile.TypeR_RDS_RS3(M"1110100----1----1110----00011011")
  val PMINU_DW = IntRegFile.TypeR_RDS_RS3(M"1110101----1----1110----00011011")
  val PMAX_DB = IntRegFile.TypeR_RDS_RS3(M"1111010----1----1110----00011011")
  val PMAX_DH = IntRegFile.TypeR_RDS_RS3(M"1111000----1----1110----00011011")
  val PMAX_DW = IntRegFile.TypeR_RDS_RS3(M"1111001----1----1110----00011011")
  val PMAXU_DB = IntRegFile.TypeR_RDS_RS3(M"1111110----1----1110----00011011")
  val PMAXU_DH = IntRegFile.TypeR_RDS_RS3(M"1111100----1----1110----00011011")
  val PMAXU_DW = IntRegFile.TypeR_RDS_RS3(M"1111101----1----1110----00011011")
  val PMSEQ_DB = IntRegFile.TypeR_RDS_RS3(M"1100010----1----1110----00011011")
  val PMSEQ_DH = IntRegFile.TypeR_RDS_RS3(M"1100000----1----1110----00011011")
  val PMSEQ_DW = IntRegFile.TypeR_RDS_RS3(M"1100001----1----1110----00011011")
  val PMSLT_DB = IntRegFile.TypeR_RDS_RS3(M"1101010----1----1110----00011011")
  val PMSLT_DH = IntRegFile.TypeR_RDS_RS3(M"1101000----1----1110----00011011")
  val PMSLT_DW = IntRegFile.TypeR_RDS_RS3(M"1101001----1----1110----00011011")
  val PMSLTU_DB = IntRegFile.TypeR_RDS_RS3(M"1101110----1----1110----00011011")
  val PMSLTU_DH = IntRegFile.TypeR_RDS_RS3(M"1101100----1----1110----00011011")
  val PMSLTU_DW = IntRegFile.TypeR_RDS_RS3(M"1101101----1----1110----00011011")

  // averaging + abs-diff subgroup (bit20 = 0, bit15 = 0 -- same slot as psadd.d*)
  val PAADD_DB = IntRegFile.TypeR_RDS_RS3(M"1001110----0----0110----00011011")
  val PAADD_DH = IntRegFile.TypeR_RDS_RS3(M"1001100----0----0110----00011011")
  val PAADD_DW = IntRegFile.TypeR_RDS_RS3(M"1001101----0----0110----00011011")
  val PAADDU_DB = IntRegFile.TypeR_RDS_RS3(M"1011110----0----0110----00011011")
  val PAADDU_DH = IntRegFile.TypeR_RDS_RS3(M"1011100----0----0110----00011011")
  val PAADDU_DW = IntRegFile.TypeR_RDS_RS3(M"1011101----0----0110----00011011")
  val PASUB_DB = IntRegFile.TypeR_RDS_RS3(M"1101110----0----0110----00011011")
  val PASUB_DH = IntRegFile.TypeR_RDS_RS3(M"1101100----0----0110----00011011")
  val PASUB_DW = IntRegFile.TypeR_RDS_RS3(M"1101101----0----0110----00011011")
  val PASUBU_DB = IntRegFile.TypeR_RDS_RS3(M"1111110----0----0110----00011011")
  val PASUBU_DH = IntRegFile.TypeR_RDS_RS3(M"1111100----0----0110----00011011")
  val PASUBU_DW = IntRegFile.TypeR_RDS_RS3(M"1111101----0----0110----00011011")
  val PABD_DB = IntRegFile.TypeR_RDS_RS3(M"1100110----0----0110----00011011")
  val PABD_DH = IntRegFile.TypeR_RDS_RS3(M"1100100----0----0110----00011011")
  val PABDU_DB = IntRegFile.TypeR_RDS_RS3(M"1110110----0----0110----00011011")
  val PABDU_DH = IntRegFile.TypeR_RDS_RS3(M"1110100----0----0110----00011011")

  val PAS_DHX = IntRegFile.TypeR_RDS_RS3(M"1000000----1----1110----00011011")
  val PSA_DHX = IntRegFile.TypeR_RDS_RS3(M"1000010----1----1110----00011011")
  val PSAS_DHX = IntRegFile.TypeR_RDS_RS3(M"1001000----1----1110----00011011")
  val PSSA_DHX = IntRegFile.TypeR_RDS_RS3(M"1001010----1----1110----00011011")
  val PAAS_DHX = IntRegFile.TypeR_RDS_RS3(M"1001100----1----1110----00011011")
  val PASA_DHX = IntRegFile.TypeR_RDS_RS3(M"1001110----1----1110----00011011")

  val PSH1ADD_DH = IntRegFile.TypeR_RDS_RS3(M"1010000----1----0110----00011011")
  val PSH1ADD_DW = IntRegFile.TypeR_RDS_RS3(M"1010001----1----0110----00011011")
  val PSSH1SADD_DH = IntRegFile.TypeR_RDS_RS3(M"1011000----1----0110----00011011")
  val PSSH1SADD_DW = IntRegFile.TypeR_RDS_RS3(M"1011001----1----0110----00011011")

  val ADDD = IntRegFile.TypeR_RDS_RS3(M"1000011----0----0110----00011011")
  val SUBD = IntRegFile.TypeR_RDS_RS3(M"1100011----0----0110----00011011")

  val WADDA = IntRegFile.TypeR_RDS_RS3(M"0000101----------010----10011011")
  val WADDAU = IntRegFile.TypeR_RDS_RS3(M"0001101----------010----10011011")
  val WSUBA = IntRegFile.TypeR_RDS_RS3(M"0100101----------010----10011011")
  val WSUBAU = IntRegFile.TypeR_RDS_RS3(M"0101101----------010----10011011")
  val PWADDA_H = IntRegFile.TypeR_RDS_RS3(M"0000100----------010----10011011")
  val PWADDA_B = IntRegFile.TypeR_RDS_RS3(M"0000110----------010----10011011")
  val PWADDAU_H = IntRegFile.TypeR_RDS_RS3(M"0001100----------010----10011011")
  val PWADDAU_B = IntRegFile.TypeR_RDS_RS3(M"0001110----------010----10011011")
  val PWSUBA_H = IntRegFile.TypeR_RDS_RS3(M"0100100----------010----10011011")
  val PWSUBA_B = IntRegFile.TypeR_RDS_RS3(M"0100110----------010----10011011")
  val PWSUBAU_H = IntRegFile.TypeR_RDS_RS3(M"0101100----------010----10011011")
  val PWSUBAU_B = IntRegFile.TypeR_RDS_RS3(M"0101110----------010----10011011")

  object Rv64 {
    // Basic add / subtract
    val PADD_B = IntRegFile.TypeR(M"1000010----------000-----0111011") // padd.b
    val PADD_H = IntRegFile.TypeR(M"1000000----------000-----0111011") // padd.h
    val PADD_W = IntRegFile.TypeR(M"1000001----------000-----0111011") // padd.w (RV64-only)
    val PADD_BS = IntRegFile.TypeR(M"1001110----------010-----0011011") // padd.bs
    val PADD_HS = IntRegFile.TypeR(M"1001100----------010-----0011011") // padd.hs
    val PADD_WS = IntRegFile.TypeR(M"1001101----------010-----0011011") // padd.ws (RV64-only)
    val PSUB_B = IntRegFile.TypeR(M"1100010----------000-----0111011") // psub.b
    val PSUB_H = IntRegFile.TypeR(M"1100000----------000-----0111011") // psub.h
    val PSUB_W = IntRegFile.TypeR(M"1100001----------000-----0111011") // psub.w (RV64-only)

    // Saturating add / subtract
    val PSADD_B = IntRegFile.TypeR(M"1001010----------000-----0111011") // psadd.b
    val PSADD_H = IntRegFile.TypeR(M"1001000----------000-----0111011") // psadd.h
    val PSADD_W = IntRegFile.TypeR(M"1001001----------000-----0111011") // psadd.w (recycled: RV32 sadd)
    val PSADDU_B = IntRegFile.TypeR(M"1011010----------000-----0111011") // psaddu.b
    val PSADDU_H = IntRegFile.TypeR(M"1011000----------000-----0111011") // psaddu.h
    val PSADDU_W = IntRegFile.TypeR(M"1011001----------000-----0111011") // psaddu.w (recycled: RV32 saddu)
    val PSSUB_B = IntRegFile.TypeR(M"1101010----------000-----0111011") // pssub.b
    val PSSUB_H = IntRegFile.TypeR(M"1101000----------000-----0111011") // pssub.h
    val PSSUB_W = IntRegFile.TypeR(M"1101001----------000-----0111011") // pssub.w (recycled: RV32 ssub)
    val PSSUBU_B = IntRegFile.TypeR(M"1111010----------000-----0111011") // pssubu.b
    val PSSUBU_H = IntRegFile.TypeR(M"1111000----------000-----0111011") // pssubu.h
    val PSSUBU_W = IntRegFile.TypeR(M"1111001----------000-----0111011") // pssubu.w (recycled: RV32 ssubu)

    // Averaging add / subtract
    val PAADD_B = IntRegFile.TypeR(M"1001110----------000-----0111011") // paadd.b
    val PAADD_H = IntRegFile.TypeR(M"1001100----------000-----0111011") // paadd.h
    val PAADD_W = IntRegFile.TypeR(M"1001101----------000-----0111011") // paadd.w (recycled: RV32 aadd)
    val PAADDU_B = IntRegFile.TypeR(M"1011110----------000-----0111011") // paaddu.b
    val PAADDU_H = IntRegFile.TypeR(M"1011100----------000-----0111011") // paaddu.h
    val PAADDU_W = IntRegFile.TypeR(M"1011101----------000-----0111011") // paaddu.w (recycled: RV32 aaddu)
    val PASUB_B = IntRegFile.TypeR(M"1101110----------000-----0111011") // pasub.b
    val PASUB_H = IntRegFile.TypeR(M"1101100----------000-----0111011") // pasub.h
    val PASUB_W = IntRegFile.TypeR(M"1101101----------000-----0111011") // pasub.w (recycled: RV32 asub)
    val PASUBU_B = IntRegFile.TypeR(M"1111110----------000-----0111011") // pasubu.b
    val PASUBU_H = IntRegFile.TypeR(M"1111100----------000-----0111011") // pasubu.h
    val PASUBU_W = IntRegFile.TypeR(M"1111101----------000-----0111011") // pasubu.w (recycled: RV32 asubu)

    // Shift-add
    val PSH1ADD_H = IntRegFile.TypeR(M"1010000----------010-----0111011") // psh1add.h
    val PSH1ADD_W = IntRegFile.TypeR(M"1010001----------010-----0111011") // psh1add.w (RV64-only)
    val PSSH1SADD_H = IntRegFile.TypeR(M"1011000----------010-----0111011") // pssh1sadd.h
    val PSSH1SADD_W = IntRegFile.TypeR(M"1011001----------010-----0111011") // pssh1sadd.w (recycled: RV32 ssh1sadd)

    // Add-subtract cross
    val PAS_HX = IntRegFile.TypeR(M"1000000----------110-----0111011") // pas.hx
    val PAS_WX = IntRegFile.TypeR(M"1000001----------110-----0111011") // pas.wx (RV64-only)
    val PSA_HX = IntRegFile.TypeR(M"1000010----------110-----0111011") // psa.hx
    val PSA_WX = IntRegFile.TypeR(M"1000011----------110-----0111011") // psa.wx (RV64-only)
    val PSAS_HX = IntRegFile.TypeR(M"1001000----------110-----0111011") // psas.hx
    val PSAS_WX = IntRegFile.TypeR(M"1001001----------110-----0111011") // psas.wx (RV64-only)
    val PSSA_HX = IntRegFile.TypeR(M"1001010----------110-----0111011") // pssa.hx
    val PSSA_WX = IntRegFile.TypeR(M"1001011----------110-----0111011") // pssa.wx (RV64-only)
    val PAAS_HX = IntRegFile.TypeR(M"1001100----------110-----0111011") // paas.hx
    val PAAS_WX = IntRegFile.TypeR(M"1001101----------110-----0111011") // paas.wx (RV64-only)
    val PASA_HX = IntRegFile.TypeR(M"1001110----------110-----0111011") // pasa.hx
    val PASA_WX = IntRegFile.TypeR(M"1001111----------110-----0111011") // pasa.wx (RV64-only)

    // Absolute difference
    val PABD_B = IntRegFile.TypeR(M"1100110----------000-----0111011") // pabd.b
    val PABD_H = IntRegFile.TypeR(M"1100100----------000-----0111011") // pabd.h
    val PABDU_B = IntRegFile.TypeR(M"1110110----------000-----0111011") // pabdu.b
    val PABDU_H = IntRegFile.TypeR(M"1110100----------000-----0111011") // pabdu.h
    val PABDSUMU_B = IntRegFile.TypeR(M"1011010----------001-----0111011") // pabdsumu.b
    val PABDSUMAU_B = IntRegFile.TypeR_RDS(M"1011110----------001-----0111011") // pabdsumau.b

    // Saturating absolute
    val PSABS_B = IntRegFile.TypeI(M"111001000111-----010-----0011011") // psabs.b
    val PSABS_H = IntRegFile.TypeI(M"111000000111-----010-----0011011") // psabs.h

    // Reduction sum
    val PREDSUM_BS = IntRegFile.TypeR(M"1001110----------100-----0011011") // predsum.bs
    val PREDSUM_HS = IntRegFile.TypeR(M"1001100----------100-----0011011") // predsum.hs
    val PREDSUM_WS = IntRegFile.TypeR(M"1001101----------100-----0011011") // predsum.ws (RV64-only)
    val PREDSUMU_BS = IntRegFile.TypeR(M"1011110----------100-----0011011") // predsumu.bs
    val PREDSUMU_HS = IntRegFile.TypeR(M"1011100----------100-----0011011") // predsumu.hs
    val PREDSUMU_WS = IntRegFile.TypeR(M"1011101----------100-----0011011") // predsumu.ws (RV64-only)

    // Min / max
    val PMIN_B = IntRegFile.TypeR(M"1110010----------110-----0111011") // pmin.b
    val PMIN_H = IntRegFile.TypeR(M"1110000----------110-----0111011") // pmin.h
    val PMIN_W = IntRegFile.TypeR(M"1110001----------110-----0111011") // pmin.w (RV64-only)
    val PMINU_B = IntRegFile.TypeR(M"1110110----------110-----0111011") // pminu.b
    val PMINU_H = IntRegFile.TypeR(M"1110100----------110-----0111011") // pminu.h
    val PMINU_W = IntRegFile.TypeR(M"1110101----------110-----0111011") // pminu.w (RV64-only)
    val PMAX_B = IntRegFile.TypeR(M"1111010----------110-----0111011") // pmax.b
    val PMAX_H = IntRegFile.TypeR(M"1111000----------110-----0111011") // pmax.h
    val PMAX_W = IntRegFile.TypeR(M"1111001----------110-----0111011") // pmax.w (RV64-only)
    val PMAXU_B = IntRegFile.TypeR(M"1111110----------110-----0111011") // pmaxu.b
    val PMAXU_H = IntRegFile.TypeR(M"1111100----------110-----0111011") // pmaxu.h
    val PMAXU_W = IntRegFile.TypeR(M"1111101----------110-----0111011") // pmaxu.w (RV64-only)

    // Compare -> element mask
    val PMSEQ_B = IntRegFile.TypeR(M"1100010----------110-----0111011") // pmseq.b
    val PMSEQ_H = IntRegFile.TypeR(M"1100000----------110-----0111011") // pmseq.h
    val PMSEQ_W = IntRegFile.TypeR(M"1100001----------110-----0111011") // pmseq.w (recycled: RV32 mseq)
    val PMSLT_B = IntRegFile.TypeR(M"1101010----------110-----0111011") // pmslt.b
    val PMSLT_H = IntRegFile.TypeR(M"1101000----------110-----0111011") // pmslt.h
    val PMSLT_W = IntRegFile.TypeR(M"1101001----------110-----0111011") // pmslt.w (recycled: RV32 mslt)
    val PMSLTU_B = IntRegFile.TypeR(M"1101110----------110-----0111011") // pmsltu.b
    val PMSLTU_H = IntRegFile.TypeR(M"1101100----------110-----0111011") // pmsltu.h
    val PMSLTU_W = IntRegFile.TypeR(M"1101101----------110-----0111011") // pmsltu.w (recycled: RV32 msltu)

    // Saturate to immediate width
    val PSATI_H = IntRegFile.TypeI(M"11100001---------100-----0011011") // psati.h
    val PSATI_W = IntRegFile.TypeI(M"1110001----------100-----0011011") // psati.w (recycled: RV32 sati)
    val SATI = IntRegFile.TypeI(M"111001-----------100-----0011011") // sati (uimm6)
    val PUSATI_H = IntRegFile.TypeI(M"10100001---------100-----0011011") // pusati.h
    val PUSATI_W = IntRegFile.TypeI(M"1010001----------100-----0011011") // pusati.w (recycled: RV32 usati)
    val USATI = IntRegFile.TypeI(M"101001-----------100-----0011011") // usati (uimm6)
  }
}

/** The draft-P packed ALU: add/subtract, compare, min/max, averaging and
 * absolute difference, all using one shared 72-bit segmented adder.
 *
 * The adder processes 64 data bits at BOTH XLENs.
 *
 * The read-out networks (compare, min/max, averaging, abs-diff, saturating
 * add/sub) are per-32-bit-word at both XLENs. RV32 elaborates one copy and
 * time-multiplexes it across the pair over two cycles (RvpDoubleWidePass)
 */
class RvpPackedAluPlugin(val layer: LaneLayer,
                         val intWritebackAt: Int,
                         val xlen: Int,
                         val postAt: Int = 1,
                         val formatAt: Int = 2,
                         val pairs: Boolean = true) extends ExecutionUnitElementSimple(layer) {

  require(xlen == 32 || xlen == 64, s"RvpPackedAluPlugin: XLEN 32 or 64, got $xlen")
  private val rv64 = xlen == 64
  // RV32 register-pair support (pair source / pair destination / destination-pair read).
  // false = the no-pairs configuration: the pair ops are not decoded and
  // the HIGH-word half of the datapath, the two-pass sequencer and the pair-write port go.
  private val withPairs = pairs && !rv64

  val ELEM_W = Payload(EW())
  val DO_SUB = Payload(Bool())
  val IS_SCALAR = Payload(Bool())
  val IS_CROSS = Payload(Bool())
  val IS_SH1 = Payload(Bool())
  val IS_D = Payload(Bool())
  val SATURATED = Payload(Bool())
  val DO_ACCUMULATE = Payload(Bool())

  val WIDEN_SIGNED = Payload(Bool())
  val SUMWIDE = Payload(UInt(36 bits))
  val SUMWIDE_HI = Payload(UInt(36 bits))
  val RD = Payload(Bits(32 bits))
  val ASRC = Payload(UInt(32 bits))
  val BEFF = Payload(UInt(32 bits))
  val RS1V = Payload(UInt(32 bits))
  val RS2V = Payload(UInt(32 bits))
  val RS3V = Payload(UInt(32 bits))
  val RDSV = Payload(UInt(32 bits))
  val ACCBASE = Payload(UInt(xlen bits))
  val SAT_SH1 = Payload(Bool())
  val ASRC_HI = Payload(UInt(32 bits))
  val BEFF_HI = Payload(UInt(32 bits))
  val SAT_SH1_HI = Payload(Bool())
  val DOUBLE_WIDE = Payload(Bool())
  val DOUBLE_WIDE_HIGH = Payload(Bits(32 bits))
  // Word 1 of the XLEN=64 datapath: the packed lane result, the rs1 half that
  // feeds its predicates, and the rs2 half that seeds the reduction. RV32 takes
  // the same three from the register pair (DOUBLE_WIDE_HIGH / RDSV / RS3V), so
  // these payloads exist only at XLEN=64.
  val RD_HI = Payload(Bits(32 bits))
  val RS1V_HI = Payload(UInt(32 bits))
  val RS2V_HI = Payload(UInt(32 bits))
  // 255 * (XLEN/8) is the widest pabdsumu.b partial sum: 1020 (10 bits) at
  // RV32, 2040 (11 bits) at RV64.
  val ABS_DIFF_SUM = Payload(UInt(log2Up(255 * xlen / 8 + 1) bits))
  val REDUCE_SUM = Payload(Bits(xlen bits))
  val ALU_RESULT = Payload(Bits(xlen bits))
  val RO_SIGNED = Payload(Bool())
  val RO_MAX = Payload(Bool())
  val RO_EQ = Payload(Bool())
  val RO_SSH1 = Payload(Bool())
  val RO_UNSIGNED = Payload(Bool())
  val RO = Payload(RvpReadout())

  val ALU_KIND = Payload(RvpAluKind())
  // XLEN=64 only: the saturation lane is the whole register (sati / usati)
  // rather than the per-32-bit-word geometry ELEM_W describes. There is no RV32 equivalent
  val SAT_XLEN = Payload(Bool())

  val logic = during setup new Logic {

    def registerOpcode(enc: MicroOp) = {
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

    el.setDecodingDefault(DO_SUB, False)
    el.setDecodingDefault(IS_CROSS, False)
    el.setDecodingDefault(IS_SCALAR, False)
    el.setDecodingDefault(IS_SH1, False)
    el.setDecodingDefault(DO_ACCUMULATE, False)
    if (rv64) el.setDecodingDefault(SAT_XLEN, False)
    // Widening, double-wide and the addd/subd are pair concepts.
    // RV64 has no pair instructions, so their decode entries are not created at XLEN=64.
    if (!rv64) {
      el.setDecodingDefault(WIDEN_SIGNED, False)
      el.setDecodingDefault(DOUBLE_WIDE, False)
      el.setDecodingDefault(IS_D, False)
    }

    val decoderService = host[DecoderService]
    val decoderRetainer = retains(decoderService.decodingLock)

    def registerOpcodeRDS_RS3(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop.spec.addRsSpec(RS3, executeAt = 0)
      uop.spec.addRsSpec(RDS, executeAt = 0)
      // these are the pair register read + write instructions. Set the appropriate flags
      uop.decode(PAIR_RD -> True, DOUBLE_WIDE -> True, PAIR_RS2 -> True)
      for (p <- List(PAIR_RS1, PAIR_RD, PAIR_RS2))
        decoderService.addMicroOpDecoding(enc, p, True)
      uop
    }

    def registerOpcodeWidenAcc(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop.spec.addRsSpec(RDS, executeAt = 0) // X[{rd_p,0}]
      uop.spec.addRsSpec(RS3, executeAt = 0) // X[{rd_p,1}], forced to x0 when rd_p == 0
      uop.decode(PAIR_RD -> True, PAIR_RD_SRC -> True)
      for (p <- List(PAIR_RD, PAIR_RD_SRC))
        decoderService.addMicroOpDecoding(enc, p, True)
      uop
    }

    awaitBuild()

    assert(Riscv.XLEN.get == xlen)

    assert(0 < postAt && postAt < formatAt && formatAt <= intWritebackAt)
    val intFormatPlugin = ifp
    val writeBack = newWriteback(intFormatPlugin, formatAt)

    // The pair write port is the RV32 pair-destination path. At XLEN=64 there
    // is no odd half to write and RvpPairWritePlugin is not even installed, so
    // nothing here may be evaluated
    val pairWrite = withPairs generate host[RvpPairWritePlugin]
    val pairRetainer = withPairs generate retains(pairWrite.elaborationLock)
    val hiPort = withPairs generate pairWrite.createPort(at = postAt) // must be <= writeAt
    if (withPairs) pairRetainer.release()

    val vxsat = host[RvpVxsatPlugin]
    val vxsatRetainer = retains(vxsat.elaborationLock)
    val vxsatPort = vxsat.createPort(at = intWritebackAt)
    vxsatRetainer.release()

    if (!rv64) {
      registerOpcode(RvpPackedAluPlugin.PADD_B).decode(ELEM_W -> EW.B, ALU_KIND -> ADDSUB)
      registerOpcode(RvpPackedAluPlugin.PADD_H).decode(ELEM_W -> EW.H, ALU_KIND -> ADDSUB)
      registerOpcode(RvpPackedAluPlugin.PSUB_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> ADDSUB)
      registerOpcode(RvpPackedAluPlugin.PSUB_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> ADDSUB)
      registerOpcode(RvpPackedAluPlugin.PADD_BS).decode(ELEM_W -> EW.B, IS_SCALAR -> True, ALU_KIND -> ADDSUB)
      registerOpcode(RvpPackedAluPlugin.PADD_HS).decode(ELEM_W -> EW.H, IS_SCALAR -> True, ALU_KIND -> ADDSUB)
      registerOpcode(RvpPackedAluPlugin.PMSLTU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> SLTU)
      registerOpcode(RvpPackedAluPlugin.PMSLTU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> SLTU)
      registerOpcode(RvpPackedAluPlugin.PMSEQ_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> CMPEQ)
      registerOpcode(RvpPackedAluPlugin.PMSEQ_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> CMPEQ)
      registerOpcode(RvpPackedAluPlugin.PMINU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> MINU)
      registerOpcode(RvpPackedAluPlugin.PMINU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> MINU)
      registerOpcode(RvpPackedAluPlugin.PMAXU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> MAXU)
      registerOpcode(RvpPackedAluPlugin.PMAXU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> MAXU)
      registerOpcode(RvpPackedAluPlugin.PMSLT_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> SLT)
      registerOpcode(RvpPackedAluPlugin.PMSLT_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> SLT)
      registerOpcode(RvpPackedAluPlugin.PMIN_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> MIN)
      registerOpcode(RvpPackedAluPlugin.PMIN_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> MIN)
      registerOpcode(RvpPackedAluPlugin.PMAX_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> MAX)
      registerOpcode(RvpPackedAluPlugin.PMAX_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> MAX)
      registerOpcode(RvpPackedAluPlugin.PAADDU_B).decode(ELEM_W -> EW.B, ALU_KIND -> AVGU)
      registerOpcode(RvpPackedAluPlugin.PAADDU_H).decode(ELEM_W -> EW.H, ALU_KIND -> AVGU)
      registerOpcode(RvpPackedAluPlugin.PAADD_B).decode(ELEM_W -> EW.B, ALU_KIND -> AVGS)
      registerOpcode(RvpPackedAluPlugin.PAADD_H).decode(ELEM_W -> EW.H, ALU_KIND -> AVGS)
      registerOpcode(RvpPackedAluPlugin.PASUB_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> AVGS)
      registerOpcode(RvpPackedAluPlugin.PASUB_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> AVGS)
      registerOpcode(RvpPackedAluPlugin.PASUBU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> AVGU)
      registerOpcode(RvpPackedAluPlugin.PASUBU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> AVGU)
      registerOpcode(RvpPackedAluPlugin.AADD).decode(ELEM_W -> EW.W, ALU_KIND -> AVGS)
      registerOpcode(RvpPackedAluPlugin.AADDU).decode(ELEM_W -> EW.W, ALU_KIND -> AVGU)
      registerOpcode(RvpPackedAluPlugin.ASUB).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> AVGS)
      registerOpcode(RvpPackedAluPlugin.ASUBU).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> AVGU)
      registerOpcode(RvpPackedAluPlugin.MSEQ).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> CMPEQ)
      registerOpcode(RvpPackedAluPlugin.MSLT).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> SLT)
      registerOpcode(RvpPackedAluPlugin.MSLTU).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> SLTU)
      registerOpcode(RvpPackedAluPlugin.PABD_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> ABSD)
      registerOpcode(RvpPackedAluPlugin.PABD_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> ABSD)
      registerOpcode(RvpPackedAluPlugin.PABDU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> ABSDU)
      registerOpcode(RvpPackedAluPlugin.PABDU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> ABSDU)
      registerOpcode(RvpPackedAluPlugin.PABDSUMU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> ABSDSUMU)
      registerOpcodeRDS(RvpPackedAluPlugin.PABDSUMAU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, DO_ACCUMULATE -> True, ALU_KIND -> ABSDSUMU)

      registerOpcodeRs1(RvpPackedAluPlugin.PSABS_B).decode(ELEM_W -> EW.B, ALU_KIND -> SABS)
      registerOpcodeRs1(RvpPackedAluPlugin.PSABS_H).decode(ELEM_W -> EW.H, ALU_KIND -> SABS)

      registerOpcode(RvpPackedAluPlugin.PAS_HX).decode(ELEM_W -> EW.H, DO_SUB -> True, IS_CROSS -> True, ALU_KIND -> ADDSUB)
      registerOpcode(RvpPackedAluPlugin.PSA_HX).decode(ELEM_W -> EW.H, IS_CROSS -> True, ALU_KIND -> ADDSUB)
      registerOpcode(RvpPackedAluPlugin.PAAS_HX).decode(ELEM_W -> EW.H, DO_SUB -> True, IS_CROSS -> True, ALU_KIND -> AVGS)
      registerOpcode(RvpPackedAluPlugin.PASA_HX).decode(ELEM_W -> EW.H, IS_CROSS -> True, ALU_KIND -> AVGS)
      registerOpcode(RvpPackedAluPlugin.PSAS_HX).decode(ELEM_W -> EW.H, DO_SUB -> True, IS_CROSS -> True, ALU_KIND -> SCROSS)
      registerOpcode(RvpPackedAluPlugin.PSSA_HX).decode(ELEM_W -> EW.H, IS_CROSS -> True, ALU_KIND -> SCROSS)

      registerOpcode(RvpPackedAluPlugin.SSH1SADD).decode(ELEM_W -> EW.W, IS_SH1 -> True, ALU_KIND -> SSH1)
      registerOpcode(RvpPackedAluPlugin.SSH1SADD_H).decode(ELEM_W -> EW.H, IS_SH1 -> True, ALU_KIND -> SSH1)
      registerOpcode(RvpPackedAluPlugin.PSH1ADD_H).decode(ELEM_W -> EW.H, IS_SH1 -> True, ALU_KIND -> ADDSUB)

      registerOpcode(RvpPackedAluPlugin.PREDSUM_BS).decode(ELEM_W -> EW.B, ALU_KIND -> REDSUMS)
      registerOpcode(RvpPackedAluPlugin.PREDSUM_HS).decode(ELEM_W -> EW.H, ALU_KIND -> REDSUMS)
      registerOpcode(RvpPackedAluPlugin.PREDSUMU_BS).decode(ELEM_W -> EW.B, ALU_KIND -> REDSUMU)
      registerOpcode(RvpPackedAluPlugin.PREDSUMU_HS).decode(ELEM_W -> EW.H, ALU_KIND -> REDSUMU)

      if (pairs) { // ---- register-pair ops, dropped by --rvp-no-pairs ----
      registerOpcodeRDS(RvpPackedAluPlugin.PREDSUM_DHS).decode(ELEM_W -> EW.H, ALU_KIND -> REDSUMS, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedAluPlugin.PREDSUM_DBS).decode(ELEM_W -> EW.B, ALU_KIND -> REDSUMS, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedAluPlugin.PREDSUMU_DHS).decode(ELEM_W -> EW.H, ALU_KIND -> REDSUMU, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedAluPlugin.PREDSUMU_DBS).decode(ELEM_W -> EW.B, ALU_KIND -> REDSUMU, DOUBLE_WIDE -> True)

      registerOpcode(RvpPackedAluPlugin.WADDU).decode(ELEM_W -> EW.W, ALU_KIND -> WIDEN, PAIR_RD -> True)
      registerOpcode(RvpPackedAluPlugin.WADD).decode(ELEM_W -> EW.W, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True, PAIR_RD -> True)
      registerOpcode(RvpPackedAluPlugin.WSUBU).decode(ELEM_W -> EW.W, ALU_KIND -> WIDEN, PAIR_RD -> True, DO_SUB -> True)
      registerOpcode(RvpPackedAluPlugin.WSUB).decode(ELEM_W -> EW.W, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True, PAIR_RD -> True, DO_SUB -> True)
      registerOpcode(RvpPackedAluPlugin.PWSUB_H).decode(ELEM_W -> EW.H, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True, PAIR_RD -> True, DO_SUB -> True)
      registerOpcode(RvpPackedAluPlugin.PWSUB_B).decode(ELEM_W -> EW.B, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True, PAIR_RD -> True, DO_SUB -> True)
      registerOpcode(RvpPackedAluPlugin.PWSUBU_H).decode(ELEM_W -> EW.H, ALU_KIND -> WIDEN, PAIR_RD -> True, DO_SUB -> True)
      registerOpcode(RvpPackedAluPlugin.PWSUBU_B).decode(ELEM_W -> EW.B, ALU_KIND -> WIDEN, PAIR_RD -> True, DO_SUB -> True)
      registerOpcode(RvpPackedAluPlugin.PWADD_H).decode(ELEM_W -> EW.H, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True, PAIR_RD -> True)
      registerOpcode(RvpPackedAluPlugin.PWADD_B).decode(ELEM_W -> EW.B, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True, PAIR_RD -> True)
      registerOpcode(RvpPackedAluPlugin.PWADDU_H).decode(ELEM_W -> EW.H, ALU_KIND -> WIDEN, PAIR_RD -> True)
      registerOpcode(RvpPackedAluPlugin.PWADDU_B).decode(ELEM_W -> EW.B, ALU_KIND -> WIDEN, PAIR_RD -> True)
      } // pairs

      registerOpcode(RvpPackedAluPlugin.PSADD_B).decode(ELEM_W -> EW.B, ALU_KIND -> SADD)
      registerOpcode(RvpPackedAluPlugin.PSADDU_B).decode(ELEM_W -> EW.B, ALU_KIND -> SADDU)
      registerOpcode(RvpPackedAluPlugin.PSADD_H).decode(ELEM_W -> EW.H, ALU_KIND -> SADD)
      registerOpcode(RvpPackedAluPlugin.PSADDU_H).decode(ELEM_W -> EW.H, ALU_KIND -> SADDU)

      registerOpcode(RvpPackedAluPlugin.PSSUB_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> SSUB)
      registerOpcode(RvpPackedAluPlugin.PSSUBU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> SSUBU)
      registerOpcode(RvpPackedAluPlugin.PSSUB_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> SSUB)
      registerOpcode(RvpPackedAluPlugin.PSSUBU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> SSUBU)

      registerOpcode(RvpPackedAluPlugin.SADD_W).decode(ELEM_W -> EW.W, ALU_KIND -> SADD)
      registerOpcode(RvpPackedAluPlugin.SADDU_W).decode(ELEM_W -> EW.W, ALU_KIND -> SADDU)
      registerOpcode(RvpPackedAluPlugin.SSUB_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> SSUB)
      registerOpcode(RvpPackedAluPlugin.SSUBU_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> SSUBU)

      registerOpcodeRs1(RvpPackedAluPlugin.SATI).decode(ELEM_W -> EW.W, ALU_KIND -> SATIW)
      registerOpcodeRs1(RvpPackedAluPlugin.USATI).decode(ELEM_W -> EW.W, ALU_KIND -> USATIW)
      registerOpcodeRs1(RvpPackedAluPlugin.PSATI_H).decode(ELEM_W -> EW.H, ALU_KIND -> SATIW)
      registerOpcodeRs1(RvpPackedAluPlugin.PUSATI_H).decode(ELEM_W -> EW.H, ALU_KIND -> USATIW)

      if (pairs) { // ---- register-pair ops, dropped by --rvp-no-pairs ----
      registerOpcodeRDS(RvpPackedAluPlugin.PADD_DHS).decode(ELEM_W -> EW.H, IS_SCALAR -> True, ALU_KIND -> ADDSUB, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedAluPlugin.PADD_DBS).decode(ELEM_W -> EW.B, IS_SCALAR -> True, ALU_KIND -> ADDSUB, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeRDS(RvpPackedAluPlugin.PADD_DWS).decode(ELEM_W -> EW.W, IS_SCALAR -> True, ALU_KIND -> ADDSUB, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedAluPlugin.PSATI_DH).decode(ELEM_W -> EW.H, ALU_KIND -> SATIW, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedAluPlugin.PSATI_DW).decode(ELEM_W -> EW.W, ALU_KIND -> SATIW, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedAluPlugin.PUSATI_DH).decode(ELEM_W -> EW.H, ALU_KIND -> USATIW, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedAluPlugin.PUSATI_DW).decode(ELEM_W -> EW.W, ALU_KIND -> USATIW, PAIR_RD -> True, DOUBLE_WIDE -> True)

      registerOpcodeImmRDS(RvpPackedAluPlugin.PSABS_DB).decode(ELEM_W -> EW.B, ALU_KIND -> SABS, PAIR_RD -> True, DOUBLE_WIDE -> True)
      registerOpcodeImmRDS(RvpPackedAluPlugin.PSABS_DH).decode(ELEM_W -> EW.H, ALU_KIND -> SABS, PAIR_RD -> True, DOUBLE_WIDE -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PADD_DW).decode(ELEM_W -> EW.W, ALU_KIND -> ADDSUB, PAIR_RD -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PADD_DH).decode(ELEM_W -> EW.H, ALU_KIND -> ADDSUB, PAIR_RD -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PADD_DB).decode(ELEM_W -> EW.B, ALU_KIND -> ADDSUB, PAIR_RD -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSUB_DW).decode(ELEM_W -> EW.W, ALU_KIND -> ADDSUB, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSUB_DH).decode(ELEM_W -> EW.H, ALU_KIND -> ADDSUB, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSUB_DB).decode(ELEM_W -> EW.B, ALU_KIND -> ADDSUB, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSADD_DW).decode(ELEM_W -> EW.W, ALU_KIND -> SADD)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSADD_DH).decode(ELEM_W -> EW.H, ALU_KIND -> SADD)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSADD_DB).decode(ELEM_W -> EW.B, ALU_KIND -> SADD)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMIN_DW).decode(ELEM_W -> EW.W, ALU_KIND -> MIN, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMIN_DH).decode(ELEM_W -> EW.H, ALU_KIND -> MIN, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMIN_DB).decode(ELEM_W -> EW.B, ALU_KIND -> MIN, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMINU_DW).decode(ELEM_W -> EW.W, ALU_KIND -> MINU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMINU_DH).decode(ELEM_W -> EW.H, ALU_KIND -> MINU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMINU_DB).decode(ELEM_W -> EW.B, ALU_KIND -> MINU, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMAX_DW).decode(ELEM_W -> EW.W, ALU_KIND -> MAX, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMAX_DH).decode(ELEM_W -> EW.H, ALU_KIND -> MAX, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMAX_DB).decode(ELEM_W -> EW.B, ALU_KIND -> MAX, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMAXU_DW).decode(ELEM_W -> EW.W, ALU_KIND -> MAXU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMAXU_DH).decode(ELEM_W -> EW.H, ALU_KIND -> MAXU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMAXU_DB).decode(ELEM_W -> EW.B, ALU_KIND -> MAXU, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSADDU_DW).decode(ELEM_W -> EW.W, ALU_KIND -> SADDU)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSADDU_DH).decode(ELEM_W -> EW.H, ALU_KIND -> SADDU)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSADDU_DB).decode(ELEM_W -> EW.B, ALU_KIND -> SADDU)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSSUB_DW).decode(ELEM_W -> EW.W, ALU_KIND -> SSUB, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSSUB_DH).decode(ELEM_W -> EW.H, ALU_KIND -> SSUB, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSSUB_DB).decode(ELEM_W -> EW.B, ALU_KIND -> SSUB, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSSUBU_DW).decode(ELEM_W -> EW.W, ALU_KIND -> SSUBU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSSUBU_DH).decode(ELEM_W -> EW.H, ALU_KIND -> SSUBU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSSUBU_DB).decode(ELEM_W -> EW.B, ALU_KIND -> SSUBU, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMSEQ_DW).decode(ELEM_W -> EW.W, ALU_KIND -> CMPEQ, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMSEQ_DH).decode(ELEM_W -> EW.H, ALU_KIND -> CMPEQ, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMSEQ_DB).decode(ELEM_W -> EW.B, ALU_KIND -> CMPEQ, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMSLT_DW).decode(ELEM_W -> EW.W, ALU_KIND -> SLT, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMSLT_DH).decode(ELEM_W -> EW.H, ALU_KIND -> SLT, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMSLT_DB).decode(ELEM_W -> EW.B, ALU_KIND -> SLT, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMSLTU_DW).decode(ELEM_W -> EW.W, ALU_KIND -> SLTU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMSLTU_DH).decode(ELEM_W -> EW.H, ALU_KIND -> SLTU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PMSLTU_DB).decode(ELEM_W -> EW.B, ALU_KIND -> SLTU, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PAADD_DW).decode(ELEM_W -> EW.W, ALU_KIND -> AVGS, PAIR_RD -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PAADD_DH).decode(ELEM_W -> EW.H, ALU_KIND -> AVGS, PAIR_RD -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PAADD_DB).decode(ELEM_W -> EW.B, ALU_KIND -> AVGS, PAIR_RD -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PAADDU_DW).decode(ELEM_W -> EW.W, ALU_KIND -> AVGU, PAIR_RD -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PAADDU_DH).decode(ELEM_W -> EW.H, ALU_KIND -> AVGU, PAIR_RD -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PAADDU_DB).decode(ELEM_W -> EW.B, ALU_KIND -> AVGU, PAIR_RD -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PASUB_DW).decode(ELEM_W -> EW.W, ALU_KIND -> AVGS, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PASUB_DH).decode(ELEM_W -> EW.H, ALU_KIND -> AVGS, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PASUB_DB).decode(ELEM_W -> EW.B, ALU_KIND -> AVGS, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PASUBU_DW).decode(ELEM_W -> EW.W, ALU_KIND -> AVGU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PASUBU_DH).decode(ELEM_W -> EW.H, ALU_KIND -> AVGU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PASUBU_DB).decode(ELEM_W -> EW.B, ALU_KIND -> AVGU, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PABD_DH).decode(ELEM_W -> EW.H, ALU_KIND -> ABSD, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PABD_DB).decode(ELEM_W -> EW.B, ALU_KIND -> ABSD, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PABDU_DH).decode(ELEM_W -> EW.H, ALU_KIND -> ABSDU, DO_SUB -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PABDU_DB).decode(ELEM_W -> EW.B, ALU_KIND -> ABSDU, DO_SUB -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PAS_DHX).decode(ELEM_W -> EW.H, ALU_KIND -> ADDSUB, DO_SUB -> True, IS_CROSS -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSA_DHX).decode(ELEM_W -> EW.H, ALU_KIND -> ADDSUB, IS_CROSS -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSAS_DHX).decode(ELEM_W -> EW.H, ALU_KIND -> SCROSS, DO_SUB -> True, IS_CROSS -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSSA_DHX).decode(ELEM_W -> EW.H, ALU_KIND -> SCROSS, IS_CROSS -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PAAS_DHX).decode(ELEM_W -> EW.H, ALU_KIND -> AVGS, DO_SUB -> True, IS_CROSS -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PASA_DHX).decode(ELEM_W -> EW.H, ALU_KIND -> AVGS, IS_CROSS -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSH1ADD_DH).decode(ELEM_W -> EW.H, ALU_KIND -> ADDSUB, IS_SH1 -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSH1ADD_DW).decode(ELEM_W -> EW.W, ALU_KIND -> ADDSUB, IS_SH1 -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSSH1SADD_DH).decode(ELEM_W -> EW.H, ALU_KIND -> SSH1, IS_SH1 -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.PSSH1SADD_DW).decode(ELEM_W -> EW.W, ALU_KIND -> SSH1, IS_SH1 -> True)

      registerOpcodeRDS_RS3(RvpPackedAluPlugin.ADDD).decode(ELEM_W -> EW.W, ALU_KIND -> ADDSUB, IS_D -> True)
      registerOpcodeRDS_RS3(RvpPackedAluPlugin.SUBD).decode(ELEM_W -> EW.W, ALU_KIND -> ADDSUB, IS_D -> True, DO_SUB -> True)

      registerOpcodeWidenAcc(RvpPackedAluPlugin.WADDAU).decode(ELEM_W -> EW.W, ALU_KIND -> WIDEN)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.WADDA).decode(ELEM_W -> EW.W, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.WSUBAU).decode(ELEM_W -> EW.W, ALU_KIND -> WIDEN, DO_SUB -> True)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.WSUBA).decode(ELEM_W -> EW.W, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True, DO_SUB -> True)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.PWSUBA_H).decode(ELEM_W -> EW.H, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True, DO_SUB -> True)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.PWSUBA_B).decode(ELEM_W -> EW.B, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True, DO_SUB -> True)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.PWSUBAU_H).decode(ELEM_W -> EW.H, ALU_KIND -> WIDEN, DO_SUB -> True)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.PWSUBAU_B).decode(ELEM_W -> EW.B, ALU_KIND -> WIDEN, DO_SUB -> True)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.PWADDA_H).decode(ELEM_W -> EW.H, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.PWADDA_B).decode(ELEM_W -> EW.B, ALU_KIND -> WIDEN, WIDEN_SIGNED -> True)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.PWADDAU_H).decode(ELEM_W -> EW.H, ALU_KIND -> WIDEN)
      registerOpcodeWidenAcc(RvpPackedAluPlugin.PWADDAU_B).decode(ELEM_W -> EW.B, ALU_KIND -> WIDEN)

      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSADDU_DW, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSADDU_DW, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSADDU_DW, PAIR_RS2, True)

      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSADDU_DH, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSADDU_DH, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSADDU_DH, PAIR_RS2, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSADDU_DB, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSADDU_DB, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSADDU_DB, PAIR_RS2, True)

      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUB_DB, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUB_DB, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUB_DB, PAIR_RS2, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUB_DW, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUB_DW, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUB_DW, PAIR_RS2, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUB_DH, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUB_DH, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUB_DH, PAIR_RS2, True)

      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUBU_DB, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUBU_DB, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUBU_DB, PAIR_RS2, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUBU_DW, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUBU_DW, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUBU_DW, PAIR_RS2, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUBU_DH, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUBU_DH, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSSUBU_DH, PAIR_RS2, True)

      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.WADD, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.WADDU, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.WSUBU, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.WSUB, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PWADD_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PWADD_B, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PWSUB_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PWSUB_B, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PWSUBU_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PWSUBU_B, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PWADDU_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PWADDU_B, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PADD_DHS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PADD_DHS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PADD_DBS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PADD_DBS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PADD_DWS, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PADD_DWS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSATI_DH, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSATI_DH, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSATI_DW, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSATI_DW, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PUSATI_DH, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PUSATI_DH, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PUSATI_DW, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PUSATI_DW, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSABS_DB, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSABS_DB, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSABS_DH, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PSABS_DH, PAIR_RD, True)
      // predsum.d* read the rs1 pair but write one plain rd: PAIR_RS1 only.
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PREDSUM_DHS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PREDSUM_DBS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PREDSUMU_DHS, PAIR_RS1, True)
      decoderService.addMicroOpDecoding(RvpPackedAluPlugin.PREDSUMU_DBS, PAIR_RS1, True)
      } // pairs
    } else {
      // ---- RV64 -----------------------------------------------------------
      // Basic add / subtract
      registerOpcode(Rv64.PADD_B).decode(ELEM_W -> EW.B, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PADD_H).decode(ELEM_W -> EW.H, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PADD_W).decode(ELEM_W -> EW.W, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PADD_BS).decode(ELEM_W -> EW.B, IS_SCALAR -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PADD_HS).decode(ELEM_W -> EW.H, IS_SCALAR -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PADD_WS).decode(ELEM_W -> EW.W, IS_SCALAR -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PSUB_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PSUB_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PSUB_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> ADDSUB)

      // Saturating add / subtract
      registerOpcode(Rv64.PSADD_B).decode(ELEM_W -> EW.B, ALU_KIND -> SADD)
      registerOpcode(Rv64.PSADD_H).decode(ELEM_W -> EW.H, ALU_KIND -> SADD)
      registerOpcode(Rv64.PSADD_W).decode(ELEM_W -> EW.W, ALU_KIND -> SADD)
      registerOpcode(Rv64.PSADDU_B).decode(ELEM_W -> EW.B, ALU_KIND -> SADDU)
      registerOpcode(Rv64.PSADDU_H).decode(ELEM_W -> EW.H, ALU_KIND -> SADDU)
      registerOpcode(Rv64.PSADDU_W).decode(ELEM_W -> EW.W, ALU_KIND -> SADDU)
      registerOpcode(Rv64.PSSUB_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> SSUB)
      registerOpcode(Rv64.PSSUB_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> SSUB)
      registerOpcode(Rv64.PSSUB_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> SSUB)
      registerOpcode(Rv64.PSSUBU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> SSUBU)
      registerOpcode(Rv64.PSSUBU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> SSUBU)
      registerOpcode(Rv64.PSSUBU_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> SSUBU)

      // Averaging add / subtract
      registerOpcode(Rv64.PAADD_B).decode(ELEM_W -> EW.B, ALU_KIND -> AVGS)
      registerOpcode(Rv64.PAADD_H).decode(ELEM_W -> EW.H, ALU_KIND -> AVGS)
      registerOpcode(Rv64.PAADD_W).decode(ELEM_W -> EW.W, ALU_KIND -> AVGS)
      registerOpcode(Rv64.PAADDU_B).decode(ELEM_W -> EW.B, ALU_KIND -> AVGU)
      registerOpcode(Rv64.PAADDU_H).decode(ELEM_W -> EW.H, ALU_KIND -> AVGU)
      registerOpcode(Rv64.PAADDU_W).decode(ELEM_W -> EW.W, ALU_KIND -> AVGU)
      registerOpcode(Rv64.PASUB_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> AVGS)
      registerOpcode(Rv64.PASUB_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> AVGS)
      registerOpcode(Rv64.PASUB_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> AVGS)
      registerOpcode(Rv64.PASUBU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> AVGU)
      registerOpcode(Rv64.PASUBU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> AVGU)
      registerOpcode(Rv64.PASUBU_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> AVGU)

      // Shift-add
      registerOpcode(Rv64.PSH1ADD_H).decode(ELEM_W -> EW.H, IS_SH1 -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PSH1ADD_W).decode(ELEM_W -> EW.W, IS_SH1 -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PSSH1SADD_H).decode(ELEM_W -> EW.H, IS_SH1 -> True, ALU_KIND -> SSH1)
      registerOpcode(Rv64.PSSH1SADD_W).decode(ELEM_W -> EW.W, IS_SH1 -> True, ALU_KIND -> SSH1)

      // Add-subtract cross
      registerOpcode(Rv64.PAS_HX).decode(
        ELEM_W -> EW.H, DO_SUB -> True, IS_CROSS -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PAS_WX).decode(
        ELEM_W -> EW.W, DO_SUB -> True, IS_CROSS -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PSA_HX).decode(ELEM_W -> EW.H, IS_CROSS -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PSA_WX).decode(ELEM_W -> EW.W, IS_CROSS -> True, ALU_KIND -> ADDSUB)
      registerOpcode(Rv64.PSAS_HX).decode(
        ELEM_W -> EW.H, DO_SUB -> True, IS_CROSS -> True, ALU_KIND -> SCROSS)
      registerOpcode(Rv64.PSAS_WX).decode(
        ELEM_W -> EW.W, DO_SUB -> True, IS_CROSS -> True, ALU_KIND -> SCROSS)
      registerOpcode(Rv64.PSSA_HX).decode(ELEM_W -> EW.H, IS_CROSS -> True, ALU_KIND -> SCROSS)
      registerOpcode(Rv64.PSSA_WX).decode(ELEM_W -> EW.W, IS_CROSS -> True, ALU_KIND -> SCROSS)
      registerOpcode(Rv64.PAAS_HX).decode(
        ELEM_W -> EW.H, DO_SUB -> True, IS_CROSS -> True, ALU_KIND -> AVGS)
      registerOpcode(Rv64.PAAS_WX).decode(
        ELEM_W -> EW.W, DO_SUB -> True, IS_CROSS -> True, ALU_KIND -> AVGS)
      registerOpcode(Rv64.PASA_HX).decode(ELEM_W -> EW.H, IS_CROSS -> True, ALU_KIND -> AVGS)
      registerOpcode(Rv64.PASA_WX).decode(ELEM_W -> EW.W, IS_CROSS -> True, ALU_KIND -> AVGS)

      // Absolute difference
      registerOpcode(Rv64.PABD_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> ABSD)
      registerOpcode(Rv64.PABD_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> ABSD)
      registerOpcode(Rv64.PABDU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> ABSDU)
      registerOpcode(Rv64.PABDU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> ABSDU)
      registerOpcode(Rv64.PABDSUMU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> ABSDSUMU)
      registerOpcodeRDS(Rv64.PABDSUMAU_B).decode(
        ELEM_W -> EW.B, DO_SUB -> True, DO_ACCUMULATE -> True, ALU_KIND -> ABSDSUMU)

      // Saturating absolute
      registerOpcodeRs1(Rv64.PSABS_B).decode(ELEM_W -> EW.B, ALU_KIND -> SABS)
      registerOpcodeRs1(Rv64.PSABS_H).decode(ELEM_W -> EW.H, ALU_KIND -> SABS)

      // Reduction sum
      registerOpcode(Rv64.PREDSUM_BS).decode(ELEM_W -> EW.B, ALU_KIND -> REDSUMS)
      registerOpcode(Rv64.PREDSUM_HS).decode(ELEM_W -> EW.H, ALU_KIND -> REDSUMS)
      registerOpcode(Rv64.PREDSUM_WS).decode(ELEM_W -> EW.W, ALU_KIND -> REDSUMS)
      registerOpcode(Rv64.PREDSUMU_BS).decode(ELEM_W -> EW.B, ALU_KIND -> REDSUMU)
      registerOpcode(Rv64.PREDSUMU_HS).decode(ELEM_W -> EW.H, ALU_KIND -> REDSUMU)
      registerOpcode(Rv64.PREDSUMU_WS).decode(ELEM_W -> EW.W, ALU_KIND -> REDSUMU)

      // Min / max
      registerOpcode(Rv64.PMIN_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> MIN)
      registerOpcode(Rv64.PMIN_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> MIN)
      registerOpcode(Rv64.PMIN_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> MIN)
      registerOpcode(Rv64.PMINU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> MINU)
      registerOpcode(Rv64.PMINU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> MINU)
      registerOpcode(Rv64.PMINU_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> MINU)
      registerOpcode(Rv64.PMAX_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> MAX)
      registerOpcode(Rv64.PMAX_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> MAX)
      registerOpcode(Rv64.PMAX_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> MAX)
      registerOpcode(Rv64.PMAXU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> MAXU)
      registerOpcode(Rv64.PMAXU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> MAXU)
      registerOpcode(Rv64.PMAXU_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> MAXU)

      // Compare -> element mask
      registerOpcode(Rv64.PMSEQ_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> CMPEQ)
      registerOpcode(Rv64.PMSEQ_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> CMPEQ)
      registerOpcode(Rv64.PMSEQ_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> CMPEQ)
      registerOpcode(Rv64.PMSLT_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> SLT)
      registerOpcode(Rv64.PMSLT_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> SLT)
      registerOpcode(Rv64.PMSLT_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> SLT)
      registerOpcode(Rv64.PMSLTU_B).decode(ELEM_W -> EW.B, DO_SUB -> True, ALU_KIND -> SLTU)
      registerOpcode(Rv64.PMSLTU_H).decode(ELEM_W -> EW.H, DO_SUB -> True, ALU_KIND -> SLTU)
      registerOpcode(Rv64.PMSLTU_W).decode(ELEM_W -> EW.W, DO_SUB -> True, ALU_KIND -> SLTU)

      // Saturate to immediate width. The packed forms are the per-word
      // network run twice and the scalar forms clamp the whole 64-bit register
      // against a uimm6 width, which is what SAT_XLEN selects.
      registerOpcodeRs1(Rv64.PSATI_H).decode(ELEM_W -> EW.H, ALU_KIND -> SATIW)
      registerOpcodeRs1(Rv64.PSATI_W).decode(ELEM_W -> EW.W, ALU_KIND -> SATIW)
      registerOpcodeRs1(Rv64.SATI).decode(ELEM_W -> EW.W, ALU_KIND -> SATIW, SAT_XLEN -> True)
      registerOpcodeRs1(Rv64.PUSATI_H).decode(ELEM_W -> EW.H, ALU_KIND -> USATIW)
      registerOpcodeRs1(Rv64.PUSATI_W).decode(ELEM_W -> EW.W, ALU_KIND -> USATIW)
      registerOpcodeRs1(Rv64.USATI).decode(ELEM_W -> EW.W, ALU_KIND -> USATIW, SAT_XLEN -> True)
    }

    decoderRetainer.release()
    uopRetainer.release()

    val prep = (!rv64) generate new el.Execute(id = 0) {
      // read integer register file
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt
      // RS3 (odd half of a pair source / destination pair) is declared only by pair ops;
      // without them the lane has no such read port.
      val rs3 = if (withPairs) up(el(IntRegFile, RS3)).asUInt else U(0, 32 bits)
      val rds = up(el(IntRegFile, RDS)).asUInt
      val sub = this (DO_SUB)
      val ew = this (ELEM_W)
      val scalar = this (IS_SCALAR)
      val half = ew === EW.H
      val word = ew === EW.W
      val cross = this (IS_CROSS)
      val shift1 = this (IS_SH1)

      // Scalar-operand instructions broadcast the low element of rs2 across all lanes.
      val rs2b = Mux(scalar,
        Mux(word,
          rs2,
          Mux(half,
            (rs2(15 downto 0) ## rs2(15 downto 0)).asUInt,
            (rs2(7 downto 0) ## rs2(7 downto 0) ##
              rs2(7 downto 0) ## rs2(7 downto 0)).asUInt)),
        rs2)


      // Cross ops pair a's low half with b's high half and vice-versa (swap b's halves).
      def crossSwap(x: UInt): UInt =
        Mux(cross, (x(15 downto 0) ## x(31 downto 16)).asUInt, x)

      // Subtract via two's complement: a - b = a + ~b + 1.
      // Cross ops subtract in one halfword and add in the other, so the complement is
      // applied per-halfword. subLo/subHi drive the low (even) and high (odd) lanes.
      val subLo = sub
      val subHi = sub ^ cross

      def crossComplement(x: UInt): UInt =
        (Mux(subHi, ~x(31 downto 16), x(31 downto 16)) ##
          Mux(subLo, ~x(15 downto 0), x(15 downto 0))).asUInt

      // Plain <<1 per lane (psh1add): word = one 32-bit lane, half = two 16-bit lanes.
      def sh1Src(x: UInt): UInt = Mux(word,
        (x(30 downto 0) ## B(0, 1 bit)).asUInt,
        ((x(30 downto 16) ## B(0, 1 bit)) ## (x(14 downto 0) ## B(0, 1 bit))).asUInt)

      // A-operand chain: saturating <<1 for SSH1, plain <<1 for IS_SH1, raw otherwise.
      def aChain(x: UInt): (UInt, Bool) = {
        val (sat, satFlag) = RvpSaturatingAddOps.signedSatShift1(x.asBits, ew)
        (Mux(ALU_KIND === SSH1, sat.asUInt, Mux(shift1, sh1Src(x), x)), satFlag)
      }

      val (aSrc, satSh1) = aChain(rs1) // low  word A, pre-complement
      // The high word exists for pair sources only; no-pairs feeds the upper adder half
      // constants so it prunes with its unread SUMWIDE_HI / DOUBLE_WIDE_HIGH outputs.
      val (aSrcHi, satSh1Hi) = if (withPairs) aChain(rds) else (U(0, 32 bits), False) // high word A, pre-complement

      val bEff = crossSwap(rs2b) // low  word B
      val bEffHi = if (withPairs) crossSwap(Mux(this (PAIR_RS2), rs3, rs2b)) else U(0, 32 bits) // high word B

      // One shared 72-bit adder serves all lanes. A guard bit sits between every byte
      // pair. prop(k) says whether guard k propagates a carry across that boundary.
      //   guard 0,1,2 -> inside the low word   3 -> between the two words   4,5,6 -> high
      val is64 = this (IS_D)

      def prop(k: Int): Bool = {
        if (k == 3) is64 else if (k == 1 || k == 5) word else half || word
      }

      // Guard k feeds byte k+1. Each independent subtract lane needs a carry-in;
      // open guards propagate the existing carry instead of injecting another one.
      def injectSub(k: Int): Bool = if (k == 0 || k == 3 || k == 4) subLo else subHi

      def aGuard(k: Int): Bits = (prop(k) || injectSub(k)).asBits

      def bGuard(k: Int): Bits = (!prop(k) && injectSub(k)).asBits

      val aData64 = (aSrcHi ## aSrc).asBits
      val bData64 = (crossComplement(bEffHi) ## crossComplement(bEff)).asBits

      // Interleave operand bytes with guard bits (plus a top pad bit) -> 72-bit vectors.
      // Layout LSB..MSB: [byte0][g0][byte1][g1][byte2][g2][byte3][pad].
      val aWide = (B(0, 1 bit) ##
        aData64(63 downto 56) ## aGuard(6) ##
        aData64(55 downto 48) ## aGuard(5) ##
        aData64(47 downto 40) ## aGuard(4) ##
        aData64(39 downto 32) ## aGuard(3) ##
        aData64(31 downto 24) ## aGuard(2) ##
        aData64(23 downto 16) ## aGuard(1) ##
        aData64(15 downto 8) ## aGuard(0) ##
        aData64(7 downto 0)).asUInt

      val bWide = (B(0, 1 bit) ##
        bData64(63 downto 56) ## bGuard(6) ##
        bData64(55 downto 48) ## bGuard(5) ##
        bData64(47 downto 40) ## bGuard(4) ##
        bData64(39 downto 32) ## bGuard(3) ##
        bData64(31 downto 24) ## bGuard(2) ##
        bData64(23 downto 16) ## bGuard(1) ##
        bData64(15 downto 8) ## bGuard(0) ##
        bData64(7 downto 0)).asUInt

      // One segmented add. subLo supplies lane 0's +1; closed guards inject the
      // corresponding +1 for every later independent subtract lane.
      val sumWide72 = aWide + bWide + subLo.asUInt
      val sumWide = sumWide72(35 downto 0) // preserve low 36 bit view

      // Drop the guard bits to recover the packed 32-bit lane result
      val rd = sumWide(34 downto 27).asBits ## sumWide(25 downto 18).asBits ##
        sumWide(16 downto 9).asBits ## sumWide(7 downto 0).asBits

      val doubleWideHigh = sumWide72(70 downto 63) ## sumWide72(61 downto 54) ##
        sumWide72(52 downto 45) ## sumWide72(43 downto 36)

      // HIGH-word payloads (guarded): only the two-pass / widening-accumulate read-outs consume them.
      if (withPairs) this (DOUBLE_WIDE_HIGH) := doubleWideHigh
      this (SUMWIDE) := sumWide
      if (withPairs) this (SUMWIDE_HI) := sumWide72(71 downto 36)
      this (RD) := rd
      if (withPairs) this (ASRC_HI) := aSrcHi
      if (withPairs) this (BEFF_HI) := bEffHi
      if (withPairs) this (SAT_SH1_HI) := satSh1Hi
      this (ASRC) := aSrc
      this (BEFF) := bEff
      this (RS1V) := rs1
      this (RS2V) := rs2
      if (withPairs) this (RS3V) := rs3
      if (withPairs) this (RDSV) := rds
      val reduceSeed = this (ALU_KIND) === REDSUMS || this (ALU_KIND) === REDSUMU
      this (ACCBASE) := Mux(this (DO_ACCUMULATE), rds,
        Mux(reduceSeed, rs2, U(0, 32 bits)))
      this (SAT_SH1) := satSh1
      // Registered readout controls keep ALU_KIND comparisons out of the post-stage
      // arithmetic, predicate, select and writeback networks.
      this (RO_SIGNED) := this (ALU_KIND).mux(
        default -> False,
        SLT -> True, MIN -> True, MAX -> True, AVGS -> True, ABSD -> True,
        REDSUMS -> True, SADD -> True, SSUB -> True, SCROSS -> True, SSH1 -> True
      )
      this (RO_MAX) := this (ALU_KIND).mux(default -> False, MAX -> True, MAXU -> True)
      this (RO_EQ) := this (ALU_KIND) === CMPEQ
      this (RO_SSH1) := this (ALU_KIND) === SSH1
      this (RO_UNSIGNED) := this (ALU_KIND) === USATIW
      this (RO) := RvpReadout.ADDSUB
      switch(this (ALU_KIND)) {
        is(SLTU, SLT, CMPEQ) {
          this (RO) := RvpReadout.CMP
        }
        is(MINU, MIN, MAXU, MAX) {
          this (RO) := RvpReadout.MINMAX
        }
        is(AVGU, AVGS) {
          this (RO) := RvpReadout.AVG
        }
        is(ABSD, ABSDU) {
          this (RO) := RvpReadout.ABSD
        }
        is(ABSDSUMU) {
          this (RO) := RvpReadout.ABSDSUMU
        }
        is(REDSUMS, REDSUMU) {
          this (RO) := RvpReadout.REDSUM
        }
        is(SADD, SADDU, SSUB, SSUBU, SCROSS, SSH1) {
          this (RO) := RvpReadout.SATAS
        }
        is(SATIW, USATIW) {
          this (RO) := RvpReadout.SATI
        }
        is(SABS) {
          this (RO) := RvpReadout.SABS
        }
        is(WIDEN) {
          this (RO) := RvpReadout.WIDEN
        }
      }
    }

    val post = (!rv64) generate new el.Execute(id = postAt) {

      val sumWide = this (SUMWIDE)
      val rd = this (RD)
      val aSrc = this (ASRC)
      val bEff = this (BEFF)
      val satSh1 = this (SAT_SH1)
      val rs1 = this (RS1V)
      val rs2 = this (RS2V)
      val ew = this (ELEM_W)
      val subLo = this (DO_SUB)
      val subHi = this (DO_SUB) ^ this (IS_CROSS)
      val satN = Mux(ew === EW.W, Decode.UOP(24 downto 20), U"0" ## Decode.UOP(23 downto 20))

      // Double-wide read-outs time-multiplex one copy of the packed ALU's back end across
      // two cycles rather than instantiating a second one. pass.first = the frozen HIGH cycle
      val isTwoPass = this (RO).mux(
        default -> False,
        RvpReadout.SATAS -> True,
        RvpReadout.MINMAX -> True,
        RvpReadout.CMP -> True,
        RvpReadout.AVG -> True,
        RvpReadout.ABSD -> True,
        RvpReadout.SATI -> True,
        RvpReadout.SABS -> True,
        RvpReadout.REDSUM -> True)
      // No-pairs: the pair payloads are never decoded (RvpPairWritePlugin, which owns their
      // defaults, is not installed), so read them as constants and drop the HIGH cycle.
      // (defs, not vals: they must not move the payloads' creation order in the netlist)
      def doubleWide = if (withPairs) this (DOUBLE_WIDE) else False
      def pairRd = if (withPairs) this (PAIR_RD) else False
      def pairRdSrc = if (withPairs) this (PAIR_RD_SRC) else False
      val isTwoPassDW = doubleWide && isTwoPass
      val pass = RvpDoubleWidePass(el, isValid && SEL && isTwoPassDW, up.isMoving, enabled = withPairs)

      // The complete input set of every read-out below, muxed HIGH/LOW once.
      val mSumWide = if (withPairs) pass.first ? this (SUMWIDE_HI) | sumWide else sumWide // UInt(36)
      val mRd = if (withPairs) pass.first ? this (DOUBLE_WIDE_HIGH) | rd else rd // Bits(32), architectural polarity
      val mRs1 = if (withPairs) pass.first ? this (RDSV) | rs1 else rs1 // UInt(32)
      val mB = if (withPairs) pass.first ? this (BEFF_HI) | bEff else bEff // UInt(32)
      val clA = if (withPairs) pass.first ? this (ASRC_HI).asBits | aSrc.asBits else aSrc.asBits
      val mSatSh1 = if (withPairs) pass.first ? this (SAT_SH1_HI) | satSh1 else satSh1

      val clB = mB.asBits
      val clRd = mRd

      // Normalize each boundary flag once: addition keeps carry-out, subtraction
      // inverts carry-out into borrow. Cross forms select subtraction per halfword.
      def subAt(j: Int): Bool = if (j < 2) subLo else subHi

      def carryFlags(wide: UInt): Seq[Bool] =
        Seq(wide(8), wide(17), wide(26), wide(35)).zipWithIndex.map {
          case (carry, j) => carry ^ subAt(j)
        }

      val carries = carryFlags(sumWide)
      val mCarries = carryFlags(mSumWide)


      // One saturation detector/injector covers signed/unsigned add/sub, mixed cross add-sub and SSH1
      val (rdSatAS, satAS) = RvpSaturatingAddOps.satAddSub(clRd, clA, clB, mCarries, ew, this (RO_SIGNED), subLo, subHi)


      val satBounds = RvpSatWidthOps.satBounds(satN.asUInt)
      val (rdSatImm, satImm) = RvpSatWidthOps.clampToBounds(mRs1.asBits, satBounds, ew, this (RO_UNSIGNED))
      val (rdSatAbs, satSatAbs) = RvpSaturatingAddOps.signedSatAbs(mRs1.asBits, ew)
      // Frozen HIGH-cycle sat flag. The stage register cannot latch while frozen, so hold
      // it explicitly for the firing cycle. The HIGH-cycle result is held in rdHiHold,
      // which reuses the write-back mux (see aluResult below). satHiHold latches for every
      // two-pass family but is only ever read behind the satFamily gate.
      val satFlagMux = this (RO).mux(
        default -> (satAS | (this (RO_SSH1) && mSatSh1)),
        RvpReadout.SATI -> satImm,
        RvpReadout.SABS -> satSatAbs)
      val satHiHold = if (withPairs) RegNextWhen(satFlagMux, pass.first) else False

      val satFamily = this (RO).mux(
        default -> False,
        RvpReadout.SATAS -> True,
        RvpReadout.SATI -> True,
        RvpReadout.SABS -> True)
      this (SATURATED) := SEL && satFamily && (satFlagMux | (doubleWide && satHiHold))

      val absDiffSumUnsigned = RvpAbsDiffOps.absDiffSumUnsigned(rd, carries)
      this (ABS_DIFF_SUM) := absDiffSumUnsigned

      // The frozen cycle reduces the high word (RDSV), the firing cycle the low word
      // Non-.d forms never freeze, so for them this is reduceSum(rs1) exactly as before.
      // The two partial sums are combined into REDUCE_SUM below, after rdHiHold.
      val reduceSum = RvpReduceOps.reduceSum(mRs1, ew === EW.H, this (RO_SIGNED))

      val lanePred = RvpPackedLanes.signedLessThan(carries, rs1, bEff, ew)

      // Upper half of one widened lane: sign-extend the true (w+1)-th bit for the
      // signed forms. For unsigned, zero-extend an add's carry but sign-extend a
      // subtract's borrow (a-b is a signed value even for unsigned operands).
      def wideHigh(w: Int, signedTop: Bool, carry: Bool): Bits = Mux(
        this (WIDEN_SIGNED),
        B(w bits, default -> signedTop),
        Mux(this (DO_SUB), B(w bits, default -> carry), B(w - 1 bits, default -> False) ## carry)
      )

      val wideH0 = wideHigh(16, lanePred(1), carries(1)) ## rd(15 downto 0)
      val wideH1 = wideHigh(16, lanePred(3), carries(3)) ## rd(31 downto 16)

      val wideB = (0 until 4).map(i => wideHigh(8, lanePred(i), carries(i)) ## rd(8 * i + 7 downto 8 * i))

      val wideB0 = wideB(1) ## wideB(0) // -> rd_lo
      val wideB1 = wideB(3) ## wideB(2) // -> rd_hi

      // Result lanes are 2*ew wide, so the boundaries sit at 16/32/48 which is the same guard-bit
      // trick as the 72-bit source adder. EW.W is one 64-bit lane (all seams open, carry propagates into the odd half)
      // EW.H two independent 32-bit lanes
      // EW.B four independent 16-bit lanes.
      // No complement: wsuba/pwsuba
      // subtract inside the widening term, the accumulate itself is always an add.
      // layout LSB..MSB: [lane0:16][g0][lane1:16][g1][lane2:16][g2][lane3:16][pad]
      val accWiden = ew.mux(
        EW.B -> (wideB1 ## wideB0),
        EW.H -> (wideH1 ## wideH0),
        default -> (wideHigh(32, lanePred(3), carries(3)) ## rd.asBits)
      )

      val accBase = if (withPairs) pairRdSrc ? (this (RS3V) ## this (RDSV)).asBits | B(0, 64 bits) else B(0, 64 bits)

      def accProp(k: Int): Bool = if (k == 1) (ew === EW.W) else (ew === EW.W || ew === EW.H)

      def accLanes(x: Bits, guard: Int => Bits): UInt = (B(0, 1 bit) ##
        x(63 downto 48) ## guard(2) ##
        x(47 downto 32) ## guard(1) ##
        x(31 downto 16) ## guard(0) ##
        x(15 downto 0)).asUInt

      val accSum = accLanes(accWiden, k => accProp(k).asBits) + accLanes(accBase, _ => B(0, 1 bit))
      val accLo = accSum(32 downto 17).asBits ## accSum(15 downto 0).asBits
      val accHi = accSum(66 downto 51).asBits ## accSum(49 downto 34).asBits

      // Compare, min/max, average and absolute difference all consume the same
      // less-than predicate vector. Signedness is selected once at runtime and word fanout is shared by compare and min/max.
      val readoutPredicates = RvpPackedLanes.lanePredicates(mCarries, mRs1, mB, ew, this (RO_SIGNED))
      val readoutPredicatesW = RvpPackedLanes.wordFanout(readoutPredicates, ew === EW.W)
      val equalityPredicates = RvpCompareOps.equalityLanes(mRd, ew)
      val comparePredicates = (equalityPredicates zip readoutPredicatesW).map {
        case (eq, lt) => this (RO_EQ) ? eq | lt
      }
      val cmpResult = RvpCompareOps.mask(comparePredicates)
      val minMaxResult = RvpMinMaxOps.select(readoutPredicatesW, mRs1, mB, this (RO_MAX))
      val avgResult = RvpAverageOps.average(mRd, readoutPredicates, ew)
      val absdResult = RvpAbsDiffOps.absDiff(mRd, ew === EW.H, readoutPredicates)

      // The hold register is written only for a true two-pass read-out. Give
      // it the matching subset mux so unrelated single-pass cones (in particular
      // ABSDSUMU) cannot become structural timing paths to its D input.
      val twoPassResult = this (RO).mux(
        default -> B(0, 32 bits),
        RvpReadout.SATAS -> rdSatAS,
        RvpReadout.MINMAX -> minMaxResult,
        RvpReadout.CMP -> cmpResult,
        RvpReadout.AVG -> avgResult,
        RvpReadout.ABSD -> absdResult,
        RvpReadout.SATI -> rdSatImm,
        RvpReadout.SABS -> rdSatAbs,
        RvpReadout.REDSUM -> reduceSum
      )
      val rdHiHold = if (withPairs) RegNextWhen(twoPassResult, pass.first) else B(0, 32 bits)

      // predsum delta: the firing cycle's low-word partial plus, for .d* forms,
      // the high-word partial held from the frozen cycle. The combine sits here,
      // at postAt, so the hold is produced and consumed in the same stage
      this (REDUCE_SUM) := (reduceSum.asUInt +
        Mux(doubleWide, rdHiHold.asUInt, U(0, 32 bits))).asBits

      // Named post-process result. ABSDSUMU finishes its 32-bit accumulation after
      // the formatAt cut and only its 10-bit byte sum crosses this stage boundary.
      val singlePassResult = this (RO).mux(
        default -> B(0, 32 bits),
        RvpReadout.ADDSUB -> rd.asBits,
        RvpReadout.WIDEN -> Mux(pairRdSrc, accLo,
          ew.mux(EW.B -> wideB0, EW.H -> wideH0, default -> rd.asBits))
      )
      val aluResult = Mux(isTwoPass, twoPassResult, singlePassResult)
      this (ALU_RESULT) := aluResult

      // pair-destination high half -> RvpPairWritePlugin (absent without pairs)
      if (withPairs) hiPort.valid := SEL && pairRd && !pass.first

      val defaultHi = withPairs generate Mux(doubleWide, this (DOUBLE_WIDE_HIGH),
        Mux(pairRdSrc, accHi, ew.mux(
          EW.B -> wideB1,
          EW.H -> wideH1,
          default -> wideHigh(32, lanePred(3), carries(3)))))
      if (withPairs) hiPort.payload := Mux(isTwoPass, rdHiHold, defaultHi)

    }


    // ---- XLEN=64 datapath ---------------------------------------------------
    // One register, two 32-bit words. The adder, its guard scheme and every
    // read-out are the same as for RV32. What changes is where word 1's operands come
    // from (rs1/rs2 bits 63:32 instead of a register pair) and that both words
    // retire together in one 64-bit write.
    val prep64 = rv64 generate new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt
      val rds = up(el(IntRegFile, RDS)).asUInt // pabdsumau.b only
      val sub = this (DO_SUB)
      val ew = this (ELEM_W)
      val scalar = this (IS_SCALAR)
      val half = ew === EW.H
      val word = ew === EW.W
      val cross = this (IS_CROSS)
      val shift1 = this (IS_SH1)

      val rs1Lo = rs1(31 downto 0)
      val rs1Hi = rs1(63 downto 32)

      // Scalar-operand forms broadcast the LOW element of rs2 over every lane.
      // The broadcast pattern is the same in both words, so it is built once at
      // word width and fed to both.
      val rs2Bcast = Mux(word,
        rs2(31 downto 0),
        Mux(half,
          (rs2(15 downto 0) ## rs2(15 downto 0)).asUInt,
          (rs2(7 downto 0) ## rs2(7 downto 0) ##
            rs2(7 downto 0) ## rs2(7 downto 0)).asUInt))
      val rs2Lo = Mux(scalar, rs2Bcast, rs2(31 downto 0))
      val rs2Hi = Mux(scalar, rs2Bcast, rs2(63 downto 32))

      // Two cross granularities exist at XLEN=64 and they are mutually
      // exclusive by element width:
      //   .hx (ew = H) swaps the halfwords INSIDE each 32-bit word, twice over
      //   .wx (ew = W) swaps the two 32-bit words of the register
      // RV32 only ever has the first, which is why its `cross` needs no such
      // split.
      val wordCross = cross && word
      val halfCross = cross && !word

      def crossSwap(x: UInt): UInt =
        Mux(halfCross, (x(15 downto 0) ## x(31 downto 16)).asUInt, x)

      // Subtract polarity per lane group: `Lo`/`Hi` are the even/odd halfword
      // of a word, `0`/`1` the low/high word
      val subLo0 = sub
      val subHi0 = sub ^ halfCross
      val subLo1 = sub ^ wordCross
      val subHi1 = sub ^ wordCross ^ halfCross

      // a - b = a + ~b + 1, applied per halfword so a cross form can complement
      // one halfword and not the other.
      def crossComplement(x: UInt, subLoW: Bool, subHiW: Bool): UInt =
        (Mux(subHiW, ~x(31 downto 16), x(31 downto 16)) ##
          Mux(subLoW, ~x(15 downto 0), x(15 downto 0))).asUInt

      // Plain <<1 per lane (psh1add.h/.w). One 32-bit lane per word, or two
      // 16-bit lanes inside it.
      def sh1Src(x: UInt): UInt = Mux(word,
        (x(30 downto 0) ## B(0, 1 bit)).asUInt,
        ((x(30 downto 16) ## B(0, 1 bit)) ## (x(14 downto 0) ## B(0, 1 bit))).asUInt)

      def aChain(x: UInt): (UInt, Bool) = {
        val (sat, satFlag) = RvpSaturatingAddOps.signedSatShift1(x.asBits, ew)
        (Mux(this (ALU_KIND) === SSH1, sat.asUInt, Mux(shift1, sh1Src(x), x)), satFlag)
      }

      val (aSrc, satSh1) = aChain(rs1Lo)
      val (aSrcHi, satSh1Hi) = aChain(rs1Hi)

      val bEff = crossSwap(Mux(wordCross, rs2Hi, rs2Lo))
      val bEffHi = crossSwap(Mux(wordCross, rs2Lo, rs2Hi))

      // Same 72-bit segmented adder and the same guard scheme as RV32
      def prop(k: Int): Bool = {
        if (k == 3) False else if (k == 1 || k == 5) word else half || word
      }

      // Guard k feeds byte k+1, so it belongs to the lane group that byte is
      // in: 0 -> word 0 even halfword, 1/2 -> word 0 odd, 3/4 -> word 1 even,
      // 5/6 -> word 1 odd
      def injectSub(k: Int): Bool = k match {
        case 0 => subLo0
        case 1 | 2 => subHi0
        case 3 | 4 => subLo1
        case _ => subHi1
      }

      def aGuard(k: Int): Bits = (prop(k) || injectSub(k)).asBits

      def bGuard(k: Int): Bits = (!prop(k) && injectSub(k)).asBits

      val aData64 = (aSrcHi ## aSrc).asBits
      val bData64 = (crossComplement(bEffHi, subLo1, subHi1) ##
        crossComplement(bEff, subLo0, subHi0)).asBits

      val aWide = (B(0, 1 bit) ##
        aData64(63 downto 56) ## aGuard(6) ##
        aData64(55 downto 48) ## aGuard(5) ##
        aData64(47 downto 40) ## aGuard(4) ##
        aData64(39 downto 32) ## aGuard(3) ##
        aData64(31 downto 24) ## aGuard(2) ##
        aData64(23 downto 16) ## aGuard(1) ##
        aData64(15 downto 8) ## aGuard(0) ##
        aData64(7 downto 0)).asUInt

      val bWide = (B(0, 1 bit) ##
        bData64(63 downto 56) ## bGuard(6) ##
        bData64(55 downto 48) ## bGuard(5) ##
        bData64(47 downto 40) ## bGuard(4) ##
        bData64(39 downto 32) ## bGuard(3) ##
        bData64(31 downto 24) ## bGuard(2) ##
        bData64(23 downto 16) ## bGuard(1) ##
        bData64(15 downto 8) ## bGuard(0) ##
        bData64(7 downto 0)).asUInt

      // Lane 0 sits in word 0's even halfword, so it takes subLo0 as carry-in.
      val sumWide72 = aWide + bWide + subLo0.asUInt

      def dropGuards(x: UInt, base: Int): Bits =
        x(base + 34 downto base + 27).asBits ## x(base + 25 downto base + 18).asBits ##
          x(base + 16 downto base + 9).asBits ## x(base + 7 downto base).asBits

      this (SUMWIDE) := sumWide72(35 downto 0)
      this (SUMWIDE_HI) := sumWide72(71 downto 36)
      this (RD) := dropGuards(sumWide72, 0)
      this (RD_HI) := dropGuards(sumWide72, 36)
      this (ASRC) := aSrc
      this (ASRC_HI) := aSrcHi
      this (BEFF) := bEff
      this (BEFF_HI) := bEffHi
      this (SAT_SH1) := satSh1
      this (SAT_SH1_HI) := satSh1Hi
      this (RS1V) := rs1Lo
      this (RS1V_HI) := rs1Hi
      this (RS2V) := rs2(31 downto 0)
      this (RS2V_HI) := rs2(63 downto 32)
      val reduceSeed = this (ALU_KIND) === REDSUMS || this (ALU_KIND) === REDSUMU
      this (ACCBASE) := Mux(this (DO_ACCUMULATE), rds,
        Mux(reduceSeed, rs2, U(0, xlen bits)))

      // Registered read-out controls, identical to RV32 as they keep ALU_KIND
      // comparisons out of the post stage's mux trees.
      this (RO_SIGNED) := this (ALU_KIND).mux(
        default -> False,
        SLT -> True, MIN -> True, MAX -> True, AVGS -> True, ABSD -> True,
        REDSUMS -> True, SADD -> True, SSUB -> True, SCROSS -> True, SSH1 -> True
      )
      this (RO_MAX) := this (ALU_KIND).mux(default -> False, MAX -> True, MAXU -> True)
      this (RO_EQ) := this (ALU_KIND) === CMPEQ
      this (RO_SSH1) := this (ALU_KIND) === SSH1
      this (RO_UNSIGNED) := this (ALU_KIND) === USATIW
      // WIDEN is the one read-out with no RV64 member at all as every widening op  is a pair op.
      this (RO) := RvpReadout.ADDSUB
      switch(this (ALU_KIND)) {
        is(SLTU, SLT, CMPEQ) {
          this (RO) := RvpReadout.CMP
        }
        is(MINU, MIN, MAXU, MAX) {
          this (RO) := RvpReadout.MINMAX
        }
        is(AVGU, AVGS) {
          this (RO) := RvpReadout.AVG
        }
        is(ABSD, ABSDU) {
          this (RO) := RvpReadout.ABSD
        }
        is(ABSDSUMU) {
          this (RO) := RvpReadout.ABSDSUMU
        }
        is(REDSUMS, REDSUMU) {
          this (RO) := RvpReadout.REDSUM
        }
        is(SADD, SADDU, SSUB, SSUBU, SCROSS, SSH1) {
          this (RO) := RvpReadout.SATAS
        }
        is(SABS) {
          this (RO) := RvpReadout.SABS
        }
        is(SATIW, USATIW) {
          this (RO) := RvpReadout.SATI
        }
      }
    }

    val post64 = rv64 generate new el.Execute(id = postAt) {
      val ew = this (ELEM_W)
      val sub = this (DO_SUB)
      val cross = this (IS_CROSS)
      val word = ew === EW.W
      val wordCross = cross && word
      val halfCross = cross && !word
      val subLo0 = sub
      val subHi0 = sub ^ halfCross
      val subLo1 = sub ^ wordCross
      val subHi1 = sub ^ wordCross ^ halfCross

      val ro = this (RO)
      val roSigned = this (RO_SIGNED)
      val roMax = this (RO_MAX)
      val roEq = this (RO_EQ)
      val roSsh1 = this (RO_SSH1)
      val roUnsigned = this (RO_UNSIGNED)

      // Saturate-to-width bounds. The generator is 64 bits wide because
      // the scalar sati/usati take a uimm6; each word's lanes read its low half,
      // which holds the same pattern for every n they can name (RvpSatWidthOps).
      val satN = Mux(this (SAT_XLEN), Decode.UOP(25 downto 20),
        Mux(word, B"0" ## Decode.UOP(24 downto 20),
          B"00" ## Decode.UOP(23 downto 20)))
      val satBounds = RvpSatWidthOps.satBounds(satN.asUInt, 64)

      /** Every read-out of one 32-bit word of the adder.
       *
       * This is the same network the RV32 post stage builds. There it is
       * elaborated once and time-multiplexed over the register pair by
       * RvpDoubleWidePass, at the price of a frozen cycle. A 64-bit result has
       * to retire in one write, so both words are elaborated here and every
       * RV64 op keeps the single-cycle latency of its RV32 counterpart.
       */
      class WordNet(sumWide: UInt, rdW: Bits, rs1W: UInt, bW: UInt, aW: Bits,
                    satSh1W: Bool, subLoW: Bool, subHiW: Bool) extends Area {
        // Normalise each lane boundary once where an add keeps the carry-out, a
        // subtract inverts it into a borrow. j = 0,1 are the even halfword's
        // byte boundaries, j = 2,3 the odd halfword's.
        val carries = Seq(sumWide(8), sumWide(17), sumWide(26), sumWide(35))
          .zipWithIndex.map { case (carry, j) => carry ^ (if (j < 2) subLoW else subHiW) }

        val (rdSatAS, satAS) = RvpSaturatingAddOps.satAddSub(
          rdW, aW, bW.asBits, carries, ew, roSigned, subLoW, subHiW)

        val predicates = RvpPackedLanes.lanePredicates(carries, rs1W, bW, ew, roSigned)
        val predicatesW = RvpPackedLanes.wordFanout(predicates, word)
        val equalities = RvpCompareOps.equalityLanes(rdW, ew)
        val cmpResult = RvpCompareOps.mask((equalities zip predicatesW).map {
          case (eq, lt) => roEq ? eq | lt
        })
        val minMaxResult = RvpMinMaxOps.select(predicatesW, rs1W, bW, roMax)
        val avgResult = RvpAverageOps.average(rdW, predicates, ew)
        val absdResult = RvpAbsDiffOps.absDiff(rdW, ew === EW.H, predicates)
        val (satAbsResult, satAbs) = RvpSaturatingAddOps.signedSatAbs(rs1W.asBits, ew)
        val (satImmResult, satImm) = RvpSatWidthOps.clampToBounds(
          rs1W.asBits, satBounds, ew, roUnsigned)
        // Byte lanes only (pabdsumu.b / pabdsumau.b), so the unsigned lane
        // predicate is the normalised carry.
        val absDiffSum = RvpAbsDiffOps.absDiffSumUnsigned(rdW, carries)

        val saturated = satAS | (roSsh1 && satSh1W)

        val result = ro.mux(
          default -> rdW,
          RvpReadout.CMP -> cmpResult,
          RvpReadout.MINMAX -> minMaxResult,
          RvpReadout.AVG -> avgResult,
          RvpReadout.ABSD -> absdResult,
          RvpReadout.SATAS -> rdSatAS,
          RvpReadout.SABS -> satAbsResult,
          RvpReadout.SATI -> satImmResult
        )
      }

      val lo = new WordNet(this (SUMWIDE), this (RD), this (RS1V), this (BEFF),
        this (ASRC).asBits, this (SAT_SH1), subLo0, subHi0)
      val hi = new WordNet(this (SUMWIDE_HI), this (RD_HI), this (RS1V_HI), this (BEFF_HI),
        this (ASRC_HI).asBits, this (SAT_SH1_HI), subLo1, subHi1)

      // The only two read-outs that cross the word boundary: predsum.*s
      // reduces the whole register into an XLEN-wide accumulator seeded by rs2,
      // and pabdsum*.b sums eight byte differences (up to 2040, 11 bits).
      val reduceSum = RvpReduceOps.reduceWideSum(
        this (RS1V_HI) @@ this (RS1V), ew, roSigned)
      this (REDUCE_SUM) := reduceSum
      this (ABS_DIFF_SUM) := lo.absDiffSum +^ hi.absDiffSum

      // sati / usati clamp the whole register instead of the two word lanes --
      // the one XLEN=64 read-out that is not simply "the RV32 network, twice".
      val (satFullResult, satFull) = RvpSatWidthOps.clampFull(
        (this (RS1V_HI) @@ this (RS1V)).asBits, satBounds, roUnsigned)

      val packedResult = hi.result ## lo.result
      this (ALU_RESULT) := ro.mux(
        default -> packedResult,
        RvpReadout.REDSUM -> reduceSum,
        RvpReadout.SATI -> Mux(this (SAT_XLEN), satFullResult, packedResult)
      )

      this (SATURATED) := SEL && ro.mux(
        default -> False,
        RvpReadout.SATAS -> (lo.saturated | hi.saturated),
        RvpReadout.SABS -> (lo.satAbs | hi.satAbs),
        RvpReadout.SATI -> Mux(this (SAT_XLEN), satFull, lo.satImm | hi.satImm)
      )
    }

    val format = new el.Execute(id = formatAt) {
      val reduce = this (RO) === RvpReadout.REDSUM
      val delta = Mux(reduce, this (REDUCE_SUM).asUInt,
        this (ABS_DIFF_SUM).resize(xlen bits))
      val accumulatedResult = (this (ACCBASE) + delta).asBits
      val result = this (RO).mux(
        default -> this (ALU_RESULT),
        RvpReadout.ABSDSUMU -> accumulatedResult,
        RvpReadout.REDSUM -> accumulatedResult)

      writeBack.valid := SEL
      writeBack.payload := result
    }


    val vxSatCommit = new el.Execute(id = intWritebackAt) {
      vxsatPort.pulse := this (SATURATED)
    }
  }
}
