// SPDX-FileCopyrightText: 2026 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute.rvp

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import spinal.lib._
import vexiiriscv.decode.DecoderService
import vexiiriscv.execute.rvp.RvpMulPlugin.Rv64
import vexiiriscv.execute.rvp.RvpPairWritePlugin.{PAIR_RD, PAIR_RD_SRC}
import vexiiriscv.execute.rvp.kinds.RvpMulNet._
import vexiiriscv.execute.rvp.kinds.RvpMulRes._
import vexiiriscv.execute.rvp.kinds.{RvpMulNet, RvpMulRes}
import vexiiriscv.execute.rvp.ops.RvpMulOps
import vexiiriscv.execute.{ExecutionUnitElementSimple, LaneLayer}
import vexiiriscv.riscv._

import scala.language.postfixOps


object RvpMulPlugin {
  val MUL_H00 = IntRegFile.TypeR(M"1000001----------011-----0111011")
  val MUL_H01 = IntRegFile.TypeR(M"1001001----------001-----0111011")
  val MUL_H11 = IntRegFile.TypeR(M"1001001----------011-----0111011")
  val MULSU_H00 = IntRegFile.TypeR(M"1110001----------011-----0111011")
  val MULSU_H11 = IntRegFile.TypeR(M"1111001----------011-----0111011")
  val MULU_H00 = IntRegFile.TypeR(M"1010001----------011-----0111011")
  val MULU_H01 = IntRegFile.TypeR(M"1011001----------001-----0111011")
  val MULU_H11 = IntRegFile.TypeR(M"1011001----------011-----0111011")
  val PMUL_H_B00 = IntRegFile.TypeR(M"1000000----------011-----0111011")
  val PMUL_H_B01 = IntRegFile.TypeR(M"1001000----------001-----0111011")
  val PMUL_H_B11 = IntRegFile.TypeR(M"1001000----------011-----0111011")
  val PMULSU_H_B00 = IntRegFile.TypeR(M"1110000----------011-----0111011")
  val PMULSU_H_B11 = IntRegFile.TypeR(M"1111000----------011-----0111011")
  val PMULU_H_B00 = IntRegFile.TypeR(M"1010000----------011-----0111011")
  val PMULU_H_B01 = IntRegFile.TypeR(M"1011000----------001-----0111011")
  val PMULU_H_B11 = IntRegFile.TypeR(M"1011000----------011-----0111011")
  val MULH_H0 = IntRegFile.TypeR(M"1010001----------111-----0111011")
  val MULH_H1 = IntRegFile.TypeR(M"1011001----------111-----0111011")
  val MULHSU_H0 = IntRegFile.TypeR(M"1010011----------111-----0111011")
  val MULHSU_H1 = IntRegFile.TypeR(M"1011011----------111-----0111011")
  val PMULH_H_B0 = IntRegFile.TypeR(M"1010000----------111-----0111011")
  val PMULH_H_B1 = IntRegFile.TypeR(M"1011000----------111-----0111011")
  val PMULHSU_H_B0 = IntRegFile.TypeR(M"1010010----------111-----0111011")
  val PMULHSU_H_B1 = IntRegFile.TypeR(M"1011010----------111-----0111011")
  val MULHR = IntRegFile.TypeR(M"1000011----------111-----0111011")
  val MULHRSU = IntRegFile.TypeR(M"1100011----------111-----0111011")
  val MULHRU = IntRegFile.TypeR(M"1001011----------111-----0111011")
  val PMULH_H = IntRegFile.TypeR(M"1000000----------111-----0111011")
  val PMULHR_H = IntRegFile.TypeR(M"1000010----------111-----0111011")
  val PMULHRSU_H = IntRegFile.TypeR(M"1100010----------111-----0111011")
  val PMULHRU_H = IntRegFile.TypeR(M"1001010----------111-----0111011")
  val PMULHSU_H = IntRegFile.TypeR(M"1100000----------111-----0111011")
  val PMULHU_H = IntRegFile.TypeR(M"1001000----------111-----0111011")
  val PM2ADD_H = IntRegFile.TypeR(M"1000000----------101-----0111011")
  val PM2ADD_HX = IntRegFile.TypeR(M"1001000----------101-----0111011")
  val PM2ADDSU_H = IntRegFile.TypeR(M"1110000----------101-----0111011")
  val PM2ADDU_H = IntRegFile.TypeR(M"1010000----------101-----0111011")
  val PM2SUB_H = IntRegFile.TypeR(M"1100000----------101-----0111011")
  val PM2SUB_HX = IntRegFile.TypeR(M"1101000----------101-----0111011")
  val PM4ADD_B = IntRegFile.TypeR(M"1000010----------101-----0111011")
  val PM4ADDSU_B = IntRegFile.TypeR(M"1110010----------101-----0111011")
  val PM4ADDU_B = IntRegFile.TypeR(M"1010010----------101-----0111011")
  val PMQ2ADD_H = IntRegFile.TypeR(M"1011000----------101-----0111011")
  val PMQR2ADD_H = IntRegFile.TypeR(M"1011010----------101-----0111011")
  val MACC_H00 = IntRegFile.TypeR_RDS(M"1000101----------011-----0111011")
  val MACC_H01 = IntRegFile.TypeR_RDS(M"1001101----------001-----0111011")
  val MACC_H11 = IntRegFile.TypeR_RDS(M"1001101----------011-----0111011")
  val MACCSU_H00 = IntRegFile.TypeR_RDS(M"1110101----------011-----0111011")
  val MACCSU_H11 = IntRegFile.TypeR_RDS(M"1111101----------011-----0111011")
  val MACCU_H00 = IntRegFile.TypeR_RDS(M"1010101----------011-----0111011")
  val MACCU_H01 = IntRegFile.TypeR_RDS(M"1011101----------001-----0111011")
  val MACCU_H11 = IntRegFile.TypeR_RDS(M"1011101----------011-----0111011")
  val PMHACC_H = IntRegFile.TypeR_RDS(M"1000100----------111-----0111011")
  val MHACC_H0 = IntRegFile.TypeR_RDS(M"1010101----------111-----0111011")
  val MHACC_H1 = IntRegFile.TypeR_RDS(M"1011101----------111-----0111011")
  val MHACCSU_H0 = IntRegFile.TypeR_RDS(M"1010111----------111-----0111011")
  val MHACCSU_H1 = IntRegFile.TypeR_RDS(M"1011111----------111-----0111011")
  val PMHACC_H_B0 = IntRegFile.TypeR_RDS(M"1010100----------111-----0111011")
  val PMHACC_H_B1 = IntRegFile.TypeR_RDS(M"1011100----------111-----0111011")
  val PMHACCSU_H_B0 = IntRegFile.TypeR_RDS(M"1010110----------111-----0111011")
  val PMHACCSU_H_B1 = IntRegFile.TypeR_RDS(M"1011110----------111-----0111011")
  val MHACC = IntRegFile.TypeR_RDS(M"1000101----------111-----0111011")
  val MHACCSU = IntRegFile.TypeR_RDS(M"1100101----------111-----0111011")
  val MHACCU = IntRegFile.TypeR_RDS(M"1001101----------111-----0111011")
  val MHRACC = IntRegFile.TypeR_RDS(M"1000111----------111-----0111011")
  val MHRACCSU = IntRegFile.TypeR_RDS(M"1100111----------111-----0111011")
  val MHRACCU = IntRegFile.TypeR_RDS(M"1001111----------111-----0111011")
  val PMHACCSU_H = IntRegFile.TypeR_RDS(M"1100100----------111-----0111011")
  val PMHACCU_H = IntRegFile.TypeR_RDS(M"1001100----------111-----0111011")
  val PMHRACC_H = IntRegFile.TypeR_RDS(M"1000110----------111-----0111011")
  val PMHRACCSU_H = IntRegFile.TypeR_RDS(M"1100110----------111-----0111011")
  val PMHRACCU_H = IntRegFile.TypeR_RDS(M"1001110----------111-----0111011")
  val PM2ADDA_H = IntRegFile.TypeR_RDS(M"1000100----------101-----0111011")
  val PM2ADDA_HX = IntRegFile.TypeR_RDS(M"1001100----------101-----0111011")
  val PM2ADDASU_H = IntRegFile.TypeR_RDS(M"1110100----------101-----0111011")
  val PM2ADDAU_H = IntRegFile.TypeR_RDS(M"1010100----------101-----0111011")
  val PM2SUBA_H = IntRegFile.TypeR_RDS(M"1100100----------101-----0111011")
  val PM2SUBA_HX = IntRegFile.TypeR_RDS(M"1101100----------101-----0111011")
  val PM4ADDA_B = IntRegFile.TypeR_RDS(M"1000110----------101-----0111011")
  val PM4ADDASU_B = IntRegFile.TypeR_RDS(M"1110110----------101-----0111011")
  val PM4ADDAU_B = IntRegFile.TypeR_RDS(M"1010110----------101-----0111011")
  val PMQ2ADDA_H = IntRegFile.TypeR_RDS(M"1011100----------101-----0111011")
  val PMQR2ADDA_H = IntRegFile.TypeR_RDS(M"1011110----------101-----0111011")
  val MQACC_H00 = IntRegFile.TypeR_RDS(M"1110100----------111-----0111011")
  val MQACC_H01 = IntRegFile.TypeR_RDS(M"1111100----------101-----0111011")
  val MQACC_H11 = IntRegFile.TypeR_RDS(M"1111100----------111-----0111011")
  val MQRACC_H00 = IntRegFile.TypeR_RDS(M"1110110----------111-----0111011")
  val MQRACC_H01 = IntRegFile.TypeR_RDS(M"1111110----------101-----0111011")
  val MQRACC_H11 = IntRegFile.TypeR_RDS(M"1111110----------111-----0111011")
  val MULQ = IntRegFile.TypeR(M"1101001----------111-----0111011")
  val MULQR = IntRegFile.TypeR(M"1101011----------111-----0111011")
  val PMULQ_H = IntRegFile.TypeR(M"1101000----------111-----0111011")
  val PMULQR_H = IntRegFile.TypeR(M"1101010----------111-----0111011")
  val PM2SADD_H = IntRegFile.TypeR(M"1100010----------101-----0111011")
  val PM2SADD_HX = IntRegFile.TypeR(M"1101010----------101-----0111011")
  val WMUL = IntRegFile.TypeR(M"0010001----------010----10011011")
  val WMULU = IntRegFile.TypeR(M"0011001----------010----10011011")
  val WMULSU = IntRegFile.TypeR(M"0110001----------010----10011011")
  val PWMUL_H = IntRegFile.TypeR(M"0010000----------010----10011011")
  val PWMUL_B = IntRegFile.TypeR(M"0010010----------010----10011011")
  val PWMULU_H = IntRegFile.TypeR(M"0011000----------010----10011011")
  val PWMULU_B = IntRegFile.TypeR(M"0011010----------010----10011011")
  val PWMULSU_H = IntRegFile.TypeR(M"0110000----------010----10011011")
  val PWMULSU_B = IntRegFile.TypeR(M"0110010----------010----10011011")
  val PM2WADD_H = IntRegFile.TypeR(M"0000011----------010----10011011")
  val PM2WADD_HX = IntRegFile.TypeR(M"0001011----------010----10011011")
  val PM2WADDU_H = IntRegFile.TypeR(M"0010011----------010----10011011")
  val PM2WADDSU_H = IntRegFile.TypeR(M"0110011----------010----10011011")
  val PM2WSUB_H = IntRegFile.TypeR(M"0100011----------010----10011011")
  val PM2WSUB_HX = IntRegFile.TypeR(M"0101011----------010----10011011")
  val PWMACC_H = IntRegFile.TypeR_RDS_RS3(M"0010100----------010----10011011")
  val WMACC = IntRegFile.TypeR_RDS_RS3(M"0010101----------010----10011011")
  val PWMACCSU_H = IntRegFile.TypeR_RDS_RS3(M"0110100----------010----10011011")
  val WMACCSU = IntRegFile.TypeR_RDS_RS3(M"0110101----------010----10011011")
  val PWMACCU_H = IntRegFile.TypeR_RDS_RS3(M"0011100----------010----10011011")
  val WMACCU = IntRegFile.TypeR_RDS_RS3(M"0011101----------010----10011011")
  val PM2WADDA_H = IntRegFile.TypeR_RDS_RS3(M"0000111----------010----10011011")
  val PM2WADDASU_H = IntRegFile.TypeR_RDS_RS3(M"0110111----------010----10011011")
  val PM2WADDAU_H = IntRegFile.TypeR_RDS_RS3(M"0010111----------010----10011011")
  val PM2WADDA_HX = IntRegFile.TypeR_RDS_RS3(M"0001111----------010----10011011")
  val PM2WSUBA_H = IntRegFile.TypeR_RDS_RS3(M"0100111----------010----10011011")
  val PM2WSUBA_HX = IntRegFile.TypeR_RDS_RS3(M"0101111----------010----10011011")
  val PMQWACC_H = IntRegFile.TypeR_RDS_RS3(M"0111100----------010----10011011")
  val MQWACC = IntRegFile.TypeR_RDS_RS3(M"0111101----------010----10011011")
  val PMQRWACC_H = IntRegFile.TypeR_RDS_RS3(M"0111110----------010----10011011")
  val MQRWACC = IntRegFile.TypeR_RDS_RS3(M"0111111----------010----10011011")

  /** The 142 RV64 mnemonics of the `mul` family
   */
  object Rv64 {
    val MUL_W00 = IntRegFile.TypeR(M"1000011----------011-----0111011") // mul.w00 (RV64-only)
    val MUL_W01 = IntRegFile.TypeR(M"1001011----------001-----0111011") // mul.w01 (RV64-only)
    val MUL_W11 = IntRegFile.TypeR(M"1001011----------011-----0111011") // mul.w11 (RV64-only)
    val MULSU_W00 = IntRegFile.TypeR(M"1110011----------011-----0111011") // mulsu.w00 (RV64-only)
    val MULSU_W11 = IntRegFile.TypeR(M"1111011----------011-----0111011") // mulsu.w11 (RV64-only)
    val MULU_W00 = IntRegFile.TypeR(M"1010011----------011-----0111011") // mulu.w00 (RV64-only)
    val MULU_W01 = IntRegFile.TypeR(M"1011011----------001-----0111011") // mulu.w01 (RV64-only)
    val MULU_W11 = IntRegFile.TypeR(M"1011011----------011-----0111011") // mulu.w11 (RV64-only)
    val PM2ADD_H = IntRegFile.TypeR(M"1000000----------101-----0111011") // pm2add.h
    val PM2ADD_HX = IntRegFile.TypeR(M"1001000----------101-----0111011") // pm2add.hx
    val PM2ADD_W = IntRegFile.TypeR(M"1000001----------101-----0111011") // pm2add.w (RV64-only)
    val PM2ADD_WX = IntRegFile.TypeR(M"1001001----------101-----0111011") // pm2add.wx (RV64-only)
    val PM2ADDSU_H = IntRegFile.TypeR(M"1110000----------101-----0111011") // pm2addsu.h
    val PM2ADDSU_W = IntRegFile.TypeR(M"1110001----------101-----0111011") // pm2addsu.w (RV64-only)
    val PM2ADDU_H = IntRegFile.TypeR(M"1010000----------101-----0111011") // pm2addu.h
    val PM2ADDU_W = IntRegFile.TypeR(M"1010001----------101-----0111011") // pm2addu.w (RV64-only)
    val PM2SADD_H = IntRegFile.TypeR(M"1100010----------101-----0111011") // pm2sadd.h
    val PM2SADD_HX = IntRegFile.TypeR(M"1101010----------101-----0111011") // pm2sadd.hx
    val PM2SUB_H = IntRegFile.TypeR(M"1100000----------101-----0111011") // pm2sub.h
    val PM2SUB_HX = IntRegFile.TypeR(M"1101000----------101-----0111011") // pm2sub.hx
    val PM2SUB_W = IntRegFile.TypeR(M"1100001----------101-----0111011") // pm2sub.w (RV64-only)
    val PM2SUB_WX = IntRegFile.TypeR(M"1101001----------101-----0111011") // pm2sub.wx (RV64-only)
    val PM4ADD_B = IntRegFile.TypeR(M"1000010----------101-----0111011") // pm4add.b
    val PM4ADD_H = IntRegFile.TypeR(M"1000011----------101-----0111011") // pm4add.h (RV64-only)
    val PM4ADDSU_B = IntRegFile.TypeR(M"1110010----------101-----0111011") // pm4addsu.b
    val PM4ADDSU_H = IntRegFile.TypeR(M"1110011----------101-----0111011") // pm4addsu.h (RV64-only)
    val PM4ADDU_B = IntRegFile.TypeR(M"1010010----------101-----0111011") // pm4addu.b
    val PM4ADDU_H = IntRegFile.TypeR(M"1010011----------101-----0111011") // pm4addu.h (RV64-only)
    val PMQ2ADD_H = IntRegFile.TypeR(M"1011000----------101-----0111011") // pmq2add.h
    val PMQ2ADD_W = IntRegFile.TypeR(M"1011001----------101-----0111011") // pmq2add.w (RV64-only)
    val PMQR2ADD_H = IntRegFile.TypeR(M"1011010----------101-----0111011") // pmqr2add.h
    val PMQR2ADD_W = IntRegFile.TypeR(M"1011011----------101-----0111011") // pmqr2add.w (RV64-only)
    val PMUL_H_B00 = IntRegFile.TypeR(M"1000000----------011-----0111011") // pmul.h.b00
    val PMUL_H_B01 = IntRegFile.TypeR(M"1001000----------001-----0111011") // pmul.h.b01
    val PMUL_H_B11 = IntRegFile.TypeR(M"1001000----------011-----0111011") // pmul.h.b11
    val PMUL_W_H00 = IntRegFile.TypeR(M"1000001----------011-----0111011") // pmul.w.h00 (recycled: RV32 mul.h00)
    val PMUL_W_H01 = IntRegFile.TypeR(M"1001001----------001-----0111011") // pmul.w.h01 (recycled: RV32 mul.h01)
    val PMUL_W_H11 = IntRegFile.TypeR(M"1001001----------011-----0111011") // pmul.w.h11 (recycled: RV32 mul.h11)
    val PMULH_H = IntRegFile.TypeR(M"1000000----------111-----0111011") // pmulh.h
    val PMULH_H_B0 = IntRegFile.TypeR(M"1010000----------111-----0111011") // pmulh.h.b0
    val PMULH_H_B1 = IntRegFile.TypeR(M"1011000----------111-----0111011") // pmulh.h.b1
    val PMULH_W = IntRegFile.TypeR(M"1000001----------111-----0111011") // pmulh.w (RV64-only)
    val PMULH_W_H0 = IntRegFile.TypeR(M"1010001----------111-----0111011") // pmulh.w.h0 (recycled: RV32 mulh.h0)
    val PMULH_W_H1 = IntRegFile.TypeR(M"1011001----------111-----0111011") // pmulh.w.h1 (recycled: RV32 mulh.h1)
    val PMULHR_H = IntRegFile.TypeR(M"1000010----------111-----0111011") // pmulhr.h
    val PMULHR_W = IntRegFile.TypeR(M"1000011----------111-----0111011") // pmulhr.w (recycled: RV32 mulhr)
    val PMULHRSU_H = IntRegFile.TypeR(M"1100010----------111-----0111011") // pmulhrsu.h
    val PMULHRSU_W = IntRegFile.TypeR(M"1100011----------111-----0111011") // pmulhrsu.w (recycled: RV32 mulhrsu)
    val PMULHRU_H = IntRegFile.TypeR(M"1001010----------111-----0111011") // pmulhru.h
    val PMULHRU_W = IntRegFile.TypeR(M"1001011----------111-----0111011") // pmulhru.w (recycled: RV32 mulhru)
    val PMULHSU_H = IntRegFile.TypeR(M"1100000----------111-----0111011") // pmulhsu.h
    val PMULHSU_H_B0 = IntRegFile.TypeR(M"1010010----------111-----0111011") // pmulhsu.h.b0
    val PMULHSU_H_B1 = IntRegFile.TypeR(M"1011010----------111-----0111011") // pmulhsu.h.b1
    val PMULHSU_W = IntRegFile.TypeR(M"1100001----------111-----0111011") // pmulhsu.w (RV64-only)
    val PMULHSU_W_H0 = IntRegFile.TypeR(M"1010011----------111-----0111011") // pmulhsu.w.h0 (recycled: RV32 mulhsu.h0)
    val PMULHSU_W_H1 = IntRegFile.TypeR(M"1011011----------111-----0111011") // pmulhsu.w.h1 (recycled: RV32 mulhsu.h1)
    val PMULHU_H = IntRegFile.TypeR(M"1001000----------111-----0111011") // pmulhu.h
    val PMULHU_W = IntRegFile.TypeR(M"1001001----------111-----0111011") // pmulhu.w (RV64-only)
    val PMULQ_H = IntRegFile.TypeR(M"1101000----------111-----0111011") // pmulq.h
    val PMULQ_W = IntRegFile.TypeR(M"1101001----------111-----0111011") // pmulq.w (recycled: RV32 mulq)
    val PMULQR_H = IntRegFile.TypeR(M"1101010----------111-----0111011") // pmulqr.h
    val PMULQR_W = IntRegFile.TypeR(M"1101011----------111-----0111011") // pmulqr.w (recycled: RV32 mulqr)
    val PMULSU_H_B00 = IntRegFile.TypeR(M"1110000----------011-----0111011") // pmulsu.h.b00
    val PMULSU_H_B11 = IntRegFile.TypeR(M"1111000----------011-----0111011") // pmulsu.h.b11
    val PMULSU_W_H00 = IntRegFile.TypeR(M"1110001----------011-----0111011") // pmulsu.w.h00 (recycled: RV32 mulsu.h00)
    val PMULSU_W_H11 = IntRegFile.TypeR(M"1111001----------011-----0111011") // pmulsu.w.h11 (recycled: RV32 mulsu.h11)
    val PMULU_H_B00 = IntRegFile.TypeR(M"1010000----------011-----0111011") // pmulu.h.b00
    val PMULU_H_B01 = IntRegFile.TypeR(M"1011000----------001-----0111011") // pmulu.h.b01
    val PMULU_H_B11 = IntRegFile.TypeR(M"1011000----------011-----0111011") // pmulu.h.b11
    val PMULU_W_H00 = IntRegFile.TypeR(M"1010001----------011-----0111011") // pmulu.w.h00 (recycled: RV32 mulu.h00)
    val PMULU_W_H01 = IntRegFile.TypeR(M"1011001----------001-----0111011") // pmulu.w.h01 (recycled: RV32 mulu.h01)
    val PMULU_W_H11 = IntRegFile.TypeR(M"1011001----------011-----0111011") // pmulu.w.h11 (recycled: RV32 mulu.h11)
    val MACC_W00 = IntRegFile.TypeR_RDS(M"1000111----------011-----0111011") // macc.w00 (RV64-only)
    val MACC_W01 = IntRegFile.TypeR_RDS(M"1001111----------001-----0111011") // macc.w01 (RV64-only)
    val MACC_W11 = IntRegFile.TypeR_RDS(M"1001111----------011-----0111011") // macc.w11 (RV64-only)
    val MACCSU_W00 = IntRegFile.TypeR_RDS(M"1110111----------011-----0111011") // maccsu.w00 (RV64-only)
    val MACCSU_W11 = IntRegFile.TypeR_RDS(M"1111111----------011-----0111011") // maccsu.w11 (RV64-only)
    val MACCU_W00 = IntRegFile.TypeR_RDS(M"1010111----------011-----0111011") // maccu.w00 (RV64-only)
    val MACCU_W01 = IntRegFile.TypeR_RDS(M"1011111----------001-----0111011") // maccu.w01 (RV64-only)
    val MACCU_W11 = IntRegFile.TypeR_RDS(M"1011111----------011-----0111011") // maccu.w11 (RV64-only)
    val MQACC_W00 = IntRegFile.TypeR_RDS(M"1110101----------111-----0111011") // mqacc.w00 (RV64-only)
    val MQACC_W01 = IntRegFile.TypeR_RDS(M"1111101----------101-----0111011") // mqacc.w01 (RV64-only)
    val MQACC_W11 = IntRegFile.TypeR_RDS(M"1111101----------111-----0111011") // mqacc.w11 (RV64-only)
    val MQRACC_W00 = IntRegFile.TypeR_RDS(M"1110111----------111-----0111011") // mqracc.w00 (RV64-only)
    val MQRACC_W01 = IntRegFile.TypeR_RDS(M"1111111----------101-----0111011") // mqracc.w01 (RV64-only)
    val MQRACC_W11 = IntRegFile.TypeR_RDS(M"1111111----------111-----0111011") // mqracc.w11 (RV64-only)
    val PM2ADDA_H = IntRegFile.TypeR_RDS(M"1000100----------101-----0111011") // pm2adda.h
    val PM2ADDA_HX = IntRegFile.TypeR_RDS(M"1001100----------101-----0111011") // pm2adda.hx
    val PM2ADDA_W = IntRegFile.TypeR_RDS(M"1000101----------101-----0111011") // pm2adda.w (RV64-only)
    val PM2ADDA_WX = IntRegFile.TypeR_RDS(M"1001101----------101-----0111011") // pm2adda.wx (RV64-only)
    val PM2ADDASU_H = IntRegFile.TypeR_RDS(M"1110100----------101-----0111011") // pm2addasu.h
    val PM2ADDASU_W = IntRegFile.TypeR_RDS(M"1110101----------101-----0111011") // pm2addasu.w (RV64-only)
    val PM2ADDAU_H = IntRegFile.TypeR_RDS(M"1010100----------101-----0111011") // pm2addau.h
    val PM2ADDAU_W = IntRegFile.TypeR_RDS(M"1010101----------101-----0111011") // pm2addau.w (RV64-only)
    val PM2SUBA_H = IntRegFile.TypeR_RDS(M"1100100----------101-----0111011") // pm2suba.h
    val PM2SUBA_HX = IntRegFile.TypeR_RDS(M"1101100----------101-----0111011") // pm2suba.hx
    val PM2SUBA_W = IntRegFile.TypeR_RDS(M"1100101----------101-----0111011") // pm2suba.w (RV64-only)
    val PM2SUBA_WX = IntRegFile.TypeR_RDS(M"1101101----------101-----0111011") // pm2suba.wx (RV64-only)
    val PM4ADDA_B = IntRegFile.TypeR_RDS(M"1000110----------101-----0111011") // pm4adda.b
    val PM4ADDA_H = IntRegFile.TypeR_RDS(M"1000111----------101-----0111011") // pm4adda.h (RV64-only)
    val PM4ADDASU_B = IntRegFile.TypeR_RDS(M"1110110----------101-----0111011") // pm4addasu.b
    val PM4ADDASU_H = IntRegFile.TypeR_RDS(M"1110111----------101-----0111011") // pm4addasu.h (RV64-only)
    val PM4ADDAU_B = IntRegFile.TypeR_RDS(M"1010110----------101-----0111011") // pm4addau.b
    val PM4ADDAU_H = IntRegFile.TypeR_RDS(M"1010111----------101-----0111011") // pm4addau.h (RV64-only)
    val PMACC_W_H00 = IntRegFile.TypeR_RDS(M"1000101----------011-----0111011") // pmacc.w.h00 (recycled: RV32 macc.h00)
    val PMACC_W_H01 = IntRegFile.TypeR_RDS(M"1001101----------001-----0111011") // pmacc.w.h01 (recycled: RV32 macc.h01)
    val PMACC_W_H11 = IntRegFile.TypeR_RDS(M"1001101----------011-----0111011") // pmacc.w.h11 (recycled: RV32 macc.h11)
    val PMACCSU_W_H00 = IntRegFile.TypeR_RDS(M"1110101----------011-----0111011") // pmaccsu.w.h00 (recycled: RV32 maccsu.h00)
    val PMACCSU_W_H11 = IntRegFile.TypeR_RDS(M"1111101----------011-----0111011") // pmaccsu.w.h11 (recycled: RV32 maccsu.h11)
    val PMACCU_W_H00 = IntRegFile.TypeR_RDS(M"1010101----------011-----0111011") // pmaccu.w.h00 (recycled: RV32 maccu.h00)
    val PMACCU_W_H01 = IntRegFile.TypeR_RDS(M"1011101----------001-----0111011") // pmaccu.w.h01 (recycled: RV32 maccu.h01)
    val PMACCU_W_H11 = IntRegFile.TypeR_RDS(M"1011101----------011-----0111011") // pmaccu.w.h11 (recycled: RV32 maccu.h11)
    val PMHACC_H = IntRegFile.TypeR_RDS(M"1000100----------111-----0111011") // pmhacc.h
    val PMHACC_H_B0 = IntRegFile.TypeR_RDS(M"1010100----------111-----0111011") // pmhacc.h.b0
    val PMHACC_H_B1 = IntRegFile.TypeR_RDS(M"1011100----------111-----0111011") // pmhacc.h.b1
    val PMHACC_W = IntRegFile.TypeR_RDS(M"1000101----------111-----0111011") // pmhacc.w (recycled: RV32 mhacc)
    val PMHACC_W_H0 = IntRegFile.TypeR_RDS(M"1010101----------111-----0111011") // pmhacc.w.h0 (recycled: RV32 mhacc.h0)
    val PMHACC_W_H1 = IntRegFile.TypeR_RDS(M"1011101----------111-----0111011") // pmhacc.w.h1 (recycled: RV32 mhacc.h1)
    val PMHACCSU_H = IntRegFile.TypeR_RDS(M"1100100----------111-----0111011") // pmhaccsu.h
    val PMHACCSU_H_B0 = IntRegFile.TypeR_RDS(M"1010110----------111-----0111011") // pmhaccsu.h.b0
    val PMHACCSU_H_B1 = IntRegFile.TypeR_RDS(M"1011110----------111-----0111011") // pmhaccsu.h.b1
    val PMHACCSU_W = IntRegFile.TypeR_RDS(M"1100101----------111-----0111011") // pmhaccsu.w (recycled: RV32 mhaccsu)
    val PMHACCSU_W_H0 = IntRegFile.TypeR_RDS(M"1010111----------111-----0111011") // pmhaccsu.w.h0 (recycled: RV32 mhaccsu.h0)
    val PMHACCSU_W_H1 = IntRegFile.TypeR_RDS(M"1011111----------111-----0111011") // pmhaccsu.w.h1 (recycled: RV32 mhaccsu.h1)
    val PMHACCU_H = IntRegFile.TypeR_RDS(M"1001100----------111-----0111011") // pmhaccu.h
    val PMHACCU_W = IntRegFile.TypeR_RDS(M"1001101----------111-----0111011") // pmhaccu.w (recycled: RV32 mhaccu)
    val PMHRACC_H = IntRegFile.TypeR_RDS(M"1000110----------111-----0111011") // pmhracc.h
    val PMHRACC_W = IntRegFile.TypeR_RDS(M"1000111----------111-----0111011") // pmhracc.w (recycled: RV32 mhracc)
    val PMHRACCSU_H = IntRegFile.TypeR_RDS(M"1100110----------111-----0111011") // pmhraccsu.h
    val PMHRACCSU_W = IntRegFile.TypeR_RDS(M"1100111----------111-----0111011") // pmhraccsu.w (recycled: RV32 mhraccsu)
    val PMHRACCU_H = IntRegFile.TypeR_RDS(M"1001110----------111-----0111011") // pmhraccu.h
    val PMHRACCU_W = IntRegFile.TypeR_RDS(M"1001111----------111-----0111011") // pmhraccu.w (recycled: RV32 mhraccu)
    val PMQ2ADDA_H = IntRegFile.TypeR_RDS(M"1011100----------101-----0111011") // pmq2adda.h
    val PMQ2ADDA_W = IntRegFile.TypeR_RDS(M"1011101----------101-----0111011") // pmq2adda.w (RV64-only)
    val PMQACC_W_H00 = IntRegFile.TypeR_RDS(M"1110100----------111-----0111011") // pmqacc.w.h00 (recycled: RV32 mqacc.h00)
    val PMQACC_W_H01 = IntRegFile.TypeR_RDS(M"1111100----------101-----0111011") // pmqacc.w.h01 (recycled: RV32 mqacc.h01)
    val PMQACC_W_H11 = IntRegFile.TypeR_RDS(M"1111100----------111-----0111011") // pmqacc.w.h11 (recycled: RV32 mqacc.h11)
    val PMQR2ADDA_H = IntRegFile.TypeR_RDS(M"1011110----------101-----0111011") // pmqr2adda.h
    val PMQR2ADDA_W = IntRegFile.TypeR_RDS(M"1011111----------101-----0111011") // pmqr2adda.w (RV64-only)
    val PMQRACC_W_H00 = IntRegFile.TypeR_RDS(M"1110110----------111-----0111011") // pmqracc.w.h00 (recycled: RV32 mqracc.h00)
    val PMQRACC_W_H01 = IntRegFile.TypeR_RDS(M"1111110----------101-----0111011") // pmqracc.w.h01 (recycled: RV32 mqracc.h01)
    val PMQRACC_W_H11 = IntRegFile.TypeR_RDS(M"1111110----------111-----0111011") // pmqracc.w.h11 (recycled: RV32 mqracc.h11)
  }
}

/** The draft-P multiply / MAC / Q-format family.
 *
 * At XLEN=32 one four-slot 17x17 partial-product array serves every net
 * organisation: S_HH is one slot, S_WW all four, P_B4 four byte lanes, and
 * the widening `w*`/`pw*` forms split its 64-bit sum across a register pair.
 *
 * At XLEN=64 that array is elaborated ONCE PER 32-BIT WORD because 98 of the 142 RV64 mnemonics are word-local:
 * a `.h`/`.b` form just gains lanes, and a recycled `.w` form
 * is the RV32 scalar op applied to each word. The remaining 44 write one
 * 64-bit value, and the `WIDE_*` payloads say how the two word nets combine.
 */
class RvpMulPlugin(val layer: LaneLayer, val xlen: Int, val pairs: Boolean = true) extends ExecutionUnitElementSimple(layer) {

  require(xlen == 32 || xlen == 64, s"RvpMulPlugin: XLEN 32 or 64, got $xlen")
  private val rv64 = xlen == 64
  // RV32 register-pair support; false = the no-pairs configuration: no
  // widening pair results, no 64-bit destination-pair accumulate, no pair-write port. The
  // plain destination-read MACs (RDS) stay.
  private val withPairs = pairs && !rv64

  val NET = Payload(RvpMulNet())
  val RES = Payload(RvpMulRes())
  val CROSS = Payload(Bool())
  val SEL_A = Payload(Bool())
  val SEL_B = Payload(Bool())
  val SGN_A = Payload(Bool())
  val SGN_B = Payload(Bool())
  val RND = Payload(Bool())
  val ACC = Payload(Bool())
  val NEG1 = Payload(Bool())
  val SAT = Payload(Bool())
  val QSAT = Payload(Bool())
  val PAIR_ACC_64 = Payload(Bool())

  // XLEN=64 only. WIDE_RES marks the 44 mnemonics whose result is one 64-bit
  // value rather than the two word nets' own read-outs. WIDE_SUM brings net 1
  // into it and WIDE_Q takes both nets Q-shifted. WORD_A/WORD_B pick net 0's
  // source words and WORD_CROSS swaps net 1's rs2 word, which together cover
  // every `.wXY` selection and the `.wx` cross reductions.
  val WIDE_RES = Payload(Bool())
  val WIDE_SUM = Payload(Bool())
  val WIDE_Q = Payload(Bool())
  val WORD_A = Payload(Bool())
  val WORD_B = Payload(Bool())
  val WORD_CROSS = Payload(Bool())

  val logic = during setup new Logic {

    def registerOpcode(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop
    }

    def registerRDSOpcode(enc: MicroOp) = {
      val uop = add(enc)
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)
      uop.spec.addRsSpec(RDS, executeAt = 2)
      uop
    }

    val decoderService = host[DecoderService]
    val decoderRetainer = retains(decoderService.decodingLock)

    def registerPairAccOpcode(enc: MicroOp) = {
      val uop = add(enc)

      // Product operands are consumed by the multiplier in E0.
      uop.spec.addRsSpec(RS1, executeAt = 0)
      uop.spec.addRsSpec(RS2, executeAt = 0)

      // Destination-pair accumulator is consumed only in E2.
      uop.spec.addRsSpec(RDS, executeAt = 2) // X[2*rd_p]
      uop.spec.addRsSpec(RS3, executeAt = 2) // X[2*rd_p+1], or x0 for rd_p=0

      uop.decode(PAIR_RD -> True, PAIR_RD_SRC -> True)

      for (p <- List(PAIR_RD, PAIR_RD_SRC))
        decoderService.addMicroOpDecoding(enc, p, True)

      uop
    }

    awaitBuild()

    assert(Riscv.XLEN.get == xlen)

    val writeBack = newWriteback(ifp, 2)

    // No RV64 draft-P instruction writes a register pair, so RvpPairWritePlugin
    // is not even elaborated at XLEN=64.
    val pairWrite = withPairs generate host[RvpPairWritePlugin]
    val pairRetainer = withPairs generate retains(pairWrite.elaborationLock)
    val hiPort = withPairs generate pairWrite.createPort(at = 2) // must be <= writeAt
    if (withPairs) pairRetainer.release()

    val vxsat = host[RvpVxsatPlugin]
    val vxsatRetainer = retains(vxsat.elaborationLock)
    val vxsatPort = vxsat.createPort(at = 2)
    vxsatRetainer.release()

    el.setDecodingDefault(SEL_A, False)
    el.setDecodingDefault(SEL_B, False)
    el.setDecodingDefault(SGN_B, False)
    el.setDecodingDefault(SGN_A, False)
    el.setDecodingDefault(CROSS, False)
    el.setDecodingDefault(RND, False)
    el.setDecodingDefault(ACC, False)
    el.setDecodingDefault(NEG1, False)
    el.setDecodingDefault(SAT, False)
    el.setDecodingDefault(QSAT, False)
    if (!rv64) el.setDecodingDefault(PAIR_ACC_64, False)
    if (rv64) {
      el.setDecodingDefault(WIDE_RES, False)
      el.setDecodingDefault(WIDE_SUM, False)
      el.setDecodingDefault(WIDE_Q, False)
      el.setDecodingDefault(WORD_A, False)
      el.setDecodingDefault(WORD_B, False)
      el.setDecodingDefault(WORD_CROSS, False)
    }

    if (!rv64) {

      registerOpcode(RvpMulPlugin.MUL_H00).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True)
      registerOpcode(RvpMulPlugin.MUL_H01).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SEL_B -> True)
      registerOpcode(RvpMulPlugin.MUL_H11).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SEL_A -> True, SEL_B -> True)
      registerOpcode(RvpMulPlugin.MULSU_H00).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True)
      registerOpcode(RvpMulPlugin.MULSU_H11).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SEL_A -> True, SEL_B -> True)
      registerOpcode(RvpMulPlugin.MULU_H00).decode(NET -> S_HH, RES -> SUM0)
      registerOpcode(RvpMulPlugin.MULU_H01).decode(NET -> S_HH, RES -> SUM0, SEL_B -> True)
      registerOpcode(RvpMulPlugin.MULU_H11).decode(NET -> S_HH, RES -> SUM0, SEL_A -> True, SEL_B -> True)

      registerOpcode(RvpMulPlugin.MULH_H0).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, SGN_B -> True)
      registerOpcode(RvpMulPlugin.MULH_H1).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, SGN_B -> True, SEL_B -> True)
      registerOpcode(RvpMulPlugin.MULHSU_H0).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True)
      registerOpcode(RvpMulPlugin.MULHSU_H1).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, SEL_B -> True)

      registerOpcode(RvpMulPlugin.MULHR).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, SGN_B -> True, RND -> True)
      registerOpcode(RvpMulPlugin.MULHRSU).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, RND -> True)
      registerOpcode(RvpMulPlugin.MULHRU).decode(NET -> S_WW, RES -> SUM32, RND -> True)

      registerOpcode(RvpMulPlugin.PMUL_H_B00).decode(NET -> P_BB, RES -> LANE_LO, SGN_A -> True, SGN_B -> True)
      registerOpcode(RvpMulPlugin.PMUL_H_B01).decode(NET -> P_BB, RES -> LANE_LO, SGN_A -> True, SGN_B -> True, SEL_B -> True)
      registerOpcode(RvpMulPlugin.PMUL_H_B11).decode(NET -> P_BB, RES -> LANE_LO, SGN_A -> True, SGN_B -> True, SEL_A -> True, SEL_B -> True)
      registerOpcode(RvpMulPlugin.PMULSU_H_B00).decode(NET -> P_BB, RES -> LANE_LO, SGN_A -> True)
      registerOpcode(RvpMulPlugin.PMULSU_H_B11).decode(NET -> P_BB, RES -> LANE_LO, SGN_A -> True, SEL_A -> True, SEL_B -> True)
      registerOpcode(RvpMulPlugin.PMULU_H_B00).decode(NET -> P_BB, RES -> LANE_LO)
      registerOpcode(RvpMulPlugin.PMULU_H_B01).decode(NET -> P_BB, RES -> LANE_LO, SEL_B -> True)
      registerOpcode(RvpMulPlugin.PMULU_H_B11).decode(NET -> P_BB, RES -> LANE_LO, SEL_A -> True, SEL_B -> True)

      registerOpcode(RvpMulPlugin.PMULH_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, SGN_B -> True)
      registerOpcode(RvpMulPlugin.PMULHR_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, SGN_B -> True, RND -> True)
      registerOpcode(RvpMulPlugin.PMULHRSU_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, RND -> True)
      registerOpcode(RvpMulPlugin.PMULHRU_H).decode(NET -> P_HH, RES -> LANE_HI, RND -> True)
      registerOpcode(RvpMulPlugin.PMULHSU_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True)
      registerOpcode(RvpMulPlugin.PMULHU_H).decode(NET -> P_HH, RES -> LANE_HI)

      registerOpcode(RvpMulPlugin.PMULH_H_B0).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, SGN_B -> True)
      registerOpcode(RvpMulPlugin.PMULH_H_B1).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, SGN_B -> True, SEL_B -> True)
      registerOpcode(RvpMulPlugin.PMULHSU_H_B0).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True)
      registerOpcode(RvpMulPlugin.PMULHSU_H_B1).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, SEL_B -> True)

      registerOpcode(RvpMulPlugin.PM2ADD_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True)
      registerOpcode(RvpMulPlugin.PM2ADD_HX).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, CROSS -> True)
      registerOpcode(RvpMulPlugin.PM2ADDSU_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True)
      registerOpcode(RvpMulPlugin.PM2ADDU_H).decode(NET -> P_HH, RES -> SUM0)
      registerOpcode(RvpMulPlugin.PM2SUB_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, NEG1 -> True)
      registerOpcode(RvpMulPlugin.PM2SUB_HX).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, CROSS -> True, NEG1 -> True)

      registerOpcode(RvpMulPlugin.PM4ADD_B).decode(NET -> P_B4, RES -> SUM0, SGN_A -> True, SGN_B -> True)
      registerOpcode(RvpMulPlugin.PM4ADDSU_B).decode(NET -> P_B4, RES -> SUM0, SGN_A -> True)
      registerOpcode(RvpMulPlugin.PM4ADDU_B).decode(NET -> P_B4, RES -> SUM0)

      registerOpcode(RvpMulPlugin.PMQ2ADD_H).decode(NET -> P_HH, RES -> SUMQ, SGN_A -> True, SGN_B -> True)
      registerOpcode(RvpMulPlugin.PMQR2ADD_H).decode(NET -> P_HH, RES -> SUMQ, SGN_A -> True, SGN_B -> True, RND -> True)

      registerOpcode(RvpMulPlugin.MULQ).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, QSAT -> True)
      registerOpcode(RvpMulPlugin.MULQR).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, QSAT -> True, RND -> True)
      registerOpcode(RvpMulPlugin.PMULQ_H).decode(NET -> P_HH, RES -> LANE_Q, SGN_A -> True, SGN_B -> True, QSAT -> True)
      registerOpcode(RvpMulPlugin.PMULQR_H).decode(NET -> P_HH, RES -> LANE_Q, SGN_A -> True, SGN_B -> True, QSAT -> True, RND -> True)
      registerOpcode(RvpMulPlugin.PM2SADD_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SAT -> True)
      registerOpcode(RvpMulPlugin.PM2SADD_HX).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SAT -> True, CROSS -> True)

      registerRDSOpcode(RvpMulPlugin.MACC_H00).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MACC_H01).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MACC_H11).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SEL_A -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MACCSU_H00).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MACCSU_H11).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SEL_A -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MACCU_H00).decode(NET -> S_HH, RES -> SUM0, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MACCU_H01).decode(NET -> S_HH, RES -> SUM0, SEL_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MACCU_H11).decode(NET -> S_HH, RES -> SUM0, SEL_A -> True, SEL_B -> True, ACC -> True)

      registerRDSOpcode(RvpMulPlugin.PMHACC_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MHACC_H0).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MHACC_H1).decode(NET -> S_WH, RES -> SUM16, SEL_B -> True, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MHACCSU_H0).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MHACCSU_H1).decode(NET -> S_WH, RES -> SUM16, SEL_B -> True, SGN_A -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMHACC_H_B0).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMHACC_H_B1).decode(NET -> P_HB, RES -> LANE_MIX, SEL_B -> True, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMHACCSU_H_B0).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMHACCSU_H_B1).decode(NET -> P_HB, RES -> LANE_MIX, SEL_B -> True, SGN_A -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MHACC).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MHACCSU).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MHACCU).decode(NET -> S_WW, RES -> SUM32, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MHRACC).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MHRACCSU).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, RND -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MHRACCU).decode(NET -> S_WW, RES -> SUM32, RND -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMHACCSU_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMHACCU_H).decode(NET -> P_HH, RES -> LANE_HI, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMHRACC_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMHRACCSU_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, RND -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMHRACCU_H).decode(NET -> P_HH, RES -> LANE_HI, RND -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PM2ADDA_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PM2ADDA_HX).decode(NET -> P_HH, RES -> SUM0, CROSS -> True, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PM2ADDASU_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PM2ADDAU_H).decode(NET -> P_HH, RES -> SUM0, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PM2SUBA_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True, NEG1 -> True)
      registerRDSOpcode(RvpMulPlugin.PM2SUBA_HX).decode(NET -> P_HH, RES -> SUM0, CROSS -> True, SGN_A -> True, SGN_B -> True, ACC -> True, NEG1 -> True)
      registerRDSOpcode(RvpMulPlugin.PM4ADDA_B).decode(NET -> P_B4, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PM4ADDASU_B).decode(NET -> P_B4, RES -> SUM0, SGN_A -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PM4ADDAU_B).decode(NET -> P_B4, RES -> SUM0, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMQ2ADDA_H).decode(NET -> P_HH, RES -> SUMQ, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.PMQR2ADDA_H).decode(NET -> P_HH, RES -> SUMQ, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MQACC_H00).decode(NET -> S_HH, RES -> SUM15, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MQACC_H01).decode(NET -> S_HH, RES -> SUM15, SEL_B -> True, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MQACC_H11).decode(NET -> S_HH, RES -> SUM15, SEL_A -> True, SEL_B -> True, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MQRACC_H00).decode(NET -> S_HH, RES -> SUM15, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MQRACC_H01).decode(NET -> S_HH, RES -> SUM15, SEL_B -> True, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(RvpMulPlugin.MQRACC_H11).decode(NET -> S_HH, RES -> SUM15, SEL_A -> True, SEL_B -> True, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True)

      if (pairs) { // ---- register-pair ops, dropped by --rvp-no-pairs ----
      registerOpcode(RvpMulPlugin.WMUL).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.WMULU).decode(NET -> S_WW, RES -> SUM0, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.WMULSU).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PWMUL_H).decode(NET -> P_HH, RES -> WIDE_H, SGN_A -> True, SGN_B -> True, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PWMUL_B).decode(NET -> P_B4, RES -> WIDE_B, SGN_A -> True, SGN_B -> True, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PWMULU_H).decode(NET -> P_HH, RES -> WIDE_H, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PWMULU_B).decode(NET -> P_B4, RES -> WIDE_B, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PWMULSU_H).decode(NET -> P_HH, RES -> WIDE_H, SGN_A -> True, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PWMULSU_B).decode(NET -> P_B4, RES -> WIDE_B, SGN_A -> True, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PM2WADD_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PM2WADD_HX).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, CROSS -> True, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PM2WADDU_H).decode(NET -> P_HH, RES -> SUM0, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PM2WADDSU_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PM2WSUB_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, NEG1 -> True, PAIR_RD -> True)
      registerOpcode(RvpMulPlugin.PM2WSUB_HX).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, CROSS -> True, NEG1 -> True, PAIR_RD -> True)

      // Signed × signed
      registerPairAccOpcode(RvpMulPlugin.PWMACC_H).decode(NET -> P_HH, RES -> WIDE_H, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerPairAccOpcode(RvpMulPlugin.WMACC).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True, PAIR_ACC_64 -> True)

      // Signed × unsigned
      registerPairAccOpcode(RvpMulPlugin.PWMACCSU_H).decode(NET -> P_HH, RES -> WIDE_H, SGN_A -> True, ACC -> True)
      registerPairAccOpcode(RvpMulPlugin.WMACCSU).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, ACC -> True, PAIR_ACC_64 -> True)

      // Unsigned × unsigned
      registerPairAccOpcode(RvpMulPlugin.PWMACCU_H).decode(NET -> P_HH, RES -> WIDE_H, ACC -> True)
      registerPairAccOpcode(RvpMulPlugin.WMACCU).decode(NET -> S_WW, RES -> SUM0, ACC -> True, PAIR_ACC_64 -> True)

      registerPairAccOpcode(RvpMulPlugin.PM2WADDA_H).decode(
        NET -> P_HH, RES -> SUM0,
        SGN_A -> True, SGN_B -> True,
        ACC -> True, PAIR_ACC_64 -> True
      )

      registerPairAccOpcode(RvpMulPlugin.PM2WADDASU_H).decode(
        NET -> P_HH, RES -> SUM0,
        SGN_A -> True,
        ACC -> True, PAIR_ACC_64 -> True
      )

      registerPairAccOpcode(RvpMulPlugin.PM2WADDAU_H).decode(
        NET -> P_HH, RES -> SUM0,
        ACC -> True, PAIR_ACC_64 -> True
      )

      registerPairAccOpcode(RvpMulPlugin.PM2WADDA_HX).decode(
        NET -> P_HH, RES -> SUM0,
        SGN_A -> True, SGN_B -> True, CROSS -> True,
        ACC -> True, PAIR_ACC_64 -> True
      )

      registerPairAccOpcode(RvpMulPlugin.PM2WSUBA_H).decode(
        NET -> P_HH, RES -> SUM0,
        SGN_A -> True, SGN_B -> True, NEG1 -> True,
        ACC -> True, PAIR_ACC_64 -> True
      )

      registerPairAccOpcode(RvpMulPlugin.PM2WSUBA_HX).decode(
        NET -> P_HH, RES -> SUM0,
        SGN_A -> True, SGN_B -> True, CROSS -> True, NEG1 -> True,
        ACC -> True, PAIR_ACC_64 -> True
      )

      // Widening Q-format pair accumulates.
      // LANE_Q/SUM31 also select the existing 2^14/2^30 rounding constants.
      registerPairAccOpcode(RvpMulPlugin.PMQWACC_H).decode(
        NET -> P_HH, RES -> LANE_Q,
        SGN_A -> True, SGN_B -> True,
        ACC -> True
      )

      registerPairAccOpcode(RvpMulPlugin.MQWACC).decode(
        NET -> S_WW, RES -> SUM31,
        SGN_A -> True, SGN_B -> True,
        ACC -> True, PAIR_ACC_64 -> True
      )

      registerPairAccOpcode(RvpMulPlugin.PMQRWACC_H).decode(
        NET -> P_HH, RES -> LANE_Q,
        SGN_A -> True, SGN_B -> True, RND -> True,
        ACC -> True
      )

      registerPairAccOpcode(RvpMulPlugin.MQRWACC).decode(
        NET -> S_WW, RES -> SUM31,
        SGN_A -> True, SGN_B -> True, RND -> True,
        ACC -> True, PAIR_ACC_64 -> True
      )

      decoderService.addMicroOpDecoding(RvpMulPlugin.WMUL, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.WMULU, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.WMULSU, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PWMUL_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PWMUL_B, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PWMULU_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PWMULU_B, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PWMULSU_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PWMULSU_B, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PM2WADD_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PM2WADD_HX, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PM2WADDU_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PM2WADDSU_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PM2WSUB_H, PAIR_RD, True)
      decoderService.addMicroOpDecoding(RvpMulPlugin.PM2WSUB_HX, PAIR_RD, True)
      } // pairs
    }

    // ---- XLEN=64 decode -------------------------------------------------------
    if (rv64) {
      registerOpcode(Rv64.PMULH_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, SGN_B -> True)
      registerOpcode(Rv64.PMULH_W).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, SGN_B -> True)
      registerOpcode(Rv64.PMULHSU_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True)
      registerOpcode(Rv64.PMULHSU_W).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True)
      registerOpcode(Rv64.PMULHU_H).decode(NET -> P_HH, RES -> LANE_HI)
      registerOpcode(Rv64.PMULHU_W).decode(NET -> S_WW, RES -> SUM32)
      registerOpcode(Rv64.PMULHR_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, SGN_B -> True, RND -> True)
      registerOpcode(Rv64.PMULHR_W).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, SGN_B -> True, RND -> True)
      registerOpcode(Rv64.PMULHRSU_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, RND -> True)
      registerOpcode(Rv64.PMULHRSU_W).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, RND -> True)
      registerOpcode(Rv64.PMULHRU_H).decode(NET -> P_HH, RES -> LANE_HI, RND -> True)
      registerOpcode(Rv64.PMULHRU_W).decode(NET -> S_WW, RES -> SUM32, RND -> True)
      registerRDSOpcode(Rv64.PMHACC_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACC_W).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACCSU_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACCSU_W).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACCU_H).decode(NET -> P_HH, RES -> LANE_HI, ACC -> True)
      registerRDSOpcode(Rv64.PMHACCU_W).decode(NET -> S_WW, RES -> SUM32, ACC -> True)
      registerRDSOpcode(Rv64.PMHRACC_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHRACC_W).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHRACCSU_H).decode(NET -> P_HH, RES -> LANE_HI, SGN_A -> True, RND -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHRACCSU_W).decode(NET -> S_WW, RES -> SUM32, SGN_A -> True, RND -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHRACCU_H).decode(NET -> P_HH, RES -> LANE_HI, RND -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHRACCU_W).decode(NET -> S_WW, RES -> SUM32, RND -> True, ACC -> True)
      registerOpcode(Rv64.PMULQ_H).decode(NET -> P_HH, RES -> LANE_Q, SGN_A -> True, SGN_B -> True, QSAT -> True)
      registerOpcode(Rv64.PMULQ_W).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, QSAT -> True)
      registerOpcode(Rv64.PMULQR_H).decode(NET -> P_HH, RES -> LANE_Q, SGN_A -> True, SGN_B -> True, RND -> True, QSAT -> True)
      registerOpcode(Rv64.PMULQR_W).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, RND -> True, QSAT -> True)
      registerOpcode(Rv64.PMUL_H_B00).decode(NET -> P_BB, RES -> LANE_LO, SGN_A -> True, SGN_B -> True)
      registerOpcode(Rv64.PMUL_W_H00).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True)
      registerOpcode(Rv64.PMUL_H_B01).decode(NET -> P_BB, RES -> LANE_LO, SGN_A -> True, SGN_B -> True, SEL_B -> True)
      registerOpcode(Rv64.PMUL_W_H01).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SEL_B -> True)
      registerOpcode(Rv64.PMUL_H_B11).decode(NET -> P_BB, RES -> LANE_LO, SGN_A -> True, SGN_B -> True, SEL_A -> True, SEL_B -> True)
      registerOpcode(Rv64.PMUL_W_H11).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SEL_A -> True, SEL_B -> True)
      registerOpcode(Rv64.PMULSU_H_B00).decode(NET -> P_BB, RES -> LANE_LO, SGN_A -> True)
      registerOpcode(Rv64.PMULSU_W_H00).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True)
      registerOpcode(Rv64.PMULSU_H_B11).decode(NET -> P_BB, RES -> LANE_LO, SGN_A -> True, SEL_A -> True, SEL_B -> True)
      registerOpcode(Rv64.PMULSU_W_H11).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SEL_A -> True, SEL_B -> True)
      registerOpcode(Rv64.PMULU_H_B00).decode(NET -> P_BB, RES -> LANE_LO)
      registerOpcode(Rv64.PMULU_W_H00).decode(NET -> S_HH, RES -> SUM0)
      registerOpcode(Rv64.PMULU_H_B01).decode(NET -> P_BB, RES -> LANE_LO, SEL_B -> True)
      registerOpcode(Rv64.PMULU_W_H01).decode(NET -> S_HH, RES -> SUM0, SEL_B -> True)
      registerOpcode(Rv64.PMULU_H_B11).decode(NET -> P_BB, RES -> LANE_LO, SEL_A -> True, SEL_B -> True)
      registerOpcode(Rv64.PMULU_W_H11).decode(NET -> S_HH, RES -> SUM0, SEL_A -> True, SEL_B -> True)
      registerRDSOpcode(Rv64.PMACC_W_H00).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMACC_W_H01).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMACC_W_H11).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SEL_A -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMACCSU_W_H00).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMACCSU_W_H11).decode(NET -> S_HH, RES -> SUM0, SGN_A -> True, SEL_A -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMACCU_W_H00).decode(NET -> S_HH, RES -> SUM0, ACC -> True)
      registerRDSOpcode(Rv64.PMACCU_W_H01).decode(NET -> S_HH, RES -> SUM0, SEL_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMACCU_W_H11).decode(NET -> S_HH, RES -> SUM0, SEL_A -> True, SEL_B -> True, ACC -> True)
      registerOpcode(Rv64.MUL_W00).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, WIDE_RES -> True)
      registerOpcode(Rv64.MUL_W01).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, WIDE_RES -> True, WORD_B -> True)
      registerOpcode(Rv64.MUL_W11).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, WIDE_RES -> True, WORD_A -> True, WORD_B -> True)
      registerOpcode(Rv64.MULSU_W00).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, WIDE_RES -> True)
      registerOpcode(Rv64.MULSU_W11).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, WIDE_RES -> True, WORD_A -> True, WORD_B -> True)
      registerOpcode(Rv64.MULU_W00).decode(NET -> S_WW, RES -> SUM0, WIDE_RES -> True)
      registerOpcode(Rv64.MULU_W01).decode(NET -> S_WW, RES -> SUM0, WIDE_RES -> True, WORD_B -> True)
      registerOpcode(Rv64.MULU_W11).decode(NET -> S_WW, RES -> SUM0, WIDE_RES -> True, WORD_A -> True, WORD_B -> True)
      registerRDSOpcode(Rv64.MACC_W00).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True, WIDE_RES -> True)
      registerRDSOpcode(Rv64.MACC_W01).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True, WIDE_RES -> True, WORD_B -> True)
      registerRDSOpcode(Rv64.MACC_W11).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True, WIDE_RES -> True, WORD_A -> True, WORD_B -> True)
      registerRDSOpcode(Rv64.MACCSU_W00).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, ACC -> True, WIDE_RES -> True)
      registerRDSOpcode(Rv64.MACCSU_W11).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, ACC -> True, WIDE_RES -> True, WORD_A -> True, WORD_B -> True)
      registerRDSOpcode(Rv64.MACCU_W00).decode(NET -> S_WW, RES -> SUM0, ACC -> True, WIDE_RES -> True)
      registerRDSOpcode(Rv64.MACCU_W01).decode(NET -> S_WW, RES -> SUM0, ACC -> True, WIDE_RES -> True, WORD_B -> True)
      registerRDSOpcode(Rv64.MACCU_W11).decode(NET -> S_WW, RES -> SUM0, ACC -> True, WIDE_RES -> True, WORD_A -> True, WORD_B -> True)
      registerOpcode(Rv64.PMULH_H_B0).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, SGN_B -> True)
      registerOpcode(Rv64.PMULH_W_H0).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, SGN_B -> True)
      registerOpcode(Rv64.PMULH_H_B1).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, SGN_B -> True, SEL_B -> True)
      registerOpcode(Rv64.PMULH_W_H1).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, SGN_B -> True, SEL_B -> True)
      registerOpcode(Rv64.PMULHSU_H_B0).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True)
      registerOpcode(Rv64.PMULHSU_W_H0).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True)
      registerOpcode(Rv64.PMULHSU_H_B1).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, SEL_B -> True)
      registerOpcode(Rv64.PMULHSU_W_H1).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, SEL_B -> True)
      registerRDSOpcode(Rv64.PMHACC_H_B0).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACC_W_H0).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACC_H_B1).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, SGN_B -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACC_W_H1).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, SGN_B -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACCSU_H_B0).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACCSU_W_H0).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACCSU_H_B1).decode(NET -> P_HB, RES -> LANE_MIX, SGN_A -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMHACCSU_W_H1).decode(NET -> S_WH, RES -> SUM16, SGN_A -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMQACC_W_H00).decode(NET -> S_HH, RES -> SUM15, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMQACC_W_H01).decode(NET -> S_HH, RES -> SUM15, SGN_A -> True, SGN_B -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMQACC_W_H11).decode(NET -> S_HH, RES -> SUM15, SGN_A -> True, SGN_B -> True, SEL_A -> True, SEL_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMQRACC_W_H00).decode(NET -> S_HH, RES -> SUM15, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMQRACC_W_H01).decode(NET -> S_HH, RES -> SUM15, SGN_A -> True, SGN_B -> True, SEL_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMQRACC_W_H11).decode(NET -> S_HH, RES -> SUM15, SGN_A -> True, SGN_B -> True, SEL_A -> True, SEL_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(Rv64.MQACC_W00).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, ACC -> True, WIDE_RES -> True, WIDE_Q -> True)
      registerRDSOpcode(Rv64.MQACC_W01).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, ACC -> True, WIDE_RES -> True, WIDE_Q -> True, WORD_B -> True)
      registerRDSOpcode(Rv64.MQACC_W11).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, ACC -> True, WIDE_RES -> True, WIDE_Q -> True, WORD_A -> True, WORD_B -> True)
      registerRDSOpcode(Rv64.MQRACC_W00).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True, WIDE_RES -> True, WIDE_Q -> True)
      registerRDSOpcode(Rv64.MQRACC_W01).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True, WIDE_RES -> True, WIDE_Q -> True, WORD_B -> True)
      registerRDSOpcode(Rv64.MQRACC_W11).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True, WIDE_RES -> True, WIDE_Q -> True, WORD_A -> True, WORD_B -> True)
      registerOpcode(Rv64.PM2ADD_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True)
      registerOpcode(Rv64.PM2ADD_W).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerOpcode(Rv64.PM2ADD_HX).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, CROSS -> True)
      registerOpcode(Rv64.PM2ADD_WX).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, WIDE_RES -> True, WIDE_SUM -> True, WORD_B -> True, WORD_CROSS -> True)
      registerOpcode(Rv64.PM2ADDSU_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True)
      registerOpcode(Rv64.PM2ADDSU_W).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerOpcode(Rv64.PM2ADDU_H).decode(NET -> P_HH, RES -> SUM0)
      registerOpcode(Rv64.PM2ADDU_W).decode(NET -> S_WW, RES -> SUM0, WIDE_RES -> True, WIDE_SUM -> True)
      registerOpcode(Rv64.PM2SUB_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, NEG1 -> True)
      registerOpcode(Rv64.PM2SUB_W).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, NEG1 -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerOpcode(Rv64.PM2SUB_HX).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, CROSS -> True, NEG1 -> True)
      registerOpcode(Rv64.PM2SUB_WX).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, NEG1 -> True, WIDE_RES -> True, WIDE_SUM -> True, WORD_B -> True, WORD_CROSS -> True)
      registerRDSOpcode(Rv64.PM2ADDA_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PM2ADDA_W).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerRDSOpcode(Rv64.PM2ADDA_HX).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, CROSS -> True, ACC -> True)
      registerRDSOpcode(Rv64.PM2ADDA_WX).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True, WORD_B -> True, WORD_CROSS -> True)
      registerRDSOpcode(Rv64.PM2ADDASU_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, ACC -> True)
      registerRDSOpcode(Rv64.PM2ADDASU_W).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerRDSOpcode(Rv64.PM2ADDAU_H).decode(NET -> P_HH, RES -> SUM0, ACC -> True)
      registerRDSOpcode(Rv64.PM2ADDAU_W).decode(NET -> S_WW, RES -> SUM0, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerRDSOpcode(Rv64.PM2SUBA_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, NEG1 -> True, ACC -> True)
      registerRDSOpcode(Rv64.PM2SUBA_W).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, NEG1 -> True, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerRDSOpcode(Rv64.PM2SUBA_HX).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, CROSS -> True, NEG1 -> True, ACC -> True)
      registerRDSOpcode(Rv64.PM2SUBA_WX).decode(NET -> S_WW, RES -> SUM0, SGN_A -> True, SGN_B -> True, NEG1 -> True, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True, WORD_B -> True, WORD_CROSS -> True)
      registerOpcode(Rv64.PM2SADD_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, SAT -> True)
      registerOpcode(Rv64.PM2SADD_HX).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, CROSS -> True, SAT -> True)
      registerOpcode(Rv64.PM4ADD_B).decode(NET -> P_B4, RES -> SUM0, SGN_A -> True, SGN_B -> True)
      registerOpcode(Rv64.PM4ADD_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerOpcode(Rv64.PM4ADDSU_B).decode(NET -> P_B4, RES -> SUM0, SGN_A -> True)
      registerOpcode(Rv64.PM4ADDSU_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerOpcode(Rv64.PM4ADDU_B).decode(NET -> P_B4, RES -> SUM0)
      registerOpcode(Rv64.PM4ADDU_H).decode(NET -> P_HH, RES -> SUM0, WIDE_RES -> True, WIDE_SUM -> True)
      registerRDSOpcode(Rv64.PM4ADDA_B).decode(NET -> P_B4, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PM4ADDA_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, SGN_B -> True, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerRDSOpcode(Rv64.PM4ADDASU_B).decode(NET -> P_B4, RES -> SUM0, SGN_A -> True, ACC -> True)
      registerRDSOpcode(Rv64.PM4ADDASU_H).decode(NET -> P_HH, RES -> SUM0, SGN_A -> True, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerRDSOpcode(Rv64.PM4ADDAU_B).decode(NET -> P_B4, RES -> SUM0, ACC -> True)
      registerRDSOpcode(Rv64.PM4ADDAU_H).decode(NET -> P_HH, RES -> SUM0, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True)
      registerOpcode(Rv64.PMQ2ADD_H).decode(NET -> P_HH, RES -> SUMQ, SGN_A -> True, SGN_B -> True)
      registerOpcode(Rv64.PMQ2ADD_W).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, WIDE_RES -> True, WIDE_SUM -> True, WIDE_Q -> True)
      registerOpcode(Rv64.PMQR2ADD_H).decode(NET -> P_HH, RES -> SUMQ, SGN_A -> True, SGN_B -> True, RND -> True)
      registerOpcode(Rv64.PMQR2ADD_W).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, RND -> True, WIDE_RES -> True, WIDE_SUM -> True, WIDE_Q -> True)
      registerRDSOpcode(Rv64.PMQ2ADDA_H).decode(NET -> P_HH, RES -> SUMQ, SGN_A -> True, SGN_B -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMQ2ADDA_W).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True, WIDE_Q -> True)
      registerRDSOpcode(Rv64.PMQR2ADDA_H).decode(NET -> P_HH, RES -> SUMQ, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True)
      registerRDSOpcode(Rv64.PMQR2ADDA_W).decode(NET -> S_WW, RES -> SUM31, SGN_A -> True, SGN_B -> True, RND -> True, ACC -> True, WIDE_RES -> True, WIDE_SUM -> True, WIDE_Q -> True)
    }

    decoderRetainer.release()
    uopRetainer.release()

    val e0 = (!rv64) generate new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt

      val qSatWord = insert(this (QSAT) && this (RES) === SUM31 && rs1 === U(0x80000000L, 32 bits) && rs2 === U(0x80000000L, 32 bits))
      val qSatLane0 = insert(this (QSAT) && this (RES) === LANE_Q && rs1(15 downto 0) === U(0x8000, 16 bits) && rs2(15 downto 0) === U(0x8000, 16 bits))
      val qSatLane1 = insert(this (QSAT) && this (RES) === LANE_Q && rs1(31 downto 16) === U(0x8000, 16 bits) && rs2(31 downto 16) === U(0x8000, 16 bits))

      val P = (0 until 4).map(k => insert(
        RvpMulOps.slot(
          RvpMulOps.slotA(rs1, k, this (NET), this (SEL_A), this (SGN_A)),
          RvpMulOps.slotB(rs2, k, this (NET), this (SEL_B), this (CROSS), this (SGN_B)),
          RvpMulOps.roundConst(this (RES), this (NEG1), this (RND), k)
        )
      ))
    }

    val e1 = (!rv64) generate new el.Execute(id = 1) {
      val T = (0 until 4).map(k => RvpMulOps.align(e0.P(k), k, this (NET), this (RES), this (NEG1)))
      val sum = T.reduceBalancedTree(_ + _)
      val S = insert(sum)

      // Normalize the complete pair-accumulate term before the E2 register. Doing
      // it here keeps RES decode off the 64-bit accumulator carry chain, and
      // keeps the routed path from launching off the shared, high-fanout RES
      // register. It only relocates existing selection logic into E1
      val pairProduct = withPairs generate insert(this (RES).mux(
        WIDE_H -> (
          e0.P(1).asBits(31 downto 0) ##
            e0.P(0).asBits(31 downto 0)
          ),
        LANE_Q -> (
          (e0.P(1) >> 15).resize(32).asBits ##
            (e0.P(0) >> 15).resize(32).asBits
          ),
        SUM31 -> (sum >> 31).resize(64).asBits,
        default -> sum.asBits
      ))

      val mulReadout = insert(RvpMulOps.readout(sum, e0.P(0), e0.P(1), this (RES)))
    }

    val e2 = (!rv64) generate new el.Execute(id = 2) {
      val rds = up(el(IntRegFile, RDS)).asUInt
      // RS3 = X[2*rd_p+1] of the destination-pair accumulate; only those ops declare it.
      val rs3 = if (withPairs) up(el(IntRegFile, RS3)).asUInt else U(0, 32 bits)

      val packed = this (RES).mux(
        LANE_LO -> True,
        LANE_HI -> True,
        LANE_MIX -> True,
        LANE_Q -> True,
        default -> False
      )

      val propagate = !packed

      val mulReadout = this (e1.mulReadout)

      val rdsBits = rds.asBits

      // Complete 64-bit term, normalized and registered in E1 before
      // accumulation.
      //
      // WIDE_H:
      //   P0[31:0] -> X[2*rd_p]
      //   P1[31:0] -> X[2*rd_p+1]
      //
      // LANE_Q:
      //   sign-extended (P0 >> 15) and (P1 >> 15), one per 32-bit lane.
      //
      // SUM31:
      //   sign-extended scalar product shifted right by 31.
      //
      // Default:
      //   unshifted scalar widening product or PM2 reduction.
      val pairAcc = if (withPairs) this (PAIR_RD_SRC) else False
      def pairRd = if (withPairs) this (PAIR_RD) else False

      // The plain accumulate forms use only RDS/mulReadout.
      // The pair-accumulate forms use RS3:RDS and the complete 64-bit widening result.
      val accTerm = if (withPairs) Mux(
        pairAcc,
        this (e1.pairProduct),
        B(0, 32 bits) ## mulReadout
      ) else B(0, 32 bits) ## mulReadout

      val accBase = Mux(
        pairAcc,
        rs3.asBits ## rds.asBits,
        B(0, 32 bits) ## rds.asBits
      )

      // The packed plain-accumulate ACCs wrap at 16 bits.
      // The scalar plain-accumulate ACCs and the packed pair-accumulate MACs wrap at 32 bits.
      // The scalar pair-accumulate MACs wrap at 64 bits.
      val acc16 = this (ACC) && !pairAcc && packed
      val acc64 = if (withPairs) this (ACC) && pairAcc && this (PAIR_ACC_64) else False

      // Guard 1 is the X[2*rd_p] / X[2*rd_p+1] boundary.
      // Open it only for a true 64-bit accumulator.
      def accProp(k: Int): Bool =
        if (k == 1) acc64
        else !acc16

      // [lane0:16][g0][lane1:16][g1][lane2:16][g2][lane3:16][pad]
      def accLanes(x: Bits, guard: Int => Bits): UInt = (
        B(0, 1 bit) ##
          x(63 downto 48) ## guard(2) ##
          x(47 downto 32) ## guard(1) ##
          x(31 downto 16) ## guard(0) ##
          x(15 downto 0)
        ).asUInt

      val accSum =
        accLanes(accTerm, k => accProp(k).asBits) +
          accLanes(accBase, _ => B(0, 1 bit))

      val accLo =
        accSum(32 downto 17).asBits ##
          accSum(15 downto 0).asBits

      val accHi =
        accSum(66 downto 51).asBits ##
          accSum(49 downto 34).asBits

      val result = Mux(this (ACC), accLo, mulReadout)



      // ---- Final saturation ---------------------------------------------------
      // The six saturating multiplies force their writeback and pulse vxsat here,
      // as two independent 16-bit lanes. A whole-word clamp drives both lanes.
      // Packed Q15 clamps each lane on its own.

      val satHi = e1.S.asBits(63 downto 31) // 33 bits; overflow iff not all-equal
      val satOvf = this (SAT) && satHi.orR && !satHi.andR
      val negClamp = satOvf && e1.S.msb // pm2sadd only: clamp toward INT32_MIN

      val hit1 = e0.qSatLane1 || e0.qSatWord || satOvf
      val hit0 = e0.qSatLane0 || e0.qSatWord || satOvf
      val clamp1 = negClamp ? U(0x8000, 16 bits).asBits | U(0x7fff, 16 bits).asBits
      val clamp0 = e0.qSatLane0 ? U(0x7fff, 16 bits).asBits |
        (negClamp ? U(0x0000, 16 bits).asBits | U(0xffff, 16 bits).asBits)

      val lane1 = hit1 ? clamp1 | result(31 downto 16)
      val lane0 = hit0 ? clamp0 | result(15 downto 0)

      if (withPairs) { // pair-destination high half -> RvpPairWritePlugin
        hiPort.valid := SEL && pairRd
        hiPort.payload := Mux(
          pairAcc,
          accHi,
          this (RES).mux(
            WIDE_H -> e0.P(1).asBits(31 downto 0),
            WIDE_B -> (
              e0.P(3).asBits(15 downto 0) ##
                e0.P(2).asBits(15 downto 0)
              ),
            default -> e1.S.asBits(63 downto 32)
          )
        )
      }

      writeBack.valid := SEL
      writeBack.payload := lane1 ## lane0
      vxsatPort.pulse := hit0 || hit1
    }

    // ---- XLEN=64 datapath -----------------------------------------------------
    // Two elaborations of the four-slot array, one per 32-bit word, plus one cross-word combine
    val words = 0 to 1

    val e0w = rv64 generate new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt

      // Only the whole-register `.wXY` forms move a word across the seam:
      // WORD_A/WORD_B pick net 0's source words (net 1 is then unused) and
      // WORD_CROSS swaps net 1's rs2 word for the `.wx` cross reductions.
      // Every other RV64 multiply is word-local, so both nets take their own.
      val a = Seq(Mux(this (WORD_A), rs1(63 downto 32), rs1(31 downto 0)),
        rs1(63 downto 32))
      val b = Seq(Mux(this (WORD_B), rs2(63 downto 32), rs2(31 downto 0)),
        Mux(this (WORD_CROSS), rs2(31 downto 0), rs2(63 downto 32)))

      // Q saturation is decided on the operands ahead of the product, as at
      // RV32 -- and per word, because every RV64 saturating multiply is
      // word-local: pmulq.w is one lane per word, pmulq.h two.
      val qSatWord = words.map(w => insert(
        this (QSAT) && this (RES) === SUM31 &&
          a(w) === U(0x80000000L, 32 bits) && b(w) === U(0x80000000L, 32 bits)))
      val qSatLane0 = words.map(w => insert(
        this (QSAT) && this (RES) === LANE_Q &&
          a(w)(15 downto 0) === U(0x8000, 16 bits) &&
          b(w)(15 downto 0) === U(0x8000, 16 bits)))
      val qSatLane1 = words.map(w => insert(
        this (QSAT) && this (RES) === LANE_Q &&
          a(w)(31 downto 16) === U(0x8000, 16 bits) &&
          b(w)(31 downto 16) === U(0x8000, 16 bits)))

      val P = words.map(w => (0 until 4).map(k => insert(
        RvpMulOps.slot(
          RvpMulOps.slotA(a(w), k, this (NET), this (SEL_A), this (SGN_A)),
          RvpMulOps.slotB(b(w), k, this (NET), this (SEL_B), this (CROSS), this (SGN_B)),
          RvpMulOps.roundConst(this (RES), this (NEG1), this (RND), k)
        ))))
    }

    val e1w = rv64 generate new el.Execute(id = 1) {
      val netSum = words.map(w => (0 until 4)
        .map(k => RvpMulOps.align(e0w.P(w)(k), k, this (NET), this (RES), this (NEG1)))
        .reduceBalancedTree(_ + _))

      // Register each complete word net before any whole-register
      // combine. Execute(2) consumes only these registered operands.
      val S = netSum.map(sum => insert(sum))

      val satOvf = words.map(w => insert(
        this (SAT) && (netSum(w)(32) ^ netSum(w)(31))))
      val satNeg = words.map(w => insert(netSum(w)(32)))

      // Both candidate results are normalized here so Execute(2) holds only the
      // accumulator carry chain and the clamp
      val packedRes = insert(
        RvpMulOps.readout(netSum(1), e0w.P(1)(0), e0w.P(1)(1), this (RES)) ##
          RvpMulOps.readout(netSum(0), e0w.P(0)(0), e0w.P(0)(1), this (RES)))
    }

    val e2w = rv64 generate new el.Execute(id = 2) {
      val rds = up(el(IntRegFile, RDS)).asBits

      val wide = this (WIDE_RES)

      // One carry-propagate chain combines the two registered word nets and,
      // for accumulate forms, RDS.  A conventional `a + b + rds` description
      // can become two cascaded 64-bit carry chains.  Reduce the three operands
      // to carry-save form in LUTs, then perform exactly one final addition.
      // `cin` occupies bit zero of the shifted carry vector (whose bit zero is
      // otherwise always clear), so subtraction's +1 costs no second adder.
      def add3(a: Bits, b: Bits, c: Bits, cin: Bool): Bits = {
        val sum = a ^ b ^ c
        val majority = (a & b) | (a & c) | (b & c)
        val carry = majority(62 downto 0) ## cin
        (sum.asUInt + carry.asUInt).asBits
      }

      val sum0 = this (e1w.S(0)).asBits
      val sum1 = this (e1w.S(1)).asBits
      val q0 = (this (e1w.S(0)) >> 31).resize(64).asBits
      val q1 = (this (e1w.S(1)) >> 31).resize(64).asBits

      val wideA = Mux(this (WIDE_Q), q0, sum0)
      val selectedB = Mux(
        this (WIDE_SUM),
        Mux(this (WIDE_Q), q1, sum1),
        B(0, 64 bits))
      val wideB = Mux(this (NEG1), ~selectedB, selectedB)
      val wideC = Mux(this (ACC), rds, B(0, 64 bits))
      val wideResult = add3(wideA, wideB, wideC, this (NEG1))

      // NEG1's +1 also lives in roundConst slot 0 of every net. So a real
      // subtraction is still (P+1) - (Q+1) = P-Q and the carry-save form only
      // changes how the final wraparound sum is implemented.
      val packedTerm = this (e1w.packedRes)

      val packed = this (RES).mux(
        LANE_LO -> True,
        LANE_HI -> True,
        LANE_MIX -> True,
        LANE_Q -> True,
        default -> False
      )
      val acc16 = this (ACC) && !wide && packed

      def accProp(k: Int): Bool = if (k == 1) False else !acc16

      // [lane0:16][g0][lane1:16][g1][lane2:16][g2][lane3:16][pad]
      def accLanes(x: Bits, guard: Int => Bits): UInt = (
        B(0, 1 bit) ##
          x(63 downto 48) ## guard(2) ##
          x(47 downto 32) ## guard(1) ##
          x(31 downto 16) ## guard(0) ##
          x(15 downto 0)
        ).asUInt

      val accSum =
        accLanes(packedTerm, k => accProp(k).asBits) +
          accLanes(rds, _ => B(0, 1 bit))

      val accResult =
        accSum(66 downto 51).asBits ##
          accSum(49 downto 34).asBits ##
          accSum(32 downto 17).asBits ##
          accSum(15 downto 0).asBits

      val packedResult = Mux(this (ACC), accResult, packedTerm)
      val result = Mux(wide, wideResult, packedResult)

      // Final saturation
      val clamped = words.flatMap(w => {
        val hit1 = e1w.satOvf(w) || e0w.qSatLane1(w) || e0w.qSatWord(w)
        val hit0 = e1w.satOvf(w) || e0w.qSatLane0(w) || e0w.qSatWord(w)
        val negClamp = e1w.satOvf(w) && e1w.satNeg(w) // pm2sadd only
        val clamp1 = negClamp ? U(0x8000, 16 bits).asBits | U(0x7fff, 16 bits).asBits
        val clamp0 = e0w.qSatLane0(w) ? U(0x7fff, 16 bits).asBits |
          (negClamp ? U(0x0000, 16 bits).asBits | U(0xffff, 16 bits).asBits)
        Seq(
          (hit0, hit0 ? clamp0 | result(32 * w + 15 downto 32 * w)),
          (hit1, hit1 ? clamp1 | result(32 * w + 31 downto 32 * w + 16)))
      })

      writeBack.valid := SEL
      writeBack.payload := clamped.map(_._2).reverse.reduce(_ ## _)
      vxsatPort.pulse := clamped.map(_._1).reduce(_ || _)
    }
  }

}
