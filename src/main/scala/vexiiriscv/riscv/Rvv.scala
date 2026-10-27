package vexiiriscv.riscv

import spinal.core._

import scala.collection.mutable

/**
 * Specifies the RISC-V integer vector instructions of Zve64x, including the EEW = 64 loads and stores; no floating point.
 * A core registers only the memory forms its ELEN and XLEN allow (VectorMemoryOps).
 * Fields the spec fixes for an instruction (vm, vs1, vs2, nf) are part of its key, so other values stay undecoded.
 */
object Rvv extends AreaObject {
  val all = mutable.LinkedHashMap[String, SingleDecoding]()

  private val X1 = List(IntRegFile -> RS1)
  private val X12 = List(IntRegFile -> RS1, IntRegFile -> RS2)
  private val XD = List(IntRegFile -> RD)

  private def define(name : String, key : String, resources : Seq[Resource]) : SingleDecoding = {
    assert(key.length == 32, name)
    val d = SingleDecoding(MaskedLiteral(key), resources)
    d.setName(name.toUpperCase.replace('.', '_'))
    all(name) = d
    d
  }

  // OP-V: funct6 vm vs2 vs1/rs1/imm funct3 vd opcode
  private def opv(name : String, f6 : String, f3 : String, vm : String, vs2 : String, vs1 : String, resources : Seq[Resource]) =
    define(name, s"$f6$vm$vs2$vs1$f3-----1010111", resources)
  private def ivv(name : String, f6 : String, vm : String = "-") = opv(name, f6, "000", vm, "-----", "-----", Nil)
  private def ivx(name : String, f6 : String, vm : String = "-") = opv(name, f6, "100", vm, "-----", "-----", X1)
  private def ivi(name : String, f6 : String, vm : String = "-") = opv(name, f6, "011", vm, "-----", "-----", Nil)
  private def mvv(name : String, f6 : String, vm : String = "-") = opv(name, f6, "010", vm, "-----", "-----", Nil)
  private def mvx(name : String, f6 : String, vm : String = "-") = opv(name, f6, "110", vm, "-----", "-----", X1)

  // LOAD-FP / STORE-FP: nf mew mop vm lumop/rs2/vs2 rs1 width vd/vs3 opcode
  private def width(eew : Int) = eew match { case 8 => "000"; case 16 => "101"; case 32 => "110"; case 64 => "111" }
  private def nfBits(nf : Int) = String.format("%3s", (nf - 1).toBinaryString).replace(' ', '0')
  private def mem(name : String, store : Boolean, nf : Int, mop : String, vm : String, rs2 : String, eew : Int, resources : Seq[Resource]) =
    define(name, s"${nfBits(nf)}0$mop$vm$rs2-----${width(eew)}-----${if(store) "0100111" else "0000111"}", resources)

  val eews = List(8, 16, 32, 64)
  val segFields = 2 to 8

  // Configuration
  val VSETVLI  = define("vsetvli",  "0" + "-" * 11 + "-----111-----1010111", List(IntRegFile -> RS1, IntRegFile -> RD))
  val VSETIVLI = define("vsetivli", "11" + "-" * 10 + "-----111-----1010111", XD)
  val VSETVL   = define("vsetvl",   "1000000" + "----------111-----1010111", List(IntRegFile -> RS1, IntRegFile -> RS2, IntRegFile -> RD))

  // Unit-stride, fault-only-first, mask, whole register
  val VLE  = eews.map(e => e -> mem(s"vle$e.v", false, 1, "00", "-", "00000", e, X1)).toMap
  val VSE  = eews.map(e => e -> mem(s"vse$e.v", true, 1, "00", "-", "00000", e, X1)).toMap
  val VLEFF = eews.map(e => e -> mem(s"vle${e}ff.v", false, 1, "00", "-", "10000", e, X1)).toMap
  val VLM_V = mem("vlm.v", false, 1, "00", "1", "01011", 8, X1)
  val VSM_V = mem("vsm.v", true, 1, "00", "1", "01011", 8, X1)
  val VLRE = (for (n <- List(1, 2, 4, 8); e <- eews) yield (n, e) -> mem(s"vl${n}re$e.v", false, n, "00", "1", "01000", e, X1)).toMap
  val VSR  = List(1, 2, 4, 8).map(n => n -> mem(s"vs${n}r.v", true, n, "00", "1", "01000", 8, X1)).toMap

  // Strided and indexed
  val VLSE   = eews.map(e => e -> mem(s"vlse$e.v", false, 1, "10", "-", "-----", e, X12)).toMap
  val VSSE   = eews.map(e => e -> mem(s"vsse$e.v", true, 1, "10", "-", "-----", e, X12)).toMap
  val VLUXEI = eews.map(e => e -> mem(s"vluxei$e.v", false, 1, "01", "-", "-----", e, X1)).toMap
  val VLOXEI = eews.map(e => e -> mem(s"vloxei$e.v", false, 1, "11", "-", "-----", e, X1)).toMap
  val VSUXEI = eews.map(e => e -> mem(s"vsuxei$e.v", true, 1, "01", "-", "-----", e, X1)).toMap
  val VSOXEI = eews.map(e => e -> mem(s"vsoxei$e.v", true, 1, "11", "-", "-----", e, X1)).toMap

  // Segments, keyed by (fields, eew)
  private def seg(f : (Int, Int) => SingleDecoding) = (for (n <- segFields; e <- eews) yield (n, e) -> f(n, e)).toMap
  val VLSEG     = seg((n, e) => mem(s"vlseg${n}e$e.v", false, n, "00", "-", "00000", e, X1))
  val VSSEG     = seg((n, e) => mem(s"vsseg${n}e$e.v", true, n, "00", "-", "00000", e, X1))
  val VLSEGFF   = seg((n, e) => mem(s"vlseg${n}e${e}ff.v", false, n, "00", "-", "10000", e, X1))
  val VLSSEG    = seg((n, e) => mem(s"vlsseg${n}e$e.v", false, n, "10", "-", "-----", e, X12))
  val VSSSEG    = seg((n, e) => mem(s"vssseg${n}e$e.v", true, n, "10", "-", "-----", e, X12))
  val VLUXSEGEI = seg((n, e) => mem(s"vluxseg${n}ei$e.v", false, n, "01", "-", "-----", e, X1))
  val VLOXSEGEI = seg((n, e) => mem(s"vloxseg${n}ei$e.v", false, n, "11", "-", "-----", e, X1))
  val VSUXSEGEI = seg((n, e) => mem(s"vsuxseg${n}ei$e.v", true, n, "01", "-", "-----", e, X1))
  val VSOXSEGEI = seg((n, e) => mem(s"vsoxseg${n}ei$e.v", true, n, "11", "-", "-----", e, X1))

  // Integer arithmetic, OPIVV / OPIVX / OPIVI
  val VADD_VV = ivv("vadd.vv", "000000"); val VADD_VX = ivx("vadd.vx", "000000"); val VADD_VI = ivi("vadd.vi", "000000")
  val VSUB_VV = ivv("vsub.vv", "000010"); val VSUB_VX = ivx("vsub.vx", "000010")
  val VRSUB_VX = ivx("vrsub.vx", "000011"); val VRSUB_VI = ivi("vrsub.vi", "000011")
  val VMINU_VV = ivv("vminu.vv", "000100"); val VMINU_VX = ivx("vminu.vx", "000100")
  val VMIN_VV = ivv("vmin.vv", "000101"); val VMIN_VX = ivx("vmin.vx", "000101")
  val VMAXU_VV = ivv("vmaxu.vv", "000110"); val VMAXU_VX = ivx("vmaxu.vx", "000110")
  val VMAX_VV = ivv("vmax.vv", "000111"); val VMAX_VX = ivx("vmax.vx", "000111")
  val VAND_VV = ivv("vand.vv", "001001"); val VAND_VX = ivx("vand.vx", "001001"); val VAND_VI = ivi("vand.vi", "001001")
  val VOR_VV = ivv("vor.vv", "001010"); val VOR_VX = ivx("vor.vx", "001010"); val VOR_VI = ivi("vor.vi", "001010")
  val VXOR_VV = ivv("vxor.vv", "001011"); val VXOR_VX = ivx("vxor.vx", "001011"); val VXOR_VI = ivi("vxor.vi", "001011")
  val VRGATHER_VV = ivv("vrgather.vv", "001100"); val VRGATHER_VX = ivx("vrgather.vx", "001100"); val VRGATHER_VI = ivi("vrgather.vi", "001100")
  val VRGATHEREI16_VV = ivv("vrgatherei16.vv", "001110")
  val VSLIDEUP_VX = ivx("vslideup.vx", "001110"); val VSLIDEUP_VI = ivi("vslideup.vi", "001110")
  val VSLIDEDOWN_VX = ivx("vslidedown.vx", "001111"); val VSLIDEDOWN_VI = ivi("vslidedown.vi", "001111")
  val VADC_VVM = ivv("vadc.vvm", "010000", "0"); val VADC_VXM = ivx("vadc.vxm", "010000", "0"); val VADC_VIM = ivi("vadc.vim", "010000", "0")
  val VMADC_VVM = ivv("vmadc.vvm", "010001", "0"); val VMADC_VXM = ivx("vmadc.vxm", "010001", "0"); val VMADC_VIM = ivi("vmadc.vim", "010001", "0")
  val VMADC_VV = ivv("vmadc.vv", "010001", "1"); val VMADC_VX = ivx("vmadc.vx", "010001", "1"); val VMADC_VI = ivi("vmadc.vi", "010001", "1")
  val VSBC_VVM = ivv("vsbc.vvm", "010010", "0"); val VSBC_VXM = ivx("vsbc.vxm", "010010", "0")
  val VMSBC_VVM = ivv("vmsbc.vvm", "010011", "0"); val VMSBC_VXM = ivx("vmsbc.vxm", "010011", "0")
  val VMSBC_VV = ivv("vmsbc.vv", "010011", "1"); val VMSBC_VX = ivx("vmsbc.vx", "010011", "1")
  val VMERGE_VVM = ivv("vmerge.vvm", "010111", "0"); val VMERGE_VXM = ivx("vmerge.vxm", "010111", "0"); val VMERGE_VIM = ivi("vmerge.vim", "010111", "0")
  val VMV_V_V = opv("vmv.v.v", "010111", "000", "1", "00000", "-----", Nil)
  val VMV_V_X = opv("vmv.v.x", "010111", "100", "1", "00000", "-----", X1)
  val VMV_V_I = opv("vmv.v.i", "010111", "011", "1", "00000", "-----", Nil)
  val VMSEQ_VV = ivv("vmseq.vv", "011000"); val VMSEQ_VX = ivx("vmseq.vx", "011000"); val VMSEQ_VI = ivi("vmseq.vi", "011000")
  val VMSNE_VV = ivv("vmsne.vv", "011001"); val VMSNE_VX = ivx("vmsne.vx", "011001"); val VMSNE_VI = ivi("vmsne.vi", "011001")
  val VMSLTU_VV = ivv("vmsltu.vv", "011010"); val VMSLTU_VX = ivx("vmsltu.vx", "011010")
  val VMSLT_VV = ivv("vmslt.vv", "011011"); val VMSLT_VX = ivx("vmslt.vx", "011011")
  val VMSLEU_VV = ivv("vmsleu.vv", "011100"); val VMSLEU_VX = ivx("vmsleu.vx", "011100"); val VMSLEU_VI = ivi("vmsleu.vi", "011100")
  val VMSLE_VV = ivv("vmsle.vv", "011101"); val VMSLE_VX = ivx("vmsle.vx", "011101"); val VMSLE_VI = ivi("vmsle.vi", "011101")
  val VMSGTU_VX = ivx("vmsgtu.vx", "011110"); val VMSGTU_VI = ivi("vmsgtu.vi", "011110")
  val VMSGT_VX = ivx("vmsgt.vx", "011111"); val VMSGT_VI = ivi("vmsgt.vi", "011111")
  val VSADDU_VV = ivv("vsaddu.vv", "100000"); val VSADDU_VX = ivx("vsaddu.vx", "100000"); val VSADDU_VI = ivi("vsaddu.vi", "100000")
  val VSADD_VV = ivv("vsadd.vv", "100001"); val VSADD_VX = ivx("vsadd.vx", "100001"); val VSADD_VI = ivi("vsadd.vi", "100001")
  val VSSUBU_VV = ivv("vssubu.vv", "100010"); val VSSUBU_VX = ivx("vssubu.vx", "100010")
  val VSSUB_VV = ivv("vssub.vv", "100011"); val VSSUB_VX = ivx("vssub.vx", "100011")
  val VSLL_VV = ivv("vsll.vv", "100101"); val VSLL_VX = ivx("vsll.vx", "100101"); val VSLL_VI = ivi("vsll.vi", "100101")
  val VSMUL_VV = ivv("vsmul.vv", "100111"); val VSMUL_VX = ivx("vsmul.vx", "100111")
  val VMVR = List(1, 2, 4, 8).map(n => n -> opv(s"vmv${n}r.v", "100111", "011", "1", "-----", s"00${nfBits(n)}", Nil)).toMap
  val VSRL_VV = ivv("vsrl.vv", "101000"); val VSRL_VX = ivx("vsrl.vx", "101000"); val VSRL_VI = ivi("vsrl.vi", "101000")
  val VSRA_VV = ivv("vsra.vv", "101001"); val VSRA_VX = ivx("vsra.vx", "101001"); val VSRA_VI = ivi("vsra.vi", "101001")
  val VSSRL_VV = ivv("vssrl.vv", "101010"); val VSSRL_VX = ivx("vssrl.vx", "101010"); val VSSRL_VI = ivi("vssrl.vi", "101010")
  val VSSRA_VV = ivv("vssra.vv", "101011"); val VSSRA_VX = ivx("vssra.vx", "101011"); val VSSRA_VI = ivi("vssra.vi", "101011")
  val VNSRL_WV = ivv("vnsrl.wv", "101100"); val VNSRL_WX = ivx("vnsrl.wx", "101100"); val VNSRL_WI = ivi("vnsrl.wi", "101100")
  val VNSRA_WV = ivv("vnsra.wv", "101101"); val VNSRA_WX = ivx("vnsra.wx", "101101"); val VNSRA_WI = ivi("vnsra.wi", "101101")
  val VNCLIPU_WV = ivv("vnclipu.wv", "101110"); val VNCLIPU_WX = ivx("vnclipu.wx", "101110"); val VNCLIPU_WI = ivi("vnclipu.wi", "101110")
  val VNCLIP_WV = ivv("vnclip.wv", "101111"); val VNCLIP_WX = ivx("vnclip.wx", "101111"); val VNCLIP_WI = ivi("vnclip.wi", "101111")
  val VWREDSUMU_VS = ivv("vwredsumu.vs", "110000"); val VWREDSUM_VS = ivv("vwredsum.vs", "110001")

  // OPMVV / OPMVX
  val VREDSUM_VS = mvv("vredsum.vs", "000000"); val VREDAND_VS = mvv("vredand.vs", "000001")
  val VREDOR_VS = mvv("vredor.vs", "000010"); val VREDXOR_VS = mvv("vredxor.vs", "000011")
  val VREDMINU_VS = mvv("vredminu.vs", "000100"); val VREDMIN_VS = mvv("vredmin.vs", "000101")
  val VREDMAXU_VS = mvv("vredmaxu.vs", "000110"); val VREDMAX_VS = mvv("vredmax.vs", "000111")
  val VAADDU_VV = mvv("vaaddu.vv", "001000"); val VAADDU_VX = mvx("vaaddu.vx", "001000")
  val VAADD_VV = mvv("vaadd.vv", "001001"); val VAADD_VX = mvx("vaadd.vx", "001001")
  val VASUBU_VV = mvv("vasubu.vv", "001010"); val VASUBU_VX = mvx("vasubu.vx", "001010")
  val VASUB_VV = mvv("vasub.vv", "001011"); val VASUB_VX = mvx("vasub.vx", "001011")
  val VSLIDE1UP_VX = mvx("vslide1up.vx", "001110"); val VSLIDE1DOWN_VX = mvx("vslide1down.vx", "001111")
  val VMV_X_S = opv("vmv.x.s", "010000", "010", "1", "-----", "00000", XD)
  val VCPOP_M = opv("vcpop.m", "010000", "010", "-", "-----", "10000", XD)
  val VFIRST_M = opv("vfirst.m", "010000", "010", "-", "-----", "10001", XD)
  val VMV_S_X = opv("vmv.s.x", "010000", "110", "1", "00000", "-----", X1)
  val VZEXT_VF4 = opv("vzext.vf4", "010010", "010", "-", "-----", "00100", Nil)
  val VSEXT_VF4 = opv("vsext.vf4", "010010", "010", "-", "-----", "00101", Nil)
  val VZEXT_VF2 = opv("vzext.vf2", "010010", "010", "-", "-----", "00110", Nil)
  val VSEXT_VF2 = opv("vsext.vf2", "010010", "010", "-", "-----", "00111", Nil)
  val VZEXT_VF8 = opv("vzext.vf8", "010010", "010", "-", "-----", "00010", Nil)
  val VSEXT_VF8 = opv("vsext.vf8", "010010", "010", "-", "-----", "00011", Nil)
  val VMSBF_M = opv("vmsbf.m", "010100", "010", "-", "-----", "00001", Nil)
  val VMSOF_M = opv("vmsof.m", "010100", "010", "-", "-----", "00010", Nil)
  val VMSIF_M = opv("vmsif.m", "010100", "010", "-", "-----", "00011", Nil)
  val VIOTA_M = opv("viota.m", "010100", "010", "-", "-----", "10000", Nil)
  val VID_V = opv("vid.v", "010100", "010", "-", "00000", "10001", Nil)
  val VCOMPRESS_VM = mvv("vcompress.vm", "010111", "1")
  val VMANDN_MM = mvv("vmandn.mm", "011000", "1"); val VMAND_MM = mvv("vmand.mm", "011001", "1")
  val VMOR_MM = mvv("vmor.mm", "011010", "1"); val VMXOR_MM = mvv("vmxor.mm", "011011", "1")
  val VMORN_MM = mvv("vmorn.mm", "011100", "1"); val VMNAND_MM = mvv("vmnand.mm", "011101", "1")
  val VMNOR_MM = mvv("vmnor.mm", "011110", "1"); val VMXNOR_MM = mvv("vmxnor.mm", "011111", "1")
  val VDIVU_VV = mvv("vdivu.vv", "100000"); val VDIVU_VX = mvx("vdivu.vx", "100000")
  val VDIV_VV = mvv("vdiv.vv", "100001"); val VDIV_VX = mvx("vdiv.vx", "100001")
  val VREMU_VV = mvv("vremu.vv", "100010"); val VREMU_VX = mvx("vremu.vx", "100010")
  val VREM_VV = mvv("vrem.vv", "100011"); val VREM_VX = mvx("vrem.vx", "100011")
  val VMULHU_VV = mvv("vmulhu.vv", "100100"); val VMULHU_VX = mvx("vmulhu.vx", "100100")
  val VMUL_VV = mvv("vmul.vv", "100101"); val VMUL_VX = mvx("vmul.vx", "100101")
  val VMULHSU_VV = mvv("vmulhsu.vv", "100110"); val VMULHSU_VX = mvx("vmulhsu.vx", "100110")
  val VMULH_VV = mvv("vmulh.vv", "100111"); val VMULH_VX = mvx("vmulh.vx", "100111")
  val VMADD_VV = mvv("vmadd.vv", "101001"); val VMADD_VX = mvx("vmadd.vx", "101001")
  val VNMSUB_VV = mvv("vnmsub.vv", "101011"); val VNMSUB_VX = mvx("vnmsub.vx", "101011")
  val VMACC_VV = mvv("vmacc.vv", "101101"); val VMACC_VX = mvx("vmacc.vx", "101101")
  val VNMSAC_VV = mvv("vnmsac.vv", "101111"); val VNMSAC_VX = mvx("vnmsac.vx", "101111")
  val VWADDU_VV = mvv("vwaddu.vv", "110000"); val VWADDU_VX = mvx("vwaddu.vx", "110000")
  val VWADD_VV = mvv("vwadd.vv", "110001"); val VWADD_VX = mvx("vwadd.vx", "110001")
  val VWSUBU_VV = mvv("vwsubu.vv", "110010"); val VWSUBU_VX = mvx("vwsubu.vx", "110010")
  val VWSUB_VV = mvv("vwsub.vv", "110011"); val VWSUB_VX = mvx("vwsub.vx", "110011")
  val VWADDU_WV = mvv("vwaddu.wv", "110100"); val VWADDU_WX = mvx("vwaddu.wx", "110100")
  val VWADD_WV = mvv("vwadd.wv", "110101"); val VWADD_WX = mvx("vwadd.wx", "110101")
  val VWSUBU_WV = mvv("vwsubu.wv", "110110"); val VWSUBU_WX = mvx("vwsubu.wx", "110110")
  val VWSUB_WV = mvv("vwsub.wv", "110111"); val VWSUB_WX = mvx("vwsub.wx", "110111")
  val VWMULU_VV = mvv("vwmulu.vv", "111000"); val VWMULU_VX = mvx("vwmulu.vx", "111000")
  val VWMULSU_VV = mvv("vwmulsu.vv", "111010"); val VWMULSU_VX = mvx("vwmulsu.vx", "111010")
  val VWMUL_VV = mvv("vwmul.vv", "111011"); val VWMUL_VX = mvx("vwmul.vx", "111011")
  val VWMACCU_VV = mvv("vwmaccu.vv", "111100"); val VWMACCU_VX = mvx("vwmaccu.vx", "111100")
  val VWMACC_VV = mvv("vwmacc.vv", "111101"); val VWMACC_VX = mvx("vwmacc.vx", "111101")
  val VWMACCUS_VX = mvx("vwmaccus.vx", "111110")
  val VWMACCSU_VV = mvv("vwmaccsu.vv", "111111"); val VWMACCSU_VX = mvx("vwmaccsu.vx", "111111")
}
