.section	__TEXT,__cstring,cstring_literals
	.balign 8
_LrxA_bytes:
	.string "'C:Melika"
.data
	.balign 8
_LrxB_closure:
	.quad	_ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	_LrxA_bytes
.section	__TEXT,__cstring,cstring_literals
	.balign 8
_Lrxv_bytes:
	.string "Melika"
.data
	.balign 8
_Lrxw_closure:
	.quad	_ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	_Lrxv_bytes
.data
	.balign 8
_Lrxs_closure:
	.quad	_ghczmprim_GHCziTypes_KindRepVar_con_info
	.quad	0
.data
	.balign 8
_Lrxt_closure:
	.quad	_ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	_Lrxs_closure+2
	.quad	_Lrxs_closure+2
	.quad	3
.data
	.balign 8
_Lrxu_closure:
	.quad	_ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	_Lrxs_closure+2
	.quad	_Lrxt_closure+4
	.quad	3
.data
	.balign 8
_Lrxx_closure:
	.quad	_ghczmprim_GHCziTypes_ZC_con_info
	.quad	_Lrxs_closure+2
	.quad	_ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3
.data
	.balign 8
_Lrxq_closure:
	.quad	_ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	_ghczmprim_GHCziTypes_zdtcConstraint_closure
	.quad	_ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	0
.data
	.balign 8
_Lrxr_closure:
	.quad	_ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	_ghczmprim_GHCziTypes_krepzdzt_closure
	.quad	_Lrxq_closure+1
	.quad	0
.section	__TEXT,__cstring,cstring_literals
	.balign 8
_Lrxo_bytes:
	.string "Lib.ASTParse"
.data
	.balign 8
_Lrxp_closure:
	.quad	_ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	_Lrxo_bytes
.section	__TEXT,__cstring,cstring_literals
	.balign 8
_Lrxm_bytes:
	.string "main"
.data
	.balign 8
_Lrxn_closure:
	.quad	_ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	_Lrxm_bytes
.data
	.balign 8
	.globl _LibziASTParse_zdtrModule_closure
_LibziASTParse_zdtrModule_closure:
	.quad	_ghczmprim_GHCziTypes_Module_con_info
	.quad	_Lrxn_closure+1
	.quad	_Lrxp_closure+1
	.quad	3
.data
	.balign 8
	.globl _LibziASTParse_zdtcMelika_closure
_LibziASTParse_zdtcMelika_closure:
	.quad	_ghczmprim_GHCziTypes_TyCon_con_info
	.quad	_LibziASTParse_zdtrModule_closure+1
	.quad	_Lrxw_closure+1
	.quad	_Lrxr_closure+4
	.quad	-2760509201895649363
	.quad	-1436578970323105568
	.quad	0
	.quad	0
.data
	.balign 8
_Lrxy_closure:
	.quad	_ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	_LibziASTParse_zdtcMelika_closure+1
	.quad	_Lrxx_closure+2
	.quad	0
.data
	.balign 8
_Lrxz_closure:
	.quad	_ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	_Lrxu_closure+4
	.quad	_Lrxy_closure+1
	.quad	0
.data
	.balign 8
	.globl _LibziASTParse_zdtczqCZCMelika_closure
_LibziASTParse_zdtczqCZCMelika_closure:
	.quad	_ghczmprim_GHCziTypes_TyCon_con_info
	.quad	_LibziASTParse_zdtrModule_closure+1
	.quad	_LrxB_closure+1
	.quad	_Lrxz_closure+4
	.quad	8084977376270537355
	.quad	1341658906566257015
	.quad	1
	.quad	0
.data
	.balign 8
_LuBT_srt:
	.quad	_stg_SRT_1_info
	.quad	_base_ControlziExceptionziBase_patError_closure
	.quad	0
.section	__TEXT,__cstring,cstring_literals
	.balign 8
_cBp_str:
	.string "Lib/ASTParse.hs:(6,9)-(7,41)|function f"
.text
	.balign 8
	.quad	_LrwB_closure-(_LsAw_info)+0
	.quad	3
	.long	15
	.long	1
_LsAw_info:
LcBc:
	mov x17, x22
	sub x15, x20, #16
	cmp x15, x28
	b.lo LcBd
LcBe:
	adrp x15, _stg_upd_frame_info@page
	add x15, x15, _stg_upd_frame_info@pageoff
	str x15, [ x20, -16 ]
	str x17, [ x20, -8 ]
	ldr x15, [ x17, 16 ]
	ldr x14, [ x17, 24 ]
	ldr x17, [ x17, 32 ]
	mov x25, x17
	mov x24, x14
	mov x23, x15
	sub x20, x20, #16
	b _LrwB_info
LcBd:
	mov x22, x17
	ldr x17, [ x19, -16 ]
	br x17
.text
	.balign 8
	.quad	12884901911
	.quad	1
	.long	14
	.long	0
_LrwB_info:
LcBj:
	mov x17, x25
	mov x15, x24
	mov x14, x23
	sub x13, x20, #32
	cmp x13, x28
	b.lo LcBk
LcBl:
	adrp x13, _LcAW_info@page
	add x13, x13, _LcAW_info@pageoff
	str x13, [ x20, -24 ]
	mov x22, x15
	str x14, [ x20, -16 ]
	str x17, [ x20, -8 ]
	sub x20, x20, #24
	and x17, x22, #7
	cbnz x17, LcAW
LcAX:
	ldr x17, [ x22 ]
	br x17
	.quad	_LrwB_closure-(_LcAW_info)+0
	.quad	2
	.long	30
	.long	1
_LcAW_info:
LcAW:
	ldr x17, [ x20, 16 ]
	mov x15, x22
	and x14, x15, #7
	mov w13, #1
	cmp x14, x13
	b.ne LcBh
LcBg:
	adrp x15, _LcBs_info@page
	add x15, x15, _LcBs_info@pageoff
	str x15, [ x20, 16 ]
	mov x22, x17
	add x20, x20, #16
	and x17, x22, #7
	cbnz x17, LcBs
LcBu:
	ldr x17, [ x22 ]
	br x17
LcBh:
	ldr x14, [ x15, 6 ]
	ldr x15, [ x15, 14 ]
	adrp x13, _LcB2_info@page
	add x13, x13, _LcB2_info@pageoff
	str x13, [ x20, -8 ]
	mov x22, x17
	str x15, [ x20 ]
	str x14, [ x20, 16 ]
	sub x20, x20, #8
	and x17, x22, #7
	cbnz x17, LcB2
LcB3:
	ldr x17, [ x22 ]
	br x17
	.quad	_LrwB_closure-(_LcB2_info)+0
	.quad	3
	.long	30
	.long	1
_LcB2_info:
LcB2:
	ldr x14, [ x20, 16 ]
	ldr x17, [ x20, 24 ]
	ldr x15, [ x20, 8 ]
	mov x13, x22
	and x12, x13, #7
	mov w11, #1
	cmp x12, x11
	b.ne LcBM
LsAk:
	adrp x23, _cBp_str@page
	add x23, x23, _cBp_str@pageoff
	add x20, x20, #32
	b _base_ControlziExceptionziBase_patError_info
LcBk:
	mov x25, x17
	mov x24, x15
	mov x23, x14
	adrp x22, _LrwB_closure@page
	add x22, x22, _LrwB_closure@pageoff
	ldr x17, [ x19, -8 ]
	br x17
	.quad	_LuBT_srt-(_LcBs_info)+0
	.quad	0
	.long	30
	.long	1
_LcBs_info:
LcBs:
	mov x17, x22
	and x17, x17, #7
	mov w15, #1
	cmp x17, x15
	b.ne LuBS
LcBA:
	adrp x22, _ghczmprim_GHCziTypes_ZMZN_closure@page
	add x22, x22, _ghczmprim_GHCziTypes_ZMZN_closure@pageoff
	add x22, x22, #1
	add x20, x20, #8
	ldr x17, [ x20 ]
	br x17
LcBM:
	add x21, x21, #104
	ldr x12, [ x19, 856 ]
	cmp x21, x12
	b.hi LcBP
LcBO:
	ldr x12, [ x13, 6 ]
	ldr x13, [ x13, 14 ]
	adrp x11, _LsAw_info@page
	add x11, x11, _LsAw_info@pageoff
	str x11, [ x21, -96 ]
	str x14, [ x21, -80 ]
	str x15, [ x21, -72 ]
	str x13, [ x21, -64 ]
	sub x15, x21, #96
	adrp x13, _stg_ap_3_upd_info@page
	add x13, x13, _stg_ap_3_upd_info@pageoff
	str x13, [ x21, -56 ]
	str x14, [ x21, -40 ]
	str x17, [ x21, -32 ]
	str x12, [ x21, -24 ]
	sub x17, x21, #56
	adrp x14, _ghczmprim_GHCziTypes_ZC_con_info@page
	add x14, x14, _ghczmprim_GHCziTypes_ZC_con_info@pageoff
	str x14, [ x21, -16 ]
	str x17, [ x21, -8 ]
	str x15, [ x21 ]
	sub x17, x21, #14
	mov x22, x17
	add x20, x20, #32
	ldr x17, [ x20 ]
	br x17
LcBP:
	mov w17, #104
	str x17, [ x19, 904 ]
	mov x22, x13
	b _stg_gc_unpt_r1
LuBS:
	sub x20, x20, #24
	b LsAk
.data
	.balign 8
_LrwB_closure:
	.quad	_LrwB_info
	.quad	_LuBT_srt
	.quad	0
.text
	.balign 8
	.quad	_LrwB_closure-(_LibziASTParse_zdfMelikaZMZN_info)+0
	.quad	12884901911
	.quad	0
	.long	14
	.long	1
	.globl _LibziASTParse_zdfMelikaZMZN_info
_LibziASTParse_zdfMelikaZMZN_info:
LcCt:
	mov x17, x25
	mov x15, x24
	mov x14, x23
LcCv:
	mov x25, x17
	mov x24, x15
	mov x23, x14
	b _LrwB_info
.data
	.balign 8
	.globl _LibziASTParse_zdfMelikaZMZN_closure
_LibziASTParse_zdfMelikaZMZN_closure:
	.quad	_LibziASTParse_zdfMelikaZMZN_info
	.quad	0
.text
	.balign 8
	.quad	4294967301
	.quad	0
	.long	14
	.long	0
	.globl _LibziASTParse_f_info
_LibziASTParse_f_info:
LcCD:
	mov x17, x23
LcCF:
	mov x22, x17
	b _stg_ap_0_fast
.data
	.balign 8
	.globl _LibziASTParse_f_closure
_LibziASTParse_f_closure:
	.quad	_LibziASTParse_f_info
.ident "GHC 9.2.8"


