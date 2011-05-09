/*
 *  MMX/3DNow!/SSE/SSE2/SSE3/SSSE3/SSE4/PNI support
 *
 *  Copyright (c) 2005 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */
#if SHIFT == 0
#define Reg MMXReg
#define SUFFIX _mmx
#define AVX_ONLY(...)
#else
#define Reg XMMReg
#define SUFFIX _xmm
#define AVX_ONLY(...) __VA_ARGS__
#endif

#define dh_alias_Reg ptr
#define dh_alias_XMMReg ptr
#define dh_alias_MMXReg ptr
#define dh_ctype_Reg Reg *
#define dh_ctype_XMMReg XMMReg *
#define dh_ctype_MMXReg MMXReg *
#define dh_is_signed_Reg dh_is_signed_ptr
#define dh_is_signed_XMMReg dh_is_signed_ptr
#define dh_is_signed_MMXReg dh_is_signed_ptr

#define DEF_HELPER_AVX128(name, suffix)					\
	DEF_HELPER_2(glue(name, suffix), void, Reg, Reg)		\
	AVX_ONLY(DEF_HELPER_3(glue(name,_avx), void, Reg, Reg, Reg))

#define DEF_HELPER_AVX128_2OP(name, suffix)				\
	DEF_HELPER_2(glue(name, suffix), void, Reg, Reg)	\
	AVX_ONLY(DEF_HELPER_2(glue(name,_avx), void, Reg, Reg))

#define DEF_HELPER_AVX256(name, suffix)					\
	DEF_HELPER_2(glue(name, suffix), void, Reg, Reg)		\
	AVX_ONLY(DEF_HELPER_3(glue(name,_avx), void, Reg, Reg, Reg))	\
	AVX_ONLY(DEF_HELPER_3(glue(name,_256), void, Reg, Reg, Reg))

#define DEF_HELPER_AVX256_2OP(name, suffix)				\
	DEF_HELPER_2(glue(name, suffix), void, Reg, Reg)	\
	AVX_ONLY(DEF_HELPER_2(glue(name,_avx), void, Reg, Reg))	\
	AVX_ONLY(DEF_HELPER_2(glue(name,_256), void, Reg, Reg))

DEF_HELPER_AVX128(psrlw,SUFFIX)
DEF_HELPER_AVX128(psraw,SUFFIX)
DEF_HELPER_AVX128(psllw,SUFFIX)
DEF_HELPER_AVX128(psrld,SUFFIX)
DEF_HELPER_AVX128(psrad,SUFFIX)
DEF_HELPER_AVX128(pslld,SUFFIX)
DEF_HELPER_AVX128(psrlq,SUFFIX)
DEF_HELPER_AVX128(psllq,SUFFIX)

#if SHIFT == 1
DEF_HELPER_AVX128(psrldq,SUFFIX)
DEF_HELPER_AVX128(pslldq,SUFFIX)
#endif

#define SSE_HELPER_B(name, F)\
    DEF_HELPER_2(glue(name, SUFFIX), void, Reg, Reg)\
    AVX_ONLY(DEF_HELPER_3(glue(name,_avx), void, Reg, Reg, Reg))

#define SSE_HELPER_W(name, F)\
    DEF_HELPER_2(glue(name, SUFFIX), void, Reg, Reg)\
    AVX_ONLY(DEF_HELPER_3(glue(name, _avx), void, Reg, Reg, Reg))

#define SSE_HELPER_L(name, F)\
    DEF_HELPER_2(glue(name, SUFFIX), void, Reg, Reg)\
    AVX_ONLY(DEF_HELPER_3(glue(name, _avx), void, Reg, Reg, Reg))

#define SSE_HELPER_Q(name, F)\
    DEF_HELPER_2(glue(name, SUFFIX), void, Reg, Reg)\
    AVX_ONLY(DEF_HELPER_3(glue(name, _avx), void, Reg, Reg, Reg))\
    AVX_ONLY(DEF_HELPER_3(glue(name, _256), void, Reg, Reg, Reg))

SSE_HELPER_B(paddb, FADD)
SSE_HELPER_W(paddw, FADD)
SSE_HELPER_L(paddl, FADD)
SSE_HELPER_Q(paddq, FADD)

SSE_HELPER_B(psubb, FSUB)
SSE_HELPER_W(psubw, FSUB)
SSE_HELPER_L(psubl, FSUB)
SSE_HELPER_Q(psubq, FSUB)

SSE_HELPER_B(paddusb, FADDUB)
SSE_HELPER_B(paddsb, FADDSB)
SSE_HELPER_B(psubusb, FSUBUB)
SSE_HELPER_B(psubsb, FSUBSB)

SSE_HELPER_W(paddusw, FADDUW)
SSE_HELPER_W(paddsw, FADDSW)
SSE_HELPER_W(psubusw, FSUBUW)
SSE_HELPER_W(psubsw, FSUBSW)

SSE_HELPER_B(pminub, FMINUB)
SSE_HELPER_B(pmaxub, FMAXUB)

SSE_HELPER_W(pminsw, FMINSW)
SSE_HELPER_W(pmaxsw, FMAXSW)

SSE_HELPER_Q(pand, FAND)
SSE_HELPER_Q(pandn, FANDN)
SSE_HELPER_Q(por, FOR)
SSE_HELPER_Q(pxor, FXOR)

SSE_HELPER_B(pcmpgtb, FCMPGTB)
SSE_HELPER_W(pcmpgtw, FCMPGTW)
SSE_HELPER_L(pcmpgtl, FCMPGTL)

SSE_HELPER_B(pcmpeqb, FCMPEQ)
SSE_HELPER_W(pcmpeqw, FCMPEQ)
SSE_HELPER_L(pcmpeql, FCMPEQ)

SSE_HELPER_W(pmullw, FMULLW)
#if SHIFT == 0
SSE_HELPER_W(pmulhrw, FMULHRW)
#endif
SSE_HELPER_W(pmulhuw, FMULHUW)
SSE_HELPER_W(pmulhw, FMULHW)

SSE_HELPER_B(pavgb, FAVG)
SSE_HELPER_W(pavgw, FAVG)

DEF_HELPER_AVX128(pmuludq, SUFFIX)
DEF_HELPER_AVX128(pmaddwd, SUFFIX)

DEF_HELPER_AVX128(psadbw, SUFFIX)
DEF_HELPER_3(glue(maskmov, SUFFIX), void, Reg, Reg, tl)
DEF_HELPER_2(glue(movl_mm_T0, SUFFIX), void, Reg, i32)
AVX_ONLY(DEF_HELPER_2(movl_mm_T0_avx, void, Reg, i32))
#ifdef TARGET_X86_64
DEF_HELPER_2(glue(movq_mm_T0, SUFFIX), void, Reg, i64)
AVX_ONLY(DEF_HELPER_2(movq_mm_T0_avx, void, Reg, i64))
#endif

#if SHIFT == 0
DEF_HELPER_3(glue(pshufw, SUFFIX), void, Reg, Reg, int)
#else
DEF_HELPER_3(shufps, void, Reg, Reg, int)
DEF_HELPER_3(shufpd, void, Reg, Reg, int)
DEF_HELPER_3(glue(pshufd, SUFFIX), void, Reg, Reg, int)
DEF_HELPER_3(glue(pshuflw, SUFFIX), void, Reg, Reg, int)
DEF_HELPER_3(glue(pshufhw, SUFFIX), void, Reg, Reg, int)
#endif

#if SHIFT == 1
/* FPU ops */
/* XXX: not accurate */

#define SSE_HELPER_S(name, F)\
    DEF_HELPER_AVX256(name ## ps,)	 \
    DEF_HELPER_AVX128(name ## ss,)	 \
    DEF_HELPER_AVX256(name ## pd,)	 \
    DEF_HELPER_AVX128(name ## sd,)

SSE_HELPER_S(add, FPU_ADD)
SSE_HELPER_S(sub, FPU_SUB)
SSE_HELPER_S(mul, FPU_MUL)
SSE_HELPER_S(div, FPU_DIV)
SSE_HELPER_S(min, FPU_MIN)
SSE_HELPER_S(max, FPU_MAX)
SSE_HELPER_S(sqrt, FPU_SQRT)


DEF_HELPER_AVX256_2OP(cvtps2pd,)
DEF_HELPER_AVX256_2OP(cvtpd2ps,)
DEF_HELPER_AVX128(cvtss2sd,)
DEF_HELPER_AVX128(cvtsd2ss,)
DEF_HELPER_AVX256_2OP(cvtdq2ps,)
DEF_HELPER_AVX256_2OP(cvtdq2pd,)
DEF_HELPER_2(cvtpi2ps, void, XMMReg, MMXReg)
DEF_HELPER_2(cvtpi2pd, void, XMMReg, MMXReg)
DEF_HELPER_2(cvtsi2ss, void, XMMReg, i32)
DEF_HELPER_2(cvtsi2ss_avx, void, XMMReg, i32)
DEF_HELPER_2(cvtsi2sd, void, XMMReg, i32)
DEF_HELPER_2(cvtsi2sd_avx, void, XMMReg, i32)

#ifdef TARGET_X86_64
DEF_HELPER_2(cvtsq2ss, void, XMMReg, i64)
DEF_HELPER_2(cvtsq2ss_avx, void, XMMReg, i64)
DEF_HELPER_2(cvtsq2sd, void, XMMReg, i64)
DEF_HELPER_2(cvtsq2sd_avx, void, XMMReg, i64)
#endif

DEF_HELPER_AVX256_2OP(cvtps2dq,)
DEF_HELPER_AVX256_2OP(cvtpd2dq,)
DEF_HELPER_2(cvtps2pi, void, MMXReg, XMMReg)
DEF_HELPER_2(cvtpd2pi, void, MMXReg, XMMReg)
DEF_HELPER_1(cvtss2si, s32, XMMReg)
DEF_HELPER_1(cvtsd2si, s32, XMMReg)
#ifdef TARGET_X86_64
DEF_HELPER_1(cvtss2sq, s64, XMMReg)
DEF_HELPER_1(cvtsd2sq, s64, XMMReg)
#endif

DEF_HELPER_AVX256_2OP(cvttps2dq,)
DEF_HELPER_AVX256_2OP(cvttpd2dq,)
DEF_HELPER_2(cvttps2pi, void, MMXReg, XMMReg)
DEF_HELPER_2(cvttpd2pi, void, MMXReg, XMMReg)
DEF_HELPER_1(cvttss2si, s32, XMMReg)
DEF_HELPER_1(cvttsd2si, s32, XMMReg)
#ifdef TARGET_X86_64
DEF_HELPER_1(cvttss2sq, s64, XMMReg)
DEF_HELPER_1(cvttsd2sq, s64, XMMReg)
#endif

DEF_HELPER_AVX256_2OP(rsqrtps,)
DEF_HELPER_AVX128_2OP(rsqrtss,)
DEF_HELPER_AVX256_2OP(rcpps,)
DEF_HELPER_AVX128_2OP(rcpss,)
DEF_HELPER_AVX128_2OP(extrq_r,)
DEF_HELPER_3(extrq_i, void, XMMReg, int, int)
DEF_HELPER_3(extrq_i_avx, void, XMMReg, int, int)
DEF_HELPER_AVX128_2OP(insertq_r,)
DEF_HELPER_3(insertq_i, void, XMMReg, int, int)
DEF_HELPER_3(insertq_i_avx, void, XMMReg, int, int)
DEF_HELPER_AVX256(haddps,)
DEF_HELPER_AVX256(haddpd,)
DEF_HELPER_AVX256(hsubps,)
DEF_HELPER_AVX256(hsubpd,)
DEF_HELPER_AVX256(addsubps,)
DEF_HELPER_AVX256(addsubpd,)

#define SSE_HELPER_CMP(name, F)\
    DEF_HELPER_AVX256( name ## ps,)	  \
    DEF_HELPER_AVX128( name ## ss,)	  \
    DEF_HELPER_AVX256( name ## pd,)	  \
    DEF_HELPER_AVX128( name ## sd,)

SSE_HELPER_CMP(cmpeq, FPU_CMPEQ)
SSE_HELPER_CMP(cmplt, FPU_CMPLT)
SSE_HELPER_CMP(cmple, FPU_CMPLE)
SSE_HELPER_CMP(cmpunord, FPU_CMPUNORD)
SSE_HELPER_CMP(cmpneq, FPU_CMPNEQ)
SSE_HELPER_CMP(cmpnlt, FPU_CMPNLT)
SSE_HELPER_CMP(cmpnle, FPU_CMPNLE)
SSE_HELPER_CMP(cmpord, FPU_CMPORD)

DEF_HELPER_2(ucomiss, void, Reg, Reg)
DEF_HELPER_2(comiss, void, Reg, Reg)
DEF_HELPER_2(ucomisd, void, Reg, Reg)
DEF_HELPER_2(comisd, void, Reg, Reg)
DEF_HELPER_1(movmskps, i32, Reg)
DEF_HELPER_1(movmskps_avx, i32, Reg)
DEF_HELPER_1(movmskps_256, i32, Reg)
DEF_HELPER_1(movmskpd, i32, Reg)
DEF_HELPER_1(movmskpd_avx, i32, Reg)
DEF_HELPER_1(movmskpd_256, i32, Reg)
#endif

DEF_HELPER_1(glue(pmovmskb, SUFFIX), i32, Reg)
DEF_HELPER_AVX128(packsswb, SUFFIX)
DEF_HELPER_AVX128(packuswb, SUFFIX)
DEF_HELPER_AVX128(packssdw, SUFFIX)
#define UNPCK_OP(base_name, base)                               \
    DEF_HELPER_AVX128(punpck ## base_name ## bw, SUFFIX) \
    DEF_HELPER_AVX128(punpck ## base_name ## wd, SUFFIX) \
    DEF_HELPER_AVX128(punpck ## base_name ## dq, SUFFIX)

UNPCK_OP(l, 0)
UNPCK_OP(h, 1)

#if SHIFT == 1
DEF_HELPER_AVX128(punpcklqdq, SUFFIX)
DEF_HELPER_AVX128(punpckhqdq, SUFFIX)
#endif

/* 3DNow! float ops */
#if SHIFT == 0
DEF_HELPER_2(pi2fd, void, MMXReg, MMXReg)
DEF_HELPER_2(pi2fw, void, MMXReg, MMXReg)
DEF_HELPER_2(pf2id, void, MMXReg, MMXReg)
DEF_HELPER_2(pf2iw, void, MMXReg, MMXReg)
DEF_HELPER_2(pfacc, void, MMXReg, MMXReg)
DEF_HELPER_2(pfadd, void, MMXReg, MMXReg)
DEF_HELPER_2(pfcmpeq, void, MMXReg, MMXReg)
DEF_HELPER_2(pfcmpge, void, MMXReg, MMXReg)
DEF_HELPER_2(pfcmpgt, void, MMXReg, MMXReg)
DEF_HELPER_2(pfmax, void, MMXReg, MMXReg)
DEF_HELPER_2(pfmin, void, MMXReg, MMXReg)
DEF_HELPER_2(pfmul, void, MMXReg, MMXReg)
DEF_HELPER_2(pfnacc, void, MMXReg, MMXReg)
DEF_HELPER_2(pfpnacc, void, MMXReg, MMXReg)
DEF_HELPER_2(pfrcp, void, MMXReg, MMXReg)
DEF_HELPER_2(pfrsqrt, void, MMXReg, MMXReg)
DEF_HELPER_2(pfsub, void, MMXReg, MMXReg)
DEF_HELPER_2(pfsubr, void, MMXReg, MMXReg)
DEF_HELPER_2(pswapd, void, MMXReg, MMXReg)
#endif

/* SSSE3 op helpers */
DEF_HELPER_AVX128(phaddw, SUFFIX)
DEF_HELPER_AVX128(phaddd, SUFFIX)
DEF_HELPER_AVX128(phaddsw, SUFFIX)
DEF_HELPER_AVX128(phsubw, SUFFIX)
DEF_HELPER_AVX128(phsubd, SUFFIX)
DEF_HELPER_AVX128(phsubsw, SUFFIX)
DEF_HELPER_AVX128(pabsb, SUFFIX)
DEF_HELPER_AVX128(pabsw, SUFFIX)
DEF_HELPER_AVX128(pabsd, SUFFIX)
DEF_HELPER_AVX128(pmaddubsw, SUFFIX)
DEF_HELPER_AVX128(pmulhrsw, SUFFIX)
DEF_HELPER_AVX128(pshufb, SUFFIX)
DEF_HELPER_AVX128(psignb, SUFFIX)
DEF_HELPER_AVX128(psignw, SUFFIX)
DEF_HELPER_AVX128(psignd, SUFFIX)
DEF_HELPER_3(glue(palignr, SUFFIX), void, Reg, Reg, s32)

/* SSE4.1 op helpers */
#if SHIFT == 1
DEF_HELPER_2(glue(pblendvb, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(blendvps, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(blendvpd, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(ptest, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovsxbw, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovsxbd, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovsxbq, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovsxwd, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovsxwq, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovsxdq, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovzxbw, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovzxbd, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovzxbq, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovzxwd, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovzxwq, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmovzxdq, SUFFIX), void, Reg, Reg)
DEF_HELPER_2(glue(pmuldq, SUFFIX), void, Reg, Reg)
DEF_HELPER_AVX256(pcmpeqq, SUFFIX)
DEF_HELPER_2(glue(packusdw, SUFFIX), void, Reg, Reg)
DEF_HELPER_AVX128(pminsb, SUFFIX)
DEF_HELPER_AVX128(pminsd, SUFFIX)
DEF_HELPER_AVX128(pminuw, SUFFIX)
DEF_HELPER_AVX128(pminud, SUFFIX)
DEF_HELPER_AVX128(pmaxsb, SUFFIX)
DEF_HELPER_AVX128(pmaxsd, SUFFIX)
DEF_HELPER_AVX128(pmaxuw, SUFFIX)
DEF_HELPER_AVX128(pmaxud, SUFFIX)
DEF_HELPER_AVX128(pmulld, SUFFIX)
DEF_HELPER_2(glue(phminposuw, SUFFIX), void, Reg, Reg)
DEF_HELPER_3(glue(roundps, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(roundpd, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(roundss, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(roundsd, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(blendps, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(blendpd, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(pblendw, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(dpps, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(dppd, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(mpsadbw, SUFFIX), void, Reg, Reg, i32)
#endif

/* SSE4.2 op helpers */
#if SHIFT == 1
DEF_HELPER_AVX256(pcmpgtq, SUFFIX)
DEF_HELPER_3(glue(pcmpestri, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(pcmpestrm, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(pcmpistri, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(glue(pcmpistrm, SUFFIX), void, Reg, Reg, i32)
DEF_HELPER_3(crc32, tl, i32, tl, i32)
DEF_HELPER_2(popcnt, tl, tl, i32)
#endif

#undef SHIFT
#undef Reg
#undef SUFFIX

#undef AVX_ONLY
#undef SSE_HELPER_B
#undef SSE_HELPER_W
#undef SSE_HELPER_L
#undef SSE_HELPER_Q
#undef SSE_HELPER_S
#undef SSE_HELPER_CMP
#undef DEF_HELPER_AVX128
#undef DEF_HELPER_AVX128_2OP
#undef DEF_HELPER_AVX256
#undef DEF_HELPER_AVX256_OP
#undef UNPCK_OP
