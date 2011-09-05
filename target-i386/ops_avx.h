/* 
 * Three operand 128/256bit AVX versions of SSE ops plus
 * new AVX instructions.
 * 
 * Operations expanded by special macro trees are still defined in ops_sse.h.
 * 
 * Should be compiled with unrolling for best performance.
 */

#if OP == 256
#define AVX256_OR_CLEAR(d, ...) __VA_ARGS__
#define ASUFFIX _256
#define NUM_S 8
#define NUM_L 8
#define NUM_D 4
#define AVX128_CLEAR_UPPER(d)
#define AVX128_ONLY(...)
#define AVX256_ONLY(...) __VA_ARGS__
#define SHIFT poisoned
#else
#define AVX256_OR_CLEAR(d, ...) avx_clear_upper(d);
#define ASUFFIX _avx
#define NUM_S 4
#define NUM_L 4
#define NUM_D 2
#define AVX128_CLEAR_UPPER(d) avx_clear_upper(d)
#define AVX128_ONLY(...) __VA_ARGS__
#define AVX256_ONLY(...)
#define SHIFT 1
#endif

#define Reg XMMReg
#define B(n) XMM_B(n)
#define W(n) XMM_W(n)
#define L(n) XMM_L(n)
#define Q(n) XMM_Q(n)

#if OP == 128
#define AVX128_SIMPLE(x,y)			\
	void helper_##x##_avx(Reg *d, Reg *s) {	\
	    avx_clear_upper(d);			\
	    helper_##x##y(d, s);             \
	}

AVX128_SIMPLE(ucomisd,)
AVX128_SIMPLE(ucomiss,)

void helper_pmuludq_avx(Reg *d, Reg *a, Reg *b)
{
    d->Q(0) = (uint64_t)a->L(0) * (uint64_t)b->L(0);
    d->Q(1) = (uint64_t)a->L(2) * (uint64_t)b->L(2);
    avx_clear_upper(d);
}

void helper_pmaddwd_avx (Reg *d, Reg *a, Reg *b)
{
    int i;

    for(i = 0; i < (2 << SHIFT); i++) {
        d->L(i) = (int16_t)a->W(2*i) * (int16_t)b->W(2*i) +
            (int16_t)a->W(2*i+1) * (int16_t)b->W(2*i+1);
    }
    avx_clear_upper(d);
}

void helper_psadbw_avx(Reg *d, Reg *a, Reg *b)
{
    unsigned int val;

    val = 0;
    val += abs1(a->B(0) - b->B(0));
    val += abs1(a->B(1) - b->B(1));
    val += abs1(a->B(2) - b->B(2));
    val += abs1(a->B(3) - b->B(3));
    val += abs1(a->B(4) - b->B(4));
    val += abs1(a->B(5) - b->B(5));
    val += abs1(a->B(6) - b->B(6));
    val += abs1(a->B(7) - b->B(7));
    d->Q(0) = val;
    val = 0;
    val += abs1(a->B(8) - b->B(8));
    val += abs1(a->B(9) - b->B(9));
    val += abs1(a->B(10) - b->B(10));
    val += abs1(a->B(11) - b->B(11));
    val += abs1(a->B(12) - b->B(12));
    val += abs1(a->B(13) - b->B(13));
    val += abs1(a->B(14) - b->B(14));
    val += abs1(a->B(15) - b->B(15));
    d->Q(1) = val;
    avx_clear_upper(d);
}

void helper_movl_mm_T0_avx(Reg *d, uint32_t val)
{
    d->L(0) = val;
    d->L(1) = 0;
    d->Q(1) = 0;
    avx_clear_upper(d);
}

#ifdef TARGET_X86_64
void helper_movq_mm_T0_avx(Reg *d, uint64_t val)
{
    d->Q(0) = val;
    d->Q(1) = 0;
    avx_clear_upper(d);
}
#endif



// XXX imm
#define PSHIFT_OP(name,op,field,type)			\
void helper_ ## name ## _avx(Reg *d, Reg *a, Reg *b)	\
{							\
    int shift;						\
    int i;						\
							\
    if (b->Q(0) > sizeof(type)*8-1) {			\
        d->Q(0) = 0;					\
        d->Q(1) = 0;					\
    } else {						\
        shift = b->B(0);				\
	for (i = 0; i < 16 / sizeof(type); i++)		\
	    d->field(i) = (type)a->field(i) op shift;	\
    }							\
    avx_clear_upper(d);					\
}

PSHIFT_OP(psrlw, >>, W, uint16_t)
PSHIFT_OP(psraw, >>, W, int16_t)
PSHIFT_OP(psllw, <<, W, uint16_t)
PSHIFT_OP(psrld, >>, L, uint32_t)
PSHIFT_OP(psrad, >>, L, int32_t)
PSHIFT_OP(pslld, <<, L, uint32_t)
PSHIFT_OP(psrlq, >>, Q, uint64_t)
PSHIFT_OP(psllq, <<, Q, uint64_t)
#undef PSHIFT_OP

void helper_psrldq_avx(Reg *d, Reg *a, Reg *b)
{
    int shift, i;

    shift = b->L(0);
    if (shift > 16)
        shift = 16;
    for(i = 0; i < 16 - shift; i++)
        d->B(i) = a->B(i + shift);
    for(i = 16 - shift; i < 16; i++)
        d->B(i) = 0;
    avx_clear_upper(d);
}

void helper_pslldq_avx(Reg *d, Reg *a, Reg *b)
{
    int shift, i;

    shift = b->L(0);
    if (shift > 16)
        shift = 16;
    for(i = 15; i >= shift; i--)
        d->B(i) = a->B(i - shift);
    for(i = 0; i < shift; i++)
        d->B(i) = 0;
    avx_clear_upper(d);
}
#endif

void glue(helper_cvtpd2ps, ASUFFIX)(Reg *d, Reg *a)
{
    d->XMM_S(0) = float64_to_float32(a->XMM_D(0), &env->sse_status);
    d->XMM_S(1) = float64_to_float32(a->XMM_D(1), &env->sse_status);
    AVX256_OR_CLEAR(d, 
    d->XMM_S(2) = float64_to_float32(a->XMM_D(2), &env->sse_status);
    d->XMM_S(3) = float64_to_float32(a->XMM_D(3), &env->sse_status);)
}

void glue(helper_cvtps2pd, ASUFFIX)(Reg *d, Reg *s)
{
    d->XMM_D(0) = float32_to_float64(s->XMM_S(0), &env->sse_status);
    d->XMM_D(1) = float32_to_float64(s->XMM_S(1), &env->sse_status);
    AVX256_OR_CLEAR(d,
    d->XMM_D(2) = float32_to_float64(s->XMM_S(2), &env->sse_status);
    d->XMM_D(3) = float32_to_float64(s->XMM_S(3), &env->sse_status);)
}

#if OP == 128
void helper_cvtss2sd_avx(Reg *d, Reg *a, Reg *b)
{
    d->XMM_D(0) = float32_to_float64(b->XMM_S(0), &env->sse_status);
    d->Q(1) = a->Q(1);
    avx_clear_upper(d);
}

void helper_cvtsd2ss_avx(Reg *d, Reg *a, Reg *b)
{
    d->XMM_S(0) = float64_to_float32(b->XMM_D(0), &env->sse_status);
    d->XMM_S(1) = a->XMM_S(1);
    d->XMM_Q(1) = a->XMM_Q(1);
    avx_clear_upper(d);
}
#endif

void glue(helper_cvtdq2ps, ASUFFIX)(Reg *d, Reg *s)
{
    int i;
    for (i = 0; i < NUM_S; i++)
	d->XMM_S(i) = int32_to_float32(s->XMM_L(i), &env->sse_status);
    AVX128_CLEAR_UPPER(d);
}

void glue(helper_cvtdq2pd, ASUFFIX)(Reg *d, Reg *s)
{
    int i;
    for (i = 0; i < NUM_D; i++) {
	int32_t l = (int32_t)s->XMM_L(i);
	d->XMM_D(i) = int32_to_float64(l, &env->sse_status);
    }
    AVX128_CLEAR_UPPER(d);
}

#if OP == 128
void helper_cvtsi2ss_avx(Reg *d, uint32_t val)
{
    d->XMM_S(0) = int32_to_float32(val, &env->sse_status);
    avx_clear_upper(d);
}

void helper_cvtsi2sd_avx(Reg *d, uint32_t val)
{
    d->XMM_D(0) = int32_to_float64(val, &env->sse_status);
    avx_clear_upper(d);
}

#ifdef TARGET_X86_64
void helper_cvtsq2ss_avx(Reg *d, uint64_t val)
{
    d->XMM_S(0) = int64_to_float32(val, &env->sse_status);
    avx_clear_upper(d);
}

void helper_cvtsq2sd_avx(Reg *d, uint64_t val)
{
    d->XMM_D(0) = int64_to_float64(val, &env->sse_status);
    avx_clear_upper(d);
}
#endif
#endif

void glue(helper_cvtps2dq, ASUFFIX)(Reg *d, Reg *s)
{
    int i;
    for (i = 0; i < NUM_L; i++)
	d->XMM_L(i) = float32_to_int32(s->XMM_S(i), &env->sse_status);
    AVX128_CLEAR_UPPER(d);
}

void glue(helper_cvtpd2dq, ASUFFIX)(Reg *d, Reg *s)
{
    d->XMM_L(0) = float64_to_int32(s->XMM_D(0), &env->sse_status);
    d->XMM_L(1) = float64_to_int32(s->XMM_D(1), &env->sse_status);
    AVX128_ONLY(d->XMM_Q(1) = 0);    
    AVX256_OR_CLEAR(d,
    d->XMM_L(2) = float64_to_int32(s->XMM_D(2), &env->sse_status);
    d->XMM_L(3) = float64_to_int32(s->XMM_D(3), &env->sse_status);
    d->XMM_Q(2) = d->XMM_Q(3) = 0;);
}

void glue(helper_cvttps2dq, ASUFFIX)(Reg *d, Reg *s)
{
    int i;
    for (i = 0; i < NUM_L; i++)
	d->XMM_L(i) = float32_to_int32_round_to_zero(s->XMM_S(i), 
						     &env->sse_status);
    AVX128_CLEAR_UPPER(d);
}

void glue(helper_cvttpd2dq, ASUFFIX)(Reg *d, Reg *s)
{
    int i;
    for (i = 0; i < NUM_L; i++) 
	d->XMM_L(i) = float64_to_int32_round_to_zero(s->XMM_D(i),
						     &env->sse_status);
    AVX128_ONLY(d->XMM_Q(1) = 0);
    AVX256_ONLY(avx_clear_upper(d));
}

void glue(helper_rsqrtps, ASUFFIX)(Reg *d, Reg *s)
{
    int i;
    for (i = 0; i < NUM_S; i++)
	d->XMM_S(i) = float32_div(float32_one,
				  float32_sqrt(s->XMM_S(i), &env->sse_status),
				  &env->sse_status);
    AVX128_CLEAR_UPPER(d);
}

#if OP == 128
void helper_rsqrtss_avx(Reg *d, Reg *s)
{
    d->XMM_S(0) = float32_div(float32_one,
                              float32_sqrt(s->XMM_S(0), &env->sse_status),
                              &env->sse_status);
    avx_clear_upper(d);
}
#endif

void glue(helper_rcpps, ASUFFIX)(Reg *d, Reg *s)
{
    int i;
    for (i = 0; i < NUM_S; i++)
	d->XMM_S(i) = float32_div(float32_one, s->XMM_S(i), &env->sse_status);;
    AVX128_CLEAR_UPPER(d);
}

#if OP == 128
void helper_rcpss_avx(Reg *d, Reg *s)
{
    d->XMM_S(0) = float32_div(float32_one, s->XMM_S(0), &env->sse_status);;
    AVX128_CLEAR_UPPER(d);
}

void helper_extrq_r_avx(Reg *d, Reg *s)
{
    d->XMM_Q(0) = helper_extrq(d->XMM_Q(0), s->XMM_B(1), s->XMM_B(0));
    AVX128_CLEAR_UPPER(d);
}

void helper_extrq_i_avx(Reg *d, int index, int length)
{
    d->XMM_Q(0) = helper_extrq(d->XMM_Q(0), index, length);
    AVX128_CLEAR_UPPER(d);
}

void helper_insertq_r_avx(Reg *d, Reg *s)
{
    d->XMM_Q(0) = helper_insertq(s->XMM_Q(0), s->XMM_B(9), s->XMM_B(8));
    AVX128_CLEAR_UPPER(d);
}

void helper_insertq_i_avx(Reg *d, int index, int length)
{
    d->XMM_Q(0) = helper_insertq(d->XMM_Q(0), index, length);
    AVX128_CLEAR_UPPER(d);
}
#endif

void glue(helper_haddps, ASUFFIX)(Reg *d, Reg *a, Reg *b)
{
    d->XMM_S(0) = a->XMM_S(0) + a->XMM_S(1);
    d->XMM_S(1) = a->XMM_S(2) + a->XMM_S(3);
    d->XMM_S(2) = b->XMM_S(0) + b->XMM_S(1);
    d->XMM_S(3) = b->XMM_S(2) + b->XMM_S(3);
    AVX128_CLEAR_UPPER(d);
}

void glue(helper_haddpd, ASUFFIX)(Reg *d, Reg *a, Reg *b)
{
    d->XMM_D(0) = a->XMM_D(0) + a->XMM_D(1);
    d->XMM_D(1) = b->XMM_D(0) + b->XMM_D(1);
    AVX128_CLEAR_UPPER(d);
}

void glue(helper_hsubps, ASUFFIX)(Reg *d, Reg *a, Reg *b)
{
    d->XMM_S(0) = a->XMM_S(0) - a->XMM_S(1);
    d->XMM_S(1) = a->XMM_S(2) - a->XMM_S(3);
    d->XMM_S(2) = b->XMM_S(0) - b->XMM_S(1);
    d->XMM_S(3) = b->XMM_S(2) - b->XMM_S(3);
    AVX256_OR_CLEAR(d,
    d->XMM_S(4) = a->XMM_S(4) - a->XMM_S(5);
    d->XMM_S(5) = a->XMM_S(6) - a->XMM_S(7);
    d->XMM_S(6) = b->XMM_S(4) - b->XMM_S(5);
    d->XMM_S(7) = b->XMM_S(6) - b->XMM_S(7));
}

void glue(helper_hsubpd, ASUFFIX)(Reg *d, Reg *a, Reg *b)
{
    d->XMM_D(0) = a->XMM_D(0) - a->XMM_D(1);
    d->XMM_D(1) = b->XMM_D(0) - b->XMM_D(1);
    AVX256_OR_CLEAR(d,
    d->XMM_D(2) = a->XMM_D(0) - a->XMM_D(2);
    d->XMM_D(3) = b->XMM_D(3) - b->XMM_D(3));
}

void glue(helper_addsubps, ASUFFIX)(Reg *d, Reg *a, Reg *b)
{
    int i;
    for (i = 0; i < NUM_S; i += 2) {
	d->XMM_S(i) = b->XMM_S(i) - a->XMM_S(i);
	d->XMM_S(i + 1) = b->XMM_S(i + 1) + a->XMM_S(i + 1);
    }
    AVX128_CLEAR_UPPER(d);
}

void glue(helper_addsubpd, ASUFFIX)(Reg *d, Reg *a, Reg *b)
{
    int i;
    for (i = 0; i < NUM_D; i += 2) {
	d->XMM_D(i) = b->XMM_D(i) - a->XMM_D(i);
	d->XMM_D(i+1) = b->XMM_D(i+1) + a->XMM_D(i+1);
    }
    AVX128_CLEAR_UPPER(d);
}

#define helper_ucomisd_avx helper_ucomisd_xmm
#define helper_comisd_avx helper_comisd_xmm
#define helper_comiss_avx helper_comiss_xmm
#define helper_ucomiss_avx helper_ucomiss_xmm

uint32_t glue(helper_movmskps, ASUFFIX)(Reg *s)
{
    uint32_t v = 0;
    int i, b;
    for (i = NUM_L-1; i >= 0; i--) {
	b = s->XMM_L(i) >> 31;
	v = (v << 1) | b;
    }
    return v;
}

uint32_t glue(helper_movmskpd, ASUFFIX)(Reg *s)
{
    int b0, b1;
    uint32_t v;
    b0 = s->XMM_L(1) >> 31;
    b1 = s->XMM_L(3) >> 31;
    v = b0 | (b1 << 1);
#if OP == 256
    b0 = s->XMM_L(5) >> 31;
    b1 = s->XMM_L(7) >> 31;
    v |= (b0 << 2) | (b1 << 3);
#endif
    return v;
}

#if OP == 128

#define helper_pmovmskb_avx helper_pmovmskb_xmm

void helper_packsswb_avx (Reg *d, Reg *a, Reg *b)
{
    d->B(0) = satsb((int16_t)a->W(0));
    d->B(1) = satsb((int16_t)a->W(1));
    d->B(2) = satsb((int16_t)a->W(2));
    d->B(3) = satsb((int16_t)a->W(3));
    d->B(4) = satsb((int16_t)a->W(4));
    d->B(5) = satsb((int16_t)a->W(5));
    d->B(6) = satsb((int16_t)a->W(6));
    d->B(7) = satsb((int16_t)a->W(7));
    d->B((4 << SHIFT) + 0) = satsb((int16_t)b->W(0));
    d->B((4 << SHIFT) + 1) = satsb((int16_t)b->W(1));
    d->B((4 << SHIFT) + 2) = satsb((int16_t)b->W(2));
    d->B((4 << SHIFT) + 3) = satsb((int16_t)b->W(3));
    d->B(12) = satsb((int16_t)b->W(4));
    d->B(13) = satsb((int16_t)b->W(5));
    d->B(14) = satsb((int16_t)b->W(6));
    d->B(15) = satsb((int16_t)b->W(7));
    avx_clear_upper(d);
}

void helper_packuswb_avx(Reg *d, Reg *a, Reg *b)
{
    d->B(0) = satub((int16_t)a->W(0));
    d->B(1) = satub((int16_t)a->W(1));
    d->B(2) = satub((int16_t)a->W(2));
    d->B(3) = satub((int16_t)a->W(3));
    d->B(4) = satub((int16_t)a->W(4));
    d->B(5) = satub((int16_t)a->W(5));
    d->B(6) = satub((int16_t)a->W(6));
    d->B(7) = satub((int16_t)a->W(7));
    d->B((4 << SHIFT) + 0) = satub((int16_t)b->W(0));
    d->B((4 << SHIFT) + 1) = satub((int16_t)b->W(1));
    d->B((4 << SHIFT) + 2) = satub((int16_t)b->W(2));
    d->B((4 << SHIFT) + 3) = satub((int16_t)b->W(3));
    d->B(12) = satub((int16_t)b->W(4));
    d->B(13) = satub((int16_t)b->W(5));
    d->B(14) = satub((int16_t)b->W(6));
    d->B(15) = satub((int16_t)b->W(7));
    avx_clear_upper(d);
}

void helper_packssdw_avx(Reg *d, Reg *a, Reg *b)
{
    d->W(0) = satsw(a->L(0));
    d->W(1) = satsw(a->L(1));
    d->W(2) = satsw(a->L(2));
    d->W(3) = satsw(a->L(3));
    d->W((2 << SHIFT) + 0) = satsw(b->L(0));
    d->W((2 << SHIFT) + 1) = satsw(b->L(1));
    d->W(6) = satsw(b->L(2));
    d->W(7) = satsw(b->L(3));
    avx_clear_upper(d);
}

#define UNPCK_OP_AVX(base_name, base)					\
void helper_punpck ## base_name ## bw_avx(Reg *d, Reg *a, Reg *b)	\
{									\
    d->B(0)  = a->B((base << (SHIFT + 2)) + 0);				\
    d->B(1)  = b->B((base << (SHIFT + 2)) + 0);				\
    d->B(2)  = a->B((base << (SHIFT + 2)) + 1);				\
    d->B(3)  = b->B((base << (SHIFT + 2)) + 1);				\
    d->B(4)  = a->B((base << (SHIFT + 2)) + 2);				\
    d->B(5)  = b->B((base << (SHIFT + 2)) + 2);				\
    d->B(6)  = a->B((base << (SHIFT + 2)) + 3);				\
    d->B(7)  = b->B((base << (SHIFT + 2)) + 3);				\
    d->B(8)  = a->B((base << (SHIFT + 2)) + 4);				\
    d->B(9)  = b->B((base << (SHIFT + 2)) + 4);				\
    d->B(10) = a->B((base << (SHIFT + 2)) + 5);				\
    d->B(11) = b->B((base << (SHIFT + 2)) + 5);				\
    d->B(12) = a->B((base << (SHIFT + 2)) + 6);				\
    d->B(13) = b->B((base << (SHIFT + 2)) + 6);				\
    d->B(14) = a->B((base << (SHIFT + 2)) + 7);				\
    d->B(15) = b->B((base << (SHIFT + 2)) + 7);				\
    avx_clear_upper(d);							\
}									\
									\
void helper_punpck ## base_name ## wd_avx(Reg *d, Reg *a, Reg *b)	\
{									\
    d->W(0) = a->W((base << (SHIFT + 1)) + 0);				\
    d->W(1) = b->W((base << (SHIFT + 1)) + 0);				\
    d->W(2) = a->W((base << (SHIFT + 1)) + 1);				\
    d->W(3) = b->W((base << (SHIFT + 1)) + 1);				\
    d->W(4) = a->W((base << (SHIFT + 1)) + 2);				\
    d->W(5) = b->W((base << (SHIFT + 1)) + 2);				\
    d->W(6) = a->W((base << (SHIFT + 1)) + 3);				\
    d->W(7) = b->W((base << (SHIFT + 1)) + 3);				\
    avx_clear_upper(d);							\
}									\
									\
void helper_punpck ## base_name ## dq_avx(Reg *d, Reg *a, Reg *b)	\
{									\
    d->L(0) = a->L((base << SHIFT) + 0);				\
    d->L(1) = b->L((base << SHIFT) + 0);				\
    d->L(2) = a->L((base << SHIFT) + 1);				\
    d->L(3) = b->L((base << SHIFT) + 1);				\
    avx_clear_upper(d);							\
}									\
									\
void helper_punpck ## base_name ## qdq_avx(Reg *d, Reg *a, Reg *b)	\
{									\
    d->Q(0) = a->Q(base);						\
    d->Q(1) = b->Q(base);						\
    avx_clear_upper(d);							\
}

UNPCK_OP_AVX(l, 0)
UNPCK_OP_AVX(h, 1)

#undef UNPCK_OP_AVX

void helper_pshufb_avx(Reg *d, Reg *a, Reg *b)
{
    int i;
    for (i = 0; i < (8 << SHIFT); i++)
        d->B(i) = (b->B(i) & 0x80) ? 0 : (a->B(b->B(i) & ((8 << SHIFT) - 1)));
    avx_clear_upper(d);
}

void helper_phaddw_avx(Reg *d, Reg *a, Reg *b)
{
    d->W(0) = (int16_t)a->W(0) + (int16_t)a->W(1);
    d->W(1) = (int16_t)a->W(2) + (int16_t)a->W(3);
    d->W(2) = (int16_t)a->W(4) + (int16_t)a->W(5);
    d->W(3) = (int16_t)a->W(6) + (int16_t)a->W(7);
    d->W((2 << SHIFT) + 0) = (int16_t)b->W(0) + (int16_t)b->W(1);
    d->W((2 << SHIFT) + 1) = (int16_t)b->W(2) + (int16_t)b->W(3);
    d->W(6) = (int16_t)b->W(4) + (int16_t)b->W(5);
    d->W(7) = (int16_t)b->W(6) + (int16_t)b->W(7);
    avx_clear_upper(d);
}

void helper_phaddd_avx(Reg *d, Reg *a, Reg *b)
{
    d->L(0) = (int32_t)a->L(0) + (int32_t)a->L(1);
    d->L(1) = (int32_t)a->L(2) + (int32_t)a->L(3);
    d->L((1 << SHIFT) + 0) = (int32_t)b->L(0) + (int32_t)b->L(1);
    d->L(3) = (int32_t)b->L(2) + (int32_t)b->L(3);
    avx_clear_upper(d);
}

void helper_phaddsw_avx(Reg *d, Reg *a, Reg *b)
{
    d->W(0) = satsw((int16_t)a->W(0) + (int16_t)a->W(1));
    d->W(1) = satsw((int16_t)a->W(2) + (int16_t)a->W(3));
    d->W(2) = satsw((int16_t)a->W(4) + (int16_t)a->W(5));
    d->W(3) = satsw((int16_t)a->W(6) + (int16_t)a->W(7));
    d->W((2 << SHIFT) + 0) = satsw((int16_t)b->W(0) + (int16_t)b->W(1));
    d->W((2 << SHIFT) + 1) = satsw((int16_t)b->W(2) + (int16_t)b->W(3));
    d->W(6) = satsw((int16_t)b->W(4) + (int16_t)b->W(5));
    d->W(7) = satsw((int16_t)b->W(6) + (int16_t)b->W(7));
    avx_clear_upper(d);
}

void helper_pmaddubsw_avx(Reg *d, Reg *a, Reg *b)
{
    d->W(0) = satsw((int8_t)b->B( 0) * (uint8_t)a->B( 0) +
                    (int8_t)b->B( 1) * (uint8_t)a->B( 1));
    d->W(1) = satsw((int8_t)b->B( 2) * (uint8_t)a->B( 2) +
                    (int8_t)b->B( 3) * (uint8_t)a->B( 3));
    d->W(2) = satsw((int8_t)b->B( 4) * (uint8_t)a->B( 4) +
                    (int8_t)b->B( 5) * (uint8_t)a->B( 5));
    d->W(3) = satsw((int8_t)b->B( 6) * (uint8_t)a->B( 6) +
                    (int8_t)b->B( 7) * (uint8_t)a->B( 7));
    d->W(4) = satsw((int8_t)b->B( 8) * (uint8_t)a->B( 8) +
                    (int8_t)b->B( 9) * (uint8_t)a->B( 9));
    d->W(5) = satsw((int8_t)b->B(10) * (uint8_t)a->B(10) +
                    (int8_t)b->B(11) * (uint8_t)a->B(11));
    d->W(6) = satsw((int8_t)b->B(12) * (uint8_t)a->B(12) +
                    (int8_t)b->B(13) * (uint8_t)a->B(13));
    d->W(7) = satsw((int8_t)b->B(14) * (uint8_t)a->B(14) +
                    (int8_t)b->B(15) * (uint8_t)a->B(15));
    avx_clear_upper(d);
}

void helper_phsubw_avx(Reg *d, Reg *a, Reg *b)
{
    d->W(0) = (int16_t)a->W(0) - (int16_t)a->W(1);
    d->W(1) = (int16_t)a->W(2) - (int16_t)a->W(3);
    d->W(2) = (int16_t)a->W(4) - (int16_t)a->W(5);
    d->W(3) = (int16_t)a->W(6) - (int16_t)a->W(7);
    d->W((2 << SHIFT) + 0) = (int16_t)b->W(0) - (int16_t)b->W(1);
    d->W((2 << SHIFT) + 1) = (int16_t)b->W(2) - (int16_t)b->W(3);
    d->W(6) = (int16_t)b->W(4) - (int16_t)b->W(5);
    d->W(7) = (int16_t)b->W(6) - (int16_t)b->W(7);
    avx_clear_upper(d);
}

void helper_phsubd_avx(Reg *d, Reg *a, Reg *b)
{
    d->L(0) = (int32_t)a->L(0) - (int32_t)a->L(1);
    d->L(1) = (int32_t)a->L(2) - (int32_t)a->L(3);
    d->L((1 << SHIFT) + 0) = (int32_t)b->L(0) - (int32_t)b->L(1);
    d->L(3) = (int32_t)b->L(2) - (int32_t)b->L(3);
    avx_clear_upper(d);
}

void helper_phsubsw_avx(Reg *d, Reg *a, Reg *b)
{
    d->W(0) = satsw((int16_t)a->W(0) - (int16_t)a->W(1));
    d->W(1) = satsw((int16_t)a->W(2) - (int16_t)a->W(3));
    d->W(2) = satsw((int16_t)a->W(4) - (int16_t)a->W(5));
    d->W(3) = satsw((int16_t)a->W(6) - (int16_t)a->W(7));
    d->W((2 << SHIFT) + 0) = satsw((int16_t)b->W(0) - (int16_t)b->W(1));
    d->W((2 << SHIFT) + 1) = satsw((int16_t)b->W(2) - (int16_t)b->W(3));
    d->W(6) = satsw((int16_t)b->W(4) - (int16_t)b->W(5));
    d->W(7) = satsw((int16_t)b->W(6) - (int16_t)b->W(7));
    avx_clear_upper(d);
}
#endif

#if 0
// xxx can we pass 64bit?
void glue(helper_vbroadcastsd, ASUFFIX)(Reg *d, uint64_t m)
{
	d->Q(0) = m;
	d->Q(1) = m;
	AVX256_OR_CLEAR(d, d->Q(2) = d->Q(3) = m);
}

void glue(helper_vbroadcastss, ASUFFIX)(Reg *d, uint32_t m)
{
	d->L(0) = m;
	d->L(1) = m;
	d->L(2) = m;
	d->L(3) = m;
	AVX256_OR_CLEAR(d, d->L(4) = d->L(5) = d->L(6) = d->L(7) = m);
}
#endif

// XXX 128 bit input


#undef Reg
#undef B
#undef W
#undef L
#undef Q
#undef SHIFT
#undef AVX256_OR_CLEAR
#undef ASUFFIX
#undef NUM_S
#undef NUM_L
#undef NUM_D
#undef AVX128_CLEAR_UPPER
#undef AVX128_ONLY
#undef AVX256_ONLY
