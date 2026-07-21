/* The printf family. One engine behind a sink callback; every public entry
 * is a thin wrapper.
 *
 * Floating output is exact. A double is m * 2^e with m < 2^53, so its
 * decimal expansion is finite; the converter below computes it with a small
 * bignum (base 10^9 limbs) and rounds half-to-even at the requested digit,
 * which is what a correctly rounded CRT prints. No tables of powers, no
 * guesswork at the last digit. */
#include "c99rt.h"

typedef char *rt_valist;
#define rt_va_start(ap, last) __builtin_va_start(ap, last)
#define rt_va_arg(ap, ty) __builtin_va_arg(ap, ty)
#define rt_va_end(ap) __builtin_va_end(ap)

#define RT_NULL ((void *)0)

typedef struct _C99MTLC_FILE FILE_;
rt_size fwrite(const void *, rt_size, rt_size, FILE_ *);
FILE_ *__c99m_iob(int);
rt_size strlen(const char *);
void *memcpy(void *, const void *, rt_size);
void *memset(void *, int, rt_size);

/* ------------------------------------------------------------------ sink */

typedef struct {
  /* out == NULL: count only (snprintf overflow tail). */
  char *out;
  rt_size cap;    /* bytes usable at out, when out is set */
  rt_size want;   /* everything the format produced */
  FILE_ *stream;  /* when writing to a FILE instead of memory */
  int failed;
} Sink;

static void sink_bytes(Sink *k, const char *p, rt_size n) {
  if (k->stream) {
    if (n && fwrite(p, 1, n, k->stream) != n)
      k->failed = 1;
    k->want += n;
    return;
  }
  if (k->out && k->want < k->cap) {
    rt_size room = k->cap - k->want;
    rt_size take = n < room ? n : room;
    memcpy(k->out + k->want, p, take);
  }
  k->want += n;
}

static void sink_ch(Sink *k, char c, rt_size repeat) {
  char pad[32];
  memset(pad, c, sizeof(pad));
  while (repeat) {
    rt_size take = repeat < sizeof(pad) ? repeat : sizeof(pad);
    sink_bytes(k, pad, take);
    repeat -= take;
  }
}

/* --------------------------------------------------------------- integers */

static int rt_utoa(char *out, unsigned long long v, int base, int upper) {
  const char *digits =
      upper ? "0123456789ABCDEF" : "0123456789abcdef";
  char tmp[64];
  int n = 0, i;
  if (v == 0)
    tmp[n++] = '0';
  while (v) {
    tmp[n++] = digits[v % (unsigned)base];
    v /= (unsigned)base;
  }
  for (i = 0; i < n; i++)
    out[i] = tmp[n - 1 - i];
  return n;
}

/* ----------------------------------------------------- exact float digits */

/* value = m * 2^e2, m < 2^53. Limbs are base 1e9, little-endian. Enough
 * limbs for 2^1074 scaled by 10^17: about 340 decimal digits either side. */
#define BN_CAP 100

typedef struct {
  unsigned limb[BN_CAP];
  int n; /* used limbs; 0 means value 0 */
} Bn;

static void bn_from_u64(Bn *b, unsigned long long v) {
  b->n = 0;
  while (v) {
    b->limb[b->n++] = (unsigned)(v % 1000000000ull);
    v /= 1000000000ull;
  }
}

/* b *= 2^k */
static void bn_shl(Bn *b, int k) {
  while (k > 0) {
    int step = k > 29 ? 29 : k; /* limb * 2^29 still fits in 64 bits */
    unsigned long long carry = 0;
    int i;
    for (i = 0; i < b->n; i++) {
      unsigned long long t =
          ((unsigned long long)b->limb[i] << step) + carry;
      b->limb[i] = (unsigned)(t % 1000000000ull);
      carry = t / 1000000000ull;
    }
    while (carry) {
      b->limb[b->n++] = (unsigned)(carry % 1000000000ull);
      carry /= 1000000000ull;
    }
    k -= step;
  }
}

/* b /= 2^k, returns 1 if any bit shifted out was nonzero (sticky) */
static int bn_shr(Bn *b, int k) {
  int sticky = 0;
  while (k > 0) {
    int step = k > 29 ? 29 : k;
    unsigned long long rem = 0;
    int i;
    for (i = b->n - 1; i >= 0; i--) {
      unsigned long long t = rem * 1000000000ull + b->limb[i];
      b->limb[i] = (unsigned)(t >> step);
      rem = t & (((unsigned long long)1 << step) - 1);
    }
    if (rem)
      sticky = 1;
    while (b->n > 0 && b->limb[b->n - 1] == 0)
      b->n--;
    k -= step;
  }
  return sticky;
}

/* b *= 10^k (k >= 0) */
static void bn_mul_pow10(Bn *b, int k) {
  while (k > 0) {
    int step = k > 9 ? 9 : k;
    unsigned long long mul = 1;
    unsigned long long carry = 0;
    int i;
    for (i = 0; i < step; i++)
      mul *= 10;
    for (i = 0; i < b->n; i++) {
      unsigned long long t = (unsigned long long)b->limb[i] * mul + carry;
      b->limb[i] = (unsigned)(t % 1000000000ull);
      carry = t / 1000000000ull;
    }
    while (carry) {
      b->limb[b->n++] = (unsigned)(carry % 1000000000ull);
      carry /= 1000000000ull;
    }
    k -= step;
  }
}

static int bn_is_zero(const Bn *b) { return b->n == 0; }

/* decimal digits of b, most significant first; returns count */
static int bn_digits(const Bn *b, char *out) {
  char tmp[BN_CAP * 9 + 1];
  int n = 0, i;
  if (b->n == 0) {
    out[0] = '0';
    return 1;
  }
  /* top limb without leading zeros, the rest zero-padded to 9 */
  n += rt_utoa(tmp + n, b->limb[b->n - 1], 10, 0);
  for (i = b->n - 2; i >= 0; i--) {
    char d9[16];
    int len = rt_utoa(d9, b->limb[i], 10, 0);
    int pad = 9 - len;
    memset(tmp + n, '0', (rt_size)pad);
    n += pad;
    memcpy(tmp + n, d9, (rt_size)len);
    n += len;
  }
  memcpy(out, tmp, (rt_size)n);
  return n;
}

/* b += 1 at digit position `at` counting from the LEAST significant of the
 * digit string of length len (a carry into the rounding position). The digit
 * string lives in `dig`; returns new length (1 more when 999... rolls over). */
static int rt_round_up(char *dig, int len) {
  int i = len - 1;
  for (;;) {
    if (i < 0) {
      /* 999 -> 1000 */
      int j;
      for (j = len; j > 0; j--)
        dig[j] = dig[j - 1];
      dig[0] = '1';
      return len + 1;
    }
    if (dig[i] != '9') {
      dig[i]++;
      return len;
    }
    dig[i] = '0';
    i--;
  }
}

/* Decompose a double. Returns the class: 0 finite, 1 inf, 2 nan.
 * Finite: *neg, mantissa m (with the implicit bit), exponent e2 so that
 * |x| = m * 2^e2. Zero comes out m == 0. */
static int rt_unpack(double x, int *neg, unsigned long long *m, int *e2) {
  unsigned long long bits;
  unsigned long long frac;
  int exp;
  memcpy(&bits, &x, 8);
  *neg = (int)(bits >> 63);
  exp = (int)((bits >> 52) & 0x7FF);
  frac = bits & 0xFFFFFFFFFFFFFull;
  if (exp == 0x7FF)
    return frac ? 2 : 1;
  if (exp == 0) {
    *m = frac; /* denormal: no implicit bit */
    *e2 = -1074;
  } else {
    *m = frac | (1ull << 52);
    *e2 = exp - 1075;
  }
  return 0;
}

/* All significant decimal digits of |x| (x finite, nonzero), plus the
 * position of the decimal point: |x| = 0.D1 D2 ... Dn * 10^decpt.
 * The digits are exact and end at the last nonzero one. */
static int rt_exact_digits(unsigned long long m, int e2, char *dig,
                           int *decpt) {
  Bn b;
  int len;
  int frac_digits = 0;
  bn_from_u64(&b, m);
  if (e2 >= 0) {
    bn_shl(&b, e2);
  } else {
    /* m/2^k = m*5^k / 10^k: multiply by 5^k, then the low k digits are
     * the fraction. 5^k = 10^k / 2^k. */
    bn_mul_pow10(&b, -e2);
    bn_shr(&b, -e2); /* exact: m*10^k has k factors of 2 to give */
    frac_digits = -e2;
  }
  len = bn_digits(&b, dig);
  *decpt = len - frac_digits;
  /* strip trailing zeros; the callers re-pad as their format demands */
  while (len > 1 && dig[len - 1] == '0')
    len--;
  return len;
}

/* Round the exact digit string to `keep` significant digits, half-even.
 * Updates len and decpt. keep <= 0 rounds away everything: the result is
 * either "0" or "1" with decpt bumped. */
static int rt_round_digits(char *dig, int len, int *decpt, int keep) {
  int roundup = 0;
  if (keep >= len)
    return len;
  if (keep < 0)
    keep = 0;
  {
    char first = dig[keep];
    int rest_nonzero = 0;
    int i;
    for (i = keep + 1; i < len; i++)
      if (dig[i] != '0') {
        rest_nonzero = 1;
        break;
      }
    if (first > '5')
      roundup = 1;
    else if (first == '5') {
      if (rest_nonzero)
        roundup = 1;
      else if (keep > 0)
        roundup = (dig[keep - 1] - '0') & 1; /* half to even */
      else
        roundup = 0; /* 0.5 -> 0 at zero kept digits: even */
    }
  }
  len = keep;
  if (roundup) {
    if (len == 0) {
      dig[0] = '1';
      len = 1;
      (*decpt)++;
    } else {
      int newlen = rt_round_up(dig, len);
      if (newlen > len) {
        (*decpt)++;
        len = newlen;
        /* the extra digit is a trailing zero of the rollover */
        len--;
        dig[len] = 0;
      }
    }
  }
  if (len == 0) {
    dig[0] = '0';
    len = 1;
  }
  return len;
}

/* ------------------------------------------------------------- one number */

#define FL_MINUS 1u
#define FL_PLUS 2u
#define FL_SPACE 4u
#define FL_HASH 8u
#define FL_ZERO 16u

typedef struct {
  unsigned flags;
  int width;
  int prec; /* -1 = none given */
} Spec;

static void emit_padded(Sink *k, char sign, char prefix_a, char prefix_b,
                        const char *body, rt_size body_len, const Spec *sp,
                        int zero_pad_to) {
  rt_size prefix_len = prefix_a ? (prefix_b ? 2 : 1) : 0;
  rt_size core = (sign ? 1 : 0) + prefix_len + body_len;
  rt_size zeros = 0;
  rt_size width = sp->width > 0 ? (rt_size)sp->width : 0;

  if (zero_pad_to > 0 && (rt_size)zero_pad_to > body_len)
    zeros = (rt_size)zero_pad_to - body_len;
  core += zeros;

  if (!(sp->flags & FL_MINUS) && width > core) {
    if ((sp->flags & FL_ZERO) && zero_pad_to < 0) {
      /* zero fill counts after the sign */
      if (sign)
        sink_bytes(k, &sign, 1);
      if (prefix_a)
        sink_bytes(k, &prefix_a, 1);
      if (prefix_b)
        sink_bytes(k, &prefix_b, 1);
      sink_ch(k, '0', width - core);
      sink_ch(k, '0', zeros);
      sink_bytes(k, body, body_len);
      return;
    }
    sink_ch(k, ' ', width - core);
  }
  if (sign)
    sink_bytes(k, &sign, 1);
  if (prefix_a)
    sink_bytes(k, &prefix_a, 1);
  if (prefix_b)
    sink_bytes(k, &prefix_b, 1);
  sink_ch(k, '0', zeros);
  sink_bytes(k, body, body_len);
  if ((sp->flags & FL_MINUS) && width > core)
    sink_ch(k, ' ', width - core);
}

static void emit_float(Sink *k, double x, char conv, Spec sp) {
  char dig[800];
  int len, decpt;
  /* Initialized because they are filled in through their addresses below.
   * A backend fault loses that write here: rt_unpack's `*neg = ...` does not
   * reach this frame's slot, so an uninitialized `neg` made every positive
   * number print with a minus sign, and only at -O1, and only when the stack
   * happened to be dirty. See tests/diff/varargs_double.c and the note in the
   * known-gaps memory; initializing is correct in its own right, but it is
   * masking a real bug, not fixing it. */
  int neg = 0;
  unsigned long long m = 0;
  int e2 = 0;
  int cls;
  int prec = sp.prec >= 0 ? sp.prec : 6;
  char body[800 + 32];
  int bl = 0;
  char sign;
  int upper = (conv == 'E' || conv == 'F' || conv == 'G');
  char c = conv;

  if (upper)
    c = (char)(conv + 32);

  cls = rt_unpack(x, &neg, &m, &e2);
  sign = neg ? '-' : (sp.flags & FL_PLUS) ? '+'
                   : (sp.flags & FL_SPACE) ? ' '
                                           : 0;
  if (cls != 0) {
    const char *s;
    if (cls == 2)
      s = upper ? "NAN" : "nan";
    else
      s = upper ? "INF" : "inf";
    /* no zero padding for specials */
    sp.flags &= ~FL_ZERO;
    emit_padded(k, sign, 0, 0, s, 3, &sp, -1);
    return;
  }

  if (m == 0) {
    len = 1;
    dig[0] = '0';
    decpt = 1; /* 0.0 prints as "0" before the point */
  } else {
    len = rt_exact_digits(m, e2, dig, &decpt);
  }

  if (c == 'g') {
    /* choose %e or %f exactly as C99 7.19.6.1 says */
    int P = prec == 0 ? 1 : prec;
    int X;
    if (m == 0) {
      X = 0;
      len = rt_round_digits(dig, len, &decpt, P);
    } else {
      len = rt_round_digits(dig, len, &decpt, P);
      X = decpt - 1;
    }
    if (X >= -4 && X < P) {
      c = 'f';
      prec = P - 1 - X;
    } else {
      c = 'e';
      prec = P - 1;
    }
    if (!(sp.flags & FL_HASH)) {
      /* trim trailing zeros of the fraction */
      while (prec > 0) {
        int idx = (c == 'f') ? decpt + prec - 1 : prec;
        char d = (idx >= 0 && idx < len) ? dig[idx] : '0';
        if (d != '0')
          break;
        prec--;
      }
    }
  } else if (c == 'e') {
    if (m != 0)
      len = rt_round_digits(dig, len, &decpt, prec + 1);
  } else { /* f */
    /* keep decpt + prec digits (may be <= 0) */
    if (m != 0)
      len = rt_round_digits(dig, len, &decpt, decpt + prec);
  }

  if (c == 'f') {
    int i;
    if (decpt <= 0) {
      body[bl++] = '0';
    } else {
      for (i = 0; i < decpt; i++)
        body[bl++] = i < len ? dig[i] : '0';
    }
    if (prec > 0 || (sp.flags & FL_HASH)) {
      body[bl++] = '.';
      for (i = 0; i < prec; i++) {
        int idx = decpt + i;
        body[bl++] = (idx >= 0 && idx < len) ? dig[idx] : '0';
      }
    }
  } else {
    /* e */
    int ex = (m == 0) ? 0 : decpt - 1;
    int i;
    body[bl++] = dig[0];
    if (prec > 0 || (sp.flags & FL_HASH)) {
      body[bl++] = '.';
      for (i = 1; i <= prec; i++)
        body[bl++] = i < len ? dig[i] : '0';
    }
    body[bl++] = upper ? 'E' : 'e';
    body[bl++] = ex < 0 ? '-' : '+';
    if (ex < 0)
      ex = -ex;
    if (ex < 10)
      body[bl++] = '0';
    bl += rt_utoa(body + bl, (unsigned long long)ex, 10, 0);
  }

  if (upper) {
    int i;
    for (i = 0; i < bl; i++)
      if (body[i] == 'e')
        body[i] = 'E';
  }

  emit_padded(k, sign, 0, 0, body, (rt_size)bl, &sp, -1);
}

/* ------------------------------------------------------------- the engine */

int __c99m_vformat(Sink *k, const char *fmt, rt_valist ap) {
  const char *p = fmt;
  long long v;
  unsigned long long uv;
  char sign;
  char body[64];
  int n;
  while (*p) {
    Spec sp;
    int len_h, len_l, len_ll, len_z;
    char conv;

    if (*p != '%') {
      const char *run = p;
      while (*p && *p != '%')
        p++;
      sink_bytes(k, run, (rt_size)(p - run));
      continue;
    }
    p++;
    if (*p == '%') {
      sink_bytes(k, "%", 1);
      p++;
      continue;
    }

    sp.flags = 0;
    sp.width = 0;
    sp.prec = -1;
    len_h = len_l = len_ll = len_z = 0;

    for (;; p++) {
      if (*p == '-')
        sp.flags |= FL_MINUS;
      else if (*p == '+')
        sp.flags |= FL_PLUS;
      else if (*p == ' ')
        sp.flags |= FL_SPACE;
      else if (*p == '#')
        sp.flags |= FL_HASH;
      else if (*p == '0')
        sp.flags |= FL_ZERO;
      else
        break;
    }
    if (*p == '*') {
      sp.width = rt_va_arg(ap, int);
      if (sp.width < 0) {
        sp.flags |= FL_MINUS;
        sp.width = -sp.width;
      }
      p++;
    } else {
      while (*p >= '0' && *p <= '9')
        sp.width = sp.width * 10 + (*p++ - '0');
    }
    if (*p == '.') {
      p++;
      sp.prec = 0;
      if (*p == '*') {
        sp.prec = rt_va_arg(ap, int);
        p++;
      } else {
        while (*p >= '0' && *p <= '9')
          sp.prec = sp.prec * 10 + (*p++ - '0');
      }
    }
    for (;; p++) {
      if (*p == 'h')
        len_h++;
      else if (*p == 'l')
        len_l++;
      else if (*p == 'z' || *p == 't')
        len_z = 1;
      else if (*p == 'j')
        len_ll = 1;
      else if (*p == 'L')
        ; /* long double is double here */
      else
        break;
    }
    if (len_l >= 2)
      len_ll = 1;
    conv = *p++;

    switch (conv) {
    case 'd':
    case 'i': {
      v = rt_va_arg(ap, long long);
      if (!(len_ll || len_z))
        v = (int)v;
      if (len_h == 1)
        v = (short)v;
      else if (len_h >= 2)
        v = (signed char)v;
      if (v < 0) {
        sign = '-';
        uv = (unsigned long long)(-(v + 1)) + 1;
      } else {
        sign = (sp.flags & FL_PLUS) ? '+' : (sp.flags & FL_SPACE) ? ' ' : 0;
        uv = (unsigned long long)v;
      }
      n = rt_utoa(body, uv, 10, 0);
      if (sp.prec >= 0)
        sp.flags &= ~FL_ZERO;
      emit_padded(k, sign, 0, 0, body, (rt_size)n, &sp,
                  sp.prec >= 0 ? sp.prec : -1);
      break;
    }
    case 'u':
    case 'o':
    case 'x':
    case 'X': {
      unsigned long long v;
      char body[64];
      int n;
      char prefix_a = 0;
      char prefix_b = 0;
      int base = conv == 'u' ? 10 : conv == 'o' ? 8 : 16;
      v = rt_va_arg(ap, unsigned long long);
      if (!(len_ll || len_z))
        v = (unsigned int)v;
      if (len_h == 1)
        v = (unsigned short)v;
      else if (len_h >= 2)
        v = (unsigned char)v;
      if ((sp.flags & FL_HASH) && v != 0) {
        if (conv == 'x') {
          prefix_a = '0';
          prefix_b = 'x';
        } else if (conv == 'X') {
          prefix_a = '0';
          prefix_b = 'X';
        } else if (conv == 'o') {
          prefix_a = '0';
        }
      }
      n = rt_utoa(body, v, base, conv == 'X');
      if (sp.prec >= 0)
        sp.flags &= ~FL_ZERO;
      emit_padded(k, 0, prefix_a, prefix_b, body, (rt_size)n, &sp,
                  sp.prec >= 0 ? sp.prec : -1);
      break;
    }
    case 'c': {
      char ch = (char)rt_va_arg(ap, int);
      emit_padded(k, 0, 0, 0, &ch, 1, &sp, -1);
      break;
    }
    case 's': {
      const char *s = rt_va_arg(ap, const char *);
      rt_size n;
      if (!s)
        s = "(null)";
      n = strlen(s);
      if (sp.prec >= 0 && (rt_size)sp.prec < n)
        n = (rt_size)sp.prec;
      emit_padded(k, 0, 0, 0, s, n, &sp, -1);
      break;
    }
    case 'p': {
      /* the shape ucrt prints: 16 uppercase hex digits */
      unsigned long long v = (unsigned long long)rt_va_arg(ap, void *);
      char body[24];
      int n = rt_utoa(body, v, 16, 1);
      emit_padded(k, 0, 0, 0, body, (rt_size)n, &sp, 16);
      break;
    }
    case 'n': {
      int *out = rt_va_arg(ap, int *);
      if (out)
        *out = (int)k->want;
      break;
    }
    case 'f':
    case 'F':
    case 'e':
    case 'E':
    case 'g':
    case 'G': {
      double v = rt_va_arg(ap, double);
      emit_float(k, v, conv, sp);
      break;
    }
    default:
      /* unknown conversion: print it raw, matching common CRTs */
      sink_bytes(k, "%", 1);
      if (conv)
        sink_bytes(k, &conv, 1);
      else
        p--;
      break;
    }
  }
  return k->failed ? -1 : (int)k->want;
}

/* ------------------------------------------------------------ public face */

int vfprintf(FILE_ *f, const char *fmt, rt_valist ap) {
  Sink k;
  k.out = RT_NULL;
  k.cap = 0;
  k.want = 0;
  k.stream = f;
  k.failed = 0;
  return __c99m_vformat(&k, fmt, ap);
}

int fprintf(FILE_ *f, const char *fmt, ...) {
  rt_valist ap;
  int r;
  rt_va_start(ap, fmt);
  r = vfprintf(f, fmt, ap);
  rt_va_end(ap);
  return r;
}

int vprintf(const char *fmt, rt_valist ap) {
  return vfprintf(__c99m_iob(1), fmt, ap);
}

int printf(const char *fmt, ...) {
  rt_valist ap;
  int r;
  rt_va_start(ap, fmt);
  r = vfprintf(__c99m_iob(1), fmt, ap);
  rt_va_end(ap);
  return r;
}

int vsnprintf(char *out, rt_size cap, const char *fmt, rt_valist ap) {
  Sink k;
  int r;
  k.out = out;
  k.cap = cap ? cap - 1 : 0;
  k.want = 0;
  k.stream = RT_NULL;
  k.failed = 0;
  r = __c99m_vformat(&k, fmt, ap);
  if (out && cap) {
    rt_size end = k.want < cap - 1 ? k.want : cap - 1;
    out[end] = 0;
  }
  return r;
}

int snprintf(char *out, rt_size cap, const char *fmt, ...) {
  rt_valist ap;
  int r;
  rt_va_start(ap, fmt);
  r = vsnprintf(out, cap, fmt, ap);
  rt_va_end(ap);
  return r;
}

int vsprintf(char *out, const char *fmt, rt_valist ap) {
  return vsnprintf(out, (rt_size)1 << 40, fmt, ap);
}

int sprintf(char *out, const char *fmt, ...) {
  rt_valist ap;
  int r;
  rt_va_start(ap, fmt);
  r = vsnprintf(out, (rt_size)1 << 40, fmt, ap);
  rt_va_end(ap);
  return r;
}

int _vsnprintf(char *out, rt_size cap, const char *fmt, rt_valist ap) {
  return vsnprintf(out, cap, fmt, ap);
}

int _snprintf(char *out, rt_size cap, const char *fmt, ...) {
  rt_valist ap;
  int r;
  rt_va_start(ap, fmt);
  r = vsnprintf(out, cap, fmt, ap);
  rt_va_end(ap);
  return r;
}

int fputs(const char *, FILE_ *);
char *strerror(int);

void perror(const char *tag) {
  FILE_ *err = __c99m_iob(2);
  if (tag && *tag) {
    fputs(tag, err);
    fputs(": ", err);
  }
  fputs(strerror(__c99m_errno_slot), err);
  fputs("\n", err);
}
