/* strtod and sscanf.
 *
 * strtod is the printing bignum run backwards: collect the decimal digits
 * exactly, then multiply or divide by two until the value sits in
 * [2^52, 2^53), counting the binary exponent and keeping a sticky bit, and
 * round half-to-even into the mantissa. Exact for every input the tests can
 * throw at it, with no power tables. */
#include "c99rt.h"

#define RT_NULL ((void *)0)
#define RT_ERANGE 34

int isspace(int);
int isdigit(int);
void *memcpy(void *, const void *, rt_size);

/* base-1e9 bignum, little-endian; big enough for ~800 decimal digits */
#define SBN_CAP 100

typedef struct {
  unsigned limb[SBN_CAP];
  int n;
} Sbn;

static void sbn_zero(Sbn *b) { b->n = 0; }

static int sbn_is_zero(const Sbn *b) { return b->n == 0; }

/* b = b*10 + d, saturating at capacity (then the value is astronomically
 * large anyway and the exponent tracking below has long since decided) */
static void sbn_push_digit(Sbn *b, int d) {
  unsigned long long carry = (unsigned)d;
  int i;
  for (i = 0; i < b->n; i++) {
    unsigned long long t = (unsigned long long)b->limb[i] * 10ull + carry;
    b->limb[i] = (unsigned)(t % 1000000000ull);
    carry = t / 1000000000ull;
  }
  while (carry && b->n < SBN_CAP) {
    b->limb[b->n++] = (unsigned)(carry % 1000000000ull);
    carry /= 1000000000ull;
  }
}

static void sbn_mul_small(Sbn *b, unsigned m) {
  unsigned long long carry = 0;
  int i;
  for (i = 0; i < b->n; i++) {
    unsigned long long t = (unsigned long long)b->limb[i] * m + carry;
    b->limb[i] = (unsigned)(t % 1000000000ull);
    carry = t / 1000000000ull;
  }
  while (carry && b->n < SBN_CAP) {
    b->limb[b->n++] = (unsigned)(carry % 1000000000ull);
    carry /= 1000000000ull;
  }
}

/* b /= 2; returns the bit shifted out */
static int sbn_half(Sbn *b) {
  unsigned long long rem = 0;
  int i;
  for (i = b->n - 1; i >= 0; i--) {
    unsigned long long t = rem * 1000000000ull + b->limb[i];
    b->limb[i] = (unsigned)(t >> 1);
    rem = t & 1;
  }
  while (b->n > 0 && b->limb[b->n - 1] == 0)
    b->n--;
  return (int)rem;
}

static void sbn_double(Sbn *b) { sbn_mul_small(b, 2); }

static int sbn_cmp(const Sbn *a, const Sbn *b) {
  int i;
  if (a->n != b->n)
    return a->n < b->n ? -1 : 1;
  for (i = a->n - 1; i >= 0; i--)
    if (a->limb[i] != b->limb[i])
      return a->limb[i] < b->limb[i] ? -1 : 1;
  return 0;
}

/* a -= b; caller guarantees a >= b */
static void sbn_sub(Sbn *a, const Sbn *b) {
  long long borrow = 0;
  int i;
  for (i = 0; i < a->n; i++) {
    long long d = (long long)a->limb[i] - borrow -
                  (i < b->n ? (long long)b->limb[i] : 0);
    if (d < 0) {
      d += 1000000000;
      borrow = 1;
    } else {
      borrow = 0;
    }
    a->limb[i] = (unsigned)d;
  }
  while (a->n > 0 && a->limb[a->n - 1] == 0)
    a->n--;
}

static unsigned long long sbn_low64(const Sbn *b) {
  unsigned long long v = 0;
  int i;
  for (i = b->n - 1; i >= 0; i--)
    v = v * 1000000000ull + b->limb[i];
  return v;
}

static double rt_make_double(int neg, unsigned long long mant, int e2) {
  unsigned long long bits;
  double out;
  int biased = e2 + 1075;
  if (mant == 0) {
    bits = 0;
  } else if (biased >= 0x7FF) {
    bits = 0x7FF0000000000000ull; /* inf */
    __c99m_errno_slot = RT_ERANGE;
  } else if (biased <= 0) {
    /* denormal: shift the mantissa down, rounding to nearest */
    int shift = 1 - biased;
    if (shift > 54) {
      bits = 0;
      __c99m_errno_slot = RT_ERANGE;
    } else {
      unsigned long long half = 1ull << (shift - 1);
      unsigned long long lost = mant & ((1ull << shift) - 1);
      unsigned long long kept = mant >> shift;
      if (lost > half || (lost == half && (kept & 1)))
        kept++;
      bits = kept; /* biased exponent 0 */
    }
  } else {
    bits = ((unsigned long long)biased << 52) |
           (mant & 0xFFFFFFFFFFFFFull);
  }
  if (neg)
    bits |= 1ull << 63;
  memcpy(&out, &bits, 8);
  return out;
}

double strtod(const char *s, char **end) {
  const char *start = s;
  const char *ok_end;
  int neg = 0;
  Sbn big;
  int dec_exp = 0; /* value = big * 10^dec_exp */
  int any = 0;
  int e2 = 0;
  int sticky = 0;
  unsigned long long mant;
  int round_bit;

  while (isspace((unsigned char)*s))
    s++;
  if (*s == '+') {
    s++;
  } else if (*s == '-') {
    neg = 1;
    s++;
  }

  /* inf / nan */
  {
    const char *p = s;
    if ((p[0] == 'i' || p[0] == 'I') && (p[1] == 'n' || p[1] == 'N') &&
        (p[2] == 'f' || p[2] == 'F')) {
      if (end)
        *end = (char *)(p + 3);
      return rt_make_double(neg, 0, 0) +
             (neg ? -1.0 : 1.0) * 1e308 * 1e308; /* inf of the right sign */
    }
    if ((p[0] == 'n' || p[0] == 'N') && (p[1] == 'a' || p[1] == 'A') &&
        (p[2] == 'n' || p[2] == 'N')) {
      unsigned long long nan_bits = 0x7FF8000000000000ull;
      double out;
      memcpy(&out, &nan_bits, 8);
      if (end)
        *end = (char *)(p + 3);
      return out;
    }
  }

  sbn_zero(&big);
  ok_end = s;

  /* significant digits; cap the collected count, track the rest in the
   * exponent (they only matter as sticky, and SBN_CAP*9 digits is far past
   * double precision) */
  {
    int collected = 0;
    int seen_point = 0;
    for (;; s++) {
      if (*s >= '0' && *s <= '9') {
        any = 1;
        if (collected < 780) {
          sbn_push_digit(&big, *s - '0');
          collected++;
          if (seen_point)
            dec_exp--;
        } else {
          if (*s != '0')
            sticky = 1;
          if (!seen_point)
            dec_exp++;
        }
      } else if (*s == '.' && !seen_point) {
        seen_point = 1;
      } else {
        break;
      }
    }
    if (any)
      ok_end = s;
  }
  if (!any) {
    if (end)
      *end = (char *)start;
    return 0.0;
  }
  if (*s == 'e' || *s == 'E') {
    const char *p = s + 1;
    int eneg = 0;
    int ev = 0;
    int edigits = 0;
    if (*p == '+') {
      p++;
    } else if (*p == '-') {
      eneg = 1;
      p++;
    }
    while (*p >= '0' && *p <= '9') {
      if (ev < 100000)
        ev = ev * 10 + (*p - '0');
      p++;
      edigits = 1;
    }
    if (edigits) {
      dec_exp += eneg ? -ev : ev;
      ok_end = p;
    }
  }
  if (end)
    *end = (char *)ok_end;

  if (sbn_is_zero(&big))
    return rt_make_double(neg, 0, 0);

  /* saturate absurd exponents before doing any bignum work */
  if (dec_exp > 350) {
    __c99m_errno_slot = RT_ERANGE;
    return rt_make_double(neg, 1ull << 52, 2000); /* forces inf */
  }
  if (dec_exp < -400) {
    __c99m_errno_slot = RT_ERANGE;
    return rt_make_double(neg, 0, 0);
  }

  /* value = N / denom with N = big * 10^max(dec_exp,0) and
   * denom = 10^max(-dec_exp,0). Scale until N/denom sits in [2^52, 2^53),
   * long-divide for 53 bits plus a round bit, round half to even. Every
   * step is exact. */
  {
    Sbn denom, lo, hi;
    int i;
    unsigned long long q = 0;
    Sbn rem;
    int bit;

    while (dec_exp > 0) {
      sbn_mul_small(&big, 10);
      dec_exp--;
    }
    sbn_zero(&denom);
    sbn_push_digit(&denom, 1);
    for (i = 0; i < -dec_exp; i++)
      sbn_mul_small(&denom, 10);

    lo = denom;
    for (i = 0; i < 52; i++)
      sbn_double(&lo);
    hi = lo;
    sbn_double(&hi);

    while (sbn_cmp(&big, &lo) < 0) {
      sbn_double(&big);
      e2--;
    }
    while (sbn_cmp(&big, &hi) >= 0) {
      sbn_double(&denom);
      sbn_double(&lo);
      sbn_double(&hi);
      e2++;
    }

    /* big/lo is now in [1, 2). Extract its leading bit, 52 fraction bits,
     * and one rounding bit. Comparing against the unscaled decimal
     * denominator here would subtract only one unit per step and build an
     * all-ones mantissa instead of performing binary long division. */
    denom = lo;
    rem = big;
    for (bit = 0; bit < 54; bit++) {
      q <<= 1;
      if (sbn_cmp(&rem, &denom) >= 0) {
        sbn_sub(&rem, &denom);
        q |= 1;
      }
      if (bit < 53)
        sbn_double(&rem);
    }
    if (!sbn_is_zero(&rem))
      sticky = 1;
    round_bit = (int)(q & 1);
    mant = q >> 1;
    if (round_bit && (sticky || (mant & 1)))
      mant++;
    if (mant == (1ull << 53)) {
      mant >>= 1;
      e2++;
    }
    return rt_make_double(neg, mant, e2);
  }
}

float strtof(const char *s, char **end) { return (float)strtod(s, end); }

/* ------------------------------------------------------------- sscanf ---- */

long long strtoll(const char *, char **, int);
unsigned long long strtoull(const char *, char **, int);

int vsscanf_core(const char *in, const char *fmt, char *ap_raw);

int sscanf(const char *in, const char *fmt, ...) {
  char *ap;
  __builtin_va_start(ap, fmt);
  {
    const char *s = in;
    const char *p = fmt;
    int converted = 0;

    while (*p) {
      if (isspace((unsigned char)*p)) {
        while (isspace((unsigned char)*s))
          s++;
        p++;
        continue;
      }
      if (*p != '%') {
        if (*s != *p)
          break;
        s++;
        p++;
        continue;
      }
      p++;
      {
        int suppress = 0;
        int width = 0;
        int has_width = 0;
        int len_l = 0, len_h = 0, len_ll = 0;
        char conv;

        if (*p == '*') {
          suppress = 1;
          p++;
        }
        while (*p >= '0' && *p <= '9') {
          width = width * 10 + (*p++ - '0');
          has_width = 1;
        }
        while (*p == 'l' || *p == 'h' || *p == 'z' || *p == 'j') {
          if (*p == 'l')
            len_l++;
          else if (*p == 'h')
            len_h++;
          else
            len_ll = 1;
          p++;
        }
        if (len_l >= 2)
          len_ll = 1;
        conv = *p++;

        if (conv == 'n') {
          if (!suppress) {
            int *out = __builtin_va_arg(ap, int *);
            *out = (int)(s - in);
          }
          continue;
        }

        if (conv == 'c') {
          int n = has_width ? width : 1;
          char *out = suppress ? RT_NULL : __builtin_va_arg(ap, char *);
          int i;
          for (i = 0; i < n; i++) {
            if (!*s)
              goto done;
            if (out)
              out[i] = *s;
            s++;
          }
          if (!suppress)
            converted++;
          continue;
        }

        if (conv == 's') {
          char *out = suppress ? RT_NULL : __builtin_va_arg(ap, char *);
          int i = 0;
          while (isspace((unsigned char)*s))
            s++;
          if (!*s)
            goto done;
          while (*s && !isspace((unsigned char)*s) &&
                 (!has_width || i < width)) {
            if (out)
              out[i] = *s;
            i++;
            s++;
          }
          if (out)
            out[i] = 0;
          if (!suppress)
            converted++;
          continue;
        }

        if (conv == '[') {
          /* scanset: %[chars] or %[^chars] */
          char set[256];
          int negate = 0;
          int i;
          char *out = RT_NULL;
          for (i = 0; i < 256; i++)
            set[i] = 0;
          if (*p == '^') {
            negate = 1;
            p++;
          }
          if (*p == ']') {
            set[(unsigned char)']'] = 1;
            p++;
          }
          while (*p && *p != ']') {
            if (p[1] == '-' && p[2] && p[2] != ']') {
              int lo = (unsigned char)p[0], hi = (unsigned char)p[2];
              for (i = lo; i <= hi; i++)
                set[i] = 1;
              p += 3;
            } else {
              set[(unsigned char)*p] = 1;
              p++;
            }
          }
          if (*p == ']')
            p++;
          if (!suppress)
            out = __builtin_va_arg(ap, char *);
          i = 0;
          while (*s && (!has_width || i < width)) {
            int inset = set[(unsigned char)*s];
            if (negate)
              inset = !inset;
            if (!inset)
              break;
            if (out)
              out[i] = *s;
            i++;
            s++;
          }
          if (i == 0)
            goto done;
          if (out)
            out[i] = 0;
          if (!suppress)
            converted++;
          continue;
        }

        /* numeric conversions skip leading space */
        while (isspace((unsigned char)*s))
          s++;

        if (conv == 'd' || conv == 'i' || conv == 'u' || conv == 'x' ||
            conv == 'o') {
          char *endp = RT_NULL;
          int base = conv == 'x' ? 16 : conv == 'o' ? 8
                     : conv == 'd' || conv == 'u' ? 10
                                                  : 0;
          long long v;
          if (conv == 'u' || conv == 'x' || conv == 'o')
            v = (long long)strtoull(s, &endp, base);
          else
            v = strtoll(s, &endp, base);
          if (endp == s)
            goto done;
          s = endp;
          if (!suppress) {
            if (len_ll) {
              long long *out = __builtin_va_arg(ap, long long *);
              *out = v;
            } else if (len_h == 1) {
              short *out = __builtin_va_arg(ap, short *);
              *out = (short)v;
            } else if (len_h >= 2) {
              signed char *out = __builtin_va_arg(ap, signed char *);
              *out = (signed char)v;
            } else if (len_l == 1) {
              long *out = __builtin_va_arg(ap, long *);
              *out = (long)v;
            } else {
              int *out = __builtin_va_arg(ap, int *);
              *out = (int)v;
            }
            converted++;
          }
          continue;
        }

        if (conv == 'f' || conv == 'e' || conv == 'g' || conv == 'a') {
          char *endp = RT_NULL;
          double v = strtod(s, &endp);
          if (endp == s)
            goto done;
          s = endp;
          if (!suppress) {
            if (len_l || len_ll) {
              double *out = __builtin_va_arg(ap, double *);
              *out = v;
            } else {
              float *out = __builtin_va_arg(ap, float *);
              *out = (float)v;
            }
            converted++;
          }
          continue;
        }

        if (conv == '%') {
          if (*s != '%')
            goto done;
          s++;
          continue;
        }

        /* unknown directive: stop */
        goto done;
      }
    }
  done:
    __builtin_va_end(ap);
    return converted;
  }
}
