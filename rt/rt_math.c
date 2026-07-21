/* <math.h>: argument reduction plus polynomials, written from the
 * definitions. Accuracy target is a couple of ulps, which survives %f and
 * %.17g printing in the differential tests; sqrt alone is exact, computed
 * as an integer square root on the scaled mantissa.
 */
#include "c99rt.h"

void *memcpy(void *, const void *, rt_size);

static const double RT_PI = 3.14159265358979323846;
static const double RT_LN2_HI = 6.93147180369123816490e-01;
static const double RT_LN2_LO = 1.90821492927058770002e-10;
static const double RT_INV_LN2 = 1.44269504088896338700;

static unsigned long long rt_bits(double x) {
  unsigned long long b;
  memcpy(&b, &x, 8);
  return b;
}

static double rt_from_bits(unsigned long long b) {
  double x;
  memcpy(&x, &b, 8);
  return x;
}

static double rt_inf(void) { return rt_from_bits(0x7FF0000000000000ull); }
static double rt_nan(void) { return rt_from_bits(0x7FF8000000000000ull); }

int _isnan(double x) {
  unsigned long long b = rt_bits(x);
  return ((b >> 52) & 0x7FF) == 0x7FF && (b & 0xFFFFFFFFFFFFFull) != 0;
}

int _finite(double x) {
  return ((rt_bits(x) >> 52) & 0x7FF) != 0x7FF;
}

int __isnan(double x) { return _isnan(x); }
int isnan_fn(double x) { return _isnan(x); }

double fabs(double x) { return rt_from_bits(rt_bits(x) & ~(1ull << 63)); }
float fabsf(float x) { return (float)fabs((double)x); }

double copysign(double x, double y) {
  return rt_from_bits((rt_bits(x) & ~(1ull << 63)) |
                      (rt_bits(y) & (1ull << 63)));
}

double floor(double x) {
  unsigned long long b = rt_bits(x);
  int exp = (int)((b >> 52) & 0x7FF) - 1023;
  unsigned long long frac_mask;
  if (exp >= 52 || _isnan(x))
    return x; /* already integral (or inf/nan) */
  if (exp < 0) {
    /* |x| < 1 */
    if (x == 0.0)
      return x;
    return (b >> 63) ? -1.0 : 0.0;
  }
  frac_mask = (1ull << (52 - exp)) - 1;
  if ((b & frac_mask) == 0)
    return x;
  if (b >> 63)
    return rt_from_bits((b & ~frac_mask)) - 1.0;
  return rt_from_bits(b & ~frac_mask);
}

double ceil(double x) { return -floor(-x); }

double trunc(double x) {
  return (x < 0.0) ? -floor(-x) : floor(x);
}

double fmod(double x, double y) {
  /* exponent-aligned repeated subtraction on the bit patterns; the
   * schoolbook method every libm starts from */
  double ax = fabs(x), ay = fabs(y);
  if (y == 0.0 || !_finite(x) || _isnan(y))
    return rt_nan();
  if (!_finite(y))
    return _isnan(y) ? y : x;
  if (ax < ay)
    return x;
  while (ax >= ay) {
    double d = ay;
    /* scale d up to just below ax by doubling */
    while (ax - d >= d)
      d += d;
    ax -= d;
  }
  return (x < 0.0) ? -ax : ax;
}

double frexp(double x, int *eout) {
  unsigned long long b = rt_bits(x);
  int exp = (int)((b >> 52) & 0x7FF);
  if (exp == 0x7FF || x == 0.0) {
    *eout = 0;
    return x;
  }
  if (exp == 0) {
    /* denormal: normalize by scaling up 2^64 first */
    double scaled = x * 18446744073709551616.0;
    double m = frexp(scaled, eout);
    *eout -= 64;
    return m;
  }
  *eout = exp - 1022;
  return rt_from_bits((b & 0x800FFFFFFFFFFFFFull) | 0x3FE0000000000000ull);
}

double ldexp(double x, int n) {
  /* multiply by 2^n in exact powers-of-two chunks */
  while (n > 1023) {
    x *= rt_from_bits((unsigned long long)(1023 + 1023) << 52);
    n -= 1023;
  }
  while (n < -1074) {
    x *= rt_from_bits(1ull); /* smallest denormal: flushes to 0/denormal */
    n += 1074;
  }
  if (n >= -1022) {
    if (n >= 0)
      return x * rt_from_bits((unsigned long long)(1023 + n) << 52);
    return x * rt_from_bits((unsigned long long)(1023 + n) << 52);
  }
  /* deep negative: two steps through denormal range */
  x *= rt_from_bits((unsigned long long)(1023 - 1022) << 52);
  n += 1022;
  return x * rt_from_bits((unsigned long long)(1023 + n) << 52);
}

float ldexpf(float x, int n) { return (float)ldexp((double)x, n); }
float frexpf(float x, int *e) { return (float)frexp((double)x, e); }

/* ---- sqrt: exact, via integer square root of the scaled mantissa ---- */

/* assemble m * 2^e with m in [2^52, 2^53) */
static double rt_make_from_mant(unsigned long long m, int e) {
  int biased = e + 1075;
  unsigned long long bits;
  if (biased <= 0 || biased >= 0x7FF) {
    /* out of normal range: fall back to ldexp on the mantissa */
    return ldexp((double)m, e);
  }
  bits = ((unsigned long long)biased << 52) | (m & 0xFFFFFFFFFFFFFull);
  return rt_from_bits(bits);
}

/* v^2 into a 128-bit hi:lo pair; v < 2^54 */
static void rt_sq128(unsigned long long v, unsigned long long *hi,
                     unsigned long long *lo) {
  unsigned long long a = v >> 32;
  unsigned long long c = v & 0xFFFFFFFFull;
  unsigned long long cross = a * c; /* < 2^54 */
  unsigned long long l = c * c;
  unsigned long long h = a * a;
  unsigned long long l2 = l + (cross << 33);
  if (l2 < l)
    h++;
  *lo = l2;
  *hi = h + (cross >> 31);
}

double sqrt(double x) {
  unsigned long long b, m;
  int exp;
  unsigned long long target_hi, target_lo; /* m << 52: up to 106 bits */
  unsigned long long root;
  int i;

  if (x < 0.0)
    return rt_nan();
  if (x == 0.0 || !_finite(x))
    return x; /* 0, inf, nan pass through */

  b = rt_bits(x);
  exp = (int)((b >> 52) & 0x7FF);
  if (exp == 0) {
    /* denormal: normalize via multiply by 2^108 (an even power) */
    double y = x * rt_from_bits((unsigned long long)(1023 + 108) << 52);
    return sqrt(y) * rt_from_bits((unsigned long long)(1023 - 54) << 52);
  }
  m = (b & 0xFFFFFFFFFFFFFull) | (1ull << 52);
  exp -= 1075; /* x = m * 2^exp */

  /* make the exponent even so the root has an integral power of two */
  if (exp & 1) {
    m <<= 1;
    exp--;
  }

  target_hi = m >> 12; /* top bits of m << 52 */
  target_lo = m << 52; /* low 64 bits */

  /* bitwise integer square root of the 106-bit target */
  root = 0;
  for (i = 53; i >= 0; i--) {
    unsigned long long try_ = root | (1ull << i);
    unsigned long long t_hi, t_lo;
    rt_sq128(try_, &t_hi, &t_lo);
    if (t_hi < target_hi || (t_hi == target_hi && t_lo <= target_lo))
      root = try_;
  }

  {
    int rexp = (exp - 52) / 2;
    unsigned long long r_hi, r_lo, rem_lo, rem_hi;
    rt_sq128(root, &r_hi, &r_lo);
    rem_lo = target_lo - r_lo;
    rem_hi = target_hi - r_hi;
    if (target_lo < r_lo)
      rem_hi--;
    /* round to nearest: up when target - root^2 > root */
    if (rem_hi > 0 || rem_lo > root)
      root++;
    if (root >= (1ull << 53)) {
      root >>= 1;
      rexp++;
    }
    return rt_make_from_mant(root, rexp);
  }
}

float sqrtf(float x) { return (float)sqrt((double)x); }

/* ---- exp and log ---- */

double exp(double x) {
  /* x = k*ln2 + r, |r| <= ln2/2; e^x = 2^k * e^r, e^r by Taylor */
  double k_d, r, term, sum;
  int k, i;
  if (_isnan(x))
    return x;
  if (x > 709.782712893384)
    return rt_inf();
  if (x < -745.133219101941)
    return 0.0;
  k_d = floor(x * RT_INV_LN2 + 0.5);
  k = (int)k_d;
  r = (x - k_d * RT_LN2_HI) - k_d * RT_LN2_LO;
  term = 1.0;
  sum = 1.0;
  for (i = 1; i <= 17; i++) {
    term = term * r / (double)i;
    sum += term;
  }
  return ldexp(sum, k);
}

float expf(float x) { return (float)exp((double)x); }

double log(double x) {
  /* x = m * 2^k with m in [sqrt(1/2), sqrt(2)); ln x = k ln2 + ln m,
   * ln m via atanh series in s = (m-1)/(m+1): ln m = 2(s + s^3/3 + ...) */
  int k;
  double m, s, s2, sum, term;
  int i;
  if (_isnan(x))
    return x;
  if (x < 0.0)
    return rt_nan();
  if (x == 0.0)
    return -rt_inf();
  if (!_finite(x))
    return x;
  m = frexp(x, &k); /* m in [0.5, 1) */
  if (m < 0.70710678118654752440) {
    m *= 2.0;
    k--;
  }
  s = (m - 1.0) / (m + 1.0);
  s2 = s * s;
  sum = 0.0;
  term = s;
  for (i = 0; i < 27; i++) {
    sum += term / (double)(2 * i + 1);
    term *= s2;
  }
  sum *= 2.0;
  return (double)k * RT_LN2_HI + ((double)k * RT_LN2_LO + sum);
}

double log10(double x) { return log(x) * 0.43429448190325182765; }
double log2(double x) { return log(x) * RT_INV_LN2; }

double pow(double x, double y) {
  /* the exact-answer cases first, then exp(y ln x) */
  if (y == 0.0)
    return 1.0;
  if (_isnan(x) || _isnan(y))
    return rt_nan();
  if (x == 1.0)
    return 1.0;
  /* integer exponents by squaring: exact for things like 5^2 */
  if (y == floor(y) && fabs(y) <= 1024.0) {
    double base = x;
    double acc = 1.0;
    long long n = (long long)y;
    int inv = n < 0;
    if (inv)
      n = -n;
    while (n) {
      if (n & 1)
        acc *= base;
      base *= base;
      n >>= 1;
    }
    return inv ? 1.0 / acc : acc;
  }
  if (x == 0.0)
    return y > 0.0 ? 0.0 : rt_inf();
  if (x < 0.0)
    return rt_nan(); /* non-integer power of a negative */
  return exp(y * log(x));
}

/* ---- trig: Cody-Waite reduction by pi/2, then Taylor kernels ---- */

static const double PIO2_HI = 1.57079632679489655800e+00;
static const double PIO2_MID = 6.12323399573676603587e-17;
static const double PIO2_LO = -1.49726980000000000000e-33;

static double rt_sin_kernel(double r) {
  /* sin r = r - r^3/3! + ... for |r| <= pi/4 */
  double r2 = r * r;
  double term = r;
  double sum = r;
  int i;
  for (i = 1; i <= 8; i++) {
    term = -term * r2 / (double)((2 * i) * (2 * i + 1));
    sum += term;
  }
  return sum;
}

static double rt_cos_kernel(double r) {
  double r2 = r * r;
  double term = 1.0;
  double sum = 1.0;
  int i;
  for (i = 1; i <= 8; i++) {
    term = -term * r2 / (double)((2 * i - 1) * (2 * i));
    sum += term;
  }
  return sum;
}

/* r = x - k * pi/2, k = nearest; returns k mod 4 */
static int rt_trig_reduce(double x, double *rout) {
  double k_d = floor(x * 0.63661977236758134308 + 0.5); /* 2/pi */
  double r = ((x - k_d * PIO2_HI) - k_d * PIO2_MID) - k_d * PIO2_LO;
  long long k = (long long)k_d;
  *rout = r;
  return (int)(((k % 4) + 4) % 4);
}

double sin(double x) {
  double r;
  int q;
  if (!_finite(x))
    return rt_nan();
  q = rt_trig_reduce(x, &r);
  if (q == 0)
    return rt_sin_kernel(r);
  if (q == 1)
    return rt_cos_kernel(r);
  if (q == 2)
    return -rt_sin_kernel(r);
  return -rt_cos_kernel(r);
}

double cos(double x) {
  double r;
  int q;
  if (!_finite(x))
    return rt_nan();
  q = rt_trig_reduce(x, &r);
  if (q == 0)
    return rt_cos_kernel(r);
  if (q == 1)
    return -rt_sin_kernel(r);
  if (q == 2)
    return -rt_cos_kernel(r);
  return rt_sin_kernel(r);
}

double tan(double x) {
  double s = sin(x);
  double c = cos(x);
  return s / c;
}

/* ---- inverse trig ---- */

double atan(double x) {
  /* reduce |x| to [0, 1) via atan(x) = pi/2 - atan(1/x), then to
   * [0, tan(pi/12)) via the pi/6 identity, then Taylor */
  int neg = x < 0.0;
  int big = 0;
  int shifted = 0;
  double sum, term, x2;
  int i;
  const double TAN_PI_6 = 0.57735026918962576451;
  const double PI_6 = 0.52359877559829887308;
  if (_isnan(x))
    return x;
  if (neg)
    x = -x;
  if (!_finite(x))
    return neg ? -RT_PI / 2 : RT_PI / 2;
  if (x > 1.0) {
    x = 1.0 / x;
    big = 1;
  }
  if (x > 0.26794919243112270647) { /* tan(pi/12) */
    x = (x * 1.73205080756887729353 - 1.0) / (1.73205080756887729353 + x);
    shifted = 1;
  }
  x2 = x * x;
  sum = 0.0;
  term = x;
  for (i = 0; i < 22; i++) {
    sum += (i & 1) ? -term / (double)(2 * i + 1) : term / (double)(2 * i + 1);
    term *= x2;
  }
  (void)TAN_PI_6;
  if (shifted)
    sum += PI_6;
  if (big)
    sum = RT_PI / 2 - sum;
  return neg ? -sum : sum;
}

double atan2(double y, double x) {
  if (x > 0.0)
    return atan(y / x);
  if (x < 0.0) {
    if (y >= 0.0)
      return atan(y / x) + RT_PI;
    return atan(y / x) - RT_PI;
  }
  /* x == 0 */
  if (y > 0.0)
    return RT_PI / 2;
  if (y < 0.0)
    return -RT_PI / 2;
  return 0.0;
}

double asin(double x) {
  if (x < -1.0 || x > 1.0)
    return rt_nan();
  if (fabs(x) == 1.0)
    return x > 0 ? RT_PI / 2 : -RT_PI / 2;
  return atan(x / sqrt(1.0 - x * x));
}

double acos(double x) {
  if (x < -1.0 || x > 1.0)
    return rt_nan();
  return RT_PI / 2 - asin(x);
}

/* ---- hyperbolics ---- */

double sinh(double x) {
  double e;
  if (fabs(x) < 1e-5) {
    /* series: avoids the cancellation in (e^x - e^-x)/2 */
    return x + x * x * x / 6.0;
  }
  e = exp(fabs(x));
  {
    double r = (e - 1.0 / e) / 2.0;
    return x < 0 ? -r : r;
  }
}

double cosh(double x) {
  double e = exp(fabs(x));
  return (e + 1.0 / e) / 2.0;
}

double tanh(double x) {
  double e2;
  if (fabs(x) > 20.0)
    return x > 0 ? 1.0 : -1.0;
  if (fabs(x) < 1e-5)
    return x - x * x * x / 3.0;
  e2 = exp(2.0 * fabs(x));
  {
    double r = (e2 - 1.0) / (e2 + 1.0);
    return x < 0 ? -r : r;
  }
}

/* ---- the rest of the <math.h> surface ---- */

double round(double x) {
  double f = floor(x);
  if (x - f >= 0.5)
    return f + 1.0;
  return f;
}

double nearbyint(double x) { return round(x); }

double modf(double x, double *ip) {
  double i = trunc(x);
  *ip = i;
  if (!_finite(x))
    return _isnan(x) ? x : 0.0;
  return x - i;
}

double fmin(double a, double b) { return a < b ? a : b; }
double fmax(double a, double b) { return a > b ? a : b; }
double fma(double a, double b, double c) { return a * b + c; }
double exp2(double x) { return pow(2.0, x); }
double cbrt(double x) {
  if (x == 0.0)
    return x;
  if (x < 0.0)
    return -pow(-x, 1.0 / 3.0);
  return pow(x, 1.0 / 3.0);
}
double hypot(double a, double b) { return sqrt(a * a + b * b); }

double expm1(double x) {
  if (fabs(x) < 1e-5)
    return x + x * x / 2.0 + x * x * x / 6.0;
  return exp(x) - 1.0;
}

double log1p(double x) {
  if (fabs(x) < 1e-5)
    return x - x * x / 2.0 + x * x * x / 3.0;
  return log(1.0 + x);
}

/* float wrappers */
float acosf(float x) { return (float)acos((double)x); }
float asinf(float x) { return (float)asin((double)x); }
float atanf(float x) { return (float)atan((double)x); }
float atan2f(float y, float x) { return (float)atan2((double)y, (double)x); }
float cosf(float x) { return (float)cos((double)x); }
float sinf(float x) { return (float)sin((double)x); }
float tanf(float x) { return (float)tan((double)x); }
float logf(float x) { return (float)log((double)x); }
float log10f(float x) { return (float)log10((double)x); }
float log2f(float x) { return (float)log2((double)x); }
float powf(float x, float y) { return (float)pow((double)x, (double)y); }
float ceilf(float x) { return (float)ceil((double)x); }
float floorf(float x) { return (float)floor((double)x); }
float fmodf(float x, float y) { return (float)fmod((double)x, (double)y); }
float roundf(float x) { return (float)round((double)x); }
float truncf(float x) { return (float)trunc((double)x); }
float fminf(float a, float b) { return a < b ? a : b; }
float fmaxf(float a, float b) { return a > b ? a : b; }
float modff(float x, float *ip) {
  double d;
  float r = (float)modf((double)x, &d);
  *ip = (float)d;
  return r;
}
