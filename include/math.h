/* C99 <math.h> — declarations; definitions from host CRT at link. */
#ifndef _MATH_H
#define _MATH_H

#define HUGE_VAL (1e308 * 10.0)
#define HUGE_VALF (1e38f * 10.0f)
#define INFINITY HUGE_VALF
#define NAN (INFINITY - INFINITY)

#define M_E 2.71828182845904523536
#define M_LOG2E 1.44269504088896340736
#define M_LOG10E 0.434294481903251827651
#define M_LN2 0.693147180559945309417
#define M_LN10 2.30258509299404568402
#define M_PI 3.14159265358979323846
#define M_PI_2 1.57079632679489661923
#define M_PI_4 0.785398163397448309616
#define M_1_PI 0.318309886183790671538
#define M_2_PI 0.636619772367581343076
#define M_SQRT2 1.41421356237309504880
#define M_SQRT1_2 0.707106781186547524401

double acos(double x);
double asin(double x);
double atan(double x);
double atan2(double y, double x);
double cos(double x);
double sin(double x);
double tan(double x);
double cosh(double x);
double sinh(double x);
double tanh(double x);
double exp(double x);
double frexp(double x, int *e);
double ldexp(double x, int e);
double log(double x);
double log10(double x);
double log2(double x);
double modf(double x, double *ip);
double pow(double x, double y);
double sqrt(double x);
double cbrt(double x);
double hypot(double x, double y);
double ceil(double x);
double fabs(double x);
double floor(double x);
double fmod(double x, double y);
double round(double x);
double trunc(double x);
double nearbyint(double x);
double copysign(double x, double y);
double fmin(double x, double y);
double fmax(double x, double y);
double fma(double x, double y, double z);
double exp2(double x);
double expm1(double x);
double log1p(double x);

float acosf(float x);
float asinf(float x);
float atanf(float x);
float atan2f(float y, float x);
float cosf(float x);
float sinf(float x);
float tanf(float x);
float expf(float x);
float logf(float x);
float log10f(float x);
float log2f(float x);
float powf(float x, float y);
float sqrtf(float x);
float ceilf(float x);
float floorf(float x);
float fmodf(float x, float y);
float roundf(float x);
float truncf(float x);
float fminf(float x, float y);
float fmaxf(float x, float y);
float modff(float x, float *ip);

/* not exported by msvcrt/ucrtbase on x64 (compiler intrinsics there) */
static float fabsf(float x) { return (float)fabs((double)x); }
static float ldexpf(float x, int e) { return (float)ldexp((double)x, e); }
static float frexpf(float x, int *e) { return (float)frexp((double)x, e); }

int _isnan(double x);
int _finite(double x);
#define isnan(x) _isnan((double)(x))
#define isinf(x) (!_finite((double)(x)) && !_isnan((double)(x)))
#define isfinite(x) (_finite((double)(x)) != 0)

#endif /* _MATH_H */
