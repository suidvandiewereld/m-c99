/* Integer parsing, qsort, and the small odds and ends of <stdlib.h>. */
#include "c99rt.h"

#define RT_NULL ((void *)0)
#define RT_ERANGE 34

int isspace(int);
int isdigit(int);
int isalpha(int);
int tolower(int);
double strtod(const char *, char **);
void *memcpy(void *, const void *, rt_size);

/* ---- strtoull core; everything else converts through it ---- */

/* local isxdigit so the core has no ctype ordering dependency */
static int isxdigit_(int c) {
  return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F');
}

static unsigned long long rt_strtox(const char *s, char **end, int base,
                                    int *neg, int *overflow) {
  unsigned long long v = 0;
  unsigned long long cap = 0xFFFFFFFFFFFFFFFFull;
  const char *start = s;
  const char *digits_start;
  *neg = 0;
  *overflow = 0;

  while (isspace((unsigned char)*s))
    s++;
  if (*s == '+') {
    s++;
  } else if (*s == '-') {
    *neg = 1;
    s++;
  }
  if ((base == 0 || base == 16) && s[0] == '0' &&
      (s[1] == 'x' || s[1] == 'X') && isxdigit_((unsigned char)s[2])) {
    s += 2;
    base = 16;
  } else if (base == 0) {
    base = (s[0] == '0') ? 8 : 10;
  }
  digits_start = s;
  for (;;) {
    int c = (unsigned char)*s;
    int d;
    if (c >= '0' && c <= '9')
      d = c - '0';
    else if (c >= 'a' && c <= 'z')
      d = c - 'a' + 10;
    else if (c >= 'A' && c <= 'Z')
      d = c - 'A' + 10;
    else
      break;
    if (d >= base)
      break;
    if (v > (cap - (unsigned long long)d) / (unsigned long long)base)
      *overflow = 1;
    v = v * (unsigned long long)base + (unsigned long long)d;
    s++;
  }
  if (end)
    *end = (char *)(s == digits_start ? start : s);
  return v;
}

unsigned long long strtoull(const char *s, char **end, int base) {
  int neg, over;
  unsigned long long v = rt_strtox(s, end, base, &neg, &over);
  if (over) {
    __c99m_errno_slot = RT_ERANGE;
    return 0xFFFFFFFFFFFFFFFFull;
  }
  return neg ? (unsigned long long)(0 - v) : v;
}

long long strtoll(const char *s, char **end, int base) {
  int neg, over;
  unsigned long long v = rt_strtox(s, end, base, &neg, &over);
  if (over || (!neg && v > 0x7FFFFFFFFFFFFFFFull) ||
      (neg && v > 0x8000000000000000ull)) {
    __c99m_errno_slot = RT_ERANGE;
    return neg ? (-0x7FFFFFFFFFFFFFFFll - 1) : 0x7FFFFFFFFFFFFFFFll;
  }
  return neg ? -(long long)v : (long long)v;
}

unsigned long strtoul(const char *s, char **end, int base) {
  int neg, over;
  unsigned long long v = rt_strtox(s, end, base, &neg, &over);
  if (over || v > 0xFFFFFFFFull) {
    __c99m_errno_slot = RT_ERANGE;
    return 0xFFFFFFFFul;
  }
  return neg ? (unsigned long)(0 - v) : (unsigned long)v;
}

long strtol(const char *s, char **end, int base) {
  int neg, over;
  unsigned long long v = rt_strtox(s, end, base, &neg, &over);
  if (over || (!neg && v > 0x7FFFFFFFull) || (neg && v > 0x80000000ull)) {
    __c99m_errno_slot = RT_ERANGE;
    return neg ? (-0x7FFFFFFFl - 1) : 0x7FFFFFFFl;
  }
  return neg ? -(long)v : (long)v;
}

int atoi(const char *s) { return (int)strtol(s, RT_NULL, 10); }
long atol(const char *s) { return strtol(s, RT_NULL, 10); }
long long atoll(const char *s) { return strtoll(s, RT_NULL, 10); }
double atof(const char *s) { return strtod(s, RT_NULL); }

int abs(int x) { return x < 0 ? -x : x; }
long labs(long x) { return x < 0 ? -x : x; }
long long llabs(long long x) { return x < 0 ? -x : x; }

/* ---- qsort: median-of-three quicksort, insertion sort below 8 ---- */

static void rt_swap(unsigned char *a, unsigned char *b, rt_size n) {
  while (n--) {
    unsigned char t = *a;
    *a++ = *b;
    *b++ = t;
  }
}

typedef int (*rt_cmp)(const void *, const void *);

static void rt_qsort(unsigned char *base, rt_size lo, rt_size hi, rt_size w,
                     rt_cmp cmp) {
  while (lo + 1 < hi) {
    rt_size n = hi - lo;
    if (n < 8) {
      rt_size i;
      for (i = lo + 1; i < hi; i++) {
        rt_size j = i;
        while (j > lo && cmp(base + j * w, base + (j - 1) * w) < 0) {
          rt_swap(base + j * w, base + (j - 1) * w, w);
          j--;
        }
      }
      return;
    }
    {
      rt_size mid = lo + n / 2;
      rt_size last = hi - 1;
      rt_size i = lo;
      rt_size store;
      /* median of first/middle/last into `last` as the pivot */
      if (cmp(base + mid * w, base + lo * w) < 0)
        rt_swap(base + mid * w, base + lo * w, w);
      if (cmp(base + last * w, base + lo * w) < 0)
        rt_swap(base + last * w, base + lo * w, w);
      if (cmp(base + mid * w, base + last * w) < 0)
        rt_swap(base + mid * w, base + last * w, w);
      store = lo;
      for (i = lo; i < last; i++) {
        if (cmp(base + i * w, base + last * w) < 0) {
          rt_swap(base + i * w, base + store * w, w);
          store++;
        }
      }
      rt_swap(base + store * w, base + last * w, w);
      /* recurse into the smaller side, loop on the larger */
      if (store - lo < hi - (store + 1)) {
        rt_qsort(base, lo, store, w, cmp);
        lo = store + 1;
      } else {
        rt_qsort(base, store + 1, hi, w, cmp);
        hi = store;
      }
    }
  }
}

void qsort(void *base, rt_size count, rt_size width, rt_cmp cmp) {
  if (count > 1 && width > 0)
    rt_qsort((unsigned char *)base, 0, count, width, cmp);
}

void *bsearch(const void *key, const void *base, rt_size count, rt_size width,
              rt_cmp cmp) {
  rt_size lo = 0, hi = count;
  while (lo < hi) {
    rt_size mid = lo + (hi - lo) / 2;
    const unsigned char *p = (const unsigned char *)base + mid * width;
    int c = cmp(key, p);
    if (c == 0)
      return (void *)p;
    if (c < 0)
      hi = mid;
    else
      lo = mid + 1;
  }
  return RT_NULL;
}

/* ---- rand: C99's example LCG ---- */

static unsigned long long rt_rand_state = 1;

void srand(unsigned seed) { rt_rand_state = seed; }

int rand(void) {
  rt_rand_state = rt_rand_state * 1103515245ull + 12345ull;
  return (int)((rt_rand_state / 65536) % 32768);
}

/* ---- system: %COMSPEC% /c command ---- */

rt_size strlen(const char *);
char *getenv(const char *);

int system(const char *command) {
  char si[104];
  char pi[24];
  char cmdbuf[4096];
  const char *shell;
  rt_size i, k;
  unsigned code = 0;
  int ok;

  if (!command)
    return 1; /* a shell exists */

  shell = getenv("COMSPEC");
  if (!shell)
    shell = "cmd.exe";

  k = 0;
  for (i = 0; shell[i] && k + 8 < sizeof(cmdbuf); i++)
    cmdbuf[k++] = shell[i];
  cmdbuf[k++] = ' ';
  cmdbuf[k++] = '/';
  cmdbuf[k++] = 'c';
  cmdbuf[k++] = ' ';
  for (i = 0; command[i] && k + 1 < sizeof(cmdbuf); i++)
    cmdbuf[k++] = command[i];
  cmdbuf[k] = 0;

  for (i = 0; i < sizeof(si); i++)
    si[i] = 0;
  for (i = 0; i < sizeof(pi); i++)
    pi[i] = 0;
  *(unsigned *)si = 104; /* STARTUPINFOA.cb */

  __c99m_stdio_flush_all();
  ok = CreateProcessA(RT_NULL, cmdbuf, RT_NULL, RT_NULL, 1, 0, RT_NULL,
                      RT_NULL, si, pi);
  if (!ok)
    return -1;
  {
    void *hproc = *(void **)pi;
    void *hthread = *((void **)pi + 1);
    WaitForSingleObject(hproc, 0xFFFFFFFFu);
    GetExitCodeProcess(hproc, &code);
    CloseHandle(hthread);
    CloseHandle(hproc);
  }
  return (int)code;
}
