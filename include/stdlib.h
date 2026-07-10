/* C99 <stdlib.h> — declarations; definitions come from the host CRT at link. */
#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#define RAND_MAX 0x7fff
#define MB_CUR_MAX 5

void *malloc(size_t n);
void *calloc(size_t n, size_t sz);
void *realloc(void *p, size_t n);
void free(void *p);

void exit(int code);
void _exit(int code);
void abort(void);
int atexit(void (*fn)(void));
char *getenv(const char *name);
int _putenv(const char *str);
#define putenv _putenv
int system(const char *cmd);

int abs(int x);
long labs(long x);
long long llabs(long long x);

typedef struct {
  int quot;
  int rem;
} div_t;
typedef struct {
  long quot;
  long rem;
} ldiv_t;
typedef struct {
  long long quot;
  long long rem;
} lldiv_t;
div_t div(int num, int den);
ldiv_t ldiv(long num, long den);
lldiv_t lldiv(long long num, long long den);

int atoi(const char *s);
long atol(const char *s);
long long atoll(const char *s);
double atof(const char *s);
long strtol(const char *s, char **end, int base);
unsigned long strtoul(const char *s, char **end, int base);
long long strtoll(const char *s, char **end, int base);
unsigned long long strtoull(const char *s, char **end, int base);
double strtod(const char *s, char **end);
float strtof(const char *s, char **end);
long long _strtoi64(const char *s, char **end, int base);
unsigned long long _strtoui64(const char *s, char **end, int base);

int rand(void);
void srand(unsigned seed);

void qsort(void *base, size_t n, size_t sz,
           int (*cmp)(const void *, const void *));
void *bsearch(const void *key, const void *base, size_t n, size_t sz,
              int (*cmp)(const void *, const void *));

char *_fullpath(char *buf, const char *path, size_t n);

/* The UCRT's pgmptr is only populated by a full CRT startup, which the
 * libmtlc-synthesized entry point does not run — calling the real
 * _get_pgmptr aborts via the invalid-parameter handler. Resolve the
 * executable path from the OS instead. */
unsigned int GetModuleFileNameA(void *mod, char *buf, unsigned int n);
static int _get_pgmptr(char **value) {
  static char __c99m_pgm_buf[260];
  static int __c99m_pgm_got = 0;
  if (!__c99m_pgm_got) {
    if (GetModuleFileNameA((void *)0, __c99m_pgm_buf, 260u) == 0u)
      return 22; /* EINVAL */
    __c99m_pgm_got = 1;
  }
  *value = __c99m_pgm_buf;
  return 0;
}
#define realpath(path, buf) _fullpath((buf), (path), 260)

int mblen(const char *s, size_t n);

#endif /* _STDLIB_H */
