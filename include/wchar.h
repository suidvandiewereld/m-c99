#ifndef _C99MTLC_WCHAR_H
#define _C99MTLC_WCHAR_H

#include <stddef.h>

/* wchar_t is 16 bits here, as it is everywhere on Windows, so a wide literal
   is UTF-16 and a character outside the basic plane takes a surrogate pair. */

#define WCHAR_MIN 0
#define WCHAR_MAX 0xffff
#ifndef WEOF
#define WEOF ((wint_t)0xffff)
#endif

typedef unsigned int wint_t;

size_t wcslen(const wchar_t *s);
wchar_t *wcscpy(wchar_t *d, const wchar_t *s);
wchar_t *wcsncpy(wchar_t *d, const wchar_t *s, size_t n);
wchar_t *wcscat(wchar_t *d, const wchar_t *s);
wchar_t *wcsncat(wchar_t *d, const wchar_t *s, size_t n);
int wcscmp(const wchar_t *a, const wchar_t *b);
int wcsncmp(const wchar_t *a, const wchar_t *b, size_t n);
wchar_t *wcschr(const wchar_t *s, wchar_t c);
wchar_t *wcsrchr(const wchar_t *s, wchar_t c);
wchar_t *wcsstr(const wchar_t *h, const wchar_t *n);
size_t wcsspn(const wchar_t *s, const wchar_t *set);
size_t wcscspn(const wchar_t *s, const wchar_t *set);
wchar_t *wcspbrk(const wchar_t *s, const wchar_t *set);

wchar_t *wmemcpy(wchar_t *d, const wchar_t *s, size_t n);
wchar_t *wmemmove(wchar_t *d, const wchar_t *s, size_t n);
wchar_t *wmemset(wchar_t *d, wchar_t c, size_t n);
int wmemcmp(const wchar_t *a, const wchar_t *b, size_t n);
wchar_t *wmemchr(const wchar_t *s, wchar_t c, size_t n);

/* The narrow/wide bridge, for the ASCII range the compiler itself needs. */
int wctob(wint_t c);
wint_t btowc(int c);
size_t wcstombs(char *d, const wchar_t *s, size_t n);
size_t mbstowcs(wchar_t *d, const char *s, size_t n);

#endif
