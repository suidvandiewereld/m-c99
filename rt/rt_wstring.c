/* <wchar.h> string handling. wchar_t is 16 bits, so these are the narrow
 * routines over 16-bit units; nothing here interprets a surrogate pair,
 * exactly as the C library does not. */
#include "c99rt.h"

typedef unsigned short wch;

rt_size wcslen(const wch *s) {
  rt_size n = 0;
  while (s[n])
    n++;
  return n;
}

wch *wcscpy(wch *d, const wch *s) {
  wch *o = d;
  while ((*d++ = *s++) != 0)
    ;
  return o;
}

wch *wcsncpy(wch *d, const wch *s, rt_size n) {
  rt_size i = 0;
  for (; i < n && s[i]; i++)
    d[i] = s[i];
  for (; i < n; i++)
    d[i] = 0;
  return d;
}

wch *wcscat(wch *d, const wch *s) {
  wcscpy(d + wcslen(d), s);
  return d;
}

wch *wcsncat(wch *d, const wch *s, rt_size n) {
  rt_size dl = wcslen(d), i = 0;
  for (; i < n && s[i]; i++)
    d[dl + i] = s[i];
  d[dl + i] = 0;
  return d;
}

int wcscmp(const wch *a, const wch *b) {
  while (*a && *a == *b) {
    a++;
    b++;
  }
  return (int)*a - (int)*b;
}

int wcsncmp(const wch *a, const wch *b, rt_size n) {
  rt_size i;
  for (i = 0; i < n; i++) {
    if (a[i] != b[i])
      return (int)a[i] - (int)b[i];
    if (!a[i])
      break;
  }
  return 0;
}

wch *wcschr(const wch *s, wch c) {
  for (;; s++) {
    if (*s == c)
      return (wch *)s;
    if (!*s)
      return (wch *)0;
  }
}

wch *wcsrchr(const wch *s, wch c) {
  const wch *last = (const wch *)0;
  for (;; s++) {
    if (*s == c)
      last = s;
    if (!*s)
      break;
  }
  return (wch *)last;
}

wch *wcsstr(const wch *h, const wch *n) {
  rt_size nl = wcslen(n), i;
  if (!nl)
    return (wch *)h;
  for (; *h; h++) {
    for (i = 0; i < nl && h[i] == n[i]; i++)
      ;
    if (i == nl)
      return (wch *)h;
  }
  return (wch *)0;
}

static int wch_in(const wch *set, wch c) {
  for (; *set; set++)
    if (*set == c)
      return 1;
  return 0;
}

rt_size wcsspn(const wch *s, const wch *set) {
  rt_size n = 0;
  while (s[n] && wch_in(set, s[n]))
    n++;
  return n;
}

rt_size wcscspn(const wch *s, const wch *set) {
  rt_size n = 0;
  while (s[n] && !wch_in(set, s[n]))
    n++;
  return n;
}

wch *wcspbrk(const wch *s, const wch *set) {
  for (; *s; s++)
    if (wch_in(set, *s))
      return (wch *)s;
  return (wch *)0;
}

wch *wmemcpy(wch *d, const wch *s, rt_size n) {
  rt_size i;
  for (i = 0; i < n; i++)
    d[i] = s[i];
  return d;
}

wch *wmemmove(wch *d, const wch *s, rt_size n) {
  rt_size i;
  if (d < s || d >= s + n) {
    for (i = 0; i < n; i++)
      d[i] = s[i];
  } else {
    for (i = n; i > 0; i--)
      d[i - 1] = s[i - 1];
  }
  return d;
}

wch *wmemset(wch *d, wch c, rt_size n) {
  rt_size i;
  for (i = 0; i < n; i++)
    d[i] = c;
  return d;
}

int wmemcmp(const wch *a, const wch *b, rt_size n) {
  rt_size i;
  for (i = 0; i < n; i++)
    if (a[i] != b[i])
      return (int)a[i] - (int)b[i];
  return 0;
}

wch *wmemchr(const wch *s, wch c, rt_size n) {
  rt_size i;
  for (i = 0; i < n; i++)
    if (s[i] == c)
      return (wch *)(s + i);
  return (wch *)0;
}

/* The narrow/wide bridge. Only the ASCII range round-trips, which is all a
 * compiler in the "C" locale ever promises. */
int wctob(unsigned c) { return c < 128u ? (int)c : -1; }

unsigned btowc(int c) {
  return (c >= 0 && c < 128) ? (unsigned)c : 0xffffu;
}

rt_size wcstombs(char *d, const wch *s, rt_size n) {
  rt_size i;
  for (i = 0; i < n && s[i]; i++)
    d[i] = (char)(s[i] < 256u ? s[i] : '?');
  if (i < n)
    d[i] = 0;
  return i;
}

rt_size mbstowcs(wch *d, const char *s, rt_size n) {
  rt_size i;
  for (i = 0; i < n && s[i]; i++)
    d[i] = (wch)(unsigned char)s[i];
  if (i < n)
    d[i] = 0;
  return i;
}
