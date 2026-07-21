/* mem* and str*: the portable half of the runtime. */
#include "c99rt.h"

#define RT_NULL ((void *)0)

void *memcpy(void *dst, const void *src, rt_size n) {
  unsigned char *d = (unsigned char *)dst;
  const unsigned char *s = (const unsigned char *)src;
  /* word-at-a-time for the bulk; the tail byte by byte */
  while (n >= 8) {
    *(unsigned long long *)d = *(const unsigned long long *)s;
    d += 8;
    s += 8;
    n -= 8;
  }
  while (n--)
    *d++ = *s++;
  return dst;
}

void *memmove(void *dst, const void *src, rt_size n) {
  unsigned char *d = (unsigned char *)dst;
  const unsigned char *s = (const unsigned char *)src;
  if (d == s || n == 0)
    return dst;
  if (d < s)
    return memcpy(dst, src, n);
  d += n;
  s += n;
  while (n--)
    *--d = *--s;
  return dst;
}

void *memset(void *dst, int c, rt_size n) {
  unsigned char *d = (unsigned char *)dst;
  unsigned char b = (unsigned char)c;
  unsigned long long w = b;
  w |= w << 8;
  w |= w << 16;
  w |= w << 32;
  while (n >= 8) {
    *(unsigned long long *)d = w;
    d += 8;
    n -= 8;
  }
  while (n--)
    *d++ = b;
  return dst;
}

int memcmp(const void *a, const void *b, rt_size n) {
  const unsigned char *x = (const unsigned char *)a;
  const unsigned char *y = (const unsigned char *)b;
  while (n--) {
    if (*x != *y)
      return (int)*x - (int)*y;
    x++;
    y++;
  }
  return 0;
}

void *memchr(const void *p, int c, rt_size n) {
  const unsigned char *s = (const unsigned char *)p;
  unsigned char b = (unsigned char)c;
  while (n--) {
    if (*s == b)
      return (void *)s;
    s++;
  }
  return RT_NULL;
}

rt_size strlen(const char *s) {
  const char *p = s;
  while (*p)
    p++;
  return (rt_size)(p - s);
}

int strcmp(const char *a, const char *b) {
  while (*a && *a == *b) {
    a++;
    b++;
  }
  return (int)(unsigned char)*a - (int)(unsigned char)*b;
}

int strncmp(const char *a, const char *b, rt_size n) {
  while (n && *a && *a == *b) {
    a++;
    b++;
    n--;
  }
  if (n == 0)
    return 0;
  return (int)(unsigned char)*a - (int)(unsigned char)*b;
}

char *strcpy(char *dst, const char *src) {
  char *d = dst;
  while ((*d++ = *src++) != 0)
    ;
  return dst;
}

char *strncpy(char *dst, const char *src, rt_size n) {
  char *d = dst;
  while (n && *src) {
    *d++ = *src++;
    n--;
  }
  while (n--)
    *d++ = 0;
  return dst;
}

char *strcat(char *dst, const char *src) {
  char *d = dst;
  while (*d)
    d++;
  while ((*d++ = *src++) != 0)
    ;
  return dst;
}

char *strncat(char *dst, const char *src, rt_size n) {
  char *d = dst;
  while (*d)
    d++;
  while (n-- && *src)
    *d++ = *src++;
  *d = 0;
  return dst;
}

char *strchr(const char *s, int c) {
  char b = (char)c;
  for (;; s++) {
    if (*s == b)
      return (char *)s;
    if (*s == 0)
      return RT_NULL;
  }
}

char *strrchr(const char *s, int c) {
  const char *hit = RT_NULL;
  char b = (char)c;
  for (;; s++) {
    if (*s == b)
      hit = s;
    if (*s == 0)
      return (char *)hit;
  }
}

char *strstr(const char *hay, const char *needle) {
  rt_size n;
  if (!*needle)
    return (char *)hay;
  n = strlen(needle);
  for (; *hay; hay++)
    if (*hay == *needle && strncmp(hay, needle, n) == 0)
      return (char *)hay;
  return RT_NULL;
}

rt_size strspn(const char *s, const char *set) {
  const char *p = s;
  while (*p && strchr(set, *p))
    p++;
  return (rt_size)(p - s);
}

rt_size strcspn(const char *s, const char *set) {
  const char *p = s;
  while (*p && !strchr(set, *p))
    p++;
  return (rt_size)(p - s);
}

char *strpbrk(const char *s, const char *set) {
  for (; *s; s++)
    if (strchr(set, *s))
      return (char *)s;
  return RT_NULL;
}

static char *rt_strtok_state;

char *strtok(char *s, const char *sep) {
  char *tok;
  if (!s)
    s = rt_strtok_state;
  if (!s)
    return RT_NULL;
  s += strspn(s, sep);
  if (!*s) {
    rt_strtok_state = RT_NULL;
    return RT_NULL;
  }
  tok = s;
  s += strcspn(s, sep);
  if (*s) {
    *s = 0;
    rt_strtok_state = s + 1;
  } else {
    rt_strtok_state = RT_NULL;
  }
  return tok;
}

void *malloc(rt_size);

char *strdup(const char *s) {
  rt_size n = strlen(s) + 1;
  char *p = (char *)malloc(n);
  if (p)
    memcpy(p, s, n);
  return p;
}

char *_strdup(const char *s) { return strdup(s); }

static int rt_lower(int c) {
  if (c >= 'A' && c <= 'Z')
    return c + 32;
  return c;
}

int _stricmp(const char *a, const char *b) {
  for (;;) {
    int x = rt_lower((unsigned char)*a);
    int y = rt_lower((unsigned char)*b);
    if (x != y)
      return x - y;
    if (x == 0)
      return 0;
    a++;
    b++;
  }
}

int _strnicmp(const char *a, const char *b, rt_size n) {
  while (n--) {
    int x = rt_lower((unsigned char)*a);
    int y = rt_lower((unsigned char)*b);
    if (x != y)
      return x - y;
    if (x == 0)
      return 0;
    a++;
    b++;
  }
  return 0;
}

char *strerror(int err) {
  static char buf[40];
  char *p = buf;
  const char *msg = "error ";
  while (*msg)
    *p++ = *msg++;
  if (err < 0) {
    *p++ = '-';
    err = -err;
  }
  {
    char tmp[12];
    int i = 0;
    if (err == 0)
      tmp[i++] = '0';
    while (err > 0) {
      tmp[i++] = (char)('0' + err % 10);
      err /= 10;
    }
    while (i > 0)
      *p++ = tmp[--i];
  }
  *p = 0;
  return buf;
}
