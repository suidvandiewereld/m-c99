/* C99 freestanding <stddef.h> for c99mtlc */
#ifndef _STDDEF_H
#define _STDDEF_H

typedef long long ptrdiff_t;
typedef unsigned long long size_t;
/* 16 bits, as on every Windows toolchain: a wide literal is UTF-16. */
typedef unsigned short wchar_t;

#define NULL ((void *)0)

#define offsetof(type, member)                                                 \
  ((size_t)((char *)&(((type *)0)->member) - (char *)0))

#endif /* _STDDEF_H */
