/* malloc/free on the process heap. HeapAlloc is the OS allocator the way
 * mmap is on Unix: below it there is only VirtualAlloc, and reimplementing
 * a general-purpose heap buys nothing for a compiler's workload. */
#include "c99rt.h"

#define RT_NULL ((void *)0)

void *malloc(rt_size n) {
  if (n == 0)
    n = 1;
  return HeapAlloc(GetProcessHeap(), 0, n);
}

void free(void *p) {
  if (p)
    HeapFree(GetProcessHeap(), 0, p);
}

void *realloc(void *p, rt_size n) {
  if (!p)
    return malloc(n);
  if (n == 0) {
    free(p);
    return RT_NULL;
  }
  return HeapReAlloc(GetProcessHeap(), 0, p, n);
}

void *calloc(rt_size count, rt_size size) {
  rt_size total;
  unsigned char *p;
  rt_size i;
  if (count != 0 && size > (rt_size)-1 / count)
    return RT_NULL;
  total = count * size;
  if (total == 0)
    total = 1;
  /* HEAP_ZERO_MEMORY = 8 */
  p = (unsigned char *)HeapAlloc(GetProcessHeap(), 8, total);
  (void)i;
  return p;
}
