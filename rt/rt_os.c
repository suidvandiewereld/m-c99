/* The Windows-CRT-shaped odds and ends: paths, stat, time. */
#include "c99rt.h"

#define RT_NULL ((void *)0)

void *malloc(rt_size);
rt_size strlen(const char *);
void *memcpy(void *, const void *, rt_size);
void *memset(void *, int, rt_size);
char *strcpy(char *, const char *);

/* ---- clock and time ---- */

long clock(void) {
  /* CLOCKS_PER_SEC is 1000 in the headers */
  long long c = 0, f = 0;
  QueryPerformanceCounter(&c);
  QueryPerformanceFrequency(&f);
  if (f == 0)
    return 0;
  return (long)(c * 1000 / f);
}

long long _time64(long long *out) {
  /* FILETIME is 100ns ticks since 1601; Unix time since 1970 */
  unsigned long long ft = 0;
  long long t;
  GetSystemTimeAsFileTime(&ft);
  t = (long long)(ft / 10000000ull) - 11644473600ll;
  if (out)
    *out = t;
  return t;
}

double difftime(long long end, long long start) {
  return (double)(end - start);
}

/* ---- directories and paths ---- */

int _mkdir(const char *path) {
  return CreateDirectoryA(path, RT_NULL) ? 0 : -1;
}

char *_getcwd(char *buf, int n) {
  char tmp[1024];
  unsigned len = GetCurrentDirectoryA(sizeof(tmp), tmp);
  if (len == 0 || len >= sizeof(tmp))
    return RT_NULL;
  if (buf) {
    if ((unsigned)n <= len)
      return RT_NULL;
    memcpy(buf, tmp, len + 1);
    return buf;
  }
  buf = (char *)malloc(len + 1);
  if (buf)
    memcpy(buf, tmp, len + 1);
  return buf;
}

char *_fullpath(char *out, const char *path, rt_size n) {
  char tmp[1024];
  unsigned len = GetFullPathNameA(path ? path : ".", sizeof(tmp), tmp,
                                  RT_NULL);
  if (len == 0 || len >= sizeof(tmp))
    return RT_NULL;
  if (out) {
    if (n <= (rt_size)len)
      return RT_NULL;
    memcpy(out, tmp, len + 1);
    return out;
  }
  out = (char *)malloc(len + 1);
  if (out)
    memcpy(out, tmp, len + 1);
  return out;
}

/* ---- stat ---- */

/* WIN32_FILE_ATTRIBUTE_DATA, spelled in DWORDs: FILETIME is two DWORDs at
 * 4-byte alignment, so a u64 field would misplace everything after it. */
struct rt_attr_data {
  unsigned attrs;
  unsigned ctime_lo, ctime_hi;
  unsigned atime_lo, atime_hi;
  unsigned mtime_lo, mtime_hi;
  unsigned size_hi;
  unsigned size_lo;
};

static unsigned long long rt_ft64(unsigned lo, unsigned hi) {
  return ((unsigned long long)hi << 32) | lo;
}

struct rt_stat64 {
  unsigned st_dev;
  unsigned short st_ino;
  unsigned short st_mode;
  short st_nlink;
  short st_uid;
  short st_gid;
  unsigned st_rdev;
  long long st_size;
  long long st_atime;
  long long st_mtime;
  long long st_ctime;
};

static long long rt_ft_to_unix(unsigned long long ft) {
  return (long long)(ft / 10000000ull) - 11644473600ll;
}

int _stat64(const char *path, struct rt_stat64 *st) {
  struct rt_attr_data ad;
  memset(&ad, 0, sizeof(ad));
  if (!GetFileAttributesExA(path, 0 /*GetFileExInfoStandard*/, &ad))
    return -1;
  memset(st, 0, sizeof(*st));
  st->st_nlink = 1;
  if (ad.attrs & 0x10u /*DIRECTORY*/) {
    st->st_mode = 0x4000 | 0x0100 | 0x0080; /* IFDIR | IREAD | IWRITE */
  } else {
    st->st_mode = 0x8000 | 0x0100; /* IFREG | IREAD */
    if (!(ad.attrs & 0x1u /*READONLY*/))
      st->st_mode |= 0x0080;
  }
  st->st_size =
      ((long long)ad.size_hi << 32) | (long long)ad.size_lo;
  st->st_atime = rt_ft_to_unix(rt_ft64(ad.atime_lo, ad.atime_hi));
  st->st_mtime = rt_ft_to_unix(rt_ft64(ad.mtime_lo, ad.mtime_hi));
  st->st_ctime = rt_ft_to_unix(rt_ft64(ad.ctime_lo, ad.ctime_hi));
  return 0;
}

int _fstat64(int fd, struct rt_stat64 *st) {
  (void)fd;
  memset(st, 0, sizeof(*st));
  st->st_mode = 0x2000; /* IFCHR: only used on std handles here */
  return 0;
}

/* ---- program path, kept compatible with the old header shim ---- */

int _get_pgmptr(char **out) {
  static char path[1024];
  static int ready;
  if (!ready) {
    if (GetModuleFileNameA(RT_NULL, path, sizeof(path)) == 0)
      return -1;
    ready = 1;
  }
  *out = path;
  return 0;
}
