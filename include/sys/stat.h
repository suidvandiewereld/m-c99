/* <sys/stat.h> shim — maps to the Windows CRT _stat64. */
#ifndef _SYS_STAT_H
#define _SYS_STAT_H

#include <stddef.h>
#include <time.h>

typedef unsigned int _dev_t;
typedef unsigned short _ino_t;
typedef unsigned short _mode_t;
typedef long long _off64_t;

struct _stat64 {
  _dev_t st_dev;
  _ino_t st_ino;
  _mode_t st_mode;
  short st_nlink;
  short st_uid;
  short st_gid;
  _dev_t st_rdev;
  long long st_size;
  time_t st_atime;
  time_t st_mtime;
  time_t st_ctime;
};

int _stat64(const char *path, struct _stat64 *st);
int _fstat64(int fd, struct _stat64 *st);

#define stat _stat64
#define fstat _fstat64

#define _S_IFMT 0xF000
#define _S_IFDIR 0x4000
#define _S_IFCHR 0x2000
#define _S_IFIFO 0x1000
#define _S_IFREG 0x8000
#define _S_IREAD 0x0100
#define _S_IWRITE 0x0080
#define _S_IEXEC 0x0040

#define S_IFMT _S_IFMT
#define S_IFDIR _S_IFDIR
#define S_IFCHR _S_IFCHR
#define S_IFREG _S_IFREG
#define S_IREAD _S_IREAD
#define S_IWRITE _S_IWRITE
#define S_IEXEC _S_IEXEC

#define S_ISDIR(m) (((m) & _S_IFMT) == _S_IFDIR)
#define S_ISREG(m) (((m) & _S_IFMT) == _S_IFREG)
#define S_ISCHR(m) (((m) & _S_IFMT) == _S_IFCHR)

#endif /* _SYS_STAT_H */
