/* Windows CRT <io.h> shim */
#ifndef _IO_H
#define _IO_H

#include <stddef.h>

int _access(const char *path, int mode);
int _open(const char *path, int flags, ...);
int _close(int fd);
int _read(int fd, void *buf, unsigned n);
int _write(int fd, const void *buf, unsigned n);
long _lseek(int fd, long off, int whence);
long long _lseeki64(int fd, long long off, int whence);
int _unlink(const char *path);
int _isatty(int fd);
int _dup(int fd);
int _dup2(int from, int to);
long long _filelengthi64(int fd);
int _chmod(const char *path, int mode);
int _setmode(int fd, int mode);

#define _O_RDONLY 0x0000
#define _O_WRONLY 0x0001
#define _O_RDWR 0x0002
#define _O_APPEND 0x0008
#define _O_CREAT 0x0100
#define _O_TRUNC 0x0200
#define _O_EXCL 0x0400
#define _O_TEXT 0x4000
#define _O_BINARY 0x8000

#endif /* _IO_H */
