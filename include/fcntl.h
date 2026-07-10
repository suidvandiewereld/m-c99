/* <fcntl.h> shim — Windows CRT flags plus POSIX aliases */
#ifndef _FCNTL_H
#define _FCNTL_H

#include <io.h>

#define O_RDONLY _O_RDONLY
#define O_WRONLY _O_WRONLY
#define O_RDWR _O_RDWR
#define O_APPEND _O_APPEND
#define O_CREAT _O_CREAT
#define O_TRUNC _O_TRUNC
#define O_EXCL _O_EXCL
#define O_TEXT _O_TEXT
#define O_BINARY _O_BINARY

#define open _open
#define close _close
#define read _read
#define write _write
#define lseek _lseek

#endif /* _FCNTL_H */
