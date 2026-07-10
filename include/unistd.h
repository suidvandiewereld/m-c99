/* <unistd.h> shim — maps POSIX names to the Windows CRT. */
#ifndef _UNISTD_H
#define _UNISTD_H

#include <stddef.h>
#include <io.h>
#include <direct.h>

typedef long long ssize_t;
typedef int pid_t;

#define STDIN_FILENO 0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4

#define access _access
#define unlink _unlink
#define isatty _isatty
#define dup _dup
#define dup2 _dup2
#define getcwd _getcwd
#define chdir _chdir
#define rmdir _rmdir

int _getpid(void);
#define getpid _getpid

#endif /* _UNISTD_H */
