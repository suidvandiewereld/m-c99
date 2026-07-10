/* Windows CRT <direct.h> shim */
#ifndef _DIRECT_H
#define _DIRECT_H

#include <stddef.h>

int _mkdir(const char *path);
int _rmdir(const char *path);
int _chdir(const char *path);
char *_getcwd(char *buf, int n);

#endif /* _DIRECT_H */
