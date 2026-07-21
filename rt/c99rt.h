/* c99rt: the self-contained C runtime for c99mtlc executables.
 *
 * Every function the compiler's own headers declare is defined here, in C99,
 * compiled by c99mtlc itself and linked into every executable. The only
 * import a finished program has is kernel32.dll: the pieces below sit
 * directly on Win32. No ucrtbase, no msvcrt.
 *
 * This header is internal to rt/. It declares the kernel32 surface the
 * runtime uses and the few cross-file helpers.
 */
#ifndef C99RT_H
#define C99RT_H

typedef unsigned long long rt_size;
typedef long long rt_ssize;

/* ---- kernel32 ---- */
void *GetProcessHeap(void);
void *HeapAlloc(void *heap, unsigned flags, rt_size bytes);
void *HeapReAlloc(void *heap, unsigned flags, void *p, rt_size bytes);
int HeapFree(void *heap, unsigned flags, void *p);
rt_size HeapSize(void *heap, unsigned flags, const void *p);

void ExitProcess(unsigned code);
char *GetCommandLineA(void);
char *GetEnvironmentStringsA(void);
unsigned GetEnvironmentVariableA(const char *name, char *buf, unsigned n);
int SetEnvironmentVariableA(const char *name, const char *value);

void *GetStdHandle(unsigned which);
int ReadFile(void *h, void *buf, unsigned n, unsigned *got, void *ov);
int WriteFile(void *h, const void *buf, unsigned n, unsigned *put, void *ov);
void *CreateFileA(const char *path, unsigned access, unsigned share,
                  void *sec, unsigned disp, unsigned flags, void *tmpl);
int CloseHandle(void *h);
int SetFilePointerEx(void *h, long long dist, long long *newpos,
                     unsigned method);
int GetFileSizeEx(void *h, long long *size);
int FlushFileBuffers(void *h);
int DeleteFileA(const char *path);
int MoveFileExA(const char *from, const char *to, unsigned flags);
unsigned GetFileType(void *h);
unsigned GetFileAttributesA(const char *path);
int GetFileAttributesExA(const char *path, int level, void *info);
int CreateDirectoryA(const char *path, void *sec);
unsigned GetCurrentDirectoryA(unsigned n, char *buf);
unsigned GetFullPathNameA(const char *path, unsigned n, char *buf,
                          char **file_part);
unsigned GetModuleFileNameA(void *mod, char *buf, unsigned n);
unsigned GetLastError(void);

int QueryPerformanceCounter(long long *out);
int QueryPerformanceFrequency(long long *out);
void GetSystemTimeAsFileTime(unsigned long long *out);

int CreateProcessA(const char *app, char *cmdline, void *pa, void *ta,
                   int inherit, unsigned flags, void *env, const char *cwd,
                   void *startupinfo, void *procinfo);
unsigned WaitForSingleObject(void *h, unsigned ms);
int GetExitCodeProcess(void *h, unsigned *code);

/* ---- shared runtime state ---- */

/* The exact-decimal converter behind %f/%e/%g and strtod. Digits of
 * m * 2^e * 10^s, correctly rounded to `want` significant digits.
 * See rt_format.c. */

/* Registered open streams, so exit() can flush them. */
void __c99m_stdio_flush_all(void);

/* One errno for the process. The compiler makes no threads. */
extern int __c99m_errno_slot;

#endif
