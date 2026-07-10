/* C99 <stdio.h> — declarations; definitions come from the host CRT at link. */
#ifndef _STDIO_H
#define _STDIO_H

#include <stddef.h>
#include <stdarg.h>

typedef struct _C99MTLC_FILE FILE;
typedef long long fpos_t;

/* UCRT per-process standard streams */
FILE *__acrt_iob_func(unsigned idx);
#define stdin (__acrt_iob_func(0))
#define stdout (__acrt_iob_func(1))
#define stderr (__acrt_iob_func(2))

#define EOF (-1)
#define BUFSIZ 512
#define FILENAME_MAX 260
#define FOPEN_MAX 20
#define L_tmpnam 260
#define TMP_MAX 32767

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define _IOFBF 0x0000
#define _IOLBF 0x0040
#define _IONBF 0x0004

int putchar(int c);
int getchar(void);
int puts(const char *s);
int printf(const char *fmt, ...);
int sprintf(char *buf, const char *fmt, ...);
int snprintf(char *buf, size_t n, const char *fmt, ...);
int fprintf(FILE *f, const char *fmt, ...);
int vprintf(const char *fmt, va_list ap);
int vfprintf(FILE *f, const char *fmt, va_list ap);
int vsprintf(char *buf, const char *fmt, va_list ap);
int vsnprintf(char *buf, size_t n, const char *fmt, va_list ap);
int scanf(const char *fmt, ...);
int sscanf(const char *s, const char *fmt, ...);
int fscanf(FILE *f, const char *fmt, ...);
void perror(const char *s);

FILE *fopen(const char *path, const char *mode);
FILE *freopen(const char *path, const char *mode, FILE *f);
int fclose(FILE *f);
int fflush(FILE *f);
size_t fread(void *p, size_t sz, size_t n, FILE *f);
size_t fwrite(const void *p, size_t sz, size_t n, FILE *f);
int fseek(FILE *f, long off, int whence);
long ftell(FILE *f);
int _fseeki64(FILE *f, long long off, int whence);
long long _ftelli64(FILE *f);
void rewind(FILE *f);
int fgetpos(FILE *f, fpos_t *pos);
int fsetpos(FILE *f, const fpos_t *pos);
int feof(FILE *f);
int ferror(FILE *f);
void clearerr(FILE *f);

int fgetc(FILE *f);
int fputc(int c, FILE *f);
int getc(FILE *f);
int putc(int c, FILE *f);
int ungetc(int c, FILE *f);
char *fgets(char *s, int n, FILE *f);
int fputs(const char *s, FILE *f);

int remove(const char *path);
int rename(const char *from, const char *to);
FILE *tmpfile(void);
char *tmpnam(char *s);
int setvbuf(FILE *f, char *buf, int mode, size_t size);
void setbuf(FILE *f, char *buf);

int _fileno(FILE *f);
#define fileno _fileno
FILE *_fdopen(int fd, const char *mode);
#define fdopen _fdopen
FILE *_popen(const char *cmd, const char *mode);
int _pclose(FILE *f);
#define popen _popen
#define pclose _pclose

#endif /* _STDIO_H */
