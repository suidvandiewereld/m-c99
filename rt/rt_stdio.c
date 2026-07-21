/* <stdio.h> on raw Win32 handles: our own FILE, our own buffering.
 *
 * One buffer per stream. `dir` says what the buffer holds right now:
 * pending writes or read-ahead. Crossing over flushes or discards, so the
 * file position stays honest. All three standard streams share the code
 * path; stderr is unbuffered, stdout picks line buffering on a console and
 * full buffering on a pipe, the same policy every CRT uses. */
#include "c99rt.h"

#define RT_NULL ((void *)0)
#define RT_EOF (-1)

#define RT_BUFSZ 4096

#define DIR_IDLE 0
#define DIR_READ 1
#define DIR_WRITE 2

/* mode bits */
#define F_READ 1u
#define F_WRITE 2u
#define F_APPEND 4u

/* buffering, matching stdio.h's _IOFBF/_IOLBF/_IONBF values */
#define BUF_FULL 0x0000
#define BUF_LINE 0x0040
#define BUF_NONE 0x0004

struct _C99MTLC_FILE {
  void *h;
  unsigned mode;    /* F_* bits */
  int dir;          /* DIR_* */
  int err;
  int eof;
  int bufmode;      /* BUF_* */
  unsigned char *buf;
  int cap;          /* buffer capacity */
  int len;          /* read: valid bytes; write: pending bytes */
  int pos;          /* read: next unread byte */
  int ungot;        /* one pushed-back byte, or -1 */
  int is_std;       /* never freed, never closed */
  struct _C99MTLC_FILE *next;
};

typedef struct _C99MTLC_FILE FILE_;

void *malloc(rt_size);
void free(void *);
void *memcpy(void *, const void *, rt_size);
rt_size strlen(const char *);

static FILE_ *rt_open_files;

static FILE_ rt_stdin_file;
static FILE_ rt_stdout_file;
static FILE_ rt_stderr_file;
static unsigned char rt_stdin_buf[RT_BUFSZ];
static unsigned char rt_stdout_buf[RT_BUFSZ];
static int rt_std_ready;

/* GetStdHandle arguments are (unsigned)-10, -11, -12 */
static void rt_std_init(void) {
  if (rt_std_ready)
    return;
  rt_std_ready = 1;

  rt_stdin_file.h = GetStdHandle((unsigned)-10);
  rt_stdin_file.mode = F_READ;
  rt_stdin_file.buf = rt_stdin_buf;
  rt_stdin_file.cap = RT_BUFSZ;
  rt_stdin_file.bufmode = BUF_FULL;
  rt_stdin_file.ungot = -1;
  rt_stdin_file.is_std = 1;

  rt_stdout_file.h = GetStdHandle((unsigned)-11);
  rt_stdout_file.mode = F_WRITE;
  rt_stdout_file.buf = rt_stdout_buf;
  rt_stdout_file.cap = RT_BUFSZ;
  /* FILE_TYPE_CHAR == 2: a console. Line-buffer there, so interleaved
   * printf/fputs come out in order; full-buffer pipes and files. */
  rt_stdout_file.bufmode =
      (GetFileType(rt_stdout_file.h) == 2u) ? BUF_LINE : BUF_FULL;
  rt_stdout_file.ungot = -1;
  rt_stdout_file.is_std = 1;

  rt_stderr_file.h = GetStdHandle((unsigned)-12);
  rt_stderr_file.mode = F_WRITE;
  rt_stderr_file.buf = RT_NULL;
  rt_stderr_file.cap = 0;
  rt_stderr_file.bufmode = BUF_NONE;
  rt_stderr_file.ungot = -1;
  rt_stderr_file.is_std = 1;
}

FILE_ *__c99m_iob(int idx) {
  rt_std_init();
  if (idx == 0)
    return &rt_stdin_file;
  if (idx == 1)
    return &rt_stdout_file;
  return &rt_stderr_file;
}

/* ---- the write side ---- */

static int rt_raw_write(FILE_ *f, const unsigned char *p, rt_size n) {
  while (n) {
    unsigned chunk = n > 0x40000000u ? 0x40000000u : (unsigned)n;
    unsigned put = 0;
    if (!WriteFile(f->h, p, chunk, &put, RT_NULL) || put == 0) {
      f->err = 1;
      return 0;
    }
    p += put;
    n -= put;
  }
  return 1;
}

static int rt_flush_write(FILE_ *f) {
  if (f->dir == DIR_WRITE && f->len > 0) {
    int ok = rt_raw_write(f, f->buf, (rt_size)f->len);
    f->len = 0;
    if (!ok)
      return RT_EOF;
  }
  if (f->dir == DIR_WRITE)
    f->dir = DIR_IDLE;
  return 0;
}

/* leaving read mode: forget read-ahead, rewind the OS position over it */
static void rt_drop_read(FILE_ *f) {
  if (f->dir == DIR_READ) {
    long long back = (long long)(f->len - f->pos);
    if (f->ungot >= 0)
      back += 1;
    if (back > 0)
      SetFilePointerEx(f->h, -back, RT_NULL, 1 /*FILE_CURRENT*/);
    f->len = 0;
    f->pos = 0;
    f->ungot = -1;
    f->dir = DIR_IDLE;
  }
}

static int rt_putbytes(FILE_ *f, const unsigned char *p, rt_size n) {
  if (!(f->mode & F_WRITE)) {
    f->err = 1;
    return 0;
  }
  rt_drop_read(f);
  f->dir = DIR_WRITE;

  if (f->bufmode == BUF_NONE || !f->buf) {
    return rt_raw_write(f, p, n);
  }

  if (f->bufmode == BUF_LINE) {
    /* buffer, but flush through the last newline */
    rt_size i;
    int last_nl = -1;
    for (i = 0; i < n; i++)
      if (p[i] == '\n')
        last_nl = (int)i;
    for (i = 0; i < n; i++) {
      if (f->len == f->cap) {
        if (rt_flush_write(f) == RT_EOF)
          return 0;
        f->dir = DIR_WRITE;
      }
      f->buf[f->len++] = p[i];
      if (last_nl >= 0 && i == (rt_size)last_nl) {
        if (rt_flush_write(f) == RT_EOF)
          return 0;
        f->dir = DIR_WRITE;
      }
    }
    return 1;
  }

  /* full buffering */
  if (n >= (rt_size)f->cap) {
    if (rt_flush_write(f) == RT_EOF)
      return 0;
    f->dir = DIR_WRITE;
    return rt_raw_write(f, p, n);
  }
  if (f->len + (int)n > f->cap) {
    if (rt_flush_write(f) == RT_EOF)
      return 0;
    f->dir = DIR_WRITE;
  }
  memcpy(f->buf + f->len, p, n);
  f->len += (int)n;
  return 1;
}

/* ---- the read side ---- */

static int rt_fill(FILE_ *f) {
  unsigned got = 0;
  if (rt_flush_write(f) == RT_EOF)
    return RT_EOF;
  f->dir = DIR_READ;
  if (!f->buf) {
    f->err = 1;
    return RT_EOF;
  }
  if (!ReadFile(f->h, f->buf, (unsigned)f->cap, &got, RT_NULL)) {
    /* ERROR_BROKEN_PIPE at the end of a pipe is eof, not error */
    if (GetLastError() != 109u)
      f->err = 1;
    got = 0;
  }
  f->len = (int)got;
  f->pos = 0;
  if (got == 0) {
    f->eof = 1;
    return RT_EOF;
  }
  return 0;
}

static int rt_getbyte(FILE_ *f) {
  if (!(f->mode & F_READ)) {
    f->err = 1;
    return RT_EOF;
  }
  if (f->ungot >= 0) {
    int c = f->ungot;
    f->ungot = -1;
    return c;
  }
  if (f->dir != DIR_READ || f->pos >= f->len) {
    if (rt_fill(f))
      return RT_EOF;
  }
  return f->buf[f->pos++];
}

/* ---- public surface ---- */

FILE_ *fopen(const char *path, const char *mode) {
  unsigned access = 0, disp = 0;
  unsigned share = 1u | 2u; /* FILE_SHARE_READ | FILE_SHARE_WRITE */
  unsigned fmode = 0;
  void *h;
  FILE_ *f;
  int plus = 0;
  const char *m;

  for (m = mode; *m; m++)
    if (*m == '+')
      plus = 1;

  if (mode[0] == 'r') {
    fmode = plus ? (F_READ | F_WRITE) : F_READ;
    access = plus ? (0x80000000u | 0x40000000u) : 0x80000000u;
    disp = 3; /* OPEN_EXISTING */
  } else if (mode[0] == 'w') {
    fmode = plus ? (F_READ | F_WRITE) : F_WRITE;
    access = plus ? (0x80000000u | 0x40000000u) : 0x40000000u;
    disp = 2; /* CREATE_ALWAYS */
  } else if (mode[0] == 'a') {
    fmode = (plus ? (F_READ | F_WRITE) : F_WRITE) | F_APPEND;
    access = plus ? (0x80000000u | 0x40000000u) : 0x40000000u;
    disp = 4; /* OPEN_ALWAYS */
  } else {
    return RT_NULL;
  }

  h = CreateFileA(path, access, share, RT_NULL, disp,
                  0x80u /*FILE_ATTRIBUTE_NORMAL*/, RT_NULL);
  if (h == (void *)(rt_ssize)-1)
    return RT_NULL;

  f = (FILE_ *)malloc(sizeof(FILE_));
  if (!f) {
    CloseHandle(h);
    return RT_NULL;
  }
  f->h = h;
  f->mode = fmode;
  f->dir = DIR_IDLE;
  f->err = 0;
  f->eof = 0;
  f->bufmode = BUF_FULL;
  f->buf = (unsigned char *)malloc(RT_BUFSZ);
  f->cap = f->buf ? RT_BUFSZ : 0;
  f->len = 0;
  f->pos = 0;
  f->ungot = -1;
  f->is_std = 0;

  if (fmode & F_APPEND)
    SetFilePointerEx(h, 0, RT_NULL, 2 /*FILE_END*/);

  rt_std_init();
  f->next = rt_open_files;
  rt_open_files = f;
  return f;
}

int fclose(FILE_ *f) {
  int r;
  FILE_ **pp;
  if (!f)
    return RT_EOF;
  r = rt_flush_write(f);
  if (f->is_std)
    return r;
  CloseHandle(f->h);
  for (pp = &rt_open_files; *pp; pp = &(*pp)->next) {
    if (*pp == f) {
      *pp = f->next;
      break;
    }
  }
  free(f->buf);
  free(f);
  return r;
}

void __c99m_stdio_flush_all(void) {
  FILE_ *f;
  if (!rt_std_ready)
    return;
  rt_flush_write(&rt_stdout_file);
  rt_flush_write(&rt_stderr_file);
  for (f = rt_open_files; f; f = f->next)
    rt_flush_write(f);
}

int fflush(FILE_ *f) {
  if (!f) {
    __c99m_stdio_flush_all();
    return 0;
  }
  return rt_flush_write(f);
}

rt_size fread(void *out, rt_size size, rt_size count, FILE_ *f) {
  unsigned char *dst = (unsigned char *)out;
  rt_size want = size * count;
  rt_size got = 0;
  if (want == 0 || !f)
    return 0;
  if (!(f->mode & F_READ)) {
    f->err = 1;
    return 0;
  }
  if (f->ungot >= 0 && want > 0) {
    *dst++ = (unsigned char)f->ungot;
    f->ungot = -1;
    got = 1;
  }
  if (rt_flush_write(f) == RT_EOF)
    return got / size;
  /* drain the read-ahead first */
  if (f->dir == DIR_READ && f->pos < f->len) {
    rt_size have = (rt_size)(f->len - f->pos);
    rt_size take = have < want - got ? have : want - got;
    memcpy(dst, f->buf + f->pos, take);
    f->pos += (int)take;
    dst += take;
    got += take;
  }
  /* big remainders go straight to the OS */
  while (got < want) {
    rt_size left = want - got;
    if (left >= (rt_size)RT_BUFSZ) {
      unsigned chunk = left > 0x40000000u ? 0x40000000u : (unsigned)left;
      unsigned n = 0;
      f->dir = DIR_READ;
      f->len = 0;
      f->pos = 0;
      if (!ReadFile(f->h, dst, chunk, &n, RT_NULL)) {
        if (GetLastError() != 109u)
          f->err = 1;
        n = 0;
      }
      if (n == 0) {
        f->eof = 1;
        break;
      }
      dst += n;
      got += n;
    } else {
      if (rt_fill(f))
        break;
      {
        rt_size have = (rt_size)(f->len - f->pos);
        rt_size take = have < left ? have : left;
        memcpy(dst, f->buf + f->pos, take);
        f->pos += (int)take;
        dst += take;
        got += take;
      }
    }
  }
  return got / size;
}

rt_size fwrite(const void *src, rt_size size, rt_size count, FILE_ *f) {
  rt_size n = size * count;
  if (n == 0 || !f)
    return 0;
  if (!rt_putbytes(f, (const unsigned char *)src, n))
    return 0;
  return count;
}

int fseek(FILE_ *f, long off, int whence) {
  if (!f)
    return -1;
  if (rt_flush_write(f) == RT_EOF)
    return -1;
  if (whence == 1 /*SEEK_CUR*/ && f->dir == DIR_READ) {
    /* the OS position is ahead of the logical one by the unread bytes */
    off -= (long)(f->len - f->pos);
    if (f->ungot >= 0)
      off -= 1;
  }
  f->len = 0;
  f->pos = 0;
  f->ungot = -1;
  f->dir = DIR_IDLE;
  f->eof = 0;
  if (!SetFilePointerEx(f->h, off, RT_NULL, (unsigned)whence))
    return -1;
  return 0;
}

long ftell(FILE_ *f) {
  long long pos = 0;
  if (!f)
    return -1;
  if (!SetFilePointerEx(f->h, 0, &pos, 1 /*FILE_CURRENT*/))
    return -1;
  if (f->dir == DIR_READ) {
    pos -= (long long)(f->len - f->pos);
    if (f->ungot >= 0)
      pos -= 1;
  } else if (f->dir == DIR_WRITE) {
    pos += (long long)f->len;
  }
  return (long)pos;
}

void rewind(FILE_ *f) {
  fseek(f, 0, 0);
  if (f)
    f->err = 0;
}

int feof(FILE_ *f) { return f ? f->eof : 0; }
int ferror(FILE_ *f) { return f ? f->err : 0; }
void clearerr(FILE_ *f) {
  if (f) {
    f->err = 0;
    f->eof = 0;
  }
}

int setvbuf(FILE_ *f, char *user_buf, int mode, rt_size size) {
  (void)user_buf;
  (void)size;
  if (!f)
    return -1;
  if (mode == BUF_NONE || mode == BUF_LINE || mode == BUF_FULL) {
    f->bufmode = mode;
    return 0;
  }
  return -1;
}

int fgetc(FILE_ *f) { return rt_getbyte(f); }
int getc(FILE_ *f) { return rt_getbyte(f); }

int getchar(void) {
  rt_std_init();
  return rt_getbyte(&rt_stdin_file);
}

int ungetc(int c, FILE_ *f) {
  if (!f || c == RT_EOF || f->ungot >= 0)
    return RT_EOF;
  f->ungot = (unsigned char)c;
  f->eof = 0;
  return (unsigned char)c;
}

char *fgets(char *out, int n, FILE_ *f) {
  int i = 0;
  if (!out || n <= 0)
    return RT_NULL;
  while (i < n - 1) {
    int c = rt_getbyte(f);
    if (c == RT_EOF)
      break;
    out[i++] = (char)c;
    if (c == '\n')
      break;
  }
  if (i == 0)
    return RT_NULL;
  out[i] = 0;
  return out;
}

int fputc(int c, FILE_ *f) {
  unsigned char b = (unsigned char)c;
  if (!f || !rt_putbytes(f, &b, 1))
    return RT_EOF;
  return b;
}

int putc(int c, FILE_ *f) { return fputc(c, f); }

int putchar(int c) {
  rt_std_init();
  return fputc(c, &rt_stdout_file);
}

int fputs(const char *s, FILE_ *f) {
  if (!f || !rt_putbytes(f, (const unsigned char *)s, strlen(s)))
    return RT_EOF;
  return 0;
}

int puts(const char *s) {
  rt_std_init();
  if (fputs(s, &rt_stdout_file) < 0)
    return RT_EOF;
  if (fputc('\n', &rt_stdout_file) < 0)
    return RT_EOF;
  return 0;
}

int remove(const char *path) { return DeleteFileA(path) ? 0 : -1; }
int _unlink(const char *path) { return remove(path); }
int unlink(const char *path) { return remove(path); }

int rename(const char *from, const char *to) {
  /* MOVEFILE_REPLACE_EXISTING | MOVEFILE_COPY_ALLOWED */
  return MoveFileExA(from, to, 1u | 2u) ? 0 : -1;
}

int _fileno(FILE_ *f) {
  rt_std_init();
  if (f == &rt_stdin_file)
    return 0;
  if (f == &rt_stdout_file)
    return 1;
  if (f == &rt_stderr_file)
    return 2;
  return 3;
}

int _isatty(int fd) {
  void *h;
  rt_std_init();
  if (fd == 0)
    h = rt_stdin_file.h;
  else if (fd == 1)
    h = rt_stdout_file.h;
  else if (fd == 2)
    h = rt_stderr_file.h;
  else
    return 0;
  return GetFileType(h) == 2u; /* FILE_TYPE_CHAR */
}

int _access(const char *path, int mode) {
  unsigned attrs = GetFileAttributesA(path);
  if (attrs == 0xFFFFFFFFu)
    return -1;
  if ((mode & 2) && (attrs & 1u /*READONLY*/))
    return -1;
  return 0;
}
