/* Program arguments and environment, with no CRT underneath.
 *
 * The mtlc startup object calls __getmainargs by msvcrt's signature before
 * main, so the runtime defines that symbol itself: GetCommandLineA plus the
 * MS quoting rules. Object symbols shadow DLL exports in the PE linker, so
 * merely defining it here is what removes the msvcrt import. */
#include "c99rt.h"

#define RT_NULL ((void *)0)

void *malloc(rt_size);
void *realloc(void *, rt_size);
rt_size strlen(const char *);
void *memcpy(void *, const void *, rt_size);
int strncmp(const char *, const char *, rt_size);

static char **rt_argv;
static int rt_argc;
static char **rt_envp;

int __c99m_errno_slot;

int *_errno(void) { return &__c99m_errno_slot; }

/* Split GetCommandLineA per the rules argv construction has always used:
 * backslashes are literal unless they precede a quote; 2n backslashes + quote
 * = n backslashes and toggle quoting; 2n+1 backslashes + quote = n
 * backslashes and a literal quote. The program name itself follows simpler
 * rules: quotes toggle, everything else is literal. */
static void rt_parse_cmdline(void) {
  const char *cmd = GetCommandLineA();
  rt_size len = strlen(cmd);
  /* worst case: every char its own argument */
  char *pool = (char *)malloc(len + 2);
  char **argv = (char **)malloc((len / 2 + 3) * sizeof(char *));
  char *out = pool;
  const char *p = cmd;
  int argc = 0;

  if (!pool || !argv) {
    rt_argc = 0;
    rt_argv = RT_NULL;
    return;
  }

  /* argv[0] */
  argv[argc++] = out;
  if (*p == '"') {
    p++;
    while (*p && *p != '"')
      *out++ = *p++;
    if (*p == '"')
      p++;
  } else {
    while (*p && *p != ' ' && *p != '\t')
      *out++ = *p++;
  }
  *out++ = 0;

  for (;;) {
    while (*p == ' ' || *p == '\t')
      p++;
    if (!*p)
      break;
    argv[argc++] = out;
    {
      int quoted = 0;
      while (*p) {
        if (!quoted && (*p == ' ' || *p == '\t'))
          break;
        if (*p == '\\') {
          rt_size slashes = 0;
          while (*p == '\\') {
            slashes++;
            p++;
          }
          if (*p == '"') {
            rt_size i;
            for (i = 0; i < slashes / 2; i++)
              *out++ = '\\';
            if (slashes % 2) {
              *out++ = '"';
              p++;
            } else {
              quoted = !quoted;
              p++;
            }
          } else {
            rt_size i;
            for (i = 0; i < slashes; i++)
              *out++ = '\\';
          }
          continue;
        }
        if (*p == '"') {
          quoted = !quoted;
          p++;
          continue;
        }
        *out++ = *p++;
      }
    }
    *out++ = 0;
  }
  argv[argc] = RT_NULL;
  rt_argc = argc;
  rt_argv = argv;
}

static void rt_parse_env(void) {
  char *block = GetEnvironmentStringsA();
  char *p = block;
  int count = 0;
  char **v;
  if (!block) {
    rt_envp = RT_NULL;
    return;
  }
  while (*p) {
    count++;
    p += strlen(p) + 1;
  }
  v = (char **)malloc((rt_size)(count + 1) * sizeof(char *));
  if (!v) {
    rt_envp = RT_NULL;
    return;
  }
  p = block;
  count = 0;
  while (*p) {
    v[count++] = p;
    p += strlen(p) + 1;
  }
  v[count] = RT_NULL;
  rt_envp = v;
}

int __getmainargs(int *argc, char ***argv, char ***envp, int expand,
                  void *startinfo) {
  (void)expand;
  (void)startinfo;
  rt_parse_cmdline();
  rt_parse_env();
  if (argc)
    *argc = rt_argc;
  if (argv)
    *argv = rt_argv;
  if (envp)
    *envp = rt_envp;
  return 0;
}

char *getenv(const char *name) {
  rt_size n;
  int i;
  if (!name)
    return RT_NULL;
  if (!rt_envp)
    rt_parse_env();
  if (!rt_envp)
    return RT_NULL;
  n = strlen(name);
  for (i = 0; rt_envp[i]; i++) {
    if (strncmp(rt_envp[i], name, n) == 0 && rt_envp[i][n] == '=')
      return rt_envp[i] + n + 1;
  }
  return RT_NULL;
}

int _putenv(const char *assignment) {
  /* NAME=value, or NAME= to delete */
  rt_size i = 0;
  char name[256];
  const char *eq = RT_NULL;
  const char *p;
  for (p = assignment; *p; p++)
    if (*p == '=') {
      eq = p;
      break;
    }
  if (!eq)
    return -1;
  for (p = assignment; p != eq && i + 1 < sizeof(name); p++)
    name[i++] = *p;
  name[i] = 0;
  if (!SetEnvironmentVariableA(name, eq[1] ? eq + 1 : RT_NULL))
    return -1;
  rt_envp = RT_NULL; /* rebuilt on next getenv */
  return 0;
}

/* The startup object calls ExitProcess directly when main returns, so the
 * flush has to live behind that very symbol: defining it here shadows the
 * kernel32 import, and the real exit happens through TerminateProcess,
 * which stays an import because nothing defines it. */
int TerminateProcess(void *proc, unsigned code);
void *GetCurrentProcess(void);

void ExitProcess(unsigned code) {
  __c99m_stdio_flush_all();
  TerminateProcess(GetCurrentProcess(), code);
}

void exit(int code) { ExitProcess((unsigned)code); }

void abort(void) { ExitProcess(3u); }
