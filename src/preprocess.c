#include "preprocess.h"

/* ---- macro table ---- */

typedef struct MacroArg {
  char *name;
} MacroArg;

typedef struct Macro {
  char *name;
  int function_like;
  int variadic; /* C99 ... / __VA_ARGS__ */
  char **params; /* stretchy names */
  char *body;    /* replacement list text */
  int predefined;
  struct Macro *next;
} Macro;

typedef struct {
  Macro *head;
  char **include_paths;
  Diag *diag;
  Arena *arena;
  char **include_stack; /* cycle detection */
  int if_depth;
  int *if_stack; /* stretchy: 0=dead, 1=active taking, 2=already taken branch */
  int output_disabled;
} PP;

static void pp_error(PP *pp, const char *path, int line, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "%s:%d:1: error: ", path ? path : "<pp>", line);
  vfprintf(stderr, fmt, ap);
  fputc('\n', stderr);
  va_end(ap);
  if (pp->diag)
    pp->diag->error_count++;
  (void)line;
}

static Macro *macro_find(PP *pp, const char *name) {
  for (Macro *m = pp->head; m; m = m->next)
    if (strcmp(m->name, name) == 0)
      return m;
  return NULL;
}

static void macro_define(PP *pp, Macro *m) {
  Macro *old = macro_find(pp, m->name);
  if (old && !old->predefined) {
    /* replace */
    old->function_like = m->function_like;
    old->variadic = m->variadic;
    old->params = m->params;
    old->body = m->body;
    return;
  }
  m->next = pp->head;
  pp->head = m;
}

static void macro_undef(PP *pp, const char *name) {
  Macro **slot = &pp->head;
  while (*slot) {
    if (strcmp((*slot)->name, name) == 0 && !(*slot)->predefined) {
      *slot = (*slot)->next;
      return;
    }
    slot = &(*slot)->next;
  }
}

static int is_ident_start(int c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}
static int is_ident_cont(int c) {
  return is_ident_start(c) || (c >= '0' && c <= '9');
}

/* ---- trigraphs + line splice (phases 1-2) ---- */

static char *phase12(Arena *a, const char *src, size_t n, size_t *out_n) {
  char *buf = NULL;
  /* Line splices delete a physical newline; re-emit the deleted newlines
   * after the logical line ends so physical line numbering is preserved
   * for diagnostics. */
  int pending_nl = 0;
  for (size_t i = 0; i < n;) {
    /* trigraphs */
    if (i + 2 < n && src[i] == '?' && src[i + 1] == '?') {
      char rep = 0;
      switch (src[i + 2]) {
      case '=': rep = '#'; break;
      case '/': rep = '\\'; break;
      case '\'': rep = '^'; break;
      case '(': rep = '['; break;
      case ')': rep = ']'; break;
      case '!': rep = '|'; break;
      case '<': rep = '{'; break;
      case '>': rep = '}'; break;
      case '-': rep = '~'; break;
      }
      if (rep) {
        buf_push(buf, rep);
        i += 3;
        continue;
      }
    }
    /* line splice */
    if (src[i] == '\\' && i + 1 < n && (src[i + 1] == '\n' ||
        (src[i + 1] == '\r' && i + 2 < n && src[i + 2] == '\n'))) {
      i += (src[i + 1] == '\r') ? 3 : 2;
      pending_nl++;
      continue;
    }
    if (src[i] == '\n') {
      buf_push(buf, '\n');
      while (pending_nl > 0) {
        buf_push(buf, '\n');
        pending_nl--;
      }
      i++;
      continue;
    }
    buf_push(buf, src[i]);
    i++;
  }
  while (pending_nl > 0) {
    buf_push(buf, '\n');
    pending_nl--;
  }
  buf_push(buf, '\0');
  *out_n = buf_len(buf) - 1;
  char *out = arena_strndup(a, buf, *out_n);
  buf_free(buf);
  return out;
}

/* ---- token helpers on raw text ---- */

static char *read_ident(Arena *a, const char *s, size_t n, size_t *i) {
  size_t start = *i;
  if (*i >= n || !is_ident_start((unsigned char)s[*i]))
    return NULL;
  (*i)++;
  while (*i < n && is_ident_cont((unsigned char)s[*i]))
    (*i)++;
  return arena_strndup(a, s + start, *i - start);
}

static long long eval_pp_expr(PP *pp, const char *path, int line, const char *expr);

/* ---- #if expression evaluator (identifiers -> 0 unless defined macro as 0-arg) ---- */

static const char *expr_s;
static size_t expr_n, expr_i;
static PP *expr_pp;
static const char *expr_path;
static int expr_line;

static void e_skip(void) {
  for (;;) {
    while (expr_i < expr_n &&
           (expr_s[expr_i] == ' ' || expr_s[expr_i] == '\t'))
      expr_i++;
    if (expr_i + 1 < expr_n && expr_s[expr_i] == '/' &&
        expr_s[expr_i + 1] == '/') {
      while (expr_i < expr_n && expr_s[expr_i] != '\n')
        expr_i++;
      continue;
    }
    break;
  }
}

static long long e_or(void);
static long long e_primary(void) {
  e_skip();
  if (expr_i >= expr_n)
    return 0;
  if (expr_s[expr_i] == '(') {
    expr_i++;
    long long v = e_or();
    e_skip();
    if (expr_i < expr_n && expr_s[expr_i] == ')')
      expr_i++;
    return v;
  }
  if (expr_s[expr_i] == '!') {
    expr_i++;
    return !e_primary();
  }
  if (expr_s[expr_i] == '~') {
    expr_i++;
    return ~e_primary();
  }
  if (expr_s[expr_i] == '+' || expr_s[expr_i] == '-') {
    int neg = expr_s[expr_i] == '-';
    expr_i++;
    long long v = e_primary();
    return neg ? -v : v;
  }
  if (is_ident_start((unsigned char)expr_s[expr_i])) {
    size_t st = expr_i;
    expr_i++;
    while (expr_i < expr_n && is_ident_cont((unsigned char)expr_s[expr_i]))
      expr_i++;
    char name[128];
    size_t len = expr_i - st;
    if (len >= sizeof(name))
      len = sizeof(name) - 1;
    memcpy(name, expr_s + st, len);
    name[len] = 0;
    if (strcmp(name, "defined") == 0) {
      e_skip();
      int paren = 0;
      if (expr_i < expr_n && expr_s[expr_i] == '(') {
        paren = 1;
        expr_i++;
        e_skip();
      }
      char *id = read_ident(expr_pp->arena, expr_s, expr_n, &expr_i);
      if (paren) {
        e_skip();
        if (expr_i < expr_n && expr_s[expr_i] == ')')
          expr_i++;
      }
      return id && macro_find(expr_pp, id) ? 1 : 0;
    }
    Macro *m = macro_find(expr_pp, name);
    if (m && !m->function_like && m->body) {
      /* expand object macro as number if possible */
      char *end = NULL;
      long long v = strtoll(m->body, &end, 0);
      if (end && end != m->body)
        return v;
    }
    return 0; /* unknown ident -> 0 in #if */
  }
  if (expr_s[expr_i] >= '0' && expr_s[expr_i] <= '9') {
    char *end = NULL;
    long long v = strtoll(expr_s + expr_i, &end, 0);
    if (end)
      expr_i = (size_t)(end - expr_s);
    /* skip U/L suffixes */
    while (expr_i < expr_n &&
           (expr_s[expr_i] == 'u' || expr_s[expr_i] == 'U' ||
            expr_s[expr_i] == 'l' || expr_s[expr_i] == 'L'))
      expr_i++;
    return v;
  }
  if (expr_s[expr_i] == '\'') {
    expr_i++;
    long long v = 0;
    if (expr_i < expr_n && expr_s[expr_i] == '\\') {
      expr_i++;
      if (expr_i < expr_n) {
        char c = expr_s[expr_i++];
        switch (c) {
        case 'n': v = '\n'; break;
        case 't': v = '\t'; break;
        case '0': v = 0; break;
        default: v = (unsigned char)c; break;
        }
      }
    } else if (expr_i < expr_n) {
      v = (unsigned char)expr_s[expr_i++];
    }
    if (expr_i < expr_n && expr_s[expr_i] == '\'')
      expr_i++;
    return v;
  }
  return 0;
}

static long long e_mul(void) {
  long long v = e_primary();
  for (;;) {
    e_skip();
    if (expr_i >= expr_n)
      return v;
    char op = expr_s[expr_i];
    if (op != '*' && op != '/' && op != '%')
      return v;
    expr_i++;
    long long r = e_primary();
    if (op == '*')
      v *= r;
    else if (op == '/')
      v = r ? v / r : 0;
    else
      v = r ? v % r : 0;
  }
}

static long long e_add(void) {
  long long v = e_mul();
  for (;;) {
    e_skip();
    if (expr_i >= expr_n)
      return v;
    char op = expr_s[expr_i];
    if (op != '+' && op != '-')
      return v;
    expr_i++;
    long long r = e_mul();
    v = (op == '+') ? v + r : v - r;
  }
}

static long long e_shift(void) {
  long long v = e_add();
  for (;;) {
    e_skip();
    if (expr_i + 1 < expr_n && expr_s[expr_i] == '<' &&
        expr_s[expr_i + 1] == '<') {
      expr_i += 2;
      v <<= e_add();
    } else if (expr_i + 1 < expr_n && expr_s[expr_i] == '>' &&
               expr_s[expr_i + 1] == '>') {
      expr_i += 2;
      v >>= e_add();
    } else
      return v;
  }
}

static long long e_rel(void) {
  long long v = e_shift();
  for (;;) {
    e_skip();
    if (expr_i + 1 < expr_n && expr_s[expr_i] == '<' &&
        expr_s[expr_i + 1] == '=') {
      expr_i += 2;
      v = v <= e_shift();
    } else if (expr_i + 1 < expr_n && expr_s[expr_i] == '>' &&
               expr_s[expr_i + 1] == '=') {
      expr_i += 2;
      v = v >= e_shift();
    } else if (expr_i < expr_n && expr_s[expr_i] == '<' &&
               !(expr_i + 1 < expr_n && expr_s[expr_i + 1] == '<')) {
      expr_i++;
      v = v < e_shift();
    } else if (expr_i < expr_n && expr_s[expr_i] == '>' &&
               !(expr_i + 1 < expr_n && expr_s[expr_i + 1] == '>')) {
      expr_i++;
      v = v > e_shift();
    } else
      return v;
  }
}

static long long e_eq(void) {
  long long v = e_rel();
  for (;;) {
    e_skip();
    if (expr_i + 1 < expr_n && expr_s[expr_i] == '=' &&
        expr_s[expr_i + 1] == '=') {
      expr_i += 2;
      v = v == e_rel();
    } else if (expr_i + 1 < expr_n && expr_s[expr_i] == '!' &&
               expr_s[expr_i + 1] == '=') {
      expr_i += 2;
      v = v != e_rel();
    } else
      return v;
  }
}

static long long e_band(void) {
  long long v = e_eq();
  for (;;) {
    e_skip();
    if (expr_i < expr_n && expr_s[expr_i] == '&' &&
        !(expr_i + 1 < expr_n && expr_s[expr_i + 1] == '&')) {
      expr_i++;
      v &= e_eq();
    } else
      return v;
  }
}

static long long e_bxor(void) {
  long long v = e_band();
  for (;;) {
    e_skip();
    if (expr_i < expr_n && expr_s[expr_i] == '^') {
      expr_i++;
      v ^= e_band();
    } else
      return v;
  }
}

static long long e_bor(void) {
  long long v = e_bxor();
  for (;;) {
    e_skip();
    if (expr_i < expr_n && expr_s[expr_i] == '|' &&
        !(expr_i + 1 < expr_n && expr_s[expr_i + 1] == '|')) {
      expr_i++;
      v |= e_bxor();
    } else
      return v;
  }
}

static long long e_and(void) {
  long long v = e_bor();
  for (;;) {
    e_skip();
    if (expr_i + 1 < expr_n && expr_s[expr_i] == '&' &&
        expr_s[expr_i + 1] == '&') {
      expr_i += 2;
      long long r = e_bor();
      v = (v && r) ? 1 : 0;
    } else
      return v;
  }
}

static long long e_or(void) {
  long long v = e_and();
  for (;;) {
    e_skip();
    if (expr_i + 1 < expr_n && expr_s[expr_i] == '|' &&
        expr_s[expr_i + 1] == '|') {
      expr_i += 2;
      long long r = e_and();
      v = (v || r) ? 1 : 0;
    } else
      return v;
  }
}

static long long eval_pp_expr(PP *pp, const char *path, int line,
                             const char *expr) {
  expr_pp = pp;
  expr_path = path;
  expr_line = line;
  expr_s = expr;
  expr_n = strlen(expr);
  expr_i = 0;
  return e_or();
}

/* ---- expansion ---- */

static char *expand_line(PP *pp, const char *path, int line, const char *in,
                         int disable_macro_name, int start_in_comment);

static char *stringify_arg(Arena *a, const char *arg) {
  char *out = NULL;
  buf_push(out, '"');
  for (const char *p = arg; *p; p++) {
    if (*p == '\\' || *p == '"')
      buf_push(out, '\\');
    if (*p != '\r')
      buf_push(out, *p);
  }
  buf_push(out, '"');
  buf_push(out, '\0');
  char *r = arena_strdup(a, out);
  buf_free(out);
  return r;
}

static char *subst_macro(PP *pp, const char *path, int line, Macro *m,
                         char **args, size_t nargs) {
  const char *body = m->body ? m->body : "";
  size_t n = strlen(body);
  char *out = NULL;
  size_t i = 0;
  while (i < n) {
    if (body[i] == '#') {
      if (i + 1 < n && body[i + 1] == '#') {
        /* paste handled after collecting left/right - simplify: emit paste op later */
        i += 2;
        /* trim space before already in out; skip space after */
        while (i < n && (body[i] == ' ' || body[i] == '\t'))
          i++;
        /* mark paste by removing trailing space from out and not adding space */
        while (buf_len(out) &&
               (out[buf_len(out) - 1] == ' ' || out[buf_len(out) - 1] == '\t'))
          BUF_HDR(out)->len--;
        /* next token will be appended glued */
        continue;
      }
      /* stringize */
      i++;
      while (i < n && (body[i] == ' ' || body[i] == '\t'))
        i++;
      size_t st = i;
      if (i < n && is_ident_start((unsigned char)body[i])) {
        i++;
        while (i < n && is_ident_cont((unsigned char)body[i]))
          i++;
        char *pname = arena_strndup(pp->arena, body + st, i - st);
        int found = -1;
        for (size_t p = 0; p < buf_len(m->params); p++)
          if (strcmp(m->params[p], pname) == 0) {
            found = (int)p;
            break;
          }
        if (found >= 0 && (size_t)found < nargs) {
          char *s = stringify_arg(pp->arena, args[found] ? args[found] : "");
          for (char *q = s; *q; q++)
            buf_push(out, *q);
        }
      }
      continue;
    }
    if (is_ident_start((unsigned char)body[i])) {
      size_t st = i;
      i++;
      while (i < n && is_ident_cont((unsigned char)body[i]))
        i++;
      char *id = arena_strndup(pp->arena, body + st, i - st);
      int found = -1;
      for (size_t p = 0; p < buf_len(m->params); p++)
        if (strcmp(m->params[p], id) == 0) {
          found = (int)p;
          break;
        }
      if (found >= 0 && (size_t)found < nargs) {
        const char *a = args[found] ? args[found] : "";
        for (const char *q = a; *q; q++)
          buf_push(out, *q);
      } else if (m->variadic && strcmp(id, "__VA_ARGS__") == 0) {
        size_t fixed = buf_len(m->params);
        for (size_t a = fixed; a < nargs; a++) {
          if (a > fixed)
            buf_push(out, ',');
          const char *av = args[a] ? args[a] : "";
          for (const char *q = av; *q; q++)
            buf_push(out, *q);
        }
      } else {
        for (const char *q = id; *q; q++)
          buf_push(out, *q);
      }
      continue;
    }
    buf_push(out, body[i]);
    i++;
  }
  buf_push(out, '\0');
  char *substituted = arena_strdup(pp->arena, out);
  buf_free(out);
  /* rescan */
  return expand_line(pp, path, line, substituted, /*disable*/ 0, 0);
}

static char *expand_line(PP *pp, const char *path, int line, const char *in,
                         int disable_macro_name, int start_in_comment) {
  (void)disable_macro_name;
  size_t n = strlen(in);
  char *out = NULL;
  size_t i = 0;
  /* the line begins inside a block comment: copy verbatim until it closes */
  if (start_in_comment) {
    while (i < n && !(in[i] == '*' && i + 1 < n && in[i + 1] == '/'))
      buf_push(out, in[i++]);
    if (i + 1 < n) {
      buf_push(out, in[i++]);
      buf_push(out, in[i++]);
    }
  }
  while (i < n) {
    /* Do not expand inside comments (C: comments are spaces before macros). */
    if (in[i] == '/' && i + 1 < n && in[i + 1] == '/') {
      while (i < n)
        buf_push(out, in[i++]);
      break;
    }
    if (in[i] == '/' && i + 1 < n && in[i + 1] == '*') {
      buf_push(out, in[i++]);
      buf_push(out, in[i++]);
      while (i + 1 < n && !(in[i] == '*' && in[i + 1] == '/'))
        buf_push(out, in[i++]);
      if (i + 1 < n) {
        buf_push(out, in[i++]);
        buf_push(out, in[i++]);
      }
      continue;
    }
    if (in[i] == '"' || in[i] == '\'') {
      char q = in[i++];
      buf_push(out, q);
      while (i < n && in[i] != q) {
        if (in[i] == '\\' && i + 1 < n) {
          buf_push(out, in[i++]);
          buf_push(out, in[i++]);
        } else
          buf_push(out, in[i++]);
      }
      if (i < n)
        buf_push(out, in[i++]);
      continue;
    }
    if (is_ident_start((unsigned char)in[i])) {
      size_t st = i;
      i++;
      while (i < n && is_ident_cont((unsigned char)in[i]))
        i++;
      char *id = arena_strndup(pp->arena, in + st, i - st);
      /* dynamic predefined macros */
      if (strcmp(id, "__FILE__") == 0) {
        buf_push(out, '"');
        for (const char *q = path ? path : "<pp>"; *q; q++)
          buf_push(out, *q == '\\' ? '/' : *q);
        buf_push(out, '"');
        continue;
      }
      if (strcmp(id, "__LINE__") == 0) {
        char tmp[16];
        snprintf(tmp, sizeof(tmp), "%d", line);
        for (const char *q = tmp; *q; q++)
          buf_push(out, *q);
        continue;
      }
      Macro *m = macro_find(pp, id);
      if (!m) {
        for (const char *q = id; *q; q++)
          buf_push(out, *q);
        continue;
      }
      if (!m->function_like) {
        char *exp = subst_macro(pp, path, line, m, NULL, 0);
        for (const char *q = exp; *q; q++)
          buf_push(out, *q);
        continue;
      }
      /* function-like: need ( */
      size_t save = i;
      while (i < n && (in[i] == ' ' || in[i] == '\t'))
        i++;
      if (i >= n || in[i] != '(') {
        i = save;
        for (const char *q = id; *q; q++)
          buf_push(out, *q);
        continue;
      }
      i++; /* ( */
      char **args = NULL;
      if (i < n && in[i] == ')') {
        i++;
      } else {
        for (;;) {
          int depth = 0;
          size_t ast = i;
          while (i < n) {
            if (in[i] == '"' || in[i] == '\'') {
              char q = in[i++];
              while (i < n && in[i] != q) {
                if (in[i] == '\\' && i + 1 < n)
                  i += 2;
                else
                  i++;
              }
              if (i < n)
                i++;
              continue;
            }
            if (in[i] == '(')
              depth++;
            else if (in[i] == ')') {
              if (depth == 0)
                break;
              depth--;
            } else if (in[i] == ',' && depth == 0)
              break;
            i++;
          }
          char *arg = arena_strndup(pp->arena, in + ast, i - ast);
          /* trim */
          while (*arg == ' ' || *arg == '\t')
            arg++;
          buf_push(args, arg);
          if (i < n && in[i] == ',') {
            i++;
            continue;
          }
          if (i < n && in[i] == ')') {
            i++;
            break;
          }
          break;
        }
      }
      char *exp =
          subst_macro(pp, path, line, m, args, buf_len(args));
      buf_free(args);
      for (const char *q = exp; *q; q++)
        buf_push(out, *q);
      continue;
    }
    buf_push(out, in[i]);
    i++;
  }
  buf_push(out, '\0');
  char *r = arena_strdup(pp->arena, out);
  buf_free(out);
  return r;
}

/* ---- include file search ---- */

static char *join_path(Arena *a, const char *dir, const char *file) {
  if (!dir || !dir[0])
    return arena_strdup(a, file);
  size_t n = strlen(dir);
  if (dir[n - 1] == '/' || dir[n - 1] == '\\')
    return arena_sprintf(a, "%s%s", dir, file);
  return arena_sprintf(a, "%s/%s", dir, file);
}

static char *dir_of(Arena *a, const char *path) {
  const char *slash = strrchr(path, '/');
  const char *bslash = strrchr(path, '\\');
  const char *p = slash;
  if (bslash && (!p || bslash > p))
    p = bslash;
  if (!p)
    return arena_strdup(a, ".");
  return arena_strndup(a, path, (size_t)(p - path));
}

static int file_exists(const char *path) {
  FILE *f = fopen(path, "rb");
  if (!f)
    return 0;
  fclose(f);
  return 1;
}

static char *find_include(PP *pp, const char *from_path, const char *name,
                          int angled) {
  if (!angled) {
    char *cand = join_path(pp->arena, dir_of(pp->arena, from_path), name);
    if (file_exists(cand))
      return cand;
  }
  for (size_t i = 0; i < buf_len(pp->include_paths); i++) {
    char *cand = join_path(pp->arena, pp->include_paths[i], name);
    if (file_exists(cand))
      return cand;
  }
  /* also try cwd */
  if (file_exists(name))
    return arena_strdup(pp->arena, name);
  return NULL;
}

/* ---- process one file ---- */

static char *preprocess_buffer(PP *pp, const char *path, const char *src0,
                               size_t n0);

static void install_predefs(PP *pp) {
  const char *defs[][2] = {
      {"__C99METTLE__", "1"},
      {"__STDC__", "1"},
      {"__STDC_VERSION__", "199901L"},
      {"__x86_64__", "1"},
      {"_WIN32", "1"},
      {"_WIN64", "1"},
      {"__WIN32__", "1"},
      {NULL, NULL},
  };
  for (int i = 0; defs[i][0]; i++) {
    Macro *m = (Macro *)arena_calloc(pp->arena, sizeof(Macro));
    m->name = arena_strdup(pp->arena, defs[i][0]);
    m->body = arena_strdup(pp->arena, defs[i][1]);
    m->predefined = 1;
    macro_define(pp, m);
  }
  /* MSVC/GCC decorations accepted as no-ops (Win64 has one calling
   * convention; attributes do not affect this backend). */
  const char *fn_noops[] = {"__declspec", "__attribute__", "__pragma", NULL};
  for (int i = 0; fn_noops[i]; i++) {
    Macro *m = (Macro *)arena_calloc(pp->arena, sizeof(Macro));
    m->name = arena_strdup(pp->arena, fn_noops[i]);
    m->function_like = 1;
    buf_push(m->params, arena_strdup(pp->arena, "x"));
    m->body = arena_strdup(pp->arena, "");
    m->predefined = 1;
    macro_define(pp, m);
  }
  const char *obj_noops[] = {"__stdcall", "__cdecl",       "__fastcall",
                             "__forceinline", "__inline",  "__restrict",
                             "__unaligned",   NULL};
  for (int i = 0; obj_noops[i]; i++) {
    Macro *m = (Macro *)arena_calloc(pp->arena, sizeof(Macro));
    m->name = arena_strdup(pp->arena, obj_noops[i]);
    m->body = arena_strdup(pp->arena, "");
    m->predefined = 1;
    macro_define(pp, m);
  }
  /* __builtin_popcount as a pure-expression macro (argument is evaluated
   * multiple times; standard SWAR popcount). Composed programmatically so
   * the parens are right by construction. */
  {
    const char *A =
        "(((unsigned)(x)) - ((((unsigned)(x)) >> 1) & 0x55555555u))";
    char *B = arena_sprintf(pp->arena,
                            "(((%s) & 0x33333333u) + (((%s) >> 2) & "
                            "0x33333333u))",
                            A, A);
    char *body = arena_sprintf(
        pp->arena,
        "((int)((((((%s) + ((%s) >> 4)) & 0x0f0f0f0fu) * 0x01010101u) >> 24) "
        "& 0xff))",
        B, B);
    Macro *m = (Macro *)arena_calloc(pp->arena, sizeof(Macro));
    m->name = arena_strdup(pp->arena, "__builtin_popcount");
    m->function_like = 1;
    buf_push(m->params, arena_strdup(pp->arena, "x"));
    m->body = body;
    m->predefined = 1;
    macro_define(pp, m);
  }
  /* Complex imaginary unit: only if user includes complex support.
   * Do NOT predefine bare `I` — it corrupts identifiers/comments (e.g. -I). */
}

/* Paren depth of one physical line, ignoring strings, chars, and comments.
 * `*in_comment` carries block-comment state across lines. */
static int line_paren_depth_c2(const char *s, int depth, int *in_comment,
                               size_t *line_comment_at) {
  size_t i = 0;
  size_t n = strlen(s);
  if (line_comment_at)
    *line_comment_at = n;
  while (i < n) {
    if (*in_comment) {
      if (s[i] == '*' && i + 1 < n && s[i + 1] == '/') {
        *in_comment = 0;
        i += 2;
        continue;
      }
      i++;
      continue;
    }
    char c = s[i];
    if (c == '/' && i + 1 < n && s[i + 1] == '/') {
      if (line_comment_at)
        *line_comment_at = i;
      break;
    }
    if (c == '/' && i + 1 < n && s[i + 1] == '*') {
      *in_comment = 1;
      i += 2;
      continue;
    }
    if (c == '"' || c == '\'') {
      char q = c;
      i++;
      while (i < n && s[i] != q) {
        if (s[i] == '\\' && i + 1 < n)
          i += 2;
        else
          i++;
      }
      if (i < n)
        i++;
      continue;
    }
    if (c == '(')
      depth++;
    else if (c == ')')
      depth--;
    i++;
  }
  return depth;
}

static int line_paren_depth_c(const char *s, int depth, int *in_comment) {
  return line_paren_depth_c2(s, depth, in_comment, NULL);
}

/* Emit a GCC-style line marker: `# <line> "<file>"` — the lexer resyncs
 * its reported location from these so diagnostics point at real source. */
static void emit_line_marker(char **out, int line, const char *path) {
  char tmp[32];
  buf_push(*out, '#');
  buf_push(*out, ' ');
  snprintf(tmp, sizeof(tmp), "%d", line);
  for (char *q = tmp; *q; q++)
    buf_push(*out, *q);
  buf_push(*out, ' ');
  buf_push(*out, '"');
  for (const char *q = path; *q; q++)
    buf_push(*out, *q == '\\' ? '/' : *q);
  buf_push(*out, '"');
  buf_push(*out, '\n');
}

static char *preprocess_buffer(PP *pp, const char *path, const char *src0,
                               size_t n0) {
  /* skip UTF-8 BOM */
  if (n0 >= 3 && (unsigned char)src0[0] == 0xEF &&
      (unsigned char)src0[1] == 0xBB && (unsigned char)src0[2] == 0xBF) {
    src0 += 3;
    n0 -= 3;
  }
  size_t n = 0;
  char *src = phase12(pp->arena, src0, n0, &n);
  char *out = NULL;
  size_t i = 0;
  int line = 1;

  /* include guard: stack */
  for (size_t k = 0; k < buf_len(pp->include_stack); k++) {
    if (strcmp(pp->include_stack[k], path) == 0) {
      pp_error(pp, path, 1, "include cycle");
      return arena_strdup(pp->arena, "");
    }
  }
  buf_push(pp->include_stack, arena_strdup(pp->arena, path));
  emit_line_marker(&out, 1, path);

  /* block-comment state persisting across physical text lines */
  int text_in_comment = 0;

  while (i < n) {
    size_t line_start = i;
    /* find end of line */
    size_t j = i;
    while (j < n && src[j] != '\n')
      j++;
    size_t line_end = j;
    size_t next = (j < n) ? j + 1 : j;

    /* raw line without newline */
    char *raw = arena_strndup(pp->arena, src + line_start, line_end - line_start);
    /* strip \r */
    size_t rl = strlen(raw);
    if (rl && raw[rl - 1] == '\r')
      raw[rl - 1] = 0;

    /* directive? (not when the line starts inside a block comment) */
    size_t k = 0;
    while (raw[k] == ' ' || raw[k] == '\t')
      k++;
    if (!text_in_comment && raw[k] == '#') {
      k++;
      while (raw[k] == ' ' || raw[k] == '\t')
        k++;
      char *dir = read_ident(pp->arena, raw, strlen(raw), &k);
      while (raw[k] == ' ' || raw[k] == '\t')
        k++;
      const char *rest = raw + k;

      if (dir && strcmp(dir, "endif") == 0) {
        if (buf_len(pp->if_stack) == 0)
          pp_error(pp, path, line, "#endif without #if");
        else {
          BUF_HDR(pp->if_stack)->len--;
          pp->output_disabled = 0;
          for (size_t t = 0; t < buf_len(pp->if_stack); t++)
            if (pp->if_stack[t] != 1)
              pp->output_disabled = 1;
        }
      } else if (dir && strcmp(dir, "else") == 0) {
        if (buf_len(pp->if_stack) == 0)
          pp_error(pp, path, line, "#else without #if");
        else {
          int st = pp->if_stack[buf_len(pp->if_stack) - 1];
          if (st == 1)
            pp->if_stack[buf_len(pp->if_stack) - 1] = 2; /* done */
          else if (st == 0)
            pp->if_stack[buf_len(pp->if_stack) - 1] = 1;
          pp->output_disabled = 0;
          for (size_t t = 0; t < buf_len(pp->if_stack); t++)
            if (pp->if_stack[t] != 1)
              pp->output_disabled = 1;
        }
      } else if (dir && (strcmp(dir, "elif") == 0)) {
        if (buf_len(pp->if_stack) == 0)
          pp_error(pp, path, line, "#elif without #if");
        else {
          int st = pp->if_stack[buf_len(pp->if_stack) - 1];
          if (st == 1)
            pp->if_stack[buf_len(pp->if_stack) - 1] = 2;
          else if (st == 0) {
            long long v = eval_pp_expr(pp, path, line, rest);
            pp->if_stack[buf_len(pp->if_stack) - 1] = v ? 1 : 0;
          }
          pp->output_disabled = 0;
          for (size_t t = 0; t < buf_len(pp->if_stack); t++)
            if (pp->if_stack[t] != 1)
              pp->output_disabled = 1;
        }
      } else if (dir && (strcmp(dir, "if") == 0 || strcmp(dir, "ifdef") == 0 ||
                         strcmp(dir, "ifndef") == 0)) {
        int take = 0;
        if (strcmp(dir, "if") == 0)
          take = eval_pp_expr(pp, path, line, rest) != 0;
        else {
          size_t ii = 0;
          char *name = read_ident(pp->arena, rest, strlen(rest), &ii);
          int def = name && macro_find(pp, name);
          take = (strcmp(dir, "ifdef") == 0) ? def : !def;
        }
        /* if parent disabled, force dead */
        int parent_dead = pp->output_disabled;
        int state = parent_dead ? 2 : (take ? 1 : 0);
        buf_push(pp->if_stack, state);
        pp->output_disabled = 0;
        for (size_t t = 0; t < buf_len(pp->if_stack); t++)
          if (pp->if_stack[t] != 1)
            pp->output_disabled = 1;
      } else if (!pp->output_disabled) {
        if (dir && strcmp(dir, "define") == 0) {
          size_t ii = 0;
          char *name = read_ident(pp->arena, rest, strlen(rest), &ii);
          if (!name) {
            pp_error(pp, path, line, "#define missing name");
          } else {
            Macro *m = (Macro *)arena_calloc(pp->arena, sizeof(Macro));
            m->name = name;
            if (rest[ii] == '(') {
              m->function_like = 1;
              ii++;
              while (rest[ii] == ' ' || rest[ii] == '\t')
                ii++;
              if (rest[ii] != ')') {
                for (;;) {
                  while (rest[ii] == ' ' || rest[ii] == '\t')
                    ii++;
                  if (rest[ii] == '.' && rest[ii + 1] == '.' &&
                      rest[ii + 2] == '.') {
                    m->variadic = 1;
                    ii += 3;
                    break;
                  }
                  char *pn = read_ident(pp->arena, rest, strlen(rest), &ii);
                  if (pn)
                    buf_push(m->params, pn);
                  while (rest[ii] == ' ' || rest[ii] == '\t')
                    ii++;
                  if (rest[ii] == ',') {
                    ii++;
                    continue;
                  }
                  break;
                }
              }
              while (rest[ii] && rest[ii] != ')')
                ii++;
              if (rest[ii] == ')')
                ii++;
            }
            while (rest[ii] == ' ' || rest[ii] == '\t')
              ii++;
            m->body = arena_strdup(pp->arena, rest + ii);
            /* strip trailing space */
            size_t bl = strlen(m->body);
            while (bl && (m->body[bl - 1] == ' ' || m->body[bl - 1] == '\t'))
              m->body[--bl] = 0;
            macro_define(pp, m);
          }
        } else if (dir && strcmp(dir, "undef") == 0) {
          size_t ii = 0;
          char *name = read_ident(pp->arena, rest, strlen(rest), &ii);
          if (name)
            macro_undef(pp, name);
        } else if (dir && strcmp(dir, "include") == 0) {
          char *inc_name = NULL;
          int angled = 0;
          if (rest[0] == '"') {
            size_t e = 1;
            while (rest[e] && rest[e] != '"')
              e++;
            inc_name = arena_strndup(pp->arena, rest + 1, e - 1);
          } else if (rest[0] == '<') {
            angled = 1;
            size_t e = 1;
            while (rest[e] && rest[e] != '>')
              e++;
            inc_name = arena_strndup(pp->arena, rest + 1, e - 1);
          } else {
            /* macro-expanded include */
            char *exp = expand_line(pp, path, line, rest, 0, 0);
            while (*exp == ' ' || *exp == '\t')
              exp++;
            if (exp[0] == '"') {
              size_t e = 1;
              while (exp[e] && exp[e] != '"')
                e++;
              inc_name = arena_strndup(pp->arena, exp + 1, e - 1);
            } else if (exp[0] == '<') {
              angled = 1;
              size_t e = 1;
              while (exp[e] && exp[e] != '>')
                e++;
              inc_name = arena_strndup(pp->arena, exp + 1, e - 1);
            }
          }
          if (!inc_name) {
            pp_error(pp, path, line, "bad #include");
          } else {
            char *found = find_include(pp, path, inc_name, angled);
            if (!found) {
              pp_error(pp, path, line, "cannot find include \"%s\"", inc_name);
            } else {
              size_t flen = 0;
              char *fsrc = read_file(found, &flen);
              if (!fsrc)
                pp_error(pp, path, line, "cannot read \"%s\"", found);
              else {
                char *inc_out = preprocess_buffer(pp, found, fsrc, flen);
                free(fsrc);
                for (char *q = inc_out; *q; q++)
                  buf_push(out, *q);
                if (buf_len(out) == 0 || out[buf_len(out) - 1] != '\n')
                  buf_push(out, '\n');
                /* resync to the line after the #include directive */
                emit_line_marker(&out, line + 1, path);
                i = next;
                line++;
                continue;
              }
            }
          }
        } else if (dir && strcmp(dir, "error") == 0) {
          pp_error(pp, path, line, "#error %s", rest);
        } else if (dir && strcmp(dir, "line") == 0) {
          /* #line number ["file"] */
          char *exp = expand_line(pp, path, line, rest, 0, 0);
          while (*exp == ' ' || *exp == '\t')
            exp++;
          long new_line = strtol(exp, &exp, 10);
          while (*exp == ' ' || *exp == '\t')
            exp++;
          const char *new_file = path;
          if (*exp == '"') {
            exp++;
            size_t e = 0;
            while (exp[e] && exp[e] != '"')
              e++;
            new_file = arena_strndup(pp->arena, exp, e);
          }
          if (new_line > 0) {
            emit_line_marker(&out, (int)new_line, new_file);
            i = next;
            line++;
            continue;
          }
        } else if (dir && strcmp(dir, "pragma") == 0) {
          /* ignore */
        } else if (dir) {
          /* unknown directive: ignore in this subset or error */
        }
      }
      buf_push(out, '\n');
      i = next;
      line++;
      continue;
    }

    if (!pp->output_disabled) {
      /* Function-like macro invocations may span physical lines. If this
       * line leaves '(' unbalanced (outside strings/chars/comments), merge
       * following lines into one logical line, then pad with blank lines to
       * preserve numbering. Directives stop the merge. */
      int merged_lines = 0;
      size_t cmt_at = 0;
      int line_start_in_comment = text_in_comment;
      int depth = line_paren_depth_c2(raw, 0, &text_in_comment, &cmt_at);
      char *logical = raw;
      if (depth > 0 && cmt_at < strlen(raw))
        logical = arena_strndup(pp->arena, raw, cmt_at);
      while (depth > 0 && next < n && merged_lines < 512) {
        /* peek next line; stop at directives */
        size_t ls = next;
        size_t le = ls;
        while (le < n && src[le] != '\n')
          le++;
        size_t k2 = ls;
        while (k2 < le && (src[k2] == ' ' || src[k2] == '\t'))
          k2++;
        if (k2 < le && src[k2] == '#')
          break;
        char *nraw = arena_strndup(pp->arena, src + ls, le - ls);
        size_t nl = strlen(nraw);
        if (nl && nraw[nl - 1] == '\r')
          nraw[nl - 1] = 0;
        size_t ncmt = 0;
        depth = line_paren_depth_c2(nraw, depth, &text_in_comment, &ncmt);
        if (ncmt < strlen(nraw))
          nraw = arena_strndup(pp->arena, nraw, ncmt);
        logical = arena_sprintf(pp->arena, "%s %s", logical, nraw);
        next = (le < n) ? le + 1 : le;
        merged_lines++;
      }
      char *exp = expand_line(pp, path, line, logical, 0, line_start_in_comment);
      for (char *q = exp; *q; q++)
        buf_push(out, *q);
      for (int m = 0; m < merged_lines; m++)
        buf_push(out, '\n');
      line += merged_lines;
    }
    buf_push(out, '\n');
    i = next;
    line++;
  }

  /* pop include stack */
  if (buf_len(pp->include_stack))
    BUF_HDR(pp->include_stack)->len--;

  buf_push(out, '\0');
  char *result = arena_strdup(pp->arena, out);
  buf_free(out);
  return result;
}

char *preprocess_source(PreprocessOptions *opt, const char *path,
                        const char *src, size_t src_len, size_t *out_len) {
  PP pp;
  memset(&pp, 0, sizeof(pp));
  pp.diag = opt->diag;
  pp.arena = opt->arena;
  pp.include_paths = opt->paths;
  install_predefs(&pp);
  char *r = preprocess_buffer(&pp, path, src, src_len);
  if (out_len)
    *out_len = r ? strlen(r) : 0;
  return r;
}

char *preprocess_file(PreprocessOptions *opt, const char *path,
                      size_t *out_len) {
  size_t n = 0;
  char *src = read_file(path, &n);
  if (!src) {
    if (opt->diag)
      diag_error(opt->diag, (SrcLoc){path, 1, 1}, "cannot read file");
    return NULL;
  }
  char *r = preprocess_source(opt, path, src, n, out_len);
  free(src);
  return r;
}
