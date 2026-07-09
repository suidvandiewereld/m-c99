#include "lexer.h"

void lexer_init(Lexer *L, Arena *arena, Diag *diag, const char *path,
                const char *src, size_t len) {
  memset(L, 0, sizeof(*L));
  L->path = path;
  L->src = src;
  L->len = len;
  L->pos = 0;
  L->line = 1;
  L->col = 1;
  L->arena = arena;
  L->diag = diag;
}

void lexer_add_typedef(Lexer *L, const char *name) {
  for (size_t i = 0; i < buf_len(L->typedef_names); i++)
    if (strcmp(L->typedef_names[i], name) == 0)
      return;
  buf_push(L->typedef_names, arena_strdup(L->arena, name));
}

int lexer_is_typedef(Lexer *L, const char *name) {
  for (size_t i = 0; i < buf_len(L->typedef_names); i++)
    if (strcmp(L->typedef_names[i], name) == 0)
      return 1;
  return 0;
}

static int at_end(Lexer *L) { return L->pos >= L->len; }

static char peek(Lexer *L) {
  return at_end(L) ? '\0' : L->src[L->pos];
}

static char peek2(Lexer *L) {
  return (L->pos + 1 >= L->len) ? '\0' : L->src[L->pos + 1];
}

static char advance(Lexer *L) {
  char c = L->src[L->pos++];
  if (c == '\n') {
    L->line++;
    L->col = 1;
  } else {
    L->col++;
  }
  return c;
}

static SrcLoc loc_here(Lexer *L) {
  SrcLoc loc = {L->path, L->line, L->col};
  return loc;
}

static Token make_tok(Lexer *L, TokenKind kind, const char *start, int len) {
  Token t;
  memset(&t, 0, sizeof(t));
  t.kind = kind;
  t.loc.file = L->path;
  /* compute start location: backtrack columns is hard; store from start */
  t.loc.line = L->line;
  t.loc.col = L->col - len;
  if (t.loc.col < 1)
    t.loc.col = 1;
  t.start = start;
  t.len = len;
  return t;
}

static void skip_whitespace_and_comments(Lexer *L) {
  for (;;) {
    char c = peek(L);
    if (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\v' ||
        c == '\f') {
      advance(L);
      continue;
    }
    if (c == '/' && peek2(L) == '/') {
      while (!at_end(L) && peek(L) != '\n')
        advance(L);
      continue;
    }
    if (c == '/' && peek2(L) == '*') {
      SrcLoc start = loc_here(L);
      advance(L);
      advance(L);
      while (!at_end(L) && !(peek(L) == '*' && peek2(L) == '/'))
        advance(L);
      if (at_end(L)) {
        diag_error(L->diag, start, "unterminated block comment");
        return;
      }
      advance(L);
      advance(L);
      continue;
    }
    /* Residual directives after preprocess (e.g. unknown): skip line. */
    if (c == '#') {
      while (!at_end(L) && peek(L) != '\n')
        advance(L);
      continue;
    }
    break;
  }
}

static TokenKind keyword_kind(const char *s, int len) {
  /* length-gated keyword table */
#define KW(str, kind)                                                          \
  if (len == (int)sizeof(str) - 1 && memcmp(s, str, len) == 0)                  \
  return kind
  KW("auto", TK_AUTO);
  KW("break", TK_BREAK);
  KW("case", TK_CASE);
  KW("char", TK_CHAR_KW);
  KW("const", TK_CONST);
  KW("continue", TK_CONTINUE);
  KW("default", TK_DEFAULT);
  KW("do", TK_DO);
  KW("double", TK_DOUBLE);
  KW("else", TK_ELSE);
  KW("enum", TK_ENUM);
  KW("extern", TK_EXTERN);
  KW("float", TK_FLOAT_KW);
  KW("for", TK_FOR);
  KW("goto", TK_GOTO);
  KW("if", TK_IF);
  KW("inline", TK_INLINE);
  KW("int", TK_INT_KW);
  KW("long", TK_LONG);
  KW("register", TK_REGISTER);
  KW("restrict", TK_RESTRICT);
  KW("return", TK_RETURN);
  KW("short", TK_SHORT);
  KW("signed", TK_SIGNED);
  KW("sizeof", TK_SIZEOF);
  KW("static", TK_STATIC);
  KW("struct", TK_STRUCT);
  KW("switch", TK_SWITCH);
  KW("typedef", TK_TYPEDEF);
  KW("union", TK_UNION);
  KW("unsigned", TK_UNSIGNED);
  KW("void", TK_VOID);
  KW("volatile", TK_VOLATILE);
  KW("while", TK_WHILE);
  KW("_Bool", TK_BOOL);
  KW("_Complex", TK_COMPLEX);
#undef KW
  return TK_IDENT;
}

static int is_ident_start(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static int is_ident_cont(char c) {
  return is_ident_start(c) || (c >= '0' && c <= '9');
}

static int hex_digit(char c) {
  if (c >= '0' && c <= '9')
    return c - '0';
  if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  return -1;
}

static int decode_escape(Lexer *L, char *out) {
  char c = advance(L);
  switch (c) {
  case '\'':
  case '"':
  case '?':
  case '\\':
    *out = c;
    return 1;
  case 'a':
    *out = '\a';
    return 1;
  case 'b':
    *out = '\b';
    return 1;
  case 'f':
    *out = '\f';
    return 1;
  case 'n':
    *out = '\n';
    return 1;
  case 'r':
    *out = '\r';
    return 1;
  case 't':
    *out = '\t';
    return 1;
  case 'v':
    *out = '\v';
    return 1;
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7': {
    int v = c - '0';
    for (int i = 0; i < 2; i++) {
      char d = peek(L);
      if (d < '0' || d > '7')
        break;
      advance(L);
      v = v * 8 + (d - '0');
    }
    *out = (char)v;
    return 1;
  }
  case 'x': {
    int v = 0;
    int digits = 0;
    for (;;) {
      int d = hex_digit(peek(L));
      if (d < 0)
        break;
      advance(L);
      v = (v << 4) | d;
      digits++;
    }
    if (!digits) {
      diag_error(L->diag, loc_here(L), "\\x used with no following hex digits");
      *out = 0;
      return 1;
    }
    *out = (char)v;
    return 1;
  }
  default:
    diag_error(L->diag, loc_here(L), "unknown escape sequence '\\%c'", c);
    *out = c;
    return 1;
  }
}

static Token lex_string(Lexer *L) {
  SrcLoc start = loc_here(L);
  const char *p0 = L->src + L->pos;
  advance(L); /* " */
  char *buf = NULL;
  while (!at_end(L) && peek(L) != '"' && peek(L) != '\n') {
    char ch;
    if (peek(L) == '\\') {
      advance(L);
      decode_escape(L, &ch);
    } else {
      ch = advance(L);
    }
    buf_push(buf, ch);
  }
  if (peek(L) != '"') {
    diag_error(L->diag, start, "unterminated string literal");
  } else {
    advance(L);
  }
  /* Adjacent string concatenation is done in the parser/lexer by peeking. */
  while (1) {
    skip_whitespace_and_comments(L);
    if (peek(L) != '"')
      break;
    advance(L);
    while (!at_end(L) && peek(L) != '"' && peek(L) != '\n') {
      char ch;
      if (peek(L) == '\\') {
        advance(L);
        decode_escape(L, &ch);
      } else {
        ch = advance(L);
      }
      buf_push(buf, ch);
    }
    if (peek(L) == '"')
      advance(L);
  }
  buf_push(buf, '\0');
  Token t = make_tok(L, TK_STRING, p0, (int)(L->src + L->pos - p0));
  t.loc = start;
  t.text = arena_strndup(L->arena, buf, buf_len(buf) - 1);
  t.ival = (long long)(buf_len(buf) - 1); /* length excluding NUL */
  buf_free(buf);
  return t;
}

static Token lex_char(Lexer *L) {
  SrcLoc start = loc_here(L);
  const char *p0 = L->src + L->pos;
  advance(L); /* ' */
  char ch = 0;
  if (peek(L) == '\\') {
    advance(L);
    decode_escape(L, &ch);
  } else if (!at_end(L) && peek(L) != '\'') {
    ch = advance(L);
  }
  if (peek(L) != '\'') {
    diag_error(L->diag, start, "unterminated character literal");
  } else {
    advance(L);
  }
  Token t = make_tok(L, TK_CHAR, p0, (int)(L->src + L->pos - p0));
  t.loc = start;
  t.ival = (unsigned char)ch;
  return t;
}

static Token lex_number(Lexer *L) {
  SrcLoc start = loc_here(L);
  const char *p0 = L->src + L->pos;
  int is_float = 0;
  int base = 10;

  if (peek(L) == '0' && (peek2(L) == 'x' || peek2(L) == 'X')) {
    advance(L);
    advance(L);
    base = 16;
    while (hex_digit(peek(L)) >= 0)
      advance(L);
  } else if (peek(L) == '0' && (peek2(L) >= '0' && peek2(L) <= '7')) {
    base = 8;
    advance(L);
    while (peek(L) >= '0' && peek(L) <= '7')
      advance(L);
  } else {
    while (peek(L) >= '0' && peek(L) <= '9')
      advance(L);
    if (peek(L) == '.' && (peek2(L) >= '0' && peek2(L) <= '9')) {
      is_float = 1;
      advance(L);
      while (peek(L) >= '0' && peek(L) <= '9')
        advance(L);
    }
    if (peek(L) == 'e' || peek(L) == 'E') {
      is_float = 1;
      advance(L);
      if (peek(L) == '+' || peek(L) == '-')
        advance(L);
      while (peek(L) >= '0' && peek(L) <= '9')
        advance(L);
    }
  }

  /* suffixes */
  int is_unsigned = 0, is_long = 0, is_long_long = 0, is_float_suf = 0;
  for (;;) {
    char c = peek(L);
    if (c == 'u' || c == 'U') {
      is_unsigned = 1;
      advance(L);
    } else if (c == 'l' || c == 'L') {
      advance(L);
      if (peek(L) == 'l' || peek(L) == 'L') {
        advance(L);
        is_long_long = 1;
      } else {
        is_long = 1;
      }
    } else if (c == 'f' || c == 'F') {
      is_float = 1;
      is_float_suf = 1;
      advance(L);
    } else {
      break;
    }
  }

  int nlen = (int)(L->src + L->pos - p0);
  char *tmp = arena_strndup(L->arena, p0, (size_t)nlen);
  Token t = make_tok(L, is_float ? TK_FLOAT : TK_INT, p0, nlen);
  t.loc = start;
  t.is_unsigned = is_unsigned;
  t.is_long = is_long;
  t.is_long_long = is_long_long;
  t.is_float = is_float_suf;
  if (is_float) {
    t.fval = strtod(tmp, NULL);
  } else {
    t.ival = (long long)strtoull(tmp, NULL, base);
  }
  return t;
}

Token lexer_next(Lexer *L) {
  skip_whitespace_and_comments(L);
  if (at_end(L)) {
    Token t = make_tok(L, TK_EOF, L->src + L->pos, 0);
    t.loc = loc_here(L);
    return t;
  }

  SrcLoc start = loc_here(L);
  const char *p0 = L->src + L->pos;
  char c = peek(L);

  if (is_ident_start(c)) {
    advance(L);
    while (is_ident_cont(peek(L)))
      advance(L);
    int nlen = (int)(L->src + L->pos - p0);
    TokenKind kw = keyword_kind(p0, nlen);
    Token t = make_tok(L, kw, p0, nlen);
    t.loc = start;
    t.text = arena_strndup(L->arena, p0, (size_t)nlen);
    return t;
  }

  if (c >= '0' && c <= '9')
    return lex_number(L);
  /* .digits float */
  if (c == '.' && peek2(L) >= '0' && peek2(L) <= '9')
    return lex_number(L);

  if (c == '"')
    return lex_string(L);
  if (c == '\'')
    return lex_char(L);

  /* multi-char operators */
  advance(L);
  char c2 = peek(L);
  char c3 = peek2(L);

#define RET1(k)                                                                \
  do {                                                                         \
    Token t = make_tok(L, k, p0, 1);                                           \
    t.loc = start;                                                             \
    return t;                                                                  \
  } while (0)
#define RET2(k)                                                                \
  do {                                                                         \
    advance(L);                                                                \
    Token t = make_tok(L, k, p0, 2);                                           \
    t.loc = start;                                                             \
    return t;                                                                  \
  } while (0)
#define RET3(k)                                                                \
  do {                                                                         \
    advance(L);                                                                \
    advance(L);                                                                \
    Token t = make_tok(L, k, p0, 3);                                           \
    t.loc = start;                                                             \
    return t;                                                                  \
  } while (0)

  switch (c) {
  case '(':
    RET1(TK_LPAREN);
  case ')':
    RET1(TK_RPAREN);
  case '{':
    RET1(TK_LBRACE);
  case '}':
    RET1(TK_RBRACE);
  case '[':
    RET1(TK_LBRACKET);
  case ']':
    RET1(TK_RBRACKET);
  case ';':
    RET1(TK_SEMI);
  case ',':
    RET1(TK_COMMA);
  case '?':
    RET1(TK_QUESTION);
  case '~':
    RET1(TK_TILDE);
  case ':':
    RET1(TK_COLON);
  case '.':
    if (c2 == '.' && c3 == '.')
      RET3(TK_ELLIPSIS);
    RET1(TK_DOT);
  case '+':
    if (c2 == '+')
      RET2(TK_INC);
    if (c2 == '=')
      RET2(TK_ADD_ASSIGN);
    RET1(TK_PLUS);
  case '-':
    if (c2 == '-')
      RET2(TK_DEC);
    if (c2 == '=')
      RET2(TK_SUB_ASSIGN);
    if (c2 == '>')
      RET2(TK_ARROW);
    RET1(TK_MINUS);
  case '*':
    if (c2 == '=')
      RET2(TK_MUL_ASSIGN);
    RET1(TK_STAR);
  case '/':
    if (c2 == '=')
      RET2(TK_DIV_ASSIGN);
    RET1(TK_SLASH);
  case '%':
    if (c2 == '=')
      RET2(TK_MOD_ASSIGN);
    RET1(TK_PERCENT);
  case '&':
    if (c2 == '&')
      RET2(TK_ANDAND);
    if (c2 == '=')
      RET2(TK_AND_ASSIGN);
    RET1(TK_AMP);
  case '|':
    if (c2 == '|')
      RET2(TK_OROR);
    if (c2 == '=')
      RET2(TK_OR_ASSIGN);
    RET1(TK_PIPE);
  case '^':
    if (c2 == '=')
      RET2(TK_XOR_ASSIGN);
    RET1(TK_CARET);
  case '!':
    if (c2 == '=')
      RET2(TK_NE);
    RET1(TK_BANG);
  case '=':
    if (c2 == '=')
      RET2(TK_EQ);
    RET1(TK_ASSIGN);
  case '<':
    if (c2 == '<' && c3 == '=')
      RET3(TK_LSHIFT_ASSIGN);
    if (c2 == '<')
      RET2(TK_LSHIFT);
    if (c2 == '=')
      RET2(TK_LE);
    RET1(TK_LT);
  case '>':
    if (c2 == '>' && c3 == '=')
      RET3(TK_RSHIFT_ASSIGN);
    if (c2 == '>')
      RET2(TK_RSHIFT);
    if (c2 == '=')
      RET2(TK_GE);
    RET1(TK_GT);
  default:
    diag_error(L->diag, start, "unexpected character '%c' (0x%02x)", c,
               (unsigned char)c);
    RET1(TK_EOF);
  }
#undef RET1
#undef RET2
#undef RET3
}
