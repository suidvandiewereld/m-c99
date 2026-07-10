#ifndef C99M_TOKEN_H
#define C99M_TOKEN_H

#include "common.h"

typedef enum {
  TK_EOF = 0,

  /* punctuation / operators */
  TK_LPAREN,    /* ( */
  TK_RPAREN,    /* ) */
  TK_LBRACE,    /* { */
  TK_RBRACE,    /* } */
  TK_LBRACKET,  /* [ */
  TK_RBRACKET,  /* ] */
  TK_SEMI,      /* ; */
  TK_COMMA,     /* , */
  TK_COLON,     /* : */
  TK_QUESTION,  /* ? */
  TK_DOT,       /* . */
  TK_ARROW,     /* -> */
  TK_ELLIPSIS,  /* ... */

  TK_PLUS,      /* + */
  TK_MINUS,     /* - */
  TK_STAR,      /* * */
  TK_SLASH,     /* / */
  TK_PERCENT,   /* % */
  TK_AMP,       /* & */
  TK_PIPE,      /* | */
  TK_CARET,     /* ^ */
  TK_TILDE,     /* ~ */
  TK_BANG,      /* ! */
  TK_LT,        /* < */
  TK_GT,        /* > */
  TK_LE,        /* <= */
  TK_GE,        /* >= */
  TK_EQ,        /* == */
  TK_NE,        /* != */
  TK_LSHIFT,    /* << */
  TK_RSHIFT,    /* >> */
  TK_ANDAND,    /* && */
  TK_OROR,      /* || */
  TK_INC,       /* ++ */
  TK_DEC,       /* -- */

  TK_ASSIGN,    /* = */
  TK_ADD_ASSIGN,
  TK_SUB_ASSIGN,
  TK_MUL_ASSIGN,
  TK_DIV_ASSIGN,
  TK_MOD_ASSIGN,
  TK_AND_ASSIGN,
  TK_OR_ASSIGN,
  TK_XOR_ASSIGN,
  TK_LSHIFT_ASSIGN,
  TK_RSHIFT_ASSIGN,

  /* literals & identifiers */
  TK_IDENT,
  TK_INT,
  TK_FLOAT,
  TK_CHAR,
  TK_STRING,

  /* keywords */
  TK_AUTO,
  TK_BREAK,
  TK_CASE,
  TK_CHAR_KW,
  TK_CONST,
  TK_CONTINUE,
  TK_DEFAULT,
  TK_DO,
  TK_DOUBLE,
  TK_ELSE,
  TK_ENUM,
  TK_EXTERN,
  TK_FLOAT_KW,
  TK_FOR,
  TK_GOTO,
  TK_IF,
  TK_INLINE,
  TK_INT_KW,
  TK_LONG,
  TK_REGISTER,
  TK_RESTRICT,
  TK_RETURN,
  TK_SHORT,
  TK_SIGNED,
  TK_SIZEOF,
  TK_STATIC,
  TK_STRUCT,
  TK_SWITCH,
  TK_TYPEDEF,
  TK_UNION,
  TK_UNSIGNED,
  TK_VOID,
  TK_VOLATILE,
  TK_WHILE,
  TK_BOOL,      /* _Bool */
  TK_COMPLEX,   /* _Complex (accepted, limited) */
  TK_INT128,    /* __int128 (lowered to a two-u64 struct) */
} TokenKind;

typedef struct Token {
  TokenKind kind;
  SrcLoc loc;
  const char *start; /* pointer into source (not owned) */
  int len;
  /* For TK_INT / TK_CHAR: ival. For TK_FLOAT: fval. For TK_STRING/IDENT: text. */
  long long ival;
  double fval;
  char *text; /* arena-allocated decoded text for string/ident */
  int is_unsigned;
  int is_long;      /* long / long long int suffix */
  int is_long_long;
  int is_float;     /* float (f) suffix vs double */
} Token;

const char *token_kind_name(TokenKind k);

#endif /* C99M_TOKEN_H */
