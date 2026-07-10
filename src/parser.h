#ifndef C99M_PARSER_H
#define C99M_PARSER_H

#include "ast.h"
#include "lexer.h"

typedef struct {
  Lexer *lexer;
  Arena *arena;
  Diag *diag;
  TypeContext *tc;
  Token tok;
  Token peek;
  int has_peek;
  /* enumerator table for parse-time constant expressions (stretchy) */
  const char **enum_names;
  long long *enum_vals;
  /* typedef name -> type table so member/declarator types resolve at parse
   * time (layout needs real sizes) (stretchy) */
  const char **typedef_names;
  Type **typedef_types;
} Parser;

void parser_init(Parser *P, Lexer *lexer, TypeContext *tc);

/* set when any TU mentions __int128 (driver appends the u128 runtime TU) */
extern int c99m_saw_int128;
Program *parse_program(Parser *P);

#endif /* C99M_PARSER_H */
