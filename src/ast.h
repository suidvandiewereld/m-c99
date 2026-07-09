#ifndef C99M_AST_H
#define C99M_AST_H

#include "ctype.h"
#include "token.h"

typedef enum {
  /* expressions */
  EX_INT,
  EX_FLOAT,
  EX_CHAR,
  EX_STRING,
  EX_IDENT,
  EX_BINARY,
  EX_UNARY,
  EX_POSTFIX, /* x++ x-- */
  EX_ASSIGN,
  EX_CALL,
  EX_INDEX,
  EX_MEMBER, /* . or -> */
  EX_CAST,
  EX_SIZEOF_EXPR,
  EX_SIZEOF_TYPE,
  EX_COND, /* ?: */
  EX_COMMA,
  EX_COMPOUND_LITERAL,
  EX_INIT_LIST,
  EX_DESIGNATOR, /* .field or [index] wrapper around init value */
  EX_BUILTIN,    /* __builtin_va_*, __real__, __imag__, etc. */

  /* statements */
  ST_EXPR,
  ST_COMPOUND,
  ST_IF,
  ST_WHILE,
  ST_DO,
  ST_FOR,
  ST_BREAK,
  ST_CONTINUE,
  ST_RETURN,
  ST_SWITCH,
  ST_CASE,
  ST_DEFAULT,
  ST_GOTO,
  ST_LABEL,
  ST_DECL, /* declaration statement */
  ST_NULL,

  /* declarations / top-level */
  D_VAR,
  D_FUNC,
  D_TYPEDEF,
  D_STRUCT, /* tag decl */
  D_ENUM,
} NodeKind;

typedef enum {
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_MOD,
  OP_EQ,
  OP_NE,
  OP_LT,
  OP_LE,
  OP_GT,
  OP_GE,
  OP_AND,  /* && */
  OP_OR,   /* || */
  OP_BITAND,
  OP_BITOR,
  OP_BITXOR,
  OP_LSHIFT,
  OP_RSHIFT,
  OP_NOT,  /* ! */
  OP_NEG,  /* - */
  OP_PLUS, /* unary + */
  OP_BITNOT,
  OP_ADDR, /* & */
  OP_DEREF,
  OP_PREINC,
  OP_PREDEC,
  OP_POSTINC,
  OP_POSTDEC,
  OP_ASSIGN,
  OP_ADD_A,
  OP_SUB_A,
  OP_MUL_A,
  OP_DIV_A,
  OP_MOD_A,
  OP_AND_A,
  OP_OR_A,
  OP_XOR_A,
  OP_SHL_A,
  OP_SHR_A,
} OpKind;

typedef struct Node Node;

typedef enum {
  SC_NONE,
  SC_TYPEDEF,
  SC_EXTERN,
  SC_STATIC,
  SC_AUTO,
  SC_REGISTER,
} StorageClass;

struct Node {
  NodeKind kind;
  SrcLoc loc;
  Type *type; /* filled by sema */

  /* common payload */
  OpKind op;
  Node *lhs;
  Node *rhs;
  Node *cond;
  Node *init; /* for / decl init */
  Node *inc;  /* for */
  Node *body;
  Node *els;

  const char *name; /* ident, label, member, function name */
  long long ival;
  double fval;
  char *str; /* string literal content */
  size_t str_len;

  Node **stmts; /* compound / call args / init list — stretchy */
  Node **params; /* function param decls */
  Node *next;    /* sibling decl in a multi-declarator statement */

  StorageClass storage;
  Type *decl_type;   /* declared type before decay */
  int is_definition; /* function has body */
  int is_variadic;
  int is_arrow; /* member access -> */
  int enum_val;
  int bit_width; /* bit-field width in member decl; -1 if none */
  int is_designated; /* init has .name = */
  const char *designator; /* field name for designated init */

  /* semantic / lowering hooks */
  struct Symbol *sym;
  int local_slot; /* filled by lowerer */
  Node **cases;   /* switch case list */
  int fptr_id;    /* function pointer id for lower */
  int tu_id;      /* translation unit index for static linkage */
};

typedef struct {
  Node **decls; /* top-level */
} Program;

Node *node_new(Arena *a, NodeKind kind, SrcLoc loc);
const char *op_to_string(OpKind op);
OpKind token_to_binop(TokenKind k);
OpKind token_to_assignop(TokenKind k);

#endif /* C99M_AST_H */
