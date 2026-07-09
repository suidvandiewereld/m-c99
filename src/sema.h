#ifndef C99M_SEMA_H
#define C99M_SEMA_H

#include "ast.h"

typedef enum {
  SYM_VAR,
  SYM_FUNC,
  SYM_TYPEDEF,
  SYM_ENUM_CONST,
  SYM_LABEL,
} SymKind;

typedef struct Symbol {
  SymKind kind;
  const char *name;      /* source / lookup name (may be mangled for static) */
  const char *link_name; /* IR/link name; defaults to name */
  Type *type;
  Node *decl;
  long long enum_val;
  int is_extern;
  int is_global;
  int is_static; /* internal linkage */
  int is_defined;
  struct Symbol *next; /* hash chain */
} Symbol;

typedef struct Scope {
  struct Scope *parent;
  Symbol **buckets;
  size_t nbuckets;
} Scope;

typedef struct {
  Arena *arena;
  Diag *diag;
  TypeContext *tc;
  Scope *scope;
  Type *current_func_ret;
  int loop_depth;
  int switch_depth;
  /* typedef table: name -> Type* */
  Symbol **typedefs;
  size_t ntypedefs;
  /* global symbols for lowering */
  Symbol **globals; /* stretchy */
} Sema;

void sema_init(Sema *S, Arena *arena, Diag *diag, TypeContext *tc);
void sema_check(Sema *S, Program *prog);
Symbol *sema_lookup(Sema *S, const char *name);

#endif /* C99M_SEMA_H */
