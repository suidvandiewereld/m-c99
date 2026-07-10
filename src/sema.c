#include "sema.h"

#define SCOPE_BUCKETS 64

static Scope *scope_push(Sema *S) {
  Scope *sc = (Scope *)arena_calloc(S->arena, sizeof(Scope));
  sc->parent = S->scope;
  sc->nbuckets = SCOPE_BUCKETS;
  sc->buckets =
      (Symbol **)arena_calloc(S->arena, SCOPE_BUCKETS * sizeof(Symbol *));
  S->scope = sc;
  return sc;
}

static void scope_pop(Sema *S) {
  if (S->scope)
    S->scope = S->scope->parent;
}

static unsigned hash_name(const char *s) {
  unsigned h = 2166136261u;
  for (; *s; s++)
    h = (h ^ (unsigned char)*s) * 16777619u;
  return h;
}

static Symbol *scope_lookup_local(Scope *sc, const char *name) {
  if (!sc)
    return NULL;
  unsigned i = hash_name(name) % sc->nbuckets;
  for (Symbol *s = sc->buckets[i]; s; s = s->next)
    if (strcmp(s->name, name) == 0)
      return s;
  return NULL;
}

Symbol *sema_lookup(Sema *S, const char *name) {
  for (Scope *sc = S->scope; sc; sc = sc->parent) {
    Symbol *s = scope_lookup_local(sc, name);
    if (s)
      return s;
  }
  return NULL;
}

static Symbol *scope_insert(Sema *S, SymKind kind, const char *name, Type *type,
                            Node *decl) {
  Symbol *exist = scope_lookup_local(S->scope, name);
  if (exist) {
    /* allow function redecl / extern redecl */
    if (kind == SYM_FUNC && exist->kind == SYM_FUNC)
      return exist;
    if (kind == SYM_VAR && exist->kind == SYM_VAR &&
        (exist->is_extern || (decl && decl->storage == SC_EXTERN)))
      return exist;
    diag_error(S->diag, decl ? decl->loc : (SrcLoc){0},
               "redefinition of '%s'", name);
    return exist;
  }
  Symbol *sym = (Symbol *)arena_calloc(S->arena, sizeof(Symbol));
  sym->kind = kind;
  sym->name = name;
  sym->type = type;
  sym->decl = decl;
  unsigned i = hash_name(name) % S->scope->nbuckets;
  sym->next = S->scope->buckets[i];
  S->scope->buckets[i] = sym;
  return sym;
}

void sema_init(Sema *S, Arena *arena, Diag *diag, TypeContext *tc) {
  memset(S, 0, sizeof(*S));
  S->arena = arena;
  S->diag = diag;
  S->tc = tc;
  scope_push(S); /* global */
}

/* ---- type resolution for typedef placeholders ---- */

static Type *resolve_type(Sema *S, Type *t) {
  if (!t)
    return t;
  if (t->enum_value == 1 && t->tag) {
    /* typedef name placeholder */
    Symbol *sym = sema_lookup(S, t->tag);
    if (sym && sym->kind == SYM_TYPEDEF)
      return resolve_type(S, sym->type);
    diag_error(S->diag, (SrcLoc){0}, "unknown type name '%s'", t->tag);
    return S->tc->ty_int;
  }
  if (t->kind == TY_PTR) {
    Type *b = resolve_type(S, t->base);
    if (b != t->base)
      return type_ptr(S->tc, b);
    return t;
  }
  if (t->kind == TY_ARRAY) {
    Type *b = resolve_type(S, t->base);
    if (b != t->base)
      return type_array(S->tc, b, t->array_len);
    return t;
  }
  if (t->kind == TY_FUNC) {
    t->base = resolve_type(S, t->base);
    for (size_t i = 0; i < t->param_count; i++)
      t->params[i] = resolve_type(S, t->params[i]);
    return t;
  }
  return t;
}

/* ---- expression checking ---- */

static Type *check_expr(Sema *S, Node *e);
static void check_stmt(Sema *S, Node *st);
static void check_decl(Sema *S, Node *d, int is_global);

static int is_lvalue(Node *e) {
  switch (e->kind) {
  case EX_IDENT:
  case EX_INDEX:
  case EX_MEMBER:
    return 1;
  case EX_UNARY:
    return e->op == OP_DEREF;
  default:
    return 0;
  }
}

static Type *apply_conv(Sema *S, Node *e, Type *to) {
  (void)S;
  if (!e || !to)
    return to;
  Type *from = e->type;
  if (!from || type_equal(from, to))
    return to;
  /* record cast by wrapping — for lowering, sema inserts EX_CAST when needed */
  return to;
}

static Type *check_binary(Sema *S, Node *e) {
  Type *lt = check_expr(S, e->lhs);
  Type *rt = check_expr(S, e->rhs);
  lt = type_decay(S->tc, lt);
  rt = type_decay(S->tc, rt);
  e->lhs->type = lt;
  e->rhs->type = rt;

  switch (e->op) {
  case OP_ADD:
    if (lt->kind == TY_PTR && type_is_integer(rt)) {
      e->type = lt;
      return lt;
    }
    if (rt->kind == TY_PTR && type_is_integer(lt)) {
      e->type = rt;
      return rt;
    }
    e->type = type_usual_arith(S->tc, lt, rt);
    return e->type;
  case OP_SUB:
    if (lt->kind == TY_PTR && rt->kind == TY_PTR) {
      e->type = S->tc->ty_llong; /* ptrdiff */
      return e->type;
    }
    if (lt->kind == TY_PTR && type_is_integer(rt)) {
      e->type = lt;
      return lt;
    }
    e->type = type_usual_arith(S->tc, lt, rt);
    return e->type;
  case OP_MUL:
  case OP_DIV:
  case OP_MOD:
    e->type = type_usual_arith(S->tc, lt, rt);
    return e->type;
  case OP_EQ:
  case OP_NE:
  case OP_LT:
  case OP_LE:
  case OP_GT:
  case OP_GE:
    e->type = S->tc->ty_int;
    return e->type;
  case OP_AND:
  case OP_OR:
    e->type = S->tc->ty_int;
    return e->type;
  case OP_BITAND:
  case OP_BITOR:
  case OP_BITXOR:
  case OP_LSHIFT:
  case OP_RSHIFT:
    e->type = type_usual_arith(S->tc, lt, rt);
    return e->type;
  default:
    e->type = S->tc->ty_int;
    return e->type;
  }
}

static Type *check_expr(Sema *S, Node *e) {
  if (!e)
    return S->tc->ty_void;
  switch (e->kind) {
  case EX_INT: {
    if (e->enum_val == 2)
      e->type = e->str ? S->tc->ty_ullong : S->tc->ty_llong;
    else if (e->enum_val == 1)
      e->type = e->str ? S->tc->ty_ulong : S->tc->ty_long;
    else if (e->str)
      e->type = S->tc->ty_uint;
    else
      e->type = S->tc->ty_int;
    return e->type;
  }
  case EX_FLOAT:
    e->type = e->enum_val ? S->tc->ty_float : S->tc->ty_double;
    return e->type;
  case EX_CHAR:
    e->type = S->tc->ty_int; /* C promotes char literals to int */
    return e->type;
  case EX_STRING:
    e->type = type_ptr(S->tc, S->tc->ty_char);
    return e->type;
  case EX_IDENT: {
    if (e->name && strcmp(e->name, "__c99m_I") == 0) {
      e->type = type_complex(S->tc, S->tc->ty_double);
      e->kind = EX_BUILTIN;
      e->name = "__c99m_I";
      return e->type;
    }
    Symbol *sym = sema_lookup(S, e->name);
    if (!sym) {
      diag_error(S->diag, e->loc, "undeclared identifier '%s'", e->name);
      e->type = S->tc->ty_int;
      return e->type;
    }
    e->sym = sym;
    if (sym->kind == SYM_ENUM_CONST) {
      e->kind = EX_INT;
      e->ival = sym->enum_val;
      e->type = S->tc->ty_int;
      return e->type;
    }
    if (sym->kind == SYM_FUNC) {
      e->type = type_ptr(S->tc, sym->type);
      return e->type;
    }
    e->type = sym->type;
    return e->type;
  }
  case EX_BINARY: {
    Type *lt0 = e->lhs ? e->lhs->type : NULL;
    Type *rt0 = e->rhs ? e->rhs->type : NULL;
    (void)lt0;
    (void)rt0;
    Type *r = check_binary(S, e);
    /* complex arithmetic: promote if either side complex */
    Type *lt = e->lhs->type, *rt = e->rhs->type;
    if ((lt && lt->kind == TY_COMPLEX) || (rt && rt->kind == TY_COMPLEX)) {
      Type *elem = S->tc->ty_double;
      if (lt && lt->kind == TY_COMPLEX && lt->base)
        elem = lt->base;
      if (rt && rt->kind == TY_COMPLEX && rt->base)
        elem = rt->base;
      if (e->op == OP_EQ || e->op == OP_NE)
        e->type = S->tc->ty_int;
      else
        e->type = type_complex(S->tc, elem);
      return e->type;
    }
    return r;
  }
  case EX_BUILTIN:
    if (e->name && strcmp(e->name, "__c99m_I") == 0) {
      e->type = type_complex(S->tc, S->tc->ty_double);
      return e->type;
    }
    e->type = e->type ? e->type : S->tc->ty_int;
    return e->type;
  case EX_UNARY: {
    Type *t = type_decay(S->tc, check_expr(S, e->lhs));
    switch (e->op) {
    case OP_NOT:
      e->type = S->tc->ty_int;
      return e->type;
    case OP_NEG:
    case OP_PLUS:
    case OP_BITNOT:
    case OP_PREINC:
    case OP_PREDEC:
      e->type = type_promote(S->tc, t);
      return e->type;
    case OP_ADDR:
      if (!is_lvalue(e->lhs) && e->lhs->kind != EX_IDENT) {
        /* allow addr of ident always */
      }
      /* Mark object as address-taken so lowering picks addressable storage
       * (needed for globals: libmtlc address_of is local/param-oriented). */
      if (e->lhs->kind == EX_IDENT && e->lhs->sym)
        e->lhs->sym->address_taken = 1;
      e->type = type_ptr(S->tc, e->lhs->type ? e->lhs->type : t);
      return e->type;
    case OP_DEREF:
      if (t->kind != TY_PTR && t->kind != TY_ARRAY) {
        diag_error(S->diag, e->loc, "indirection requires pointer operand");
        e->type = S->tc->ty_int;
      } else {
        e->type = t->base;
      }
      return e->type;
    default:
      e->type = t;
      return t;
    }
  }
  case EX_POSTFIX: {
    Type *t = check_expr(S, e->lhs);
    e->type = type_promote(S->tc, type_decay(S->tc, t));
    return e->type;
  }
  case EX_ASSIGN: {
    Type *lt = check_expr(S, e->lhs);
    Type *rt = type_decay(S->tc, check_expr(S, e->rhs));
    if (!is_lvalue(e->lhs))
      diag_error(S->diag, e->loc, "lvalue required as left operand of assignment");
    e->type = lt;
    (void)rt;
    (void)apply_conv;
    return e->type;
  }
  case EX_CALL: {
    /* builtins */
    if (e->lhs->kind == EX_IDENT && e->lhs->name) {
      const char *bn = e->lhs->name;
      if (strcmp(bn, "__builtin_va_start") == 0 ||
          strcmp(bn, "__builtin_va_end") == 0) {
        for (size_t i = 0; i < buf_len(e->stmts); i++)
          check_expr(S, e->stmts[i]);
        e->type = S->tc->ty_void;
        e->kind = EX_BUILTIN;
        e->name = bn;
        return e->type;
      }
      if (strcmp(bn, "__builtin_va_arg") == 0) {
        if (buf_len(e->stmts) >= 1)
          check_expr(S, e->stmts[0]);
        e->decl_type = resolve_type(S, e->decl_type);
        e->type = e->decl_type ? e->decl_type : S->tc->ty_int;
        e->kind = EX_BUILTIN;
        e->name = bn;
        return e->type;
      }
      if (strcmp(bn, "__real__") == 0 || strcmp(bn, "__imag__") == 0) {
        Type *t = check_expr(S, e->stmts[0]);
        e->type = (t && t->kind == TY_COMPLEX) ? t->base : S->tc->ty_double;
        e->kind = EX_BUILTIN;
        e->name = bn;
        return e->type;
      }
    }
    Type *ft = type_decay(S->tc, check_expr(S, e->lhs));
    Type *fnty = ft;
    if (ft->kind == TY_PTR && ft->base && ft->base->kind == TY_FUNC)
      fnty = ft->base;
    if (ft->kind == TY_FUNC)
      fnty = ft;
    if (fnty->kind != TY_FUNC) {
      diag_error(S->diag, e->loc, "called object is not a function");
      e->type = S->tc->ty_int;
      return e->type;
    }
    size_t nargs = buf_len(e->stmts);
    if (!fnty->is_variadic && nargs != fnty->param_count) {
      diag_error(S->diag, e->loc,
                 "wrong number of arguments (got %zu, expected %zu)", nargs,
                 fnty->param_count);
    }
    for (size_t i = 0; i < nargs; i++) {
      check_expr(S, e->stmts[i]);
      e->stmts[i]->type = type_decay(S->tc, e->stmts[i]->type);
    }
    e->type = fnty->base;
    return e->type;
  }
  case EX_INDEX: {
    Type *lt = type_decay(S->tc, check_expr(S, e->lhs));
    Type *rt = check_expr(S, e->rhs);
    (void)rt;
    if (lt->kind != TY_PTR) {
      diag_error(S->diag, e->loc, "subscripted value is not a pointer");
      e->type = S->tc->ty_int;
    } else {
      e->type = lt->base;
    }
    return e->type;
  }
  case EX_MEMBER: {
    Type *t = check_expr(S, e->lhs);
    if (e->is_arrow) {
      t = type_decay(S->tc, t);
      if (t->kind != TY_PTR) {
        diag_error(S->diag, e->loc, "-> on non-pointer");
        e->type = S->tc->ty_int;
        return e->type;
      }
      t = t->base;
    }
    /* Resolve tag if incomplete placeholder */
    if (t && t->tag && (t->kind == TY_STRUCT || t->kind == TY_UNION) &&
        t->is_incomplete) {
      Type *full = type_tag_lookup(S->tc, t->tag, t->kind == TY_UNION);
      if (full)
        t = full;
    }
    if (!t || (t->kind != TY_STRUCT && t->kind != TY_UNION)) {
      diag_error(S->diag, e->loc, "member reference on non-struct");
      e->type = S->tc->ty_int;
      return e->type;
    }
    StructMember *m = type_find_member(S->tc, t, e->name);
    if (!m) {
      diag_error(S->diag, e->loc, "no member named '%s'", e->name);
      e->type = S->tc->ty_int;
      return e->type;
    }
    e->ival = (long long)m->offset;
    e->type = m->type;
    return e->type;
  }
  case EX_CAST: {
    e->decl_type = resolve_type(S, e->decl_type);
    check_expr(S, e->lhs);
    e->type = e->decl_type;
    return e->type;
  }
  case EX_SIZEOF_EXPR: {
    /* sizeof does not decay arrays/functions */
    check_expr(S, e->lhs);
    e->type = S->tc->ty_ullong;
    {
      Type *st = e->lhs->type;
      if (st && st->kind == TY_ARRAY && st->array_len && st->base)
        e->ival = (long long)(st->array_len * st->base->size);
      else
        e->ival = (long long)(st ? st->size : 0);
    }
    return e->type;
  }
  case EX_SIZEOF_TYPE: {
    e->decl_type = resolve_type(S, e->decl_type);
    e->type = S->tc->ty_ullong;
    e->ival = (long long)(e->decl_type ? e->decl_type->size : 0);
    return e->type;
  }
  case EX_COND: {
    check_expr(S, e->cond);
    Type *t = type_decay(S->tc, check_expr(S, e->lhs));
    Type *u = type_decay(S->tc, check_expr(S, e->rhs));
    if (type_is_arithmetic(t) && type_is_arithmetic(u))
      e->type = type_usual_arith(S->tc, t, u);
    else if (t->kind == TY_PTR)
      e->type = t;
    else
      e->type = u;
    return e->type;
  }
  case EX_COMMA: {
    check_expr(S, e->lhs);
    e->type = type_decay(S->tc, check_expr(S, e->rhs));
    return e->type;
  }
  case EX_INIT_LIST:
    for (size_t i = 0; i < buf_len(e->stmts); i++)
      check_expr(S, e->stmts[i]);
    e->type = e->decl_type ? resolve_type(S, e->decl_type) : S->tc->ty_int;
    return e->type;
  case EX_COMPOUND_LITERAL:
    e->decl_type = resolve_type(S, e->decl_type);
    e->type = e->decl_type;
    if (e->init)
      check_expr(S, e->init);
    return e->type;
  default:
    e->type = S->tc->ty_int;
    return e->type;
  }
}

static void check_stmt(Sema *S, Node *st) {
  if (!st)
    return;
  switch (st->kind) {
  case ST_NULL:
    break;
  case ST_EXPR:
    check_expr(S, st->lhs);
    break;
  case ST_COMPOUND:
    scope_push(S);
    for (size_t i = 0; i < buf_len(st->stmts); i++)
      check_stmt(S, st->stmts[i]);
    scope_pop(S);
    break;
  case ST_IF:
    check_expr(S, st->cond);
    check_stmt(S, st->body);
    if (st->els)
      check_stmt(S, st->els);
    break;
  case ST_WHILE:
  case ST_DO:
    check_expr(S, st->cond);
    S->loop_depth++;
    check_stmt(S, st->body);
    S->loop_depth--;
    break;
  case ST_FOR:
    scope_push(S);
    if (st->init)
      check_stmt(S, st->init);
    if (st->cond)
      check_expr(S, st->cond);
    if (st->inc)
      check_expr(S, st->inc);
    S->loop_depth++;
    check_stmt(S, st->body);
    S->loop_depth--;
    scope_pop(S);
    break;
  case ST_BREAK:
    if (S->loop_depth == 0 && S->switch_depth == 0)
      diag_error(S->diag, st->loc, "break outside loop or switch");
    break;
  case ST_CONTINUE:
    if (S->loop_depth == 0)
      diag_error(S->diag, st->loc, "continue outside loop");
    break;
  case ST_RETURN:
    if (st->lhs) {
      check_expr(S, st->lhs);
    } else if (S->current_func_ret && S->current_func_ret->kind != TY_VOID) {
      diag_error(S->diag, st->loc, "return with no value in non-void function");
    }
    break;
  case ST_SWITCH:
    check_expr(S, st->cond);
    S->switch_depth++;
    check_stmt(S, st->body);
    S->switch_depth--;
    break;
  case ST_CASE:
    if (S->switch_depth == 0)
      diag_error(S->diag, st->loc, "case label not in switch");
    check_expr(S, st->lhs);
    check_stmt(S, st->body);
    break;
  case ST_DEFAULT:
    if (S->switch_depth == 0)
      diag_error(S->diag, st->loc, "default label not in switch");
    check_stmt(S, st->body);
    break;
  case ST_GOTO:
  case ST_LABEL:
    if (st->body)
      check_stmt(S, st->body);
    break;
  case ST_DECL:
    for (Node *d = st->lhs; d; d = d->next)
      check_decl(S, d, 0);
    break;
  default:
    break;
  }
}

static void check_decl(Sema *S, Node *d, int is_global) {
  if (!d)
    return;
  d->decl_type = resolve_type(S, d->decl_type);
  d->type = d->decl_type;

  if (d->kind == D_TYPEDEF) {
    if (!d->name) {
      diag_error(S->diag, d->loc, "typedef requires a name");
      return;
    }
    Symbol *sym = scope_insert(S, SYM_TYPEDEF, d->name, d->type, d);
    d->sym = sym;
    return;
  }

  if (d->kind == D_ENUM) {
    Symbol *sym = scope_insert(S, SYM_ENUM_CONST, d->name, d->type, d);
    sym->enum_val = d->ival;
    d->sym = sym;
    return;
  }

  if (d->kind == D_STRUCT)
    return;

  if (d->kind == D_FUNC) {
    if (!d->name) {
      diag_error(S->diag, d->loc, "function requires a name");
      return;
    }
    Symbol *sym = scope_insert(S, SYM_FUNC, d->name, d->type, d);
    sym->is_static = (d->storage == SC_STATIC);
    sym->is_extern =
        !sym->is_static && ((d->storage == SC_EXTERN) || !d->is_definition);
    sym->is_global = 1;
    sym->link_name = d->name;
    if (d->is_definition)
      sym->is_defined = 1;
    d->sym = sym;
    if (is_global)
      buf_push(S->globals, sym);

    if (d->is_definition) {
      scope_push(S);
      S->current_func_ret = d->type->base;
      for (size_t i = 0; i < buf_len(d->params); i++) {
        Node *p = d->params[i];
        p->decl_type = resolve_type(S, p->decl_type);
        p->type = type_decay(S->tc, p->decl_type);
        if (p->name) {
          Symbol *ps = scope_insert(S, SYM_VAR, p->name, p->type, p);
          p->sym = ps;
        }
      }
      check_stmt(S, d->body);
      S->current_func_ret = NULL;
      scope_pop(S);
    }
    return;
  }

  if (d->kind == D_VAR) {
    if (!d->name) {
      diag_error(S->diag, d->loc, "variable requires a name");
      return;
    }
    Type *ty = d->type;
    Symbol *sym = d->sym;
    if (!sym) {
      sym = scope_insert(S, SYM_VAR, d->name, ty, d);
      sym->is_static = (d->storage == SC_STATIC);
      sym->is_extern = !sym->is_static && (d->storage == SC_EXTERN);
      sym->is_global = is_global || d->storage == SC_STATIC;
      sym->link_name = d->name;
      d->sym = sym;
      if (sym->is_global)
        buf_push(S->globals, sym);
    } else {
      sym->type = ty;
      d->type = ty;
    }
    if (d->init)
      check_expr(S, d->init);
    return;
  }
}

void sema_check(Sema *S, Program *prog) {
  /* First pass: register all global functions and vars for forward refs. */
  for (size_t i = 0; i < buf_len(prog->decls); i++) {
    Node *d = prog->decls[i];
    if (d->kind == D_FUNC && d->name) {
      d->decl_type = resolve_type(S, d->decl_type);
      d->type = d->decl_type;
      Symbol *sym = scope_insert(S, SYM_FUNC, d->name, d->type, d);
      sym->is_global = 1;
      sym->is_static = (d->storage == SC_STATIC);
      sym->is_extern = !sym->is_static && !d->is_definition;
      sym->link_name = d->name;
      if (d->is_definition)
        sym->is_defined = 1;
      d->sym = sym;
      buf_push(S->globals, sym);
    } else if (d->kind == D_TYPEDEF && d->name) {
      d->decl_type = resolve_type(S, d->decl_type);
      d->type = d->decl_type;
      scope_insert(S, SYM_TYPEDEF, d->name, d->type, d);
    } else if (d->kind == D_ENUM) {
      Symbol *sym = scope_insert(S, SYM_ENUM_CONST, d->name, S->tc->ty_int, d);
      sym->enum_val = d->ival;
    } else if (d->kind == D_VAR && d->name) {
      d->decl_type = resolve_type(S, d->decl_type);
      d->type = d->decl_type;
      Symbol *sym = scope_insert(S, SYM_VAR, d->name, d->type, d);
      sym->is_global = 1;
      sym->is_static = (d->storage == SC_STATIC);
      sym->is_extern = !sym->is_static && (d->storage == SC_EXTERN);
      sym->link_name = d->name;
      d->sym = sym;
      buf_push(S->globals, sym);
    }
  }
  /* Second pass: full check */
  for (size_t i = 0; i < buf_len(prog->decls); i++) {
    Node *d = prog->decls[i];
    if (d->kind == D_FUNC && d->is_definition) {
      d->decl_type = resolve_type(S, d->decl_type);
      d->type = d->decl_type;
      scope_push(S);
      S->current_func_ret = d->type->base;
      for (size_t j = 0; j < buf_len(d->params); j++) {
        Node *p = d->params[j];
        p->decl_type = resolve_type(S, p->decl_type);
        p->type = type_decay(S->tc, p->decl_type);
        if (p->name) {
          Symbol *ps = scope_insert(S, SYM_VAR, p->name, p->type, p);
          p->sym = ps;
        }
      }
      check_stmt(S, d->body);
      S->current_func_ret = NULL;
      scope_pop(S);
    } else if (d->kind == D_VAR) {
      check_decl(S, d, 1);
    }
  }
}
