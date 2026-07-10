#include "ctype.h"

static Type *mk_prim(TypeContext *tc, TypeKind k, size_t size, size_t align) {
  Type *t = (Type *)arena_calloc(tc->arena, sizeof(Type));
  t->kind = k;
  t->size = size;
  t->align = align;
  return t;
}

Type *type_tag_lookup(TypeContext *tc, const char *tag, int is_union) {
  if (!tag)
    return NULL;
  for (size_t i = 0; i < tc->ntags; i++) {
    Type *t = tc->tags[i];
    if (!t || !t->tag || strcmp(t->tag, tag) != 0)
      continue;
    if (is_union && t->kind != TY_UNION)
      continue;
    if (!is_union && t->kind != TY_STRUCT && t->kind != TY_ENUM)
      continue;
    return t;
  }
  return NULL;
}

void type_tag_register(TypeContext *tc, Type *t) {
  if (!t || !t->tag)
    return;
  for (size_t i = 0; i < tc->ntags; i++) {
    if (tc->tags[i] && tc->tags[i]->tag &&
        strcmp(tc->tags[i]->tag, t->tag) == 0 &&
        tc->tags[i]->kind == t->kind) {
      tc->tags[i] = t;
      return;
    }
  }
  size_t n = tc->ntags + 1;
  Type **arr = (Type **)arena_alloc(tc->arena, n * sizeof(Type *));
  if (tc->ntags)
    memcpy(arr, tc->tags, tc->ntags * sizeof(Type *));
  arr[tc->ntags] = t;
  tc->tags = arr;
  tc->ntags = n;
}

void type_context_init(TypeContext *tc, Arena *arena) {
  memset(tc, 0, sizeof(*tc));
  tc->arena = arena;
  /* LP64-like sizes on the libmtlc x86-64/Windows target: long is 32 on Win64
   * (LLP64). Match Windows/MSVC for host PE path. */
  tc->ty_void = mk_prim(tc, TY_VOID, 0, 1);
  tc->ty_bool = mk_prim(tc, TY_BOOL, 1, 1);
  tc->ty_char = mk_prim(tc, TY_CHAR, 1, 1);
  tc->ty_schar = mk_prim(tc, TY_SCHAR, 1, 1);
  tc->ty_uchar = mk_prim(tc, TY_UCHAR, 1, 1);
  tc->ty_short = mk_prim(tc, TY_SHORT, 2, 2);
  tc->ty_ushort = mk_prim(tc, TY_USHORT, 2, 2);
  tc->ty_int = mk_prim(tc, TY_INT, 4, 4);
  tc->ty_uint = mk_prim(tc, TY_UINT, 4, 4);
  tc->ty_long = mk_prim(tc, TY_LONG, 4, 4); /* Windows LLP64 */
  tc->ty_ulong = mk_prim(tc, TY_ULONG, 4, 4);
  tc->ty_llong = mk_prim(tc, TY_LLONG, 8, 8);
  tc->ty_ullong = mk_prim(tc, TY_ULLONG, 8, 8);
  tc->ty_float = mk_prim(tc, TY_FLOAT, 4, 4);
  tc->ty_double = mk_prim(tc, TY_DOUBLE, 8, 8);
  tc->ty_ldouble = mk_prim(tc, TY_LDOUBLE, 8, 8);
}

Type *type_ptr(TypeContext *tc, Type *base) {
  Type *t = (Type *)arena_calloc(tc->arena, sizeof(Type));
  t->kind = TY_PTR;
  t->base = base;
  t->size = 8;
  t->align = 8;
  return t;
}

Type *type_array(TypeContext *tc, Type *base, size_t len) {
  Type *t = (Type *)arena_calloc(tc->arena, sizeof(Type));
  t->kind = TY_ARRAY;
  t->base = base;
  t->array_len = len;
  t->align = base ? base->align : 1;
  if (len && base && type_is_complete(base))
    t->size = base->size * len;
  else {
    t->size = 0;
    t->is_incomplete = 1;
  }
  return t;
}

Type *type_func(TypeContext *tc, Type *ret, Type **params, size_t nparams,
                int variadic) {
  Type *t = (Type *)arena_calloc(tc->arena, sizeof(Type));
  t->kind = TY_FUNC;
  t->base = ret;
  t->param_count = nparams;
  t->is_variadic = variadic;
  t->size = 1;
  t->align = 1;
  if (nparams) {
    t->params = (Type **)arena_alloc(tc->arena, nparams * sizeof(Type *));
    memcpy(t->params, params, nparams * sizeof(Type *));
  }
  return t;
}

Type *type_struct_create(TypeContext *tc, const char *tag, int is_union) {
  Type *t = (Type *)arena_calloc(tc->arena, sizeof(Type));
  t->kind = is_union ? TY_UNION : TY_STRUCT;
  t->tag = tag;
  t->is_union = is_union;
  t->is_incomplete = 1;
  t->align = 1;
  return t;
}

void type_struct_add_member(TypeContext *tc, Type *st, const char *name,
                            Type *mty) {
  (void)tc;
  StructMember m;
  memset(&m, 0, sizeof(m));
  m.name = name;
  m.type = mty;
  m.offset = 0;
  buf_push(st->members, m);
}

void type_struct_add_bitfield(TypeContext *tc, Type *st, const char *name,
                              Type *mty, int width) {
  StructMember m;
  memset(&m, 0, sizeof(m));
  m.name = name;
  m.type = mty ? mty : tc->ty_uint;
  m.is_bitfield = 1;
  m.bit_width = width;
  buf_push(st->members, m);
}

Type *type_complex(TypeContext *tc, Type *real_ty) {
  Type *t = (Type *)arena_calloc(tc->arena, sizeof(Type));
  t->kind = TY_COMPLEX;
  t->base = real_ty ? real_ty : tc->ty_double;
  t->size = t->base->size * 2;
  t->align = t->base->align;
  return t;
}

static size_t align_up(size_t x, size_t a) {
  return (x + a - 1) & ~(a - 1);
}

void type_struct_finish(Type *st) {
  size_t off = 0;
  size_t max_align = 1;
  size_t max_size = 0;
  /* bitfield packing into unsigned int units (4 bytes) on LLP64 */
  size_t bf_unit_off = 0;
  int bf_bit = 0;
  int in_bf = 0;

  for (size_t i = 0; i < buf_len(st->members); i++) {
    StructMember *sm = &st->members[i];
    Type *mt = sm->type;
    if (!mt)
      continue;

    if (sm->is_bitfield) {
      int w = sm->bit_width;
      if (w == 0) {
        /* zero-width: force new unit */
        if (in_bf && bf_bit > 0) {
          off = bf_unit_off + 4;
          in_bf = 0;
          bf_bit = 0;
        }
        continue;
      }
      if (st->is_union) {
        sm->offset = 0;
        sm->bit_offset = 0;
        if (4 > max_size)
          max_size = 4;
        if (4 > max_align)
          max_align = 4;
        continue;
      }
      if (!in_bf || bf_bit + w > 32) {
        if (in_bf)
          off = bf_unit_off + 4;
        off = align_up(off, 4);
        bf_unit_off = off;
        bf_bit = 0;
        in_bf = 1;
        if (4 > max_align)
          max_align = 4;
      }
      sm->offset = bf_unit_off;
      sm->bit_offset = bf_bit;
      bf_bit += w;
      continue;
    }

    if (in_bf) {
      off = bf_unit_off + 4;
      in_bf = 0;
      bf_bit = 0;
    }
    if (!type_is_complete(mt))
      continue;
    if (mt->align > max_align)
      max_align = mt->align;
    if (st->is_union) {
      sm->offset = 0;
      if (mt->size > max_size)
        max_size = mt->size;
    } else {
      off = align_up(off, mt->align);
      sm->offset = off;
      off += mt->size;
    }
  }
  if (in_bf)
    off = bf_unit_off + 4;

  st->align = max_align ? max_align : 1;
  if (st->is_union)
    st->size = align_up(max_size, st->align);
  else
    st->size = align_up(off, st->align);
  st->is_incomplete = 0;
}

int type_is_integer(const Type *t) {
  if (!t)
    return 0;
  switch (t->kind) {
  case TY_BOOL:
  case TY_CHAR:
  case TY_SCHAR:
  case TY_UCHAR:
  case TY_SHORT:
  case TY_USHORT:
  case TY_INT:
  case TY_UINT:
  case TY_LONG:
  case TY_ULONG:
  case TY_LLONG:
  case TY_ULLONG:
  case TY_ENUM:
    return 1;
  default:
    return 0;
  }
}

int type_is_unsigned(const Type *t) {
  if (!t)
    return 0;
  switch (t->kind) {
  case TY_BOOL:
  case TY_UCHAR:
  case TY_USHORT:
  case TY_UINT:
  case TY_ULONG:
  case TY_ULLONG:
    return 1;
  default:
    return 0;
  }
}

int type_is_float(const Type *t) {
  return t && (t->kind == TY_FLOAT || t->kind == TY_DOUBLE ||
               t->kind == TY_LDOUBLE);
}

int type_is_arithmetic(const Type *t) {
  return type_is_integer(t) || type_is_float(t);
}

int type_is_scalar(const Type *t) {
  return type_is_arithmetic(t) || (t && t->kind == TY_PTR);
}

int type_is_pointer_like(const Type *t) {
  return t && (t->kind == TY_PTR || t->kind == TY_ARRAY);
}

int type_is_complete(const Type *t) {
  if (!t)
    return 0;
  if (t->kind == TY_VOID)
    return 0;
  if (t->is_incomplete)
    return 0;
  if (t->kind == TY_ARRAY)
    return t->array_len > 0 && type_is_complete(t->base);
  return 1;
}

int type_equal(const Type *a, const Type *b) {
  if (a == b)
    return 1;
  if (!a || !b || a->kind != b->kind)
    return 0;
  switch (a->kind) {
  case TY_PTR:
    return type_equal(a->base, b->base);
  case TY_ARRAY:
    return a->array_len == b->array_len && type_equal(a->base, b->base);
  case TY_FUNC:
    if (!type_equal(a->base, b->base) || a->param_count != b->param_count ||
        a->is_variadic != b->is_variadic)
      return 0;
    for (size_t i = 0; i < a->param_count; i++)
      if (!type_equal(a->params[i], b->params[i]))
        return 0;
    return 1;
  case TY_STRUCT:
  case TY_UNION:
    return a == b; /* nominal */
  default:
    return 1;
  }
}

int type_compatible(const Type *a, const Type *b) {
  if (type_equal(a, b))
    return 1;
  if (!a || !b)
    return 0;
  if (a->kind == TY_PTR && b->kind == TY_PTR) {
    if (a->base->kind == TY_VOID || b->base->kind == TY_VOID)
      return 1;
    return type_compatible(a->base, b->base);
  }
  if (type_is_integer(a) && type_is_integer(b))
    return 1;
  if (type_is_float(a) && type_is_float(b))
    return 1;
  return 0;
}

Type *type_decay(TypeContext *tc, Type *t) {
  if (!t)
    return t;
  if (t->kind == TY_ARRAY)
    return type_ptr(tc, t->base);
  if (t->kind == TY_FUNC)
    return type_ptr(tc, t);
  return t;
}

int type_int_rank(const Type *t) {
  if (!t)
    return 0;
  switch (t->kind) {
  case TY_BOOL:
    return 1;
  case TY_CHAR:
  case TY_SCHAR:
  case TY_UCHAR:
    return 2;
  case TY_SHORT:
  case TY_USHORT:
    return 3;
  case TY_INT:
  case TY_UINT:
  case TY_ENUM:
    return 4;
  case TY_LONG:
  case TY_ULONG:
    return 5;
  case TY_LLONG:
  case TY_ULLONG:
    return 6;
  default:
    return 0;
  }
}

Type *type_promote(TypeContext *tc, Type *t) {
  if (!t || !type_is_integer(t))
    return t;
  if (type_int_rank(t) < type_int_rank(tc->ty_int))
    return tc->ty_int;
  return t;
}

Type *type_usual_arith(TypeContext *tc, Type *a, Type *b) {
  a = type_promote(tc, a);
  b = type_promote(tc, b);
  if (type_is_float(a) || type_is_float(b)) {
    if (a->kind == TY_LDOUBLE || b->kind == TY_LDOUBLE)
      return tc->ty_ldouble;
    if (a->kind == TY_DOUBLE || b->kind == TY_DOUBLE)
      return tc->ty_double;
    return tc->ty_float;
  }
  /* both integer after promotion */
  if (type_equal(a, b))
    return a;
  int au = type_is_unsigned(a), bu = type_is_unsigned(b);
  int ar = type_int_rank(a), br = type_int_rank(b);
  if (au == bu)
    return ar >= br ? a : b;
  /* one signed, one unsigned */
  Type *u = au ? a : b;
  Type *s = au ? b : a;
  if (type_int_rank(u) >= type_int_rank(s))
    return u;
  /* signed can represent all unsigned values of lower rank */
  if (s->size > u->size)
    return s;
  /* otherwise convert both to unsigned version of signed */
  switch (s->kind) {
  case TY_INT:
    return tc->ty_uint;
  case TY_LONG:
    return tc->ty_ulong;
  case TY_LLONG:
    return tc->ty_ullong;
  default:
    return u;
  }
}

StructMember *type_find_member(TypeContext *tc, Type *st, const char *name) {
  if (!st)
    return NULL;
  for (size_t i = 0; i < buf_len(st->members); i++)
    if (st->members[i].name && strcmp(st->members[i].name, name) == 0)
      return &st->members[i];
  /* recurse into anonymous struct/union members (C11 6.7.2.1p13),
   * synthesizing a member whose offset is relative to the outer type */
  for (size_t i = 0; i < buf_len(st->members); i++) {
    StructMember *am = &st->members[i];
    if (am->name || !am->type ||
        (am->type->kind != TY_STRUCT && am->type->kind != TY_UNION))
      continue;
    StructMember *inner = type_find_member(tc, am->type, name);
    if (inner) {
      StructMember *r = (StructMember *)arena_calloc(tc->arena, sizeof(*r));
      *r = *inner;
      r->offset += am->offset;
      return r;
    }
  }
  return NULL;
}

const char *type_to_string(TypeContext *tc, const Type *t) {
  if (!t)
    return "<null>";
  switch (t->kind) {
  case TY_VOID:
    return "void";
  case TY_BOOL:
    return "_Bool";
  case TY_CHAR:
    return "char";
  case TY_SCHAR:
    return "signed char";
  case TY_UCHAR:
    return "unsigned char";
  case TY_SHORT:
    return "short";
  case TY_USHORT:
    return "unsigned short";
  case TY_INT:
    return "int";
  case TY_UINT:
    return "unsigned int";
  case TY_LONG:
    return "long";
  case TY_ULONG:
    return "unsigned long";
  case TY_LLONG:
    return "long long";
  case TY_ULLONG:
    return "unsigned long long";
  case TY_FLOAT:
    return "float";
  case TY_DOUBLE:
    return "double";
  case TY_LDOUBLE:
    return "long double";
  case TY_PTR:
    return arena_sprintf(tc->arena, "%s*", type_to_string(tc, t->base));
  case TY_ARRAY:
    return arena_sprintf(tc->arena, "%s[%zu]", type_to_string(tc, t->base),
                         t->array_len);
  case TY_FUNC:
    return arena_sprintf(tc->arena, "fn returning %s",
                         type_to_string(tc, t->base));
  case TY_STRUCT:
    return t->tag ? arena_sprintf(tc->arena, "struct %s", t->tag) : "struct";
  case TY_UNION:
    return t->tag ? arena_sprintf(tc->arena, "union %s", t->tag) : "union";
  case TY_ENUM:
    return t->tag ? arena_sprintf(tc->arena, "enum %s", t->tag) : "enum";
  case TY_COMPLEX:
    return arena_sprintf(tc->arena, "_Complex %s",
                         type_to_string(tc, t->base));
  }
  return "<type>";
}
