#include "lower.h"

typedef struct LoopCtx {
  const char *break_l;
  const char *continue_l;
  struct LoopCtx *parent;
} LoopCtx;

typedef struct {
  const char *name;
  Type *fn_type; /* TY_FUNC */
  int id;
} FPtrEntry;

typedef struct {
  Sema *sema;
  Diag *diag;
  Arena *arena;
  TypeContext *tc;
  MtlcBuilder *builder;
  MtlcFn *fn;
  LoopCtx *loop;
  int label_id;
  int tmp_id;
  int str_id;
  int blob_id;
  Symbol **local_syms;
  MtlcValue *local_vals;
  size_t nlocals;
  /* array/struct locals: symbol -> base pointer value (storage is pointer) */
  Symbol **ptr_syms;
  MtlcValue *ptr_vals;
  size_t nptrs;
  char **str_names;
  char **str_contents;
  size_t *str_lens;
  Type *ret_type;
  int emitted_return;
  /* function pointer table */
  FPtrEntry *fptrs;
  size_t nfptrs;
  /* current function's hidden va pointer local, if any */
  MtlcValue va_param;
  int has_va;
  /* sret pointer for aggregate returns */
  MtlcValue sret_param;
  int has_sret;
  /* complex: I constant global */
  int complex_i_ready;
  /* file-scope decls needing runtime ctor (strings, aggregates, &g, etc.) */
  Node **global_runtime_inits;
  /* all addressable object globals (name + size) for eager allocate in ctor */
  char **agg_global_names;
  size_t *agg_global_bytes;
  int need_global_init_call;
  /* extern functions already declared to the builder (stretchy) */
  const char **declared_externs;
  /* string ptr-globals already lazily-init-checked in the CURRENT function
   * (stretchy; reset per function) */
  const char **fn_str_ensured;
} Lower;

/* All calls to an extern variadic pad the tail to this arity so every call
 * site agrees with one module-level signature (Win64 varargs read arg slots
 * from memory, so extra zero i64 args are harmless). */
#define C99M_VARIADIC_PAD 32

static const MtlcType *mtlc_of(Type *t);
static MtlcValue gen_expr(Lower *L, Node *e);
static void gen_stmt(Lower *L, Node *st);
static void gen_bool(Lower *L, Node *e, const char *true_l, const char *false_l);
static MtlcValue gen_lvalue_addr(Lower *L, Node *e);
static MtlcValue gen_string(Lower *L, const char *data, size_t len);
static void gen_init_into(Lower *L, Type *ty, MtlcValue base_addr, Node *init,
                          size_t *seq_index);

static char *fresh_label(Lower *L, const char *prefix) {
  return arena_sprintf(L->arena, ".L%s%d", prefix, L->label_id++);
}

static char *fresh_tmp(Lower *L, const char *prefix) {
  return arena_sprintf(L->arena, "%s_%d", prefix, L->tmp_id++);
}

/* Immortal blob type of `bytes` size for stack-ish storage via pointer+malloc
 * from a function-scoped arena simulated by a single alloca-like malloc at
 * entry. Fixed arrays still get true contiguous storage (not per-element).
 * We use one malloc per array/VLA for contiguous memory; fixed sizes use a
 * module-level BSS region when possible. */
static const MtlcType *mtlc_blob(size_t bytes) {
  /* Represent as [bytes x i8] via pointer to i8 for addressing; allocation
   * size is tracked by the caller. */
  (void)bytes;
  return mtlc_type_pointer(mtlc_type_scalar(MTLC_TYPE_UINT8));
}

static const MtlcType *mtlc_of(Type *t) {
  if (!t)
    return mtlc_type_scalar(MTLC_TYPE_INT32);
  switch (t->kind) {
  case TY_VOID:
    return mtlc_type_scalar(MTLC_TYPE_VOID);
  case TY_BOOL:
    return mtlc_type_scalar(MTLC_TYPE_BOOL);
  case TY_CHAR:
  case TY_SCHAR:
    return mtlc_type_scalar(MTLC_TYPE_INT8);
  case TY_UCHAR:
    return mtlc_type_scalar(MTLC_TYPE_UINT8);
  case TY_SHORT:
    return mtlc_type_scalar(MTLC_TYPE_INT16);
  case TY_USHORT:
    return mtlc_type_scalar(MTLC_TYPE_UINT16);
  case TY_INT:
  case TY_ENUM:
    return mtlc_type_scalar(MTLC_TYPE_INT32);
  case TY_UINT:
    return mtlc_type_scalar(MTLC_TYPE_UINT32);
  case TY_LONG:
    return mtlc_type_scalar(MTLC_TYPE_INT32);
  case TY_ULONG:
    return mtlc_type_scalar(MTLC_TYPE_UINT32);
  case TY_LLONG:
    return mtlc_type_scalar(MTLC_TYPE_INT64);
  case TY_ULLONG:
    return mtlc_type_scalar(MTLC_TYPE_UINT64);
  case TY_FLOAT:
    return mtlc_type_scalar(MTLC_TYPE_FLOAT32);
  case TY_DOUBLE:
  case TY_LDOUBLE:
    return mtlc_type_scalar(MTLC_TYPE_FLOAT64);
  case TY_PTR:
    /* Function pointers are integer ids in this ABI. */
    if (t->base && t->base->kind == TY_FUNC)
      return mtlc_type_scalar(MTLC_TYPE_INT64);
    if (!t->base || t->base->kind == TY_VOID || t->base->kind == TY_STRUCT ||
        t->base->kind == TY_UNION || t->base->kind == TY_COMPLEX)
      return mtlc_type_pointer(mtlc_type_scalar(MTLC_TYPE_UINT8));
    return mtlc_type_pointer(mtlc_of(t->base));
  case TY_ARRAY:
    if (!t->base)
      return mtlc_type_pointer(mtlc_type_scalar(MTLC_TYPE_UINT8));
    return mtlc_type_pointer(mtlc_of(t->base));
  case TY_FUNC:
    return mtlc_type_scalar(MTLC_TYPE_INT64); /* function id */
  case TY_STRUCT:
  case TY_UNION:
  case TY_COMPLEX:
    return mtlc_type_pointer(mtlc_type_scalar(MTLC_TYPE_UINT8));
  }
  return mtlc_type_scalar(MTLC_TYPE_INT32);
}

static int is_agg(Type *t) {
  return t && (t->kind == TY_STRUCT || t->kind == TY_UNION ||
               t->kind == TY_ARRAY || t->kind == TY_COMPLEX);
}

static MtlcValue local_of(Lower *L, Symbol *sym) {
  for (size_t i = 0; i < L->nlocals; i++)
    if (L->local_syms[i] == sym)
      return L->local_vals[i];
  return MTLC_NO_VALUE;
}

static MtlcValue ptr_of(Lower *L, Symbol *sym) {
  for (size_t i = 0; i < L->nptrs; i++)
    if (L->ptr_syms[i] == sym)
      return L->ptr_vals[i];
  return MTLC_NO_VALUE;
}

static void bind_local(Lower *L, Symbol *sym, MtlcValue v) {
  size_t n = L->nlocals + 1;
  Symbol **syms = (Symbol **)arena_alloc(L->arena, n * sizeof(Symbol *));
  MtlcValue *vals = (MtlcValue *)arena_alloc(L->arena, n * sizeof(MtlcValue));
  if (L->nlocals) {
    memcpy(syms, L->local_syms, L->nlocals * sizeof(Symbol *));
    memcpy(vals, L->local_vals, L->nlocals * sizeof(MtlcValue));
  }
  syms[L->nlocals] = sym;
  vals[L->nlocals] = v;
  L->local_syms = syms;
  L->local_vals = vals;
  L->nlocals = n;
}

static void bind_ptr(Lower *L, Symbol *sym, MtlcValue v) {
  size_t n = L->nptrs + 1;
  Symbol **syms = (Symbol **)arena_alloc(L->arena, n * sizeof(Symbol *));
  MtlcValue *vals = (MtlcValue *)arena_alloc(L->arena, n * sizeof(MtlcValue));
  if (L->nptrs) {
    memcpy(syms, L->ptr_syms, L->nptrs * sizeof(Symbol *));
    memcpy(vals, L->ptr_vals, L->nptrs * sizeof(MtlcValue));
  }
  syms[L->nptrs] = sym;
  vals[L->nptrs] = v;
  L->ptr_syms = syms;
  L->ptr_vals = vals;
  L->nptrs = n;
}

static MtlcValue cast_to(Lower *L, MtlcValue v, Type *from, Type *to) {
  if (!from || !to || type_equal(from, to))
    return v;
  if (from->kind == TY_ARRAY && to->kind == TY_PTR)
    return v;
  if (from->kind == TY_FUNC && to->kind == TY_PTR)
    return v;
  if (is_agg(from) || is_agg(to))
    return v;
  return mtlc_cast(L->fn, v, mtlc_of(to));
}

static int fptr_id_for(Lower *L, const char *name) {
  for (size_t i = 0; i < L->nfptrs; i++)
    if (strcmp(L->fptrs[i].name, name) == 0)
      return L->fptrs[i].id;
  return -1;
}

static void register_fptr(Lower *L, const char *name, Type *ft) {
  if (fptr_id_for(L, name) >= 0)
    return;
  FPtrEntry e;
  e.name = name;
  e.fn_type = ft;
  e.id = (int)L->nfptrs + 1; /* 0 = null */
  size_t n = L->nfptrs + 1;
  FPtrEntry *arr = (FPtrEntry *)arena_alloc(L->arena, n * sizeof(FPtrEntry));
  if (L->nfptrs)
    memcpy(arr, L->fptrs, L->nfptrs * sizeof(FPtrEntry));
  arr[L->nfptrs] = e;
  L->fptrs = arr;
  L->nfptrs = n;
}

static const MtlcType *i8p_ty(void) {
  return mtlc_type_pointer(mtlc_type_scalar(MTLC_TYPE_UINT8));
}

/*
 * Immortal blob type of `bytes` for stack allocation via mtlc_local.
 * libmtlc honors custom size/alignment on DECLARE_LOCAL when the type is
 * registered by name (builder finish registers every type it sees).
 */
typedef struct BlobType {
  size_t bytes;
  MtlcType ty;
  char name[32];
  struct BlobType *next;
} BlobType;

static BlobType *blob_types; /* process-lifetime freelist */

static const MtlcType *blob_type(size_t bytes) {
  if (bytes == 0)
    bytes = 1;
  /* Align up to 8 for natural pointer/stack alignment. */
  size_t aligned = (bytes + 7u) & ~7u;
  for (BlobType *b = blob_types; b; b = b->next)
    if (b->bytes == aligned)
      return &b->ty;
  BlobType *b = (BlobType *)calloc(1, sizeof(BlobType));
  if (!b)
    fatal("out of memory");
  b->bytes = aligned;
  snprintf(b->name, sizeof(b->name), "blob%zu", aligned);
  b->ty.kind = MTLC_TYPE_ARRAY;
  b->ty.name = b->name;
  b->ty.size = aligned;
  b->ty.alignment = 8;
  b->ty.base_type = (MtlcType *)mtlc_type_scalar(MTLC_TYPE_UINT8);
  b->ty.array_size = aligned;
  b->next = blob_types;
  blob_types = b;
  return &b->ty;
}

/* Stack object: local of blob type + address. Contiguous, no malloc. */
static MtlcValue stack_bytes(Lower *L, size_t bytes, const char *name) {
  const MtlcType *bt = blob_type(bytes);
  const MtlcType *i8p = i8p_ty();
  char *nm = name ? (char *)name : fresh_tmp(L, "stk");
  MtlcValue loc = mtlc_local(L->fn, nm, bt);
  return mtlc_address_of(L->fn, loc, i8p);
}

/* Heap allocation (VLAs, persistent globals). */
static MtlcValue heap_bytes(Lower *L, size_t bytes, MtlcValue dyn_bytes) {
  const MtlcType *i8p = i8p_ty();
  const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
  MtlcValue n = (dyn_bytes != MTLC_NO_VALUE)
                    ? dyn_bytes
                    : mtlc_const_int(L->fn, u64, (long long)(bytes ? bytes : 1));
  MtlcValue args[1] = {n};
  return mtlc_call(L->fn, "malloc", args, 1, i8p);
}

/* Fixed-size objects: stack. Dynamic size: heap. */
static MtlcValue alloc_bytes(Lower *L, size_t bytes, int is_vla,
                             MtlcValue vla_bytes) {
  if (is_vla)
    return heap_bytes(L, 0, vla_bytes);
  return stack_bytes(L, bytes ? bytes : 1, NULL);
}

static int extern_is_declared(Lower *L, const char *name) {
  for (size_t i = 0; i < buf_len(L->declared_externs); i++)
    if (strcmp(L->declared_externs[i], name) == 0)
      return 1;
  return 0;
}

/* Declare an extern function once with a prototype-accurate signature.
 * Variadics get their fixed params plus an i64 tail padded to
 * C99M_VARIADIC_PAD parameters so every call site can agree. */
static void ensure_extern_fn(Lower *L, const char *name, Type *ft) {
  if (extern_is_declared(L, name))
    return;
  buf_push(L->declared_externs, name);
  size_t fixed = ft ? ft->param_count : 0;
  int sret = ft && ft->base && is_agg(ft->base) && ft->base->kind != TY_ARRAY;
  int variadic = ft && ft->is_variadic;
  size_t nparams = variadic ? C99M_VARIADIC_PAD : fixed;
  if (nparams < fixed)
    nparams = fixed;
  size_t total = nparams + (sret ? 1 : 0);
  const char **pnames =
      total ? (const char **)arena_alloc(L->arena, total * sizeof(char *))
            : NULL;
  const MtlcType **ptypes =
      total ? (const MtlcType **)arena_alloc(L->arena, total * sizeof(MtlcType *))
            : NULL;
  size_t off = 0;
  if (sret) {
    pnames[0] = "__sret";
    ptypes[0] = i8p_ty();
    off = 1;
  }
  for (size_t j = 0; j < nparams; j++) {
    pnames[off + j] = arena_sprintf(L->arena, "p%zu", j);
    if (j < fixed)
      ptypes[off + j] = mtlc_of(ft->params[j]);
    else
      ptypes[off + j] = mtlc_type_scalar(MTLC_TYPE_INT64);
  }
  const MtlcType *rt =
      sret ? i8p_ty()
           : (ft && ft->base && ft->base->kind != TY_VOID
                  ? mtlc_of(ft->base)
                  : mtlc_type_scalar(MTLC_TYPE_VOID));
  mtlc_builder_function(L->builder, name, rt, pnames, ptypes, total, 1);
}

/* Reinterpret a float value's bits as i64 via a stack slot (for variadic
 * tails: Win64 varargs read doubles from integer arg slots). */
static MtlcValue float_bits_as_i64(Lower *L, MtlcValue v, const MtlcType *fty) {
  const MtlcType *i64 = mtlc_type_scalar(MTLC_TYPE_INT64);
  const MtlcType *f64 = mtlc_type_scalar(MTLC_TYPE_FLOAT64);
  /* default argument promotion: float -> double */
  v = mtlc_cast(L->fn, v, f64);
  (void)fty;
  MtlcValue slot = alloc_bytes(L, 8, 0, MTLC_NO_VALUE);
  mtlc_store(L->fn, slot, v, f64);
  MtlcValue ip = mtlc_cast(L->fn, slot, mtlc_type_pointer(i64));
  return mtlc_load(L->fn, ip, i64);
}

/* Zero `n` bytes at addr (C99 aggregate default zero). Unrolled for small n. */
static void mem_zero(Lower *L, MtlcValue addr, size_t n) {
  const MtlcType *i8 = mtlc_type_scalar(MTLC_TYPE_UINT8);
  const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
  const MtlcType *i8p = i8p_ty();
  MtlcValue z = mtlc_const_int(L->fn, i8, 0);
  if (n <= 64) {
    for (size_t i = 0; i < n; i++) {
      MtlcValue off = mtlc_const_int(L->fn, u64, (long long)i);
      MtlcValue a = mtlc_binary(L->fn, "+", addr, off, i8p);
      mtlc_store(L->fn, a, z, i8);
    }
    return;
  }
  /* IR loop for larger objects */
  MtlcValue i = mtlc_local(L->fn, fresh_tmp(L, "zi"), u64);
  mtlc_assign(L->fn, i, mtlc_const_int(L->fn, u64, 0));
  char *top = fresh_label(L, "ztop");
  char *body = fresh_label(L, "zbody");
  char *end = fresh_label(L, "zend");
  mtlc_label(L->fn, top);
  MtlcValue lim = mtlc_const_int(L->fn, u64, (long long)n);
  MtlcValue cmp = mtlc_binary(L->fn, "<", i, lim, mtlc_type_scalar(MTLC_TYPE_INT32));
  mtlc_branch_if_zero(L->fn, cmp, end);
  mtlc_label(L->fn, body);
  {
    MtlcValue a = mtlc_binary(L->fn, "+", addr, i, i8p);
    mtlc_store(L->fn, a, z, i8);
    MtlcValue one = mtlc_const_int(L->fn, u64, 1);
    mtlc_assign(L->fn, i, mtlc_binary(L->fn, "+", i, one, u64));
    mtlc_jump(L->fn, top);
  }
  mtlc_label(L->fn, end);
}

/* Copy `n` bytes from src to dst. */
static void mem_copy(Lower *L, MtlcValue dst, MtlcValue src, size_t n) {
  const MtlcType *i8 = mtlc_type_scalar(MTLC_TYPE_UINT8);
  const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
  const MtlcType *i8p = i8p_ty();
  if (n <= 64) {
    for (size_t i = 0; i < n; i++) {
      MtlcValue off = mtlc_const_int(L->fn, u64, (long long)i);
      MtlcValue sa = mtlc_binary(L->fn, "+", src, off, i8p);
      MtlcValue da = mtlc_binary(L->fn, "+", dst, off, i8p);
      MtlcValue b = mtlc_load(L->fn, sa, i8);
      mtlc_store(L->fn, da, b, i8);
    }
    return;
  }
  MtlcValue i = mtlc_local(L->fn, fresh_tmp(L, "ci"), u64);
  mtlc_assign(L->fn, i, mtlc_const_int(L->fn, u64, 0));
  char *top = fresh_label(L, "ctop");
  char *body = fresh_label(L, "cbody");
  char *end = fresh_label(L, "cend");
  mtlc_label(L->fn, top);
  MtlcValue lim = mtlc_const_int(L->fn, u64, (long long)n);
  MtlcValue cmp = mtlc_binary(L->fn, "<", i, lim, mtlc_type_scalar(MTLC_TYPE_INT32));
  mtlc_branch_if_zero(L->fn, cmp, end);
  mtlc_label(L->fn, body);
  {
    MtlcValue sa = mtlc_binary(L->fn, "+", src, i, i8p);
    MtlcValue da = mtlc_binary(L->fn, "+", dst, i, i8p);
    mtlc_store(L->fn, da, mtlc_load(L->fn, sa, i8), i8);
    mtlc_assign(L->fn, i,
                mtlc_binary(L->fn, "+", i, mtlc_const_int(L->fn, u64, 1), u64));
    mtlc_jump(L->fn, top);
  }
  mtlc_label(L->fn, end);
}

static const char *sym_link(Symbol *sym) {
  if (!sym)
    return NULL;
  return sym->link_name ? sym->link_name : sym->name;
}

/* Ensure file-scope object-pointer global is allocated and zeroed.
 * Used for aggregates and for scalar globals that have their address taken. */
static MtlcValue ensure_agg_global(Lower *L, const char *name, size_t bytes) {
  const MtlcType *i8p = i8p_ty();
  MtlcValue g = mtlc_global_ref(L->fn, name);
  char *done = fresh_label(L, "gdone");
  char *need = fresh_label(L, "ginit");
  mtlc_branch_if_zero(L->fn, g, need);
  mtlc_jump(L->fn, done);
  mtlc_label(L->fn, need);
  {
    /* Globals need heap (or static data); stack would dangle after return. */
    MtlcValue mem = heap_bytes(L, bytes ? bytes : 1, MTLC_NO_VALUE);
    mem_zero(L, mem, bytes ? bytes : 1);
    mtlc_assign(L->fn, g, mem);
  }
  mtlc_label(L->fn, done);
  return mtlc_global_ref(L->fn, name);
}

/* Apply one file-scope initializer inside __c99m_init_globals. */
static void apply_global_init(Lower *L, Node *d) {
  if (!d || !d->name)
    return;
  Type *ty = d->type;
  int addr_obj = is_agg(ty) || (ty && ty->kind == TY_ARRAY) ||
                 (d->sym && d->sym->address_taken);

  if (addr_obj) {
    size_t bytes = ty && ty->size ? ty->size : 8;
    if (ty && ty->kind == TY_ARRAY && ty->array_len && ty->base)
      bytes = ty->array_len * (ty->base->size ? ty->base->size : 1);
    MtlcValue base = ensure_agg_global(L, d->name, bytes);
    if (!d->init)
      return;
    if (d->init->kind == EX_INIT_LIST || d->init->kind == EX_COMPOUND_LITERAL ||
        d->init->kind == EX_STRING) {
      gen_init_into(L, ty, base, d->init, NULL);
    } else if (ty && (ty->kind == TY_STRUCT || ty->kind == TY_UNION ||
                      ty->kind == TY_COMPLEX || ty->kind == TY_ARRAY)) {
      gen_init_into(L, ty, base, d->init, NULL);
    } else {
      /* Address-taken scalar: store rvalue through pointer. */
      MtlcValue v = gen_expr(L, d->init);
      v = cast_to(L, v, d->init->type, ty);
      mtlc_store(L->fn, base, v, mtlc_of(ty));
    }
    return;
  }

  if (ty && ty->kind == TY_PTR && d->init) {
    MtlcValue g = mtlc_global_ref(L->fn, d->name);
    if (d->init->kind == EX_STRING) {
      MtlcValue s = gen_string(L, d->init->str, d->init->str_len);
      mtlc_assign(L->fn, g, s);
      return;
    }
    if (d->init->kind == EX_UNARY && d->init->op == OP_ADDR) {
      MtlcValue addr = gen_lvalue_addr(L, d->init->lhs);
      mtlc_assign(L->fn, g, addr);
      return;
    }
    /* General pointer init (including decays). */
    MtlcValue v = gen_expr(L, d->init);
    mtlc_assign(L->fn, g, v);
  }
}

static int is_addr_global(Symbol *sym) {
  return sym && sym->is_global &&
         (sym->address_taken ||
          (sym->type && (is_agg(sym->type) || sym->type->kind == TY_ARRAY)));
}

/*
 * String literals: content packed into u64 data globals (payload in .data),
 * then on first use copied once into a permanent heap buffer pointed to by a
 * pointer global. Public builder cannot yield a reliable char* into .data
 * (mtlc_address_of is defined for locals/params only; probe loads 0 from &global).
 * Result: static duration, one malloc per unique string, no per-access fill.
 */
static MtlcValue gen_string(Lower *L, const char *data, size_t len) {
  const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
  const MtlcType *i8 = mtlc_type_scalar(MTLC_TYPE_INT8);
  const MtlcType *i8p = mtlc_type_pointer(i8);

  char *ptrname = NULL;
  for (size_t i = 0; i < buf_len(L->str_names); i++) {
    if (L->str_lens[i] == len && memcmp(L->str_contents[i], data, len) == 0) {
      ptrname = L->str_names[i];
      break;
    }
  }
  if (!ptrname) {
    int id = L->str_id++;
    char *base = arena_sprintf(L->arena, ".str%d", id);
    size_t total = len + 1;
    size_t words = (total + 7) / 8;
    for (size_t w = 0; w < words; w++) {
      unsigned long long pack = 0;
      for (size_t b = 0; b < 8; b++) {
        size_t idx = w * 8 + b;
        unsigned char ch =
            (idx < total) ? (idx < len ? (unsigned char)data[idx] : 0) : 0;
        pack |= ((unsigned long long)ch) << (8 * b);
      }
      char *wn = arena_sprintf(L->arena, "%s_%zu", base, w);
      mtlc_builder_global(L->builder, wn, u64, (long long)pack, 0);
    }
    ptrname = arena_sprintf(L->arena, "%s_p", base);
    mtlc_builder_global(L->builder, ptrname, i8p, 0, 0);
    buf_push(L->str_names, ptrname);
    buf_push(L->str_contents, arena_strndup(L->arena, data, len));
    buf_push(L->str_lens, len);
  }

  /* Lazy materialization must be checked in EVERY function that uses the
   * literal: functions run in arbitrary order, so relying on the module-wide
   * first lowering site leaves the pointer NULL in earlier-running code. */
  int ensured_here = 0;
  for (size_t i = 0; i < buf_len(L->fn_str_ensured); i++)
    if (strcmp(L->fn_str_ensured[i], ptrname) == 0) {
      ensured_here = 1;
      break;
    }
  if (!ensured_here) {
    size_t total = len + 1;
    MtlcValue g = mtlc_global_ref(L->fn, ptrname);
    char *done = fresh_label(L, "sd");
    char *need = fresh_label(L, "si");
    mtlc_branch_if_zero(L->fn, g, need);
    mtlc_jump(L->fn, done);
    mtlc_label(L->fn, need);
    {
      MtlcValue mem = heap_bytes(L, total, MTLC_NO_VALUE);
      for (size_t i = 0; i < total; i++) {
        unsigned char ch = (i < len) ? (unsigned char)data[i] : 0;
        MtlcValue off = mtlc_const_int(L->fn, u64, (long long)i);
        MtlcValue addr = mtlc_binary(L->fn, "+", mem, off, i8p);
        mtlc_store(L->fn, addr, mtlc_const_int(L->fn, i8, ch), i8);
      }
      mtlc_assign(L->fn, g, mem);
    }
    mtlc_label(L->fn, done);
    buf_push(L->fn_str_ensured, ptrname);
  }
  return mtlc_global_ref(L->fn, ptrname);
}

static MtlcValue gen_lvalue_addr(Lower *L, Node *e) {
  switch (e->kind) {
  case EX_IDENT: {
    if (!e->sym)
      return MTLC_NO_VALUE;
    const char *ln = sym_link(e->sym);
    MtlcValue p = ptr_of(L, e->sym);
    if (p != MTLC_NO_VALUE)
      return p;
    if (e->sym->is_global) {
      if (is_addr_global(e->sym)) {
        size_t sz = e->sym->type && e->sym->type->size ? e->sym->type->size
                                                       : (e->type && e->type->size ? e->type->size : 8);
        return ensure_agg_global(L, ln ? ln : e->name, sz);
      }
      MtlcValue g = mtlc_global_ref(L->fn, ln ? ln : e->name);
      return mtlc_address_of(L->fn, g, mtlc_type_pointer(mtlc_of(e->type)));
    }
    MtlcValue loc = local_of(L, e->sym);
    if (e->type && e->type->kind == TY_ARRAY)
      return loc;
    return mtlc_address_of(L->fn, loc, mtlc_type_pointer(mtlc_of(e->type)));
  }
  case EX_UNARY:
    if (e->op == OP_DEREF)
      return gen_expr(L, e->lhs);
    break;
  case EX_INDEX: {
    MtlcValue base = gen_expr(L, e->lhs);
    MtlcValue idx = gen_expr(L, e->rhs);
    Type *et = e->type;
    size_t esz = et && et->size ? et->size : 1;
    if (e->lhs->type && e->lhs->type->kind == TY_ARRAY && e->lhs->type->base)
      esz = e->lhs->type->base->size;
    else if (e->lhs->type && e->lhs->type->kind == TY_PTR && e->lhs->type->base)
      esz = e->lhs->type->base->size ? e->lhs->type->base->size : 1;
    const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
    MtlcValue scale = mtlc_const_int(L->fn, u64, (long long)esz);
    idx = mtlc_cast(L->fn, idx, u64);
    MtlcValue off = mtlc_binary(L->fn, "*", idx, scale, u64);
    base = mtlc_cast(L->fn, base, u64);
    MtlcValue sum = mtlc_binary(L->fn, "+", base, off, u64);
    return mtlc_cast(L->fn, sum, mtlc_type_pointer(mtlc_of(et)));
  }
  case EX_MEMBER: {
    MtlcValue base =
        e->is_arrow ? gen_expr(L, e->lhs) : gen_lvalue_addr(L, e->lhs);
    const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
    MtlcValue off = mtlc_const_int(L->fn, u64, e->ival);
    base = mtlc_cast(L->fn, base, u64);
    MtlcValue sum = mtlc_binary(L->fn, "+", base, off, u64);
    return mtlc_cast(L->fn, sum, mtlc_type_pointer(mtlc_of(e->type)));
  }
  default:
    diag_error(L->diag, e->loc, "not an lvalue");
    break;
  }
  return MTLC_NO_VALUE;
}

static const char *binop_mtlc(OpKind op) {
  switch (op) {
  case OP_ADD: return "+";
  case OP_SUB: return "-";
  case OP_MUL: return "*";
  case OP_DIV: return "/";
  case OP_MOD: return "%";
  case OP_EQ: return "==";
  case OP_NE: return "!=";
  case OP_LT: return "<";
  case OP_LE: return "<=";
  case OP_GT: return ">";
  case OP_GE: return ">=";
  case OP_BITAND: return "&";
  case OP_BITOR: return "|";
  case OP_BITXOR: return "^";
  case OP_LSHIFT: return "<<";
  case OP_RSHIFT: return ">>";
  default: return "+";
  }
}

static int is_cmp(OpKind op) {
  return op == OP_EQ || op == OP_NE || op == OP_LT || op == OP_LE ||
         op == OP_GT || op == OP_GE;
}

static void gen_bool(Lower *L, Node *e, const char *true_l,
                     const char *false_l) {
  if (e->kind == EX_BINARY && e->op == OP_AND) {
    char *mid = fresh_label(L, "and");
    gen_bool(L, e->lhs, mid, false_l);
    mtlc_label(L->fn, mid);
    gen_bool(L, e->rhs, true_l, false_l);
    return;
  }
  if (e->kind == EX_BINARY && e->op == OP_OR) {
    char *mid = fresh_label(L, "or");
    gen_bool(L, e->lhs, true_l, mid);
    mtlc_label(L->fn, mid);
    gen_bool(L, e->rhs, true_l, false_l);
    return;
  }
  if (e->kind == EX_UNARY && e->op == OP_NOT) {
    gen_bool(L, e->lhs, false_l, true_l);
    return;
  }
  MtlcValue v = gen_expr(L, e);
  mtlc_branch_if_zero(L->fn, v, false_l);
  mtlc_jump(L->fn, true_l);
}

/* Bitfield load/store */
static MtlcValue bf_load(Lower *L, MtlcValue addr, StructMember *m) {
  const MtlcType *u32 = mtlc_type_scalar(MTLC_TYPE_UINT32);
  MtlcValue word = mtlc_load(L->fn, addr, u32);
  MtlcValue sh =
      mtlc_const_int(L->fn, u32, m->bit_offset);
  MtlcValue shifted = mtlc_binary(L->fn, ">>", word, sh, u32);
  unsigned mask = (m->bit_width >= 32) ? 0xffffffffu : (1u << m->bit_width) - 1u;
  MtlcValue mv = mtlc_const_int(L->fn, u32, (long long)mask);
  return mtlc_binary(L->fn, "&", shifted, mv, u32);
}

static void bf_store(Lower *L, MtlcValue addr, StructMember *m, MtlcValue val) {
  const MtlcType *u32 = mtlc_type_scalar(MTLC_TYPE_UINT32);
  MtlcValue word = mtlc_load(L->fn, addr, u32);
  unsigned mask = (m->bit_width >= 32) ? 0xffffffffu : (1u << m->bit_width) - 1u;
  MtlcValue mv = mtlc_const_int(L->fn, u32, (long long)mask);
  val = mtlc_cast(L->fn, val, u32);
  val = mtlc_binary(L->fn, "&", val, mv, u32);
  MtlcValue sh = mtlc_const_int(L->fn, u32, m->bit_offset);
  MtlcValue shifted = mtlc_binary(L->fn, "<<", val, sh, u32);
  MtlcValue clearm = mtlc_const_int(L->fn, u32, (long long)(~(mask << m->bit_offset)));
  word = mtlc_binary(L->fn, "&", word, clearm, u32);
  word = mtlc_binary(L->fn, "|", word, shifted, u32);
  mtlc_store(L->fn, addr, word, u32);
}

static StructMember *member_info(Lower *L, Type *st, const char *name) {
  return type_find_member(L->tc, st, name);
}

/* Complex as {double re; double im} at addr */
static MtlcValue cplx_alloc(Lower *L) {
  return alloc_bytes(L, 16, 0, MTLC_NO_VALUE);
}

static void cplx_store(Lower *L, MtlcValue addr, MtlcValue re, MtlcValue im) {
  const MtlcType *f64 = mtlc_type_scalar(MTLC_TYPE_FLOAT64);
  const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
  mtlc_store(L->fn, addr, re, f64);
  MtlcValue off = mtlc_const_int(L->fn, u64, 8);
  MtlcValue a = mtlc_cast(L->fn, addr, u64);
  MtlcValue ai = mtlc_binary(L->fn, "+", a, off, u64);
  ai = mtlc_cast(L->fn, ai, mtlc_type_pointer(f64));
  mtlc_store(L->fn, ai, im, f64);
}

static void cplx_load(Lower *L, MtlcValue addr, MtlcValue *re, MtlcValue *im) {
  const MtlcType *f64 = mtlc_type_scalar(MTLC_TYPE_FLOAT64);
  const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
  *re = mtlc_load(L->fn, addr, f64);
  MtlcValue off = mtlc_const_int(L->fn, u64, 8);
  MtlcValue a = mtlc_cast(L->fn, addr, u64);
  MtlcValue ai = mtlc_binary(L->fn, "+", a, off, u64);
  ai = mtlc_cast(L->fn, ai, mtlc_type_pointer(f64));
  *im = mtlc_load(L->fn, ai, f64);
}

static MtlcValue gen_indirect_call(Lower *L, MtlcValue fp, Node *call) {
  /* function pointers are real code addresses: call through the value */
  int is_void = call->type && call->type->kind == TY_VOID;
  size_t n = buf_len(call->stmts);
  MtlcValue *args =
      n ? (MtlcValue *)arena_alloc(L->arena, n * sizeof(MtlcValue)) : NULL;
  for (size_t i = 0; i < n; i++)
    args[i] = gen_expr(L, call->stmts[i]);
  const MtlcType *rt = is_void ? mtlc_type_scalar(MTLC_TYPE_VOID)
                               : mtlc_of(call->type);
  return mtlc_call_indirect(L->fn, fp, args, n, rt);
}

static void gen_init_into(Lower *L, Type *ty, MtlcValue base_addr, Node *init,
                          size_t *seq_index) {
  if (!init)
    return;
  if (init->kind == EX_COMPOUND_LITERAL) {
    /* Use the compound's initializer list against target storage. */
    if (init->init)
      gen_init_into(L, ty, base_addr, init->init, seq_index);
    return;
  }
  if (init->kind == EX_INIT_LIST && ty &&
      (ty->kind == TY_STRUCT || ty->kind == TY_UNION)) {
    size_t seq = 0;
    for (size_t i = 0; i < buf_len(init->stmts); i++) {
      Node *item = init->stmts[i];
      StructMember *m = NULL;
      if (item->is_designated && item->designator)
        m = type_find_member(L->tc, ty, item->designator);
      else {
        if (seq < buf_len(ty->members))
          m = &ty->members[seq];
        seq++;
      }
      if (!m)
        continue;
      const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
      MtlcValue off = mtlc_const_int(L->fn, u64, (long long)m->offset);
      MtlcValue a = mtlc_cast(L->fn, base_addr, u64);
      MtlcValue addr = mtlc_binary(L->fn, "+", a, off, u64);
      addr = mtlc_cast(L->fn, addr, mtlc_type_pointer(mtlc_of(m->type)));
      if (m->is_bitfield) {
        MtlcValue v = gen_expr(L, item);
        bf_store(L, addr, m, v);
      } else if (is_agg(m->type) && item->kind == EX_INIT_LIST) {
        gen_init_into(L, m->type, addr, item, NULL);
      } else {
        MtlcValue v = gen_expr(L, item);
        mtlc_store(L->fn, addr, v, mtlc_of(m->type));
      }
    }
    return;
  }
  if (init->kind == EX_INIT_LIST && ty && ty->kind == TY_ARRAY) {
    size_t seq = 0;
    for (size_t i = 0; i < buf_len(init->stmts); i++) {
      Node *item = init->stmts[i];
      size_t idx = seq++;
      if (item->is_designated && item->init && item->init->kind == EX_INT)
        idx = (size_t)item->init->ival;
      size_t esz = ty->base->size;
      const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
      MtlcValue off =
          mtlc_const_int(L->fn, u64, (long long)(idx * esz));
      MtlcValue a = mtlc_cast(L->fn, base_addr, u64);
      MtlcValue addr = mtlc_binary(L->fn, "+", a, off, u64);
      addr = mtlc_cast(L->fn, addr, mtlc_type_pointer(mtlc_of(ty->base)));
      if (ty->base && is_agg(ty->base) &&
          (item->kind == EX_INIT_LIST || item->kind == EX_STRING ||
           item->kind == EX_COMPOUND_LITERAL)) {
        gen_init_into(L, ty->base, addr, item, NULL);
      } else {
        MtlcValue v = gen_expr(L, item);
        mtlc_store(L->fn, addr, v, mtlc_of(ty->base));
      }
    }
    return;
  }
  if (init->kind == EX_STRING && ty && ty->kind == TY_ARRAY) {
    size_t n = init->str_len + 1;
    if (ty->array_len && n > ty->array_len)
      n = ty->array_len;
    for (size_t i = 0; i < n; i++) {
      char ch = (i < init->str_len) ? init->str[i] : 0;
      const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
      MtlcValue off = mtlc_const_int(L->fn, u64, (long long)i);
      MtlcValue a = mtlc_cast(L->fn, base_addr, u64);
      MtlcValue addr = mtlc_binary(L->fn, "+", a, off, u64);
      addr = mtlc_cast(L->fn, addr, mtlc_type_pointer(mtlc_type_scalar(MTLC_TYPE_INT8)));
      mtlc_store(L->fn, addr,
                 mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_INT8),
                                (unsigned char)ch),
                 mtlc_type_scalar(MTLC_TYPE_INT8));
    }
    return;
  }
  /* aggregate copy-initialization: `P y = x;` — copy the object bytes */
  if (ty && is_agg(ty) && ty->kind != TY_COMPLEX && init->kind != EX_INIT_LIST &&
      init->type && is_agg(init->type)) {
    MtlcValue src = gen_expr(L, init); /* aggregate rvalues are addresses */
    size_t sz = ty->size ? ty->size : 8;
    mem_copy(L, base_addr, src, sz);
    return;
  }
  /* scalar or complex value */
  MtlcValue v = gen_expr(L, init);
  if (ty && ty->kind == TY_COMPLEX) {
    MtlcValue re, im;
    if (init->type && init->type->kind == TY_COMPLEX) {
      cplx_load(L, v, &re, &im);
      cplx_store(L, base_addr, re, im);
    } else {
      cplx_store(L, base_addr, v,
                 mtlc_const_float(L->fn, mtlc_type_scalar(MTLC_TYPE_FLOAT64), 0));
    }
    return;
  }
  mtlc_store(L->fn, base_addr, v, mtlc_of(ty));
  (void)seq_index;
}

static MtlcValue gen_expr(Lower *L, Node *e) {
  if (!e)
    return MTLC_NO_VALUE;
  switch (e->kind) {
  case EX_INT:
  case EX_CHAR:
    return mtlc_const_int(L->fn, mtlc_of(e->type), e->ival);
  case EX_FLOAT:
    return mtlc_const_float(L->fn, mtlc_of(e->type), e->fval);
  case EX_STRING:
    return gen_string(L, e->str, e->str_len);
  case EX_IDENT: {
    if (!e->sym) {
      if (e->name && strcmp(e->name, "__c99m_I") == 0)
        goto cplx_I;
      return mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_INT32), 0);
    }
    if (e->sym->kind == SYM_FUNC) {
      const char *ln = sym_link(e->sym) ? sym_link(e->sym) : e->name;
      if (e->sym->is_extern)
        ensure_extern_fn(L, ln, e->sym->type);
      return mtlc_function_address(L->fn, ln);
    }
    /* ptr_of locals always hold the base address of array/struct/complex
     * storage. After array-to-pointer decay the AST type is TY_PTR — still
     * return that address. Never mtlc_load through it (that would deref). */
    MtlcValue p = ptr_of(L, e->sym);
    if (p != MTLC_NO_VALUE)
      return p;
    if (e->sym->is_global) {
      const char *ln = sym_link(e->sym) ? sym_link(e->sym) : e->name;
      if (is_addr_global(e->sym)) {
        size_t sz = e->sym->type && e->sym->type->size ? e->sym->type->size : 8;
        MtlcValue base = ensure_agg_global(L, ln, sz);
        if (is_agg(e->type) || (e->type && e->type->kind == TY_ARRAY))
          return base;
        /* Scalar global with address taken: load through pointer storage. */
        return mtlc_load(L->fn, base, mtlc_of(e->type));
      }
      return mtlc_global_ref(L->fn, ln);
    }
    return local_of(L, e->sym);
  }
  case EX_BUILTIN: {
    if (e->name && strcmp(e->name, "__c99m_I") == 0) {
    cplx_I:;
      MtlcValue addr = cplx_alloc(L);
      cplx_store(L, addr,
                 mtlc_const_float(L->fn, mtlc_type_scalar(MTLC_TYPE_FLOAT64), 0.0),
                 mtlc_const_float(L->fn, mtlc_type_scalar(MTLC_TYPE_FLOAT64), 1.0));
      return addr;
    }
    if (e->name && strcmp(e->name, "__builtin_va_start") == 0) {
      /* ap = va_param */
      if (L->has_va && buf_len(e->stmts) >= 1) {
        Node *apn = e->stmts[0];
        MtlcValue ap_addr = gen_lvalue_addr(L, apn);
        mtlc_store(L->fn, ap_addr, L->va_param,
                   mtlc_type_pointer(mtlc_type_scalar(MTLC_TYPE_UINT8)));
      }
      return MTLC_NO_VALUE;
    }
    if (e->name && strcmp(e->name, "__builtin_va_end") == 0)
      return MTLC_NO_VALUE;
    if (e->name && strcmp(e->name, "__builtin_va_arg") == 0) {
      Type *ty = e->decl_type ? e->decl_type : L->tc->ty_int;
      Node *apn = e->stmts[0];
      MtlcValue ap_slot = gen_lvalue_addr(L, apn);
      const MtlcType *i8p =
          mtlc_type_pointer(mtlc_type_scalar(MTLC_TYPE_UINT8));
      MtlcValue ap = mtlc_load(L->fn, ap_slot, i8p);
      MtlcValue val = mtlc_load(L->fn, ap, mtlc_of(ty));
      MtlcValue eight =
          mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_UINT64), 8);
      MtlcValue nap = mtlc_binary(L->fn, "+", ap, eight, i8p);
      mtlc_store(L->fn, ap_slot, nap, i8p);
      return val;
    }
    if (e->name && (strcmp(e->name, "__real__") == 0 ||
                    strcmp(e->name, "__imag__") == 0)) {
      MtlcValue addr = gen_expr(L, e->stmts[0]);
      MtlcValue re, im;
      cplx_load(L, addr, &re, &im);
      return (strcmp(e->name, "__real__") == 0) ? re : im;
    }
    return mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_INT32), 0);
  }
  case EX_BINARY: {
    if (e->op == OP_AND || e->op == OP_OR) {
      MtlcValue r = mtlc_local(L->fn, fresh_tmp(L, "sc"),
                               mtlc_type_scalar(MTLC_TYPE_INT32));
      char *t = fresh_label(L, "t");
      char *f = fresh_label(L, "f");
      char *end = fresh_label(L, "end");
      gen_bool(L, e, t, f);
      mtlc_label(L->fn, t);
      mtlc_assign(L->fn, r,
                  mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_INT32), 1));
      mtlc_jump(L->fn, end);
      mtlc_label(L->fn, f);
      mtlc_assign(L->fn, r,
                  mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_INT32), 0));
      mtlc_label(L->fn, end);
      return r;
    }
    /* complex */
    if (e->type && e->type->kind == TY_COMPLEX) {
      MtlcValue la = gen_expr(L, e->lhs);
      MtlcValue ra = gen_expr(L, e->rhs);
      MtlcValue lr, li, rr, ri;
      if (e->lhs->type && e->lhs->type->kind == TY_COMPLEX)
        cplx_load(L, la, &lr, &li);
      else {
        lr = la;
        li = mtlc_const_float(L->fn, mtlc_type_scalar(MTLC_TYPE_FLOAT64), 0);
      }
      if (e->rhs->type && e->rhs->type->kind == TY_COMPLEX)
        cplx_load(L, ra, &rr, &ri);
      else {
        rr = ra;
        ri = mtlc_const_float(L->fn, mtlc_type_scalar(MTLC_TYPE_FLOAT64), 0);
      }
      const MtlcType *f64 = mtlc_type_scalar(MTLC_TYPE_FLOAT64);
      MtlcValue out = cplx_alloc(L);
      if (e->op == OP_ADD) {
        cplx_store(L, out, mtlc_binary(L->fn, "+", lr, rr, f64),
                   mtlc_binary(L->fn, "+", li, ri, f64));
      } else if (e->op == OP_SUB) {
        cplx_store(L, out, mtlc_binary(L->fn, "-", lr, rr, f64),
                   mtlc_binary(L->fn, "-", li, ri, f64));
      } else if (e->op == OP_MUL) {
        /* (lr+li i)(rr+ri i) = lr*rr - li*ri + (lr*ri+li*rr)i */
        MtlcValue re = mtlc_binary(
            L->fn, "-", mtlc_binary(L->fn, "*", lr, rr, f64),
            mtlc_binary(L->fn, "*", li, ri, f64), f64);
        MtlcValue im = mtlc_binary(
            L->fn, "+", mtlc_binary(L->fn, "*", lr, ri, f64),
            mtlc_binary(L->fn, "*", li, rr, f64), f64);
        cplx_store(L, out, re, im);
      } else {
        cplx_store(L, out, lr, li);
      }
      return out;
    }
    MtlcValue lhs = gen_expr(L, e->lhs);
    MtlcValue rhs = gen_expr(L, e->rhs);
    Type *lt = type_decay(L->tc, e->lhs->type);
    Type *rt = type_decay(L->tc, e->rhs->type);

    if (e->op == OP_ADD && lt && lt->kind == TY_PTR && type_is_integer(rt)) {
      size_t esz = lt->base && lt->base->size ? lt->base->size : 1;
      const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
      MtlcValue scale = mtlc_const_int(L->fn, u64, (long long)esz);
      rhs = mtlc_cast(L->fn, rhs, u64);
      MtlcValue off = mtlc_binary(L->fn, "*", rhs, scale, u64);
      lhs = mtlc_cast(L->fn, lhs, u64);
      MtlcValue sum = mtlc_binary(L->fn, "+", lhs, off, u64);
      return mtlc_cast(L->fn, sum, mtlc_of(e->type));
    }
    if (e->op == OP_ADD && rt && rt->kind == TY_PTR && type_is_integer(lt)) {
      size_t esz = rt->base && rt->base->size ? rt->base->size : 1;
      const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
      MtlcValue scale = mtlc_const_int(L->fn, u64, (long long)esz);
      lhs = mtlc_cast(L->fn, lhs, u64);
      MtlcValue off = mtlc_binary(L->fn, "*", lhs, scale, u64);
      rhs = mtlc_cast(L->fn, rhs, u64);
      MtlcValue sum = mtlc_binary(L->fn, "+", rhs, off, u64);
      return mtlc_cast(L->fn, sum, mtlc_of(e->type));
    }
    if (e->op == OP_SUB && lt && lt->kind == TY_PTR && rt && rt->kind == TY_PTR) {
      const MtlcType *i64 = mtlc_type_scalar(MTLC_TYPE_INT64);
      lhs = mtlc_cast(L->fn, lhs, i64);
      rhs = mtlc_cast(L->fn, rhs, i64);
      MtlcValue diff = mtlc_binary(L->fn, "-", lhs, rhs, i64);
      size_t esz = lt->base && lt->base->size ? lt->base->size : 1;
      if (esz > 1) {
        MtlcValue scale = mtlc_const_int(L->fn, i64, (long long)esz);
        diff = mtlc_binary(L->fn, "/", diff, scale, i64);
      }
      return cast_to(L, diff, L->tc->ty_llong, e->type);
    }
    if (e->op == OP_SUB && lt && lt->kind == TY_PTR && type_is_integer(rt)) {
      size_t esz = lt->base && lt->base->size ? lt->base->size : 1;
      const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
      MtlcValue scale = mtlc_const_int(L->fn, u64, (long long)esz);
      rhs = mtlc_cast(L->fn, rhs, u64);
      MtlcValue off = mtlc_binary(L->fn, "*", rhs, scale, u64);
      lhs = mtlc_cast(L->fn, lhs, u64);
      MtlcValue sum = mtlc_binary(L->fn, "-", lhs, off, u64);
      return mtlc_cast(L->fn, sum, mtlc_of(e->type));
    }

    Type *ct = e->type;
    if (is_cmp(e->op)) {
      /* Compare in the common operand type: mtlc keys signed-vs-unsigned and
       * float-vs-int comparison selection off the result_type. */
      if (type_is_arithmetic(lt) && type_is_arithmetic(rt))
        ct = type_usual_arith(L->tc, lt, rt);
      else
        ct = L->tc->ty_ullong; /* pointer comparisons are unsigned */
    } else if (type_is_arithmetic(lt) && type_is_arithmetic(rt))
      ct = type_usual_arith(L->tc, lt, rt);

    lhs = cast_to(L, lhs, lt, ct);
    rhs = cast_to(L, rhs, rt, ct);
    const MtlcType *rt_mtlc = mtlc_of(ct);
    MtlcValue r = mtlc_binary(L->fn, binop_mtlc(e->op), lhs, rhs, rt_mtlc);
    if (is_cmp(e->op) && type_is_float(ct))
      r = mtlc_cast(L->fn, r, mtlc_type_scalar(MTLC_TYPE_INT32));
    return r;
  }
  case EX_UNARY: {
    if (e->op == OP_ADDR) {
      if (e->lhs->kind == EX_IDENT && e->lhs->sym &&
          e->lhs->sym->kind == SYM_FUNC) {
        const char *ln =
            sym_link(e->lhs->sym) ? sym_link(e->lhs->sym) : e->lhs->name;
        if (e->lhs->sym->is_extern)
          ensure_extern_fn(L, ln, e->lhs->sym->type);
        return mtlc_function_address(L->fn, ln);
      }
      return gen_lvalue_addr(L, e->lhs);
    }
    if (e->op == OP_DEREF) {
      MtlcValue p = gen_expr(L, e->lhs);
      if (e->type && is_agg(e->type))
        return p;
      return mtlc_load(L->fn, p, mtlc_of(e->type));
    }
    if (e->op == OP_PREINC || e->op == OP_PREDEC) {
      MtlcValue addr = gen_lvalue_addr(L, e->lhs);
      MtlcValue cur = mtlc_load(L->fn, addr, mtlc_of(e->lhs->type));
      MtlcValue one = mtlc_const_int(L->fn, mtlc_of(e->type), 1);
      MtlcValue nv = mtlc_binary(L->fn, e->op == OP_PREINC ? "+" : "-", cur, one,
                                 mtlc_of(e->type));
      mtlc_store(L->fn, addr, nv, mtlc_of(e->lhs->type));
      return nv;
    }
    MtlcValue v = gen_expr(L, e->lhs);
    if (e->op == OP_PLUS)
      return v;
    if (e->op == OP_NEG)
      return mtlc_unary(L->fn, "-", v, mtlc_of(e->type));
    if (e->op == OP_BITNOT)
      return mtlc_unary(L->fn, "~", v, mtlc_of(e->type));
    if (e->op == OP_NOT)
      return mtlc_unary(L->fn, "!", v, mtlc_type_scalar(MTLC_TYPE_INT32));
    return v;
  }
  case EX_POSTFIX: {
    MtlcValue addr = gen_lvalue_addr(L, e->lhs);
    MtlcValue cur = mtlc_load(L->fn, addr, mtlc_of(e->lhs->type));
    MtlcValue one = mtlc_const_int(L->fn, mtlc_of(e->type), 1);
    MtlcValue nv = mtlc_binary(L->fn, e->op == OP_POSTINC ? "+" : "-", cur, one,
                               mtlc_of(e->type));
    mtlc_store(L->fn, addr, nv, mtlc_of(e->lhs->type));
    return cur;
  }
  case EX_ASSIGN: {
    /* bitfield member assign */
    if (e->lhs->kind == EX_MEMBER) {
      Type *st = e->lhs->lhs->type;
      if (e->lhs->is_arrow && st && st->kind == TY_PTR)
        st = st->base;
      StructMember *m = st ? member_info(L, st, e->lhs->name) : NULL;
      if (m && m->is_bitfield) {
        MtlcValue addr = gen_lvalue_addr(L, e->lhs);
        MtlcValue rhs = gen_expr(L, e->rhs);
        if (e->op != OP_ASSIGN) {
          MtlcValue cur = bf_load(L, addr, m);
          rhs = mtlc_binary(L->fn, binop_mtlc(e->op == OP_ADD_A ? OP_ADD : OP_SUB),
                            cur, rhs, mtlc_of(e->type));
        }
        bf_store(L, addr, m, rhs);
        return rhs;
      }
    }
    /* Aggregate assignment: copy object representation. */
    if (e->op == OP_ASSIGN && e->lhs->type && is_agg(e->lhs->type) &&
        e->lhs->type->kind != TY_ARRAY) {
      MtlcValue dst = gen_lvalue_addr(L, e->lhs);
      MtlcValue src = gen_expr(L, e->rhs); /* address of aggregate rvalue */
      size_t sz = e->lhs->type->size ? e->lhs->type->size : 1;
      mem_copy(L, dst, src, sz);
      return dst;
    }
    MtlcValue rhs = gen_expr(L, e->rhs);
    if (e->op != OP_ASSIGN) {
      MtlcValue addr = gen_lvalue_addr(L, e->lhs);
      MtlcValue cur = mtlc_load(L->fn, addr, mtlc_of(e->lhs->type));
      OpKind bop = OP_ADD;
      switch (e->op) {
      case OP_ADD_A: bop = OP_ADD; break;
      case OP_SUB_A: bop = OP_SUB; break;
      case OP_MUL_A: bop = OP_MUL; break;
      case OP_DIV_A: bop = OP_DIV; break;
      case OP_MOD_A: bop = OP_MOD; break;
      case OP_AND_A: bop = OP_BITAND; break;
      case OP_OR_A: bop = OP_BITOR; break;
      case OP_XOR_A: bop = OP_BITXOR; break;
      case OP_SHL_A: bop = OP_LSHIFT; break;
      case OP_SHR_A: bop = OP_RSHIFT; break;
      default: break;
      }
      rhs = mtlc_binary(L->fn, binop_mtlc(bop), cur, rhs, mtlc_of(e->type));
      mtlc_store(L->fn, addr, rhs, mtlc_of(e->lhs->type));
      return rhs;
    }
    if (e->lhs->kind == EX_IDENT && e->lhs->sym && !e->lhs->sym->is_global &&
        ptr_of(L, e->lhs->sym) == MTLC_NO_VALUE) {
      MtlcValue loc = local_of(L, e->lhs->sym);
      rhs = cast_to(L, rhs, e->rhs->type, e->lhs->type);
      mtlc_assign(L->fn, loc, rhs);
      return loc;
    }
    if (e->lhs->kind == EX_IDENT && e->lhs->sym && e->lhs->sym->is_global &&
        !is_agg(e->lhs->type) && !e->lhs->sym->address_taken) {
      const char *ln = sym_link(e->lhs->sym);
      MtlcValue g = mtlc_global_ref(L->fn, ln ? ln : e->lhs->name);
      rhs = cast_to(L, rhs, e->rhs->type, e->lhs->type);
      mtlc_assign(L->fn, g, rhs);
      return g;
    }
    if (e->lhs->kind == EX_IDENT && e->lhs->sym && e->lhs->sym->is_global &&
        e->lhs->sym->address_taken && !is_agg(e->lhs->type)) {
      MtlcValue addr = gen_lvalue_addr(L, e->lhs);
      rhs = cast_to(L, rhs, e->rhs->type, e->lhs->type);
      mtlc_store(L->fn, addr, rhs, mtlc_of(e->lhs->type));
      return rhs;
    }
    MtlcValue addr = gen_lvalue_addr(L, e->lhs);
    if (e->lhs->type && e->lhs->type->kind == TY_COMPLEX) {
      MtlcValue re, im;
      cplx_load(L, rhs, &re, &im);
      cplx_store(L, addr, re, im);
      return addr;
    }
    rhs = cast_to(L, rhs, e->rhs->type, e->lhs->type);
    mtlc_store(L->fn, addr, rhs, mtlc_of(e->lhs->type));
    return rhs;
  }
  case EX_CALL: {
    /* already rewritten builtins as EX_BUILTIN in sema; handle if still CALL */
    if (e->lhs->kind == EX_IDENT && e->lhs->name &&
        strncmp(e->lhs->name, "__builtin_", 10) == 0) {
      e->kind = EX_BUILTIN;
      e->name = e->lhs->name;
      return gen_expr(L, e);
    }
    if (e->lhs->kind == EX_IDENT && e->lhs->name &&
        (strcmp(e->lhs->name, "__real__") == 0 ||
         strcmp(e->lhs->name, "__imag__") == 0)) {
      e->kind = EX_BUILTIN;
      e->name = e->lhs->name;
      return gen_expr(L, e);
    }

    size_t n = buf_len(e->stmts);
    /* Direct call by name */
    if (e->lhs->kind == EX_IDENT && e->lhs->sym &&
        e->lhs->sym->kind == SYM_FUNC) {
      const char *cname = e->lhs->name;
      Type *ft = e->lhs->sym->type;
      const char *lname = sym_link(e->lhs->sym);
      if (lname)
        cname = lname;
      int sret = ft && is_agg(ft->base) && ft->base->kind != TY_ARRAY;
      if (e->lhs->sym->is_extern)
        ensure_extern_fn(L, cname, ft);
      /* Extern variadic: fixed args per prototype, tail as i64 padded to the
       * canonical arity (see C99M_VARIADIC_PAD). */
      if (ft && ft->is_variadic && e->lhs->sym->is_extern) {
        size_t fixed = ft->param_count;
        size_t total = C99M_VARIADIC_PAD;
        if (total < n)
          total = n; /* over-long call: cannot pad consistently, best effort */
        const MtlcType *i64 = mtlc_type_scalar(MTLC_TYPE_INT64);
        MtlcValue *args =
            (MtlcValue *)arena_alloc(L->arena, (total ? total : 1) * sizeof(MtlcValue));
        for (size_t i = 0; i < n; i++) {
          MtlcValue v = gen_expr(L, e->stmts[i]);
          if (i < fixed) {
            args[i] = mtlc_cast(L->fn, v, mtlc_of(ft->params[i]));
          } else if (e->stmts[i]->type && type_is_float(e->stmts[i]->type)) {
            args[i] = float_bits_as_i64(L, v, mtlc_of(e->stmts[i]->type));
          } else {
            args[i] = mtlc_cast(L->fn, v, i64);
          }
        }
        for (size_t i = n; i < total; i++)
          args[i] = mtlc_const_int(L->fn, i64, 0);
        const MtlcType *rt = mtlc_of(e->type);
        if (e->type && e->type->kind == TY_VOID)
          rt = mtlc_type_scalar(MTLC_TYPE_VOID);
        return mtlc_call(L->fn, cname, args, total, rt);
      }
      /* User-defined variadic: pack trailing args into buffer */
      if (ft && ft->is_variadic && !e->lhs->sym->is_extern) {
        size_t fixed = ft->param_count;
        size_t extra = n > fixed ? n - fixed : 0;
        size_t base_off = sret ? 1 : 0;
        MtlcValue *args = (MtlcValue *)arena_alloc(
            L->arena, (fixed + 1 + base_off) * sizeof(MtlcValue));
        MtlcValue sret_buf = MTLC_NO_VALUE;
        if (sret) {
          size_t sz = ft->base->size ? ft->base->size : 8;
          sret_buf = alloc_bytes(L, sz, 0, MTLC_NO_VALUE);
          mem_zero(L, sret_buf, sz);
          args[0] = sret_buf;
        }
        for (size_t i = 0; i < fixed && i < n; i++)
          args[base_off + i] = gen_expr(L, e->stmts[i]);
        MtlcValue buf = alloc_bytes(L, (extra ? extra : 1) * 8, 0, MTLC_NO_VALUE);
        for (size_t i = 0; i < extra; i++) {
          Node *argn = e->stmts[fixed + i];
          MtlcValue v = gen_expr(L, argn);
          MtlcValue off = mtlc_const_int(
              L->fn, mtlc_type_scalar(MTLC_TYPE_UINT64), (long long)(i * 8));
          MtlcValue a = mtlc_binary(L->fn, "+", buf, off, i8p_ty());
          if (argn->type && type_is_float(argn->type)) {
            /* default argument promotion: store the double's bits, not a
             * numeric int conversion */
            const MtlcType *f64 = mtlc_type_scalar(MTLC_TYPE_FLOAT64);
            v = mtlc_cast(L->fn, v, f64);
            MtlcValue fp = mtlc_cast(L->fn, a, mtlc_type_pointer(f64));
            mtlc_store(L->fn, fp, v, f64);
          } else {
            v = mtlc_cast(L->fn, v, mtlc_type_scalar(MTLC_TYPE_INT64));
            mtlc_store(L->fn, a, v, mtlc_type_scalar(MTLC_TYPE_INT64));
          }
        }
        args[base_off + fixed] = buf;
        MtlcValue r = mtlc_call(L->fn, cname, args, fixed + 1 + base_off,
                                sret ? i8p_ty() : mtlc_of(e->type));
        return sret ? sret_buf : r;
      }
      /* Aggregate return: hidden sret first arg */
      if (sret) {
        size_t sz = ft->base->size ? ft->base->size : 8;
        MtlcValue sret_buf = alloc_bytes(L, sz, 0, MTLC_NO_VALUE);
        mem_zero(L, sret_buf, sz);
        MtlcValue *args =
            (MtlcValue *)arena_alloc(L->arena, (n + 1) * sizeof(MtlcValue));
        args[0] = sret_buf;
        for (size_t i = 0; i < n; i++)
          args[i + 1] = gen_expr(L, e->stmts[i]);
        mtlc_call(L->fn, cname, args, n + 1, i8p_ty());
        return sret_buf;
      }
      /* Plain direct call (extern already declared via prototype above). */
      MtlcValue *args =
          n ? (MtlcValue *)arena_alloc(L->arena, n * sizeof(MtlcValue)) : NULL;
      for (size_t i = 0; i < n; i++) {
        args[i] = gen_expr(L, e->stmts[i]);
        if (e->lhs->sym->is_extern && ft && i < ft->param_count)
          args[i] = mtlc_cast(L->fn, args[i], mtlc_of(ft->params[i]));
      }
      const MtlcType *rt = mtlc_of(e->type);
      if (e->type && e->type->kind == TY_VOID)
        rt = mtlc_type_scalar(MTLC_TYPE_VOID);
      return mtlc_call(L->fn, cname, args, n, rt);
    }
    /* indirect */
    MtlcValue fp = gen_expr(L, e->lhs);
    return gen_indirect_call(L, fp, e);
  }
  case EX_INDEX: {
    MtlcValue addr = gen_lvalue_addr(L, e);
    /* The ELEMENT type decides value-vs-address: e->type may have been
     * decayed in place by sema (e.g. as a call argument), which must not
     * turn an aggregate element into an 8-byte load. */
    Type *elem = e->type;
    if (e->lhs->type &&
        (e->lhs->type->kind == TY_ARRAY || e->lhs->type->kind == TY_PTR) &&
        e->lhs->type->base)
      elem = e->lhs->type->base;
    if (elem && is_agg(elem))
      return addr;
    return mtlc_load(L->fn, addr, mtlc_of(e->type));
  }
  case EX_MEMBER: {
    Type *st = e->lhs->type;
    if (e->is_arrow && st && st->kind == TY_PTR)
      st = st->base;
    StructMember *m = st ? member_info(L, st, e->name) : NULL;
    MtlcValue addr = gen_lvalue_addr(L, e);
    if (m && m->is_bitfield)
      return bf_load(L, addr, m);
    /* same in-place-decay hazard as EX_INDEX: judge by the member's type */
    if ((m && m->type && is_agg(m->type)) || (e->type && is_agg(e->type)))
      return addr;
    return mtlc_load(L->fn, addr, mtlc_of(e->type));
  }
  case EX_CAST: {
    MtlcValue v = gen_expr(L, e->lhs);
    if (e->type && e->type->kind == TY_COMPLEX)
      return v;
    return mtlc_cast(L->fn, v, mtlc_of(e->type));
  }
  case EX_SIZEOF_EXPR:
  case EX_SIZEOF_TYPE:
    return mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_UINT64), e->ival);
  case EX_COND: {
    MtlcValue r = mtlc_local(L->fn, fresh_tmp(L, "cond"),
                             is_agg(e->type) ? mtlc_blob(8) : mtlc_of(e->type));
    char *t = fresh_label(L, "ct");
    char *f = fresh_label(L, "cf");
    char *end = fresh_label(L, "ce");
    gen_bool(L, e->cond, t, f);
    mtlc_label(L->fn, t);
    mtlc_assign(L->fn, r, gen_expr(L, e->lhs));
    mtlc_jump(L->fn, end);
    mtlc_label(L->fn, f);
    mtlc_assign(L->fn, r, gen_expr(L, e->rhs));
    mtlc_label(L->fn, end);
    return r;
  }
  case EX_COMMA:
    gen_expr(L, e->lhs);
    return gen_expr(L, e->rhs);
  case EX_COMPOUND_LITERAL: {
    Type *ty = e->decl_type ? e->decl_type : e->type;
    size_t sz = ty && ty->size ? ty->size : 8;
    MtlcValue mem = alloc_bytes(L, sz, 0, MTLC_NO_VALUE);
    if (e->init)
      gen_init_into(L, ty, mem, e->init, NULL);
    return mem;
  }
  case EX_INIT_LIST:
    return mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_INT32), 0);
  default:
    return mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_INT32), 0);
  }
}

static void gen_var_decl(Lower *L, Node *d) {
  if (!d || d->kind != D_VAR || !d->sym)
    return;
  Type *ty = d->type;

  if (ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_UNION ||
      ty->kind == TY_COMPLEX) {
    size_t bytes = ty->size ? ty->size : 8;
    int is_vla = ty->kind == TY_ARRAY && ty->is_vla;
    MtlcValue mem;
    if (is_vla) {
      size_t esz = ty->base && ty->base->size ? ty->base->size : 1;
      MtlcValue count;
      if (d->rhs)
        count = gen_expr(L, d->rhs);
      else
        count = mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_UINT64), 1);
      count = mtlc_cast(L->fn, count, mtlc_type_scalar(MTLC_TYPE_UINT64));
      MtlcValue es =
          mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_UINT64), (long long)esz);
      MtlcValue nb = mtlc_binary(L->fn, "*", count, es,
                                 mtlc_type_scalar(MTLC_TYPE_UINT64));
      mem = heap_bytes(L, 0, nb);
      /* VLA: indeterminate until written. */
    } else {
      /* Fixed array/struct/complex: true stack blob (no malloc). */
      mem = stack_bytes(L, bytes ? bytes : 1, d->name);
      mem_zero(L, mem, bytes ? bytes : 1);
    }
    /* Keep a pointer local to the object for indexing and decay. */
    MtlcValue loc = mtlc_local(L->fn, arena_sprintf(L->arena, "%s_p", d->name),
                               i8p_ty());
    mtlc_assign(L->fn, loc, mem);
    bind_local(L, d->sym, loc);
    bind_ptr(L, d->sym, loc);
    /* Init from call returning aggregate: pass our storage as sret. */
    if (d->init && d->init->kind == EX_CALL && ty->kind != TY_ARRAY &&
        is_agg(ty)) {
      Node *call = d->init;
      if (call->lhs && call->lhs->kind == EX_IDENT && call->lhs->sym &&
          call->lhs->sym->kind == SYM_FUNC) {
        const char *cname = sym_link(call->lhs->sym);
        if (!cname)
          cname = call->lhs->name;
        size_t n = buf_len(call->stmts);
        MtlcValue *args =
            (MtlcValue *)arena_alloc(L->arena, (n + 1) * sizeof(MtlcValue));
        args[0] = loc;
        for (size_t i = 0; i < n; i++)
          args[i + 1] = gen_expr(L, call->stmts[i]);
        mtlc_call(L->fn, cname, args, n + 1, i8p_ty());
        return;
      }
    }
    if (d->init)
      gen_init_into(L, ty, loc, d->init, NULL);
    return;
  }

  MtlcValue loc = mtlc_local(L->fn, d->name, mtlc_of(ty));
  bind_local(L, d->sym, loc);
  if (d->init) {
    if (d->init->kind == EX_STRING && ty->kind == TY_PTR) {
      mtlc_assign(L->fn, loc, gen_expr(L, d->init));
    } else {
      MtlcValue v = gen_expr(L, d->init);
      v = cast_to(L, v, d->init->type, ty);
      mtlc_assign(L->fn, loc, v);
    }
  }
}

/* Evaluate case label constant (char/int). */
static long long case_value(Node *e) {
  if (!e)
    return 0;
  if (e->kind == EX_INT || e->kind == EX_CHAR)
    return e->ival;
  if (e->kind == EX_IDENT && e->sym && e->sym->kind == SYM_ENUM_CONST)
    return e->sym->enum_val;
  return e->ival;
}

/*
 * Flatten switch body. Parser nests `case A: case B: stmt` as
 * CASE(A, body=CASE(B, body=stmt)). Unwrap so every label is a peer and
 * share the same statement index (C multi-case labels).
 */
static void gen_switch(Lower *L, Node *st) {
  MtlcValue cv = gen_expr(L, st->cond);
  char *end = fresh_label(L, "swend");
  LoopCtx ctx = {end, L->loop ? L->loop->continue_l : end, L->loop};
  L->loop = &ctx;

  Node **raw = NULL;
  if (st->body && st->body->kind == ST_COMPOUND) {
    for (size_t i = 0; i < buf_len(st->body->stmts); i++)
      buf_push(raw, st->body->stmts[i]);
  } else if (st->body) {
    buf_push(raw, st->body);
  }

  /* flat_stmts[i] = executable stmt; labels point at an index into flat_stmts */
  Node **flat_stmts = NULL;
  char **case_labels = NULL;
  long long *case_vals = NULL;
  size_t *case_stmt_index = NULL;
  char *def_label = NULL;
  size_t def_index = (size_t)-1;

  for (size_t i = 0; i < buf_len(raw); i++) {
    Node *s = raw[i];
    /* Unwrap leading case/default chain. */
    Node **labels = NULL; /* ST_CASE / ST_DEFAULT nodes */
    while (s && (s->kind == ST_CASE || s->kind == ST_DEFAULT)) {
      buf_push(labels, s);
      s = s->body;
    }
    size_t stmt_i = buf_len(flat_stmts);
    buf_push(flat_stmts, s); /* may be NULL for empty case */

    for (size_t li = 0; li < buf_len(labels); li++) {
      Node *lb = labels[li];
      if (lb->kind == ST_CASE) {
        char *l = fresh_label(L, "case");
        buf_push(case_labels, l);
        buf_push(case_vals, case_value(lb->lhs));
        buf_push(case_stmt_index, stmt_i);
      } else if (lb->kind == ST_DEFAULT) {
        def_label = fresh_label(L, "default");
        def_index = stmt_i;
      }
    }
    buf_free(labels);
  }
  buf_free(raw);

  /* dispatch */
  for (size_t i = 0; i < buf_len(case_labels); i++) {
    MtlcValue k =
        mtlc_const_int(L->fn, mtlc_type_scalar(MTLC_TYPE_INT32), case_vals[i]);
    /* char cases: compare as int (already 0..255 / signed char value) */
    MtlcValue eq =
        mtlc_binary(L->fn, "==", cv, k, mtlc_type_scalar(MTLC_TYPE_INT32));
    char *next = fresh_label(L, "swn");
    mtlc_branch_if_zero(L->fn, eq, next);
    mtlc_jump(L->fn, case_labels[i]);
    mtlc_label(L->fn, next);
  }
  if (def_label)
    mtlc_jump(L->fn, def_label);
  else
    mtlc_jump(L->fn, end);

  /* body with fall-through across cases */
  for (size_t i = 0; i < buf_len(flat_stmts); i++) {
    for (size_t c = 0; c < buf_len(case_stmt_index); c++)
      if (case_stmt_index[c] == i)
        mtlc_label(L->fn, case_labels[c]);
    if (def_label && def_index == i)
      mtlc_label(L->fn, def_label);
    if (flat_stmts[i])
      gen_stmt(L, flat_stmts[i]);
  }

  mtlc_label(L->fn, end);
  L->loop = ctx.parent;
  buf_free(flat_stmts);
  buf_free(case_labels);
  buf_free(case_vals);
  buf_free(case_stmt_index);
}

static void gen_stmt(Lower *L, Node *st) {
  if (!st)
    return;
  /* emitted_return means "control cannot fall off the end here". Any
   * non-return statement (including an if whose branch returned) reopens
   * the fallthrough path, so reset — the final implicit return is then
   * emitted (an extra dead ret in all-paths-return functions is harmless).
   * Without this, a void function with an early `return;` falls off its
   * end into the next function's code. */
  if (st->kind != ST_RETURN && st->kind != ST_COMPOUND &&
      st->kind != ST_CASE && st->kind != ST_DEFAULT)
    L->emitted_return = 0;
  switch (st->kind) {
  case ST_NULL:
    break;
  case ST_EXPR:
    gen_expr(L, st->lhs);
    break;
  case ST_COMPOUND:
    for (size_t i = 0; i < buf_len(st->stmts); i++)
      gen_stmt(L, st->stmts[i]);
    break;
  case ST_IF: {
    char *t = fresh_label(L, "then");
    char *f = fresh_label(L, "else");
    char *end = fresh_label(L, "endif");
    gen_bool(L, st->cond, t, f);
    mtlc_label(L->fn, t);
    gen_stmt(L, st->body);
    mtlc_jump(L->fn, end);
    mtlc_label(L->fn, f);
    if (st->els)
      gen_stmt(L, st->els);
    mtlc_label(L->fn, end);
    break;
  }
  case ST_WHILE: {
    char *top = fresh_label(L, "wtop");
    char *body = fresh_label(L, "wbody");
    char *end = fresh_label(L, "wend");
    LoopCtx ctx = {end, top, L->loop};
    L->loop = &ctx;
    mtlc_label(L->fn, top);
    gen_bool(L, st->cond, body, end);
    mtlc_label(L->fn, body);
    gen_stmt(L, st->body);
    mtlc_jump(L->fn, top);
    mtlc_label(L->fn, end);
    L->loop = ctx.parent;
    break;
  }
  case ST_DO: {
    char *body = fresh_label(L, "dbody");
    char *cond = fresh_label(L, "dcond");
    char *end = fresh_label(L, "dend");
    LoopCtx ctx = {end, cond, L->loop};
    L->loop = &ctx;
    mtlc_label(L->fn, body);
    gen_stmt(L, st->body);
    mtlc_label(L->fn, cond);
    gen_bool(L, st->cond, body, end);
    mtlc_label(L->fn, end);
    L->loop = ctx.parent;
    break;
  }
  case ST_FOR: {
    char *top = fresh_label(L, "ftop");
    char *body = fresh_label(L, "fbody");
    char *inc = fresh_label(L, "finc");
    char *end = fresh_label(L, "fend");
    if (st->init)
      gen_stmt(L, st->init);
    LoopCtx ctx = {end, inc, L->loop};
    L->loop = &ctx;
    mtlc_label(L->fn, top);
    if (st->cond)
      gen_bool(L, st->cond, body, end);
    else
      mtlc_jump(L->fn, body);
    mtlc_label(L->fn, body);
    gen_stmt(L, st->body);
    mtlc_label(L->fn, inc);
    if (st->inc)
      gen_expr(L, st->inc);
    mtlc_jump(L->fn, top);
    mtlc_label(L->fn, end);
    L->loop = ctx.parent;
    break;
  }
  case ST_BREAK:
    if (L->loop)
      mtlc_jump(L->fn, L->loop->break_l);
    break;
  case ST_CONTINUE:
    if (L->loop)
      mtlc_jump(L->fn, L->loop->continue_l);
    break;
  case ST_RETURN: {
    if (st->lhs) {
      if (L->has_sret && L->ret_type && is_agg(L->ret_type)) {
        MtlcValue src = gen_expr(L, st->lhs);
        size_t sz = L->ret_type->size ? L->ret_type->size : 1;
        mem_copy(L, L->sret_param, src, sz);
        mtlc_return(L->fn, L->sret_param);
      } else {
        MtlcValue v = gen_expr(L, st->lhs);
        if (L->ret_type && !is_agg(L->ret_type))
          v = cast_to(L, v, st->lhs->type, L->ret_type);
        mtlc_return(L->fn, v);
      }
    } else {
      mtlc_return(L->fn, MTLC_NO_VALUE);
    }
    L->emitted_return = 1;
    break;
  }
  case ST_SWITCH:
    gen_switch(L, st);
    break;
  case ST_CASE:
  case ST_DEFAULT:
    gen_stmt(L, st->body);
    break;
  case ST_GOTO:
    mtlc_jump(L->fn, arena_sprintf(L->arena, ".G%s", st->name));
    break;
  case ST_LABEL:
    mtlc_label(L->fn, arena_sprintf(L->arena, ".G%s", st->name));
    gen_stmt(L, st->body);
    break;
  case ST_DECL:
    for (Node *d = st->lhs; d; d = d->next)
      gen_var_decl(L, d);
    break;
  default:
    break;
  }
  /* A structured statement with internal labels (if/loops/switch) reopens
   * the fallthrough path even when a nested `return` ran; only statements
   * that merely wrap a trailing child keep the child's verdict. */
  if (st->kind == ST_IF || st->kind == ST_WHILE || st->kind == ST_DO ||
      st->kind == ST_FOR || st->kind == ST_SWITCH)
    L->emitted_return = 0;
}

static void declare_runtime(MtlcBuilder *b) {
  const MtlcType *i32 = mtlc_type_scalar(MTLC_TYPE_INT32);
  const MtlcType *u64 = mtlc_type_scalar(MTLC_TYPE_UINT64);
  const MtlcType *i8 = mtlc_type_scalar(MTLC_TYPE_INT8);
  const MtlcType *pvoid = mtlc_type_pointer(i8);
  const MtlcType *v = mtlc_type_scalar(MTLC_TYPE_VOID);

  const char *pn1[] = {"n"};
  const MtlcType *pt1[] = {u64};
  mtlc_builder_function(b, "malloc", pvoid, pn1, pt1, 1, 1);
  const char *pf1[] = {"p"};
  const MtlcType *pft1[] = {pvoid};
  mtlc_builder_function(b, "free", v, pf1, pft1, 1, 1);
  const char *pc1[] = {"c"};
  const MtlcType *pct1[] = {i32};
  mtlc_builder_function(b, "putchar", i32, pc1, pct1, 1, 1);
  mtlc_builder_function(b, "getchar", i32, NULL, NULL, 0, 1);
  const char *pe1[] = {"code"};
  const MtlcType *pet1[] = {i32};
  mtlc_builder_function(b, "exit", v, pe1, pet1, 1, 1);
}

static void gen_function(Lower *L, Node *fn) {
  if (!fn->is_definition)
    return;

  Type *ft = fn->type;
  size_t nparams = buf_len(fn->params);
  int is_var = ft->is_variadic;
  int is_sret = ft->base && is_agg(ft->base) && ft->base->kind != TY_ARRAY;
  size_t ir_params = nparams + (is_var ? 1 : 0) + (is_sret ? 1 : 0);
  size_t arg0 = is_sret ? 1 : 0;

  const char **pnames =
      ir_params ? (const char **)arena_alloc(L->arena, ir_params * sizeof(char *))
                : NULL;
  const MtlcType **ptypes =
      ir_params
          ? (const MtlcType **)arena_alloc(L->arena, ir_params * sizeof(MtlcType *))
          : NULL;
  if (is_sret) {
    pnames[0] = "__sret";
    ptypes[0] = i8p_ty();
  }
  for (size_t i = 0; i < nparams; i++) {
    pnames[arg0 + i] = fn->params[i]->name ? fn->params[i]->name
                                           : arena_sprintf(L->arena, "arg%zu", i);
    ptypes[arg0 + i] = mtlc_of(fn->params[i]->type);
  }
  if (is_var) {
    pnames[arg0 + nparams] = "__va";
    ptypes[arg0 + nparams] = i8p_ty();
  }

  register_fptr(L, fn->name, ft);

  const MtlcType *ret_mtlc =
      is_sret ? i8p_ty()
              : (ft->base && ft->base->kind == TY_VOID
                     ? mtlc_type_scalar(MTLC_TYPE_VOID)
                     : mtlc_of(ft->base));

  MtlcFn *mf = mtlc_builder_function(L->builder, fn->name, ret_mtlc, pnames,
                                     ptypes, ir_params, 0);
  if (!mf)
    return;

  L->fn = mf;
  L->nlocals = 0;
  L->local_syms = NULL;
  L->local_vals = NULL;
  L->nptrs = 0;
  L->ptr_syms = NULL;
  L->ptr_vals = NULL;
  L->ret_type = ft->base;
  L->emitted_return = 0;
  L->fn_str_ensured = NULL;
  L->loop = NULL;
  L->has_va = is_var;
  L->va_param = MTLC_NO_VALUE;
  L->has_sret = is_sret;
  L->sret_param = MTLC_NO_VALUE;

  if (is_sret)
    L->sret_param = mtlc_fn_param(mf, 0);
  for (size_t i = 0; i < nparams; i++) {
    MtlcValue p = mtlc_fn_param(mf, arg0 + i);
    if (!fn->params[i]->sym)
      continue;
    Type *pt = fn->params[i]->type;
    if (pt && is_agg(pt) && pt->kind != TY_ARRAY) {
      /* Aggregate passed by value: the incoming IR value is a pointer to
       * the caller's object. Copy into our own storage (C by-value
       * semantics) and bind like an aggregate local. */
      size_t sz = pt->size ? pt->size : 8;
      MtlcValue copy = stack_bytes(
          L, sz,
          arena_sprintf(L->arena, "%s_byval",
                        fn->params[i]->name ? fn->params[i]->name : "arg"));
      mem_copy(L, copy, p, sz);
      MtlcValue loc = mtlc_local(
          L->fn,
          arena_sprintf(L->arena, "%s_p",
                        fn->params[i]->name ? fn->params[i]->name : "arg"),
          i8p_ty());
      mtlc_assign(L->fn, loc, copy);
      bind_local(L, fn->params[i]->sym, loc);
      bind_ptr(L, fn->params[i]->sym, loc);
      continue;
    }
    bind_local(L, fn->params[i]->sym, p);
  }
  if (is_var)
    L->va_param = mtlc_fn_param(mf, arg0 + nparams);

  /* Run file-scope string-pointer constructors before user main. */
  if (L->need_global_init_call && fn->name && strcmp(fn->name, "main") == 0)
    mtlc_call(mf, "__c99m_init_globals", NULL, 0,
              mtlc_type_scalar(MTLC_TYPE_VOID));

  gen_stmt(L, fn->body);

  if (!L->emitted_return) {
    if (is_sret)
      mtlc_return(mf, L->sret_param);
    else if (ft->base && ft->base->kind == TY_VOID)
      mtlc_return(mf, MTLC_NO_VALUE);
    else
      mtlc_return(mf, mtlc_const_int(mf, mtlc_of(ft->base), 0));
  }
}

MtlcModule *lower_program(Sema *S, Program *prog, Diag *diag) {
  Lower L;
  memset(&L, 0, sizeof(L));
  L.sema = S;
  L.diag = diag;
  L.arena = S->arena;
  L.tc = S->tc;
  L.builder = mtlc_builder_create();
  if (!L.builder) {
    diag_error(diag, (SrcLoc){0}, "failed to create IR builder");
    return NULL;
  }

  declare_runtime(L.builder);

  for (size_t i = 0; i < buf_len(prog->decls); i++) {
    Node *d = prog->decls[i];
    if (d->kind == D_VAR && d->name) {
      long long init = 0;
      if (d->init && d->init->kind == EX_INT)
        init = d->init->ival;
      int is_extern = d->storage == SC_EXTERN;
      /* Aggregates, arrays, and address-taken scalars: pointer globals,
       * allocated/inited in __c99m_init_globals (and lazily if missed). */
      int addr_obj = is_agg(d->type) || (d->type && d->type->kind == TY_ARRAY) ||
                     (d->sym && d->sym->address_taken);
      if (addr_obj) {
        mtlc_builder_global(L.builder, d->name, i8p_ty(), 0, is_extern);
        size_t sz = d->type && d->type->size ? d->type->size : 8;
        if (d->type && d->type->kind == TY_ARRAY && d->type->array_len &&
            d->type->base)
          sz = d->type->array_len *
               (d->type->base->size ? d->type->base->size : 1);
        buf_push(L.agg_global_names, d->name);
        buf_push(L.agg_global_bytes, sz);
        /* Always run through ctor so static initializers apply. */
        buf_push(L.global_runtime_inits, d);
      } else if (d->type && d->type->kind == TY_PTR && d->init &&
                 d->init->kind != EX_INT) {
        /* Pointer global with non-integer init (string, &obj, cast, ...). */
        mtlc_builder_global(L.builder, d->name, mtlc_of(d->type), 0, is_extern);
        buf_push(L.global_runtime_inits, d);
      } else {
        mtlc_builder_global(L.builder, d->name, mtlc_of(d->type), init,
                            is_extern);
      }
    } else if (d->kind == D_FUNC) {
      /* Prototypes are declared lazily at first reference (ensure_extern_fn)
       * so unused CRT/Win32 declarations never enter the module. Definitions
       * register their fptr id in gen_function. */
      (void)0;
    }
  }

  /* Runtime ctor: allocate addressable globals, apply static initializers,
   * materialize pointer-to-global and string pointer inits (declaration order). */
  if (buf_len(L.global_runtime_inits) > 0) {
    L.need_global_init_call = 1;
    MtlcFn *initfn = mtlc_builder_function(
        L.builder, "__c99m_init_globals", mtlc_type_scalar(MTLC_TYPE_VOID), NULL,
        NULL, 0, 0);
    if (initfn) {
      L.fn = initfn;
      L.fn_str_ensured = NULL;
      L.nlocals = 0;
      L.local_syms = NULL;
      L.local_vals = NULL;
      L.nptrs = 0;
      L.has_va = 0;
      L.has_sret = 0;
      L.emitted_return = 0;
      /* Pass 1: allocate every addressable object global (null-initialized). */
      for (size_t ai = 0; ai < buf_len(L.agg_global_names); ai++)
        ensure_agg_global(&L, L.agg_global_names[ai], L.agg_global_bytes[ai]);
      /* Pass 2: apply initializers in source order. */
      for (size_t gi = 0; gi < buf_len(L.global_runtime_inits); gi++)
        apply_global_init(&L, L.global_runtime_inits[gi]);
      mtlc_return(initfn, MTLC_NO_VALUE);
    }
  }

  for (size_t i = 0; i < buf_len(prog->decls); i++) {
    Node *d = prog->decls[i];
    if (d->kind == D_FUNC && d->is_definition)
      gen_function(&L, d);
  }

  MtlcModule *mod = mtlc_builder_finish(L.builder);
  if (!mod)
    diag_error(diag, (SrcLoc){0}, "IR builder failed");
  return mod;
}
