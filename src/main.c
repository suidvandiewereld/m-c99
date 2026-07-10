#include "lexer.h"
#include "lower.h"
#include "parser.h"
#include "preprocess.h"
#include "sema.h"

#include <mtlc/build.h>
#include <mtlc/mtlc.h>

#include <stdio.h>
#include <string.h>

static void usage(const char *argv0) {
  fprintf(stderr,
          "C99Mettle - C99 compiler (libmtlc backend)\n"
          "Usage: %s [options] <file.c>...\n"
          "Options:\n"
          "  -o <path>     output executable (default: a.exe / a.out)\n"
          "  -I <dir>      add include search path\n"
          "  -E            preprocess only (write to stdout)\n"
          "  -O0/-O1/-O    optimization off / on\n"
          "  -c            emit object only (.o / .obj)\n"
          "  -S            emit object (same as -c; no asm text yet)\n"
          "  --emit-ir     finish after lowering (smoke test; no output file)\n"
          "  -h, --help    this help\n"
          "\nBackend: %s\n",
          argv0, mtlc_version());
}

/* Parse with stable lexer storage on arena. */

/* ---- __int128 runtime: unsigned 128-bit ops over a two-u64 struct.
 * Appended as an extra TU whenever any input mentions __int128; sema
 * rewrites u128 operators into these calls. Shift-subtract division. ---- */
static const char *c99m_u128_runtime_src =
    "typedef struct __c99m_u128 { unsigned long long lo; unsigned long long hi; } __c99m_u128;\n"
    "__c99m_u128 __c99m_u128_from_u64(unsigned long long x) {\n"
    "  __c99m_u128 r; r.lo = x; r.hi = 0; return r;\n"
    "}\n"
    "unsigned long long __c99m_u128_to_u64(__c99m_u128 a) { return a.lo; }\n"
    "__c99m_u128 __c99m_u128_add(__c99m_u128 a, __c99m_u128 b) {\n"
    "  __c99m_u128 r; r.lo = a.lo + b.lo;\n"
    "  r.hi = a.hi + b.hi + (r.lo < a.lo ? 1ULL : 0ULL); return r;\n"
    "}\n"
    "__c99m_u128 __c99m_u128_sub(__c99m_u128 a, __c99m_u128 b) {\n"
    "  __c99m_u128 r; r.lo = a.lo - b.lo;\n"
    "  r.hi = a.hi - b.hi - (a.lo < b.lo ? 1ULL : 0ULL); return r;\n"
    "}\n"
    "__c99m_u128 __c99m_u128_neg(__c99m_u128 a) {\n"
    "  __c99m_u128 z; z.lo = 0; z.hi = 0; return __c99m_u128_sub(z, a);\n"
    "}\n"
    "__c99m_u128 __c99m_u128_not(__c99m_u128 a) {\n"
    "  __c99m_u128 r; r.lo = ~a.lo; r.hi = ~a.hi; return r;\n"
    "}\n"
    "__c99m_u128 __c99m_u128_and(__c99m_u128 a, __c99m_u128 b) {\n"
    "  __c99m_u128 r; r.lo = a.lo & b.lo; r.hi = a.hi & b.hi; return r;\n"
    "}\n"
    "__c99m_u128 __c99m_u128_or(__c99m_u128 a, __c99m_u128 b) {\n"
    "  __c99m_u128 r; r.lo = a.lo | b.lo; r.hi = a.hi | b.hi; return r;\n"
    "}\n"
    "__c99m_u128 __c99m_u128_xor(__c99m_u128 a, __c99m_u128 b) {\n"
    "  __c99m_u128 r; r.lo = a.lo ^ b.lo; r.hi = a.hi ^ b.hi; return r;\n"
    "}\n"
    "__c99m_u128 __c99m_u128_shl(__c99m_u128 a, int k) {\n"
    "  __c99m_u128 r; k = k & 127;\n"
    "  if (k == 0) return a;\n"
    "  if (k >= 64) { r.hi = a.lo << (k - 64); r.lo = 0; return r; }\n"
    "  r.hi = (a.hi << k) | (a.lo >> (64 - k));\n"
    "  r.lo = a.lo << k; return r;\n"
    "}\n"
    "__c99m_u128 __c99m_u128_shr(__c99m_u128 a, int k) {\n"
    "  __c99m_u128 r; k = k & 127;\n"
    "  if (k == 0) return a;\n"
    "  if (k >= 64) { r.lo = a.hi >> (k - 64); r.hi = 0; return r; }\n"
    "  r.lo = (a.lo >> k) | (a.hi << (64 - k));\n"
    "  r.hi = a.hi >> k; return r;\n"
    "}\n"
    "static unsigned long long __c99m_umulhi64(unsigned long long a, unsigned long long b) {\n"
    "  unsigned long long a0 = a & 0xffffffffULL; unsigned long long a1 = a >> 32;\n"
    "  unsigned long long b0 = b & 0xffffffffULL; unsigned long long b1 = b >> 32;\n"
    "  unsigned long long t = a0 * b0;\n"
    "  unsigned long long k = t >> 32;\n"
    "  unsigned long long w1;\n"
    "  unsigned long long w2;\n"
    "  t = a1 * b0 + k; w1 = t & 0xffffffffULL; w2 = t >> 32;\n"
    "  t = a0 * b1 + w1; k = t >> 32;\n"
    "  return a1 * b1 + w2 + k;\n"
    "}\n"
    "__c99m_u128 __c99m_u128_mul(__c99m_u128 a, __c99m_u128 b) {\n"
    "  __c99m_u128 r; r.lo = a.lo * b.lo;\n"
    "  r.hi = __c99m_umulhi64(a.lo, b.lo) + a.lo * b.hi + a.hi * b.lo;\n"
    "  return r;\n"
    "}\n"
    "int __c99m_u128_lt(__c99m_u128 a, __c99m_u128 b) {\n"
    "  if (a.hi != b.hi) return a.hi < b.hi ? 1 : 0;\n"
    "  return a.lo < b.lo ? 1 : 0;\n"
    "}\n"
    "int __c99m_u128_eq(__c99m_u128 a, __c99m_u128 b) {\n"
    "  return (a.lo == b.lo && a.hi == b.hi) ? 1 : 0;\n"
    "}\n"
    "int __c99m_u128_ne(__c99m_u128 a, __c99m_u128 b) { return !__c99m_u128_eq(a, b); }\n"
    "int __c99m_u128_gt(__c99m_u128 a, __c99m_u128 b) { return __c99m_u128_lt(b, a); }\n"
    "int __c99m_u128_le(__c99m_u128 a, __c99m_u128 b) { return !__c99m_u128_lt(b, a); }\n"
    "int __c99m_u128_ge(__c99m_u128 a, __c99m_u128 b) { return !__c99m_u128_lt(a, b); }\n"
    "__c99m_u128 __c99m_u128_divmod(__c99m_u128 n, __c99m_u128 d, __c99m_u128 *rem) {\n"
    "  __c99m_u128 q; __c99m_u128 r; int i;\n"
    "  q.lo = 0; q.hi = 0; r.lo = 0; r.hi = 0;\n"
    "  if (d.lo == 0 && d.hi == 0) { if (rem) *rem = r; return q; }\n"
    "  for (i = 127; i >= 0; i--) {\n"
    "    unsigned long long bit;\n"
    "    if (i >= 64) bit = (n.hi >> (i - 64)) & 1ULL; else bit = (n.lo >> i) & 1ULL;\n"
    "    r.hi = (r.hi << 1) | (r.lo >> 63);\n"
    "    r.lo = (r.lo << 1) | bit;\n"
    "    if (r.hi > d.hi || (r.hi == d.hi && r.lo >= d.lo)) {\n"
    "      unsigned long long borrow = (r.lo < d.lo) ? 1ULL : 0ULL;\n"
    "      r.lo = r.lo - d.lo;\n"
    "      r.hi = r.hi - d.hi - borrow;\n"
    "      if (i >= 64) q.hi = q.hi | (1ULL << (i - 64)); else q.lo = q.lo | (1ULL << i);\n"
    "    }\n"
    "  }\n"
    "  if (rem) *rem = r;\n"
    "  return q;\n"
    "}\n"
    "__c99m_u128 __c99m_u128_div(__c99m_u128 a, __c99m_u128 b) {\n"
    "  return __c99m_u128_divmod(a, b, (__c99m_u128 *)0);\n"
    "}\n"
    "__c99m_u128 __c99m_u128_mod(__c99m_u128 a, __c99m_u128 b) {\n"
    "  __c99m_u128 r; __c99m_u128_divmod(a, b, &r); return r;\n"
    "}\n";

static Program *compile_tu_stable(Arena *arena, Diag *diag, TypeContext *tc,
                                  PreprocessOptions *ppopt, const char *path) {
  size_t plen = 0;
  char *pre = preprocess_file(ppopt, path, &plen);
  if (!pre)
    return NULL;

  Lexer *lexer = (Lexer *)arena_calloc(arena, sizeof(Lexer));
  lexer_init(lexer, arena, diag, path, pre, plen);

  Parser parser;
  parser_init(&parser, lexer, tc);
  return parse_program(&parser);
}

/* True if a decl chain introduces `name` as a local/param binding. */
static int decl_chain_binds(Node *d, const char *name) {
  for (; d; d = d->next) {
    if ((d->kind == D_VAR || d->kind == D_FUNC || d->kind == D_TYPEDEF) &&
        d->name && strcmp(d->name, name) == 0)
      return 1;
  }
  return 0;
}

/*
 * Rename free uses of file-scope static `old` -> `mangled`, respecting
 * block-scope shadowing (locals/params/typedefs with the same name).
 */
static void rename_static_uses(Node *n, const char *old, const char *mangled,
                               int shadowed) {
  if (!n)
    return;

  switch (n->kind) {
  case ST_COMPOUND: {
    int sh = shadowed;
    for (size_t i = 0; i < buf_len(n->stmts); i++) {
      Node *s = n->stmts[i];
      if (s && s->kind == ST_DECL && decl_chain_binds(s->lhs, old))
        sh = 1;
      rename_static_uses(s, old, mangled, sh);
    }
    return;
  }
  case ST_FOR: {
    int sh = shadowed;
    if (n->init && n->init->kind == ST_DECL &&
        decl_chain_binds(n->init->lhs, old))
      sh = 1;
    rename_static_uses(n->init, old, mangled, sh);
    rename_static_uses(n->cond, old, mangled, sh);
    rename_static_uses(n->inc, old, mangled, sh);
    rename_static_uses(n->body, old, mangled, sh);
    return;
  }
  case ST_DECL:
    /* Initializers see outer scope for C99 (declarator not yet in scope for
     * its own init in most cases); still, once this decl binds `old`, later
     * siblings in the same chain keep the name. Do not rename the decl's
     * own name field. */
    for (Node *d = n->lhs; d; d = d->next) {
      rename_static_uses(d->init, old, mangled, shadowed);
      rename_static_uses(d->rhs, old, mangled, shadowed);
    }
    return;
  case D_VAR:
  case D_FUNC:
  case D_TYPEDEF:
    rename_static_uses(n->init, old, mangled, shadowed);
    rename_static_uses(n->body, old, mangled, shadowed);
    rename_static_uses(n->rhs, old, mangled, shadowed);
    for (size_t i = 0; i < buf_len(n->params); i++)
      rename_static_uses(n->params[i], old, mangled, shadowed);
    return;
  case EX_IDENT:
    if (!shadowed && n->name && strcmp(n->name, old) == 0)
      n->name = mangled;
    return;
  default:
    break;
  }

  /* Function params shadow for the body. */
  if (n->kind == D_FUNC && n->is_definition) {
    int sh = shadowed;
    for (size_t i = 0; i < buf_len(n->params); i++) {
      Node *p = n->params[i];
      if (p && p->name && strcmp(p->name, old) == 0)
        sh = 1;
    }
    rename_static_uses(n->body, old, mangled, sh);
    return;
  }

  rename_static_uses(n->lhs, old, mangled, shadowed);
  rename_static_uses(n->rhs, old, mangled, shadowed);
  rename_static_uses(n->cond, old, mangled, shadowed);
  rename_static_uses(n->init, old, mangled, shadowed);
  rename_static_uses(n->inc, old, mangled, shadowed);
  rename_static_uses(n->body, old, mangled, shadowed);
  rename_static_uses(n->els, old, mangled, shadowed);
  for (size_t i = 0; i < buf_len(n->stmts); i++)
    rename_static_uses(n->stmts[i], old, mangled, shadowed);
  for (size_t i = 0; i < buf_len(n->params); i++)
    rename_static_uses(n->params[i], old, mangled, shadowed);
  if (n->next)
    rename_static_uses(n->next, old, mangled, shadowed);
}

int main(int argc, char **argv) {
  const char *output = NULL;
  int opt_level = 1;
  int obj_only = 0;
  const char *static_prefix = "st";
  int emit_ir_only = 0;
  int preprocess_only = 0;
  char **inputs = NULL;
  char **include_paths = NULL;

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
      usage(argv[0]);
      return 0;
    }
    if (strcmp(argv[i], "-o") == 0) {
      if (i + 1 >= argc)
        fatal("missing argument for -o");
      output = argv[++i];
      continue;
    }
    if (strcmp(argv[i], "-I") == 0) {
      if (i + 1 >= argc)
        fatal("missing argument for -I");
      buf_push(include_paths, argv[++i]);
      continue;
    }
    if (strncmp(argv[i], "-I", 2) == 0 && argv[i][2]) {
      buf_push(include_paths, argv[i] + 2);
      continue;
    }
    if (strcmp(argv[i], "-E") == 0) {
      preprocess_only = 1;
      continue;
    }
    if (strncmp(argv[i], "--static-prefix=", 16) == 0) {
      static_prefix = argv[i] + 16;
      continue;
    }
    if (strcmp(argv[i], "-O0") == 0) {
      opt_level = 0;
      continue;
    }
    if (strcmp(argv[i], "-O") == 0 || strcmp(argv[i], "-O1") == 0 ||
        strcmp(argv[i], "-O2") == 0 || strcmp(argv[i], "-O3") == 0) {
      opt_level = 1;
      continue;
    }
    if (strcmp(argv[i], "-c") == 0 || strcmp(argv[i], "-S") == 0) {
      obj_only = 1;
      continue;
    }
    if (strcmp(argv[i], "--emit-ir") == 0) {
      emit_ir_only = 1;
      continue;
    }
    if (argv[i][0] == '-')
      fatal("unknown option '%s'", argv[i]);
    buf_push(inputs, argv[i]);
  }

  if (buf_len(inputs) == 0) {
    usage(argv[0]);
    return 1;
  }

  Arena arena;
  arena_init(&arena);
  Diag diag = {0};

  PreprocessOptions ppopt;
  memset(&ppopt, 0, sizeof(ppopt));
  ppopt.arena = &arena;
  ppopt.diag = &diag;
  /* Default system include: <repo>/include (freestanding + CRT decls). */
  {
    char *def_inc = NULL;
#ifdef _WIN32
    /* Prefer relative include/ next to cwd (project root when built). */
    def_inc = "include";
#else
    def_inc = "include";
#endif
    buf_push(include_paths, def_inc);
  }
  ppopt.paths = include_paths;

  if (preprocess_only) {
    for (size_t i = 0; i < buf_len(inputs); i++) {
      size_t plen = 0;
      char *pre = preprocess_file(&ppopt, inputs[i], &plen);
      if (!pre || diag.error_count) {
        fprintf(stderr, "preprocess failed\n");
        arena_free(&arena);
        buf_free(inputs);
        buf_free(include_paths);
        return 1;
      }
      fputs(pre, stdout);
    }
    arena_free(&arena);
    buf_free(inputs);
    buf_free(include_paths);
    return 0;
  }

  if (!output) {
#ifdef _WIN32
    output = obj_only ? "a.obj" : "a.exe";
#else
    output = obj_only ? "a.o" : "a.out";
#endif
  }

  TypeContext tc;
  type_context_init(&tc, &arena);

  Program *merged = (Program *)arena_calloc(&arena, sizeof(Program));
  for (size_t i = 0; i < buf_len(inputs); i++) {
    Program *prog = compile_tu_stable(&arena, &diag, &tc, &ppopt, inputs[i]);
    if (!prog || diag.error_count) {
      fprintf(stderr, "%d error(s) generated while parsing %s.\n",
              diag.error_count, inputs[i]);
      arena_free(&arena);
      buf_free(inputs);
      buf_free(include_paths);
      return 1;
    }
    /* Internal linkage: mangle file-scope static names so multi-TU merge
     * does not expose them to other units' externs. Only free uses are
     * rewritten; block-scope names that shadow the static are left alone. */
    for (size_t d = 0; d < buf_len(prog->decls); d++) {
      Node *decl = prog->decls[d];
      decl->tu_id = (int)i;
      if (decl->storage == SC_STATIC && decl->name &&
          (decl->kind == D_VAR || decl->kind == D_FUNC)) {
        const char *old = decl->name;
        char *mangled =
            arena_sprintf(&arena, "__%s%zu_%s", static_prefix, i, old);
        for (size_t j = 0; j < buf_len(prog->decls); j++) {
          Node *fn = prog->decls[j];
          if (fn->kind == D_VAR && fn->init && fn != decl) {
            /* file-scope initializers may reference the static too */
            rename_static_uses(fn->init, old, mangled, 0);
            continue;
          }
          if (fn->kind != D_FUNC || !fn->is_definition)
            continue;
          int sh = 0;
          for (size_t p = 0; p < buf_len(fn->params); p++) {
            Node *par = fn->params[p];
            if (par && par->name && strcmp(par->name, old) == 0)
              sh = 1;
          }
          rename_static_uses(fn->body, old, mangled, sh);
        }
        decl->name = mangled;
      }
      buf_push(merged->decls, decl);
    }
  }

  /* Any TU used __int128: append the u128 helper runtime as one more TU. */
  if (c99m_saw_int128) {
    size_t rl = strlen(c99m_u128_runtime_src);
    char *pre = preprocess_source(&ppopt, "<c99m-u128-runtime>",
                                  c99m_u128_runtime_src, rl, &rl);
    if (pre) {
      Lexer *rlex = (Lexer *)arena_calloc(&arena, sizeof(Lexer));
      lexer_init(rlex, &arena, &diag, "<c99m-u128-runtime>", pre, rl);
      Parser rp;
      parser_init(&rp, rlex, &tc);
      Program *rprog = parse_program(&rp);
      if (rprog && !diag.error_count) {
        for (size_t d = 0; d < buf_len(rprog->decls); d++) {
          Node *decl = rprog->decls[d];
          decl->tu_id = (int)buf_len(inputs);
          if (decl->storage == SC_STATIC && decl->name &&
              (decl->kind == D_VAR || decl->kind == D_FUNC)) {
            const char *old_name = decl->name;
            char *mangled =
                arena_sprintf(&arena, "__stU_%s", old_name);
            for (size_t j = 0; j < buf_len(rprog->decls); j++) {
              Node *fn = rprog->decls[j];
              if (fn->kind == D_FUNC && fn->is_definition)
                rename_static_uses(fn->body, old_name, mangled, 0);
            }
            decl->name = mangled;
          }
          buf_push(merged->decls, decl);
        }
      }
    }
  }

  Sema sema;
  sema_init(&sema, &arena, &diag, &tc);
  sema_check(&sema, merged);
  if (diag.error_count) {
    fprintf(stderr, "%d error(s) generated.\n", diag.error_count);
    arena_free(&arena);
    buf_free(inputs);
    buf_free(include_paths);
    return 1;
  }

  MtlcModule *mod = lower_program(&sema, merged, &diag);
  if (!mod || diag.error_count) {
    fprintf(stderr, "lowering failed (%d error(s)).\n", diag.error_count);
    arena_free(&arena);
    buf_free(inputs);
    buf_free(include_paths);
    return 1;
  }

  if (emit_ir_only) {
    printf("OK: lowered %zu file(s) (%zu functions)\n", buf_len(inputs),
           mtlc_module_function_count(mod));
    mtlc_module_destroy(mod);
    arena_free(&arena);
    buf_free(inputs);
    buf_free(include_paths);
    return 0;
  }

  MtlcContext *ctx = mtlc_context_create();
  if (!ctx)
    fatal("mtlc_context_create failed");
  mtlc_context_set_opt_level(ctx, opt_level);
  mtlc_context_set_whole_program(ctx, obj_only ? 0 : 1);

  if (opt_level > 0) {
    if (!mtlc_optimize(ctx, mod)) {
      fprintf(stderr, "optimization failed\n");
      mtlc_context_destroy(ctx);
      mtlc_module_destroy(mod);
      arena_free(&arena);
      buf_free(inputs);
      buf_free(include_paths);
      return 1;
    }
  }

  int ok;
  if (obj_only)
    ok = mtlc_emit_object(ctx, mod, output);
  else
    ok = mtlc_build_executable(ctx, mod, output);

  mtlc_context_destroy(ctx);
  mtlc_module_destroy(mod);
  arena_free(&arena);
  buf_free(inputs);
  buf_free(include_paths);

  if (!ok) {
    fprintf(stderr, "code generation / link failed\n");
    return 1;
  }
  return 0;
}
