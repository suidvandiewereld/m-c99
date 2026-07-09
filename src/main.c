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
        char *mangled = arena_sprintf(&arena, "__st%zu_%s", i, old);
        for (size_t j = 0; j < buf_len(prog->decls); j++) {
          Node *fn = prog->decls[j];
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
