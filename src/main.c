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
    for (size_t d = 0; d < buf_len(prog->decls); d++)
      buf_push(merged->decls, prog->decls[d]);
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
