#ifndef C99M_PREPROCESS_H
#define C99M_PREPROCESS_H

#include "common.h"

typedef struct {
  char **paths; /* stretchy -I dirs */
  Diag *diag;
  Arena *arena;
  int keep_comments; /* unused, reserved */
} PreprocessOptions;

/* Preprocess `path` (or memory `src` if path content already loaded).
 * Returns arena-allocated full translation unit text, or NULL on error.
 * On success *out_len is set. */
char *preprocess_file(PreprocessOptions *opt, const char *path, size_t *out_len);

/* Preprocess an already-loaded buffer with a display name for diagnostics. */
char *preprocess_source(PreprocessOptions *opt, const char *path,
                        const char *src, size_t src_len, size_t *out_len);

#endif /* C99M_PREPROCESS_H */
