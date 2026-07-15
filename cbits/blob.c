/* Caller-owned MtlcType descriptors that mtlc/build.h has no constructor for.
 *
 * mtlc_type_scalar/mtlc_type_pointer only mint scalars and pointers; an array
 * type of N bytes has to be an MtlcType the frontend fills in and owns. Doing
 * that from Haskell would mean hardcoding the struct's field offsets, which
 * would break silently the next time mtlc/type.h changes. Here the C compiler
 * computes them.
 *
 * Descriptors are interned and immortal: codegen holds the pointer well past
 * the call that produced it, so they must never be freed.
 */
#include <mtlc/type.h>

#include <stdio.h>
#include <stdlib.h>

typedef struct BlobType {
  size_t bytes;
  MtlcType ty;
  char name[32];
  struct BlobType *next;
} BlobType;

static BlobType *blob_types;

/* An array-of-uint8 type of `bytes` (rounded up to 8), for stack storage of a
 * C aggregate via mtlc_local. */
const MtlcType *c99m_blob_type(size_t bytes) {
  if (bytes == 0)
    bytes = 1;
  size_t aligned = (bytes + 7u) & ~(size_t)7u;

  for (BlobType *b = blob_types; b; b = b->next)
    if (b->bytes == aligned)
      return &b->ty;

  BlobType *b = (BlobType *)calloc(1, sizeof(BlobType));
  if (!b) {
    fprintf(stderr, "c99mtlc: out of memory\n");
    exit(1);
  }
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
