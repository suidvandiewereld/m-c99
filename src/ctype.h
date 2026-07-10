/* C frontend type system (not libmtlc types — those are produced at lowering). */
#ifndef C99M_CTYPE_H
#define C99M_CTYPE_H

#include "common.h"

typedef enum {
  TY_VOID,
  TY_BOOL,
  TY_CHAR,      /* plain char, treated as signed for simplicity */
  TY_SCHAR,
  TY_UCHAR,
  TY_SHORT,
  TY_USHORT,
  TY_INT,
  TY_UINT,
  TY_LONG,
  TY_ULONG,
  TY_LLONG,
  TY_ULLONG,
  TY_FLOAT,
  TY_DOUBLE,
  TY_LDOUBLE,   /* lowered as double */
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
  TY_STRUCT,
  TY_UNION,
  TY_ENUM,
  TY_COMPLEX, /* _Complex float/double; base is float or double element */
} TypeKind;

typedef struct Type Type;
typedef struct StructMember {
  const char *name;
  Type *type;
  size_t offset;
  int is_bitfield;
  int bit_offset; /* bit offset within the containing storage unit */
  int bit_width;
} StructMember;

struct Type {
  TypeKind kind;
  size_t size;
  size_t align;
  int is_const;
  int is_incomplete; /* struct/union/array incomplete */

  Type *base;           /* pointer/array element / function return */
  size_t array_len;     /* 0 = incomplete/unsized */
  int is_vla;           /* unsupported; diagnosed */

  /* function */
  Type **params;
  size_t param_count;
  int is_variadic;
  int oldstyle; /* empty param list K&R: unspecified */

  /* struct/union/enum */
  const char *tag;
  StructMember *members; /* stretchy */
  int is_union;
  long long enum_value; /* for enum constants stored separately */
};

typedef struct {
  Arena *arena;
  /* tag namespace: struct/union/enum name -> Type* */
  Type **tags;
  size_t ntags;
  /* cached primitives */
  Type *ty_void;
  Type *ty_bool;
  Type *ty_char;
  Type *ty_schar;
  Type *ty_uchar;
  Type *ty_short;
  Type *ty_ushort;
  Type *ty_int;
  Type *ty_uint;
  Type *ty_long;
  Type *ty_ulong;
  Type *ty_llong;
  Type *ty_ullong;
  Type *ty_float;
  Type *ty_double;
  Type *ty_ldouble;
} TypeContext;

void type_context_init(TypeContext *tc, Arena *arena);
Type *type_tag_lookup(TypeContext *tc, const char *tag, int is_union);
void type_tag_register(TypeContext *tc, Type *t);

Type *type_ptr(TypeContext *tc, Type *base);
Type *type_array(TypeContext *tc, Type *base, size_t len);
Type *type_func(TypeContext *tc, Type *ret, Type **params, size_t nparams,
                int variadic);
Type *type_struct_create(TypeContext *tc, const char *tag, int is_union);
void type_struct_add_member(TypeContext *tc, Type *st, const char *name,
                            Type *mty);
void type_struct_add_bitfield(TypeContext *tc, Type *st, const char *name,
                              Type *mty, int width);
void type_struct_finish(Type *st);
Type *type_complex(TypeContext *tc, Type *real_ty);

int type_is_integer(const Type *t);
int type_is_unsigned(const Type *t);
int type_is_float(const Type *t);
int type_is_arithmetic(const Type *t);
int type_is_scalar(const Type *t);
int type_is_pointer_like(const Type *t); /* ptr or array */
int type_is_complete(const Type *t);
int type_equal(const Type *a, const Type *b);
int type_compatible(const Type *a, const Type *b);

Type *type_decay(TypeContext *tc, Type *t); /* array/func → pointer */
Type *type_promote(TypeContext *tc, Type *t); /* integer promotions */
Type *type_usual_arith(TypeContext *tc, Type *a, Type *b);

/* Ranking for conversions: higher = wider. */
int type_int_rank(const Type *t);

const char *type_to_string(TypeContext *tc, const Type *t);
StructMember *type_find_member(TypeContext *tc, Type *st, const char *name);

#endif /* C99M_CTYPE_H */
