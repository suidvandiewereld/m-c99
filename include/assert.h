/* C99 <assert.h> */
#ifndef _ASSERT_H_DECLS
#define _ASSERT_H_DECLS
void _assert(const char *msg, const char *file, unsigned line);
#endif

/* assert may be re-defined on each inclusion depending on NDEBUG */
#undef assert
#ifdef NDEBUG
#define assert(e) ((void)0)
#else
#define assert(e) ((e) ? (void)0 : _assert(#e, __FILE__, __LINE__))
#endif
