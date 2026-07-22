/* setjmp/longjmp. See include/setjmp.h for why setjmp is a macro.
 *
 * Everything the pair needs is already in kernel32: RtlCaptureContext writes
 * its caller's registers and resume point into a CONTEXT, RtlRestoreContext
 * puts them back. No assembly, and nothing here has to know which registers
 * the ABI calls callee-saved. */
#include "c99rt.h"

#define RT_JMP_CONTEXT_SIZE 1232

struct __c99m_jmp_buf {
  volatile int __val;
  unsigned char __ctx[RT_JMP_CONTEXT_SIZE + 16];
};

/* A CONTEXT holds xmm registers, so it must be 16-byte aligned. The buffer
 * carries the slack for that rather than demanding an aligned jmp_buf. */
void *__c99m_jmp_ctx(struct __c99m_jmp_buf *b) {
  unsigned long long a = (unsigned long long)(void *)b->__ctx;
  return (void *)((a + 15u) & ~(unsigned long long)15u);
}

void longjmp(struct __c99m_jmp_buf *b, int val) {
  if (!b)
    return;
  /* C99 7.13.2.1p3: longjmp(b, 0) makes setjmp return 1, never 0. */
  b->__val = val ? val : 1;
  RtlRestoreContext(__c99m_jmp_ctx(b), (void *)0);
}
