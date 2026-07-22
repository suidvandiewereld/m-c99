#ifndef _C99MTLC_SETJMP_H
#define _C99MTLC_SETJMP_H

/* setjmp/longjmp on the machine state Windows already knows how to save.
 *
 * RtlCaptureContext fills a CONTEXT with its CALLER's registers and resume
 * point. That is the whole trick: setjmp is a macro (C99 7.13 allows it), so
 * the capture happens in the function that wrote setjmp, and the frame it
 * records is that function's own, still live when longjmp arrives. Restoring
 * the context resumes just after the capture, where the macro reads the value
 * longjmp left behind.
 *
 * A CONTEXT is 1232 bytes and has to be 16-byte aligned, so the buffer carries
 * slack and __c99m_jmp_ctx rounds a pointer up inside it.
 */

#define _C99MTLC_JMP_CONTEXT_SIZE 1232

struct __c99m_jmp_buf {
  /* longjmp writes this, and the resumed setjmp reads it: it has to come from
     memory both times, never from a register the restore would overwrite. */
  volatile int __val;
  unsigned char __ctx[_C99MTLC_JMP_CONTEXT_SIZE + 16];
};

typedef struct __c99m_jmp_buf jmp_buf[1];

void *__c99m_jmp_ctx(struct __c99m_jmp_buf *b);
void RtlCaptureContext(void *ctx);

void longjmp(struct __c99m_jmp_buf *b, int val);
#define _longjmp longjmp

#define setjmp(b) ((b)->__val = 0, RtlCaptureContext(__c99m_jmp_ctx(b)), (b)->__val)
#define _setjmp(b) setjmp(b)

#endif
