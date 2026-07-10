/* C99 <signal.h> — Windows CRT signal set. */
#ifndef _SIGNAL_H
#define _SIGNAL_H

typedef int sig_atomic_t;

#define SIGINT 2
#define SIGILL 4
#define SIGABRT 22
#define SIGFPE 8
#define SIGSEGV 11
#define SIGTERM 15

#define SIG_DFL ((void (*)(int))0)
#define SIG_IGN ((void (*)(int))1)
#define SIG_ERR ((void (*)(int))-1)

void (*signal(int sig, void (*handler)(int)))(int);
int raise(int sig);

#endif /* _SIGNAL_H */
