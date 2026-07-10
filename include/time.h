/* C99 <time.h> — declarations; definitions come from the host CRT at link. */
#ifndef _TIME_H
#define _TIME_H

#include <stddef.h>

typedef long long time_t;
typedef long clock_t;

#define CLOCKS_PER_SEC 1000

struct tm {
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;
};

clock_t clock(void);
double difftime(time_t end, time_t start);
time_t _time64(time_t *t);
#define time _time64
time_t _mktime64(struct tm *tp);
#define mktime _mktime64
char *asctime(const struct tm *tp);
char *_ctime64(const time_t *t);
#define ctime _ctime64
struct tm *_gmtime64(const time_t *t);
#define gmtime _gmtime64
struct tm *_localtime64(const time_t *t);
#define localtime _localtime64
size_t strftime(char *s, size_t max, const char *fmt, const struct tm *tp);

#endif /* _TIME_H */
