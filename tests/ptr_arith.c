/* Pointer arithmetic scales by the pointee size.
 *
 * libmtlc's binary op does no implicit scaling, so ++, --, += and -= each have
 * to apply it themselves the way p + n does. They did not, and p++ on an int*
 * advanced one byte. Each check compares the mutating form against the
 * equivalent p + n, which was always correct.
 */

struct big {
  int a[7]; /* 28 bytes: stride is neither 1 nor a power of two */
};

int main(void) {
  int a[8];
  int *p = a;
  int *q = a;

  p++;
  q = q + 1;
  if ((char *)p - (char *)q != 0) return 1;

  --p;
  q = q - 1;
  if ((char *)p - (char *)q != 0) return 2;

  p += 3;
  q = q + 3;
  if ((char *)p - (char *)q != 0) return 3;

  p -= 2;
  q = q - 2;
  if ((char *)p - (char *)q != 0) return 4;

  /* the stride has to come from the pointee, not a fixed word size */
  {
    struct big s[4];
    struct big *bp = s;
    struct big *bq = s;

    bp++;
    bq = bq + 1;
    if ((char *)bp - (char *)bq != 0) return 5;

    bp += 2;
    bq = bq + 2;
    if ((char *)bp - (char *)bq != 0) return 6;
  }

  /* char* keeps a stride of 1 */
  {
    char c[8];
    char *cp = c;
    cp++;
    if (cp - c != 1) return 7;
  }

  /* plain scalars must not be scaled */
  {
    int i = 5;
    i++;
    ++i;
    i += 3;
    i -= 2;
    if (i != 8) return 8;
  }

  /* the idiom the bug actually broke */
  {
    int v[4];
    int *w = v;
    int *end = v + 4;
    int sum = 0;
    int k;
    for (k = 0; k < 4; k++) v[k] = k + 1;
    while (w != end) sum += *w++;
    if (sum != 10) return 9;
  }

  return 42;
}
