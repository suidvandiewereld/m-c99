/* Multi-arg printf via real libc; also a simple user vararg consumer. */
int printf(const char *fmt, ...);

static int sum(int n, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, n);
  int s = 0;
  int i;
  for (i = 0; i < n; i = i + 1) {
    s = s + __builtin_va_arg(ap, int);
  }
  __builtin_va_end(ap);
  return s;
}

int main(void) {
  int s = sum(3, 10, 20, 12);
  if (s != 42)
    return 1;
  printf("%d\n", s);
  return 0;
}
