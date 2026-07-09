#define A 10
#define B(x) ((x) * 2)
#define CAT(a, b) a##b
#define STR(x) #x
int putchar(int c);
int main(void) {
  int CAT(fo, o) = A + B(6); /* 10 + 12 = 22 */
  /* ensure stringize produced something with digit */
  const char *s = STR(hello);
  if (s[0] != 'h')
    return 1;
  return foo;
}
