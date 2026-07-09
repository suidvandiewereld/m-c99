struct S {
  int a;
  int b;
  int c;
};
int main(void) {
  struct S s = {.b = 2, .a = 1, .c = 3};
  struct S t = (struct S){.a = 10, .b = 20, .c = 12};
  return s.a + s.b + s.c + (t.a + t.b - t.c); /* 6 + 18 = 24 */
}
