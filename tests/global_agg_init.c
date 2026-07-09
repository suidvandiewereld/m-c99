/* File-scope aggregate static initializer must apply before main. */
struct S {
  int a;
  int b;
};
struct S g = {3, 4};

int main(void) {
  return g.a + g.b; /* 7 */
}
