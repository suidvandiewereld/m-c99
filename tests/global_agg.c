struct S {
  int a;
  int b;
};
struct S g;
int main(void) {
  g.a = 3;
  g.b = 4;
  return g.a + g.b; /* 7 */
}
