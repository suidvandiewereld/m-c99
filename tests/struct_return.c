struct S {
  int a;
  int b;
};
struct S make(void) {
  struct S s;
  s.a = 3;
  s.b = 4;
  return s;
}
int main(void) {
  struct S t = make();
  return t.a + t.b; /* 7 */
}
