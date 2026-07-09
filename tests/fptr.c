int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }
int main(void) {
  int (*fp)(int, int);
  fp = add;
  int x = fp(20, 22);
  fp = sub;
  return x + fp(10, 10); /* 42 + 0 = 42 */
}
