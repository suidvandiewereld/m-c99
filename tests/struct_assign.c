struct S {
  int a;
  int b;
};
int main(void) {
  struct S x;
  x.a = 3;
  x.b = 4;
  struct S y;
  y = x;
  if (y.a != 3)
    return 1;
  if (y.b != 4)
    return 2;
  return 0;
}
