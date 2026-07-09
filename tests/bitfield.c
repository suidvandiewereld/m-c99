struct B {
  unsigned int a : 3;
  unsigned int b : 5;
  unsigned int c : 8;
};
int main(void) {
  struct B x;
  x.a = 5;
  x.b = 17;
  x.c = 200;
  if (x.a != 5)
    return 1;
  if (x.b != 17)
    return 2;
  if (x.c != 200)
    return 3;
  return (int)(x.a + x.b); /* 22 */
}
