int main(void) {
  int a[4] = {1, 2};
  if (a[0] != 1)
    return 1;
  if (a[1] != 2)
    return 2;
  if (a[2] != 0)
    return 3;
  if (a[3] != 0)
    return 4;
  return 0;
}
