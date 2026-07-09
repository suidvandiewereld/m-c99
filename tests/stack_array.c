int main(void) {
  int a[5];
  int i;
  for (i = 0; i < 5; i = i + 1)
    a[i] = i + 1;
  int s = 0;
  for (i = 0; i < 5; i = i + 1)
    s = s + a[i];
  return s; /* 15 */
}
