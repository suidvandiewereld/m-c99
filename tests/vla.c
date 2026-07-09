int sum_n(int n) {
  int a[n];
  int i;
  int s = 0;
  for (i = 0; i < n; i = i + 1) {
    a[i] = i + 1;
    s = s + a[i];
  }
  return s;
}
int main(void) {
  return sum_n(5); /* 15 */
}
