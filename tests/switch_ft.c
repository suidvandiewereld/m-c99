int main(void) {
  int x = 1;
  int r = 0;
  switch (x) {
  case 1:
    r = r + 1;
    /* fall through */
  case 2:
    r = r + 10;
    break;
  default:
    r = 99;
  }
  return r; /* 11 */
}
