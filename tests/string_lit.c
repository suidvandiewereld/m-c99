int main(void) {
  const char *s = "Hi";
  if (s[0] != 'H')
    return 1;
  if (s[1] != 'i')
    return 2;
  if (s[2] != 0)
    return 3;
  char buf[4] = "Yo";
  if (buf[0] != 'Y' || buf[1] != 'o' || buf[2] != 0)
    return 4;
  return 0;
}
