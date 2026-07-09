/* Must NOT see static secret from static_a.c.
 * We define our own secret = 0; if static mangling failed, both TUs would
 * collide at link or share one symbol and this init would clobber 41. */
int secret = 0;
int get_secret(void);
int main(void) {
  int from_fn = get_secret();
  if (from_fn != 41)
    return 1;
  /* Local secret is 0; static in A is private. */
  if (secret != 0)
    return 41;
  return 0;
}
