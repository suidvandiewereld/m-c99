/* File-scope static must not capture block-scope names of the same spelling. */
static int secret = 41;

int main(void) {
  int secret = 0;
  return secret; /* must be 0, not 41 */
}
