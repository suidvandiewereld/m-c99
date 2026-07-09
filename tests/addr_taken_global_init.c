/* Address-taken scalar with static init; *p must see 7. */
int g = 7;
int *p = &g;

int main(void) {
  if (*p != 7)
    return 1;
  if (g != 7)
    return 2;
  return 0;
}
