/* <ctype.h>, C locale only. Functions rather than macros: the headers
 * declare them and code takes their address. */

static int rt_in(int c, int lo, int hi) { return c >= lo && c <= hi; }

int isdigit(int c) { return rt_in(c, '0', '9'); }
int isupper(int c) { return rt_in(c, 'A', 'Z'); }
int islower(int c) { return rt_in(c, 'a', 'z'); }
int isalpha(int c) { return isupper(c) || islower(c); }
int isalnum(int c) { return isalpha(c) || isdigit(c); }
int isxdigit(int c) {
  return isdigit(c) || rt_in(c, 'a', 'f') || rt_in(c, 'A', 'F');
}
int isspace(int c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\v' || c == '\f' ||
         c == '\r';
}
int isblank(int c) { return c == ' ' || c == '\t'; }
int iscntrl(int c) { return rt_in(c, 0, 31) || c == 127; }
int isprint(int c) { return rt_in(c, 32, 126); }
int isgraph(int c) { return rt_in(c, 33, 126); }
int ispunct(int c) { return isgraph(c) && !isalnum(c); }

int tolower(int c) { return isupper(c) ? c + 32 : c; }
int toupper(int c) { return islower(c) ? c - 32 : c; }
