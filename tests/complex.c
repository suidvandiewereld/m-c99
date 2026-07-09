int main(void) {
  _Complex double z = 3.0 + 4.0 * I;
  double r = __real__(z);
  double im = __imag__(z);
  /* |z|^2 = 9+16 = 25 */
  double mag2 = r * r + im * im;
  if (mag2 < 24.5 || mag2 > 25.5)
    return 1;
  _Complex double w = z + z;
  if (__real__(w) < 5.5 || __real__(w) > 6.5)
    return 2;
  return 0;
}
