#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static double absd(double value) { return value < 0.0 ? -value : value; }

static int check_heap(void) {
  unsigned char *p = (unsigned char *)calloc(8, 1);
  int i;
  if (!p)
    return 1;
  for (i = 0; i < 8; i++)
    if (p[i] != 0)
      return 2;
  p = (unsigned char *)realloc(p, 32);
  if (!p)
    return 3;
  for (i = 0; i < 32; i++)
    p[i] = (unsigned char)i;
  for (i = 0; i < 32; i++)
    if (p[i] != (unsigned char)i)
      return 4;
  free(p);
  return 0;
}

static int check_format_and_scan(void) {
  char buf[160];
  char word[8];
  char *end;
  int si = 0;
  unsigned ui = 0;
  double fp = 0.0;
  double parsed;
  int n = snprintf(buf, sizeof buf,
                   "%d %u %#x %08d %.2f %.2e %.3g", -42, 4000000000u,
                   0x2au, 17, 3.25, 3.25, 3.25);
  const char *expected =
      "-42 4000000000 0x2a 00000017 3.25 3.25e+00 3.25";
  if (n != (int)strlen(expected) || strcmp(buf, expected) != 0)
    return 10;

  parsed = strtod(" -12.5e2tail", &end);
  if (parsed != -1250.0 || strcmp(end, "tail") != 0)
    return 11;

  if (sscanf("-17 2a 3.5 word", "%d %x %lf %7s", &si, &ui, &fp, word) != 4)
    return 12;
  if (si != -17 || ui != 0x2au || fp != 3.5 || strcmp(word, "word") != 0)
    return 13;
  return 0;
}

static int check_math(void) {
  if (sqrt(81.0) != 9.0 || pow(2.0, 10.0) != 1024.0)
    return 20;
  if (absd(sin(0.5) - 0.479425538604203) > 1e-12)
    return 21;
  if (absd(sin(2.0) - 0.909297426825682) > 1e-12)
    return 24;
  if (absd(cos(0.5) - 0.877582561890373) > 1e-12)
    return 22;
  if (floor(-2.25) != -3.0 || ceil(-2.25) != -2.0 || fmod(17.0, 5.0) != 2.0)
    return 23;
  return 0;
}

static int check_file(void) {
  const char *tmp = getenv("TEMP");
  char path[512];
  char got[16];
  const char payload[] = "c99rt-file";
  FILE *f;
  int result = 0;

  if (!tmp || !*tmp)
    tmp = ".";
  if (snprintf(path, sizeof path, "%s\\c99mtlc-runtime-selftest.tmp", tmp) < 0)
    return 30;
  remove(path);
  f = fopen(path, "w+b");
  if (!f)
    return 31;
  if (fwrite(payload, 1, sizeof payload, f) != sizeof payload)
    result = 32;
  else if (ftell(f) != (long)sizeof payload)
    result = 33;
  else if (fseek(f, 0, SEEK_SET) != 0)
    result = 34;
  else if (fread(got, 1, sizeof payload, f) != sizeof payload)
    result = 35;
  else if (memcmp(got, payload, sizeof payload) != 0)
    result = 36;
  if (fclose(f) != 0 && result == 0)
    result = 37;
  if (remove(path) != 0 && result == 0)
    result = 38;
  return result;
}

int main(void) {
  int result = check_heap();
  if (!result)
    result = check_format_and_scan();
  if (!result)
    result = check_math();
  if (!result)
    result = check_file();
  return result;
}
