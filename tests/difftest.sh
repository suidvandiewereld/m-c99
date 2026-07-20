#!/usr/bin/env bash
# Differential test: compile the same C file with gcc and with c99mtlc, run
# both, and compare what they print.
#
# gcc is the oracle. A disagreement is a c99mtlc bug until shown otherwise,
# so each case must be strictly conforming C99: no undefined behaviour, no
# implementation-defined results, nothing that depends on a specific libc.
#
# Usage: tests/difftest.sh file.c [file.c ...]
#   PASS  both compilers agree at -O0 and -O1
#   DIFF  they disagree, or the two c99mtlc optimization levels disagree
#   SKIP  c99mtlc could not compile it (a gap, not a miscompile)

set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CC99="$ROOT/bin/c99mtlc.exe"
WORK="${TMPDIR:-/tmp}/difftest.$$"
mkdir -p "$WORK"
trap 'rm -rf "$WORK"' EXIT

pass=0; diff=0; skip=0

for src in "$@"; do
  name="$(basename "$src")"

  # the oracle
  if ! gcc -std=c99 -O0 -w "$src" -o "$WORK/gcc.exe" 2>"$WORK/gcc.log"; then
    printf '%-28s SKIP  gcc rejected it (the test itself is wrong)\n' "$name"
    skip=$((skip+1)); continue
  fi
  gcc_out="$("$WORK/gcc.exe" 2>&1)"; gcc_rc=$?

  fail=""
  for opt in -O0 -O1; do
    if ! "$CC99" $opt -I "$ROOT/include" "$src" -o "$WORK/m$opt.exe" >"$WORK/m.log" 2>&1; then
      printf '%-28s SKIP  c99mtlc %s could not compile it\n' "$name" "$opt"
      sed 's/^/      /' "$WORK/m.log" | grep -E 'error' | head -3
      fail="skip"; break
    fi
    m_out="$("$WORK/m$opt.exe" 2>&1)"; m_rc=$?

    if [ "$m_out" != "$gcc_out" ] || [ "$m_rc" != "$gcc_rc" ]; then
      printf '%-28s DIFF  at %s\n' "$name" "$opt"
      echo "      gcc     (rc=$gcc_rc): $gcc_out"
      echo "      c99mtlc (rc=$m_rc): $m_out"
      fail="diff"; break
    fi
  done

  case "$fail" in
    diff) diff=$((diff+1)) ;;
    skip) skip=$((skip+1)) ;;
    *) printf '%-28s PASS\n' "$name"; pass=$((pass+1)) ;;
  esac
done

echo
echo "pass=$pass diff=$diff skip=$skip"
[ "$diff" -eq 0 ]
