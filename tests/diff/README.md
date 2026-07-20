# Differential tests

Each `.c` file here is compiled twice, once by gcc and once by c99mtlc, run
both ways, and the output compared. gcc is the oracle.

```
bash tests/difftest.sh tests/diff/*.c
```

Every case must be strictly conforming C99 with no undefined or
implementation-defined behaviour, or gcc's answer is not authoritative and a
disagreement proves nothing.

The harness runs c99mtlc at both `-O0` and `-O1`, so it catches a backend
miscompile (the two levels disagree) as well as a frontend one (both disagree
with gcc).

## Current state

All twenty-five pass. Ten were failing when this directory was created, and
each has a reproduction here so a regression cannot slip back in:

- `++`/`--` on a scalar global
- signed bit-fields
- `int >> unsigned`
- a nested shift
- a `case` in a nested block (and Duff's device)
- flexible array members
- a function pointer returning a struct
- a variadic call destroying a `double`
- a call with many arguments clobbering the caller's floating-point locals
- the header/archive skew that made the backend crash on ordinary C

The last three were backend faults, fixed in MettleToolchain and vendored back
in through `vendor-libmtlc.sh`. See issues #13, #14 and #15.

## Adding a case

Print results rather than returning them: an exit code is one byte and hides
the interesting half of a difference. Print integers, or the bit pattern of a
float, so the comparison does not depend on how a libc formats `%f`.
