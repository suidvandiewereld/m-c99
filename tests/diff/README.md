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

Two of these fail, and both are checked in deliberately: each is a real,
reproduced miscompile and the file is the smallest program that shows it.

| File | What is wrong | Issue |
|---|---|---|
| `shift_nested.c` | a shift whose left operand is itself a shift becomes logical | #13 |
| `many_args.c` | a call with many arguments clobbers the caller's floating-point locals | #14 |

Both were found while fixing others, which is the argument for keeping a
corpus rather than a checklist.

The other twenty-two pass, including the seven that were failing when this
directory was created: `++`/`--` on a scalar global, signed bit-fields,
`int >> unsigned`, a `case` in a nested block (and Duff's device), flexible
array members, a function pointer returning a struct, and a variadic call
destroying a `double`.

## Adding a case

Print results rather than returning them: an exit code is one byte and hides
the interesting half of a difference. Print integers, or the bit pattern of a
float, so the comparison does not depend on how a libc formats `%f`.
