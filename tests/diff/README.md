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

Seven of these fail. They are checked in deliberately: each one is a real,
reproduced miscompile, and the file is the smallest program that shows it.

| File | What is wrong |
|---|---|
| `global_incdec.c` | `++`/`--` on a scalar global yields a value computed from 0 |
| `bitfield_signed.c` | a signed bit-field is not sign-extended on read |
| `shift_mixed.c` | `int >> unsigned` becomes a logical shift |
| `switch_nested_case.c` | a `case` inside a nested block is unreachable |
| `flexible_array.c` | a flexible array member aliases the member before it |
| `fptr_struct.c` | calling a function pointer that returns a struct crashes |
| `vararg_cast.c` | an int derived from a float cast is passed as raw float bits |

The other sixteen pass, which is the useful half of that number: struct copies,
recursion, designated initializers, compound literals, unions, multidimensional
arrays, goto out of nested loops, unsigned division, integer promotions and
`va_arg` are all correct.

## Adding a case

Print results rather than returning them: an exit code is one byte and hides
the interesting half of a difference. Print integers, or the bit pattern of a
float, so the comparison does not depend on how a libc formats `%f`.
