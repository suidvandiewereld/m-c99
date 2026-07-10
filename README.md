# C99Mettle

C99 frontend that lowers to [libmtlc](libmtlc/) (native IR, opt, x86-64 PE).

```bat
build.bat
bin\c99mtlc.exe tests\fib.c -o bin\fib.exe
powershell -File tests\run_suite.ps1
```

Flags: `-o`, `-I`, `-E`, `-c`, `-O0`/`-O`, `--emit-ir`, `--static-prefix=`.
`include/` is on the search path by default.
