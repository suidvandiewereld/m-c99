# C99Mettle

A C99 compiler frontend that lowers to [libmtlc](libmtlc/) (native IR,
optimizer, x86-64 PE). Written in Haskell; reaches libmtlc over its C API.

```bat
build.bat
bin\c99mtlc.exe tests\fib.c -o bin\fib.exe
powershell -File tests\run_suite.ps1
```

Flags: `-o`, `-I`, `-E`, `-c`, `-O0`/`-O`, `--emit-ir`, `--static-prefix=`.
`include/` is on the search path by default.

Diagnostics: `-Wno-<group>`, `-Werror`, `-w`, `--max-errors=N`,
`--error-format=json`, `--color=auto|always|never`, `--explain <CODE>`,
`--help-warnings`. See [docs/diagnostics.md](docs/diagnostics.md).

```
error[E0102]: undeclared identifier 'coutner'
  --> hello.c:6:12
  |
5 | int main(void) {
6 |     return coutner + 1;
  |            ^^^^^^^ not found in this scope
7 | }
   = help: did you mean 'counter'?
```

Building needs GHC (via [GHCup](https://www.haskell.org/ghcup/)). Every
dependency is a GHC boot library, so `build.bat` uses `ghc --make` and needs no
package index; `c99mtlc.cabal` is there for `cabal build` if you prefer it.

## Layout

| | |
|---|---|
| `src/C99/Common.hs` | source locations and the diagnostic type |
| `src/C99/Diag.hs` | rendering: snippets, carets, colour, JSON, "did you mean" |
| `src/C99/Explain.hs` | the error-code table behind `--explain` |
| `src/C99/Preprocess.hs` | `#include`, macros, `#if`, and the `# n "file"` line markers the lexer resyncs on |
| `src/C99/Lexer.hs` | tokens |
| `src/C99/Parser.hs` | recursive descent; carries the typedef table the C grammar needs to stay unambiguous |
| `src/C99/CType.hs` | the type system, layout, and the usual arithmetic conversions |
| `src/C99/Ast.hs` | the AST, and the symbol table's shape |
| `src/C99/Sema.hs` | scopes, type checking, `__int128` rewriting |
| `src/C99/Lower.hs` | libmtlc IR generation |
| `src/C99/StaticRename.hs` | file-scope `static` mangling, so merged translation units don't collide |
| `src/Mtlc.hs`, `src/Mtlc/FFI.hs` | the libmtlc binding |
| `cbits/blob.c` | the one `MtlcType` that libmtlc's `build.h` has no constructor for |
| `app/Main.hs` | the driver |

The AST is three ADTs — `Expr`, `Stmt`, `Decl` — and the passes rebuild the
tree rather than mutating it: sema returns a program in which every expression
has a type and every name resolves to a `SymId`. Symbols are referenced by id,
not by value, because sema keeps mutating them after the reference is made —
`&x` marks `x` address-taken long after `x`'s declaration was walked.

## Correctness notes

A few C99 corners this frontend gets right that a quick implementation tends to
miss:

- **Block-scope statics have static duration.** `static int n;` inside a
  function is a module-level global with an internal, per-symbol link name — not
  stack storage — so it persists across calls. `include/stdlib.h`'s
  `_get_pgmptr` relies on this.
- **A definition outranks its `extern` re-declarations.** An object declared
  `extern` in a header and defined in one unit keeps the definition's linkage
  and type, even when the header wrote `extern int t[];` with no bound.
- **Cast precedence** follows C99 6.5.4: `(T)a < b` is `((T)a) < b`.
- **Declarator nesting**: `int a[3][4]` is array[3] of array[4] of int;
  `int (*fp)(int)` is a pointer-to-function.

Known limitation: `int (*fp_arr[3])(void)` — an array of function pointers —
parses as a plain function type. Plain function pointers, arrays of pointers,
and pointers to arrays are all correct.

One caveat: a struct member whose array bound is a `sizeof` expression the
parse-time folder cannot evaluate (e.g. `int mix[sizeof(K)/sizeof(K[0])];`)
stays incomplete and is laid out at offset 0, overlapping earlier members.
Fixing it properly means retaining member bound expressions and folding them in
sema once object types are known.
