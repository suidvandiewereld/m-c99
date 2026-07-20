# Diagnostics

What a c99mtlc diagnostic looks like, and how to control it.

## The shape

```
error[E0102]: undeclared identifier 'coutner'
  --> hello.c:6:12
  |
5 | int main(void) {
6 |     return coutner + 1;
  |            ^^^^^^^ not found in this scope
7 | }
   = help: did you mean 'counter'?

error: could not compile `hello.c` due to 1 previous error
```

Five parts, each optional except the first two:

| Part | What it is |
|---|---|
| `error[E0102]:` | severity, a stable code, and the sentence |
| `--> file:line:col` | where, in a form editors can jump to |
| the frame | one line of context either side, with the line underlined |
| `^^^^ label` | what is wrong with *that* span specifically |
| `= help:` | what to do about it |

A diagnostic that points at a second place in the source prints it as a note
with its own frame:

```
error[E0100]: redefinition of 'x'
  --> a.c:2:1
  |
1 | int x = 1;
2 | int x = 2;
  |     ^ redefined here
3 | int main(void) { return x; }
note: previous declaration of 'x' is here
  --> a.c:1:1
  |
1 | int x = 1;
  |     ^
```

The output is ASCII on purpose (`-->`, `|`, `^`, `=`). Box-drawing characters
turn into mojibake on a stock Windows console, and this compiler is built
there.

Everything goes to stderr, so it never pollutes a `-E` pipeline.

## Error codes

Codes are stable: once published, a number keeps its meaning. Ask what one
means:

```
c99mtlc --explain E0102
```

Case and brackets are ignored, so `--explain e0102` and `--explain [E0102]`
both work. `--explain list` prints the index.

## Warnings

Every group is on by default. Turn one off with `-Wno-<group>`, and see the
list with `--help-warnings`.

| Group | Fires on |
|---|---|
| `unused` | a block-scope variable nothing reads |
| `unreachable-code` | a statement control can never arrive at |
| `missing-return` | a value-returning function that can run off its end |

`-Werror` turns every warning that is still on into an error, and the build
fails. `-w` silences all of them.

A group exists only when the check behind it exists. A flag that silences
nothing is worse than a missing flag, because it tells you the compiler checks
something it does not.

## The false-positive budget is zero

This is the rule that decides what each warning does *not* say, and it is worth
stating plainly: a warning nobody trusts is worse than no warning, because it
trains people to pass `-w`.

`unused` therefore says nothing about a parameter (an unused one is often
required by a signature), nothing about anything `extern`, `static` or global
(another unit may read it), and nothing about a name starting with `_`, which
is how you say "declared on purpose, not used". Any resolution of the name
counts as a use, writes included, so `x = f();` keeps `x` quiet. That is what
gcc's `-Wunused-variable` does as well; set-but-never-read is a stricter,
separate check (gcc's `-Wunused-but-set-variable`) and would get its own group
if added.

`missing-return` asks whether control can fall off the end, and every case it
is unsure about answers "no". It understands that a `switch` covering every
path with a `default` cannot fall out, that `for (;;)` with no `break` never
finishes, that both arms of an `if`/`else` returning is enough, and that a call
to `exit`, `abort` or `ExitProcess` does not come back. `main` is exempt, since
C99 6.5.2.2p5 defines falling off the end of `main` as `return 0`.

That list is not academic. Before the `switch` and noreturn cases were handled,
`missing-return` produced **118 warnings on the 112-file Mettle codebase, every
one of them wrong**. It now produces none there, and the shapes that caused
them are the `ok_*` functions in `tests/diag/flow.c`.

`unreachable-code` reports only the *first* dead statement in a run, because
listing every one turns a single mistake into a wall of output, and it stops at
a label, since a `goto` can land there.

The suggested fix names the exact rename:

```
warning: unused variable 'scratch'
  --> a.c:3:5
  |
2 |     int used = 1;
3 |     int scratch = 2;
  |         ^^^^^^^ declared here and never read
4 |     return used;
   = help: remove it, or rename it to '_scratch' to keep it and say so on purpose
```

## Colour

`--color=auto` (the default) uses colour when stderr is a terminal. The
environment overrides it in both directions, in the order everyone else
settled on:

1. `CLICOLOR_FORCE` set and not `0`: colour, even when piped
2. `NO_COLOR` set: no colour
3. `TERM=dumb`: no colour
4. otherwise: colour when stderr is a terminal

`--color=always` and `--color=never` skip all of that.

## Machine-readable output

`--error-format=json` writes one JSON object per line:

```json
{"severity":"error","file":"hello.c","line":6,"column":12,"length":7,
 "message":"undeclared identifier 'coutner'","code":"E0102",
 "label":"not found in this scope","help":"did you mean 'counter'?"}
```

`length` is the caret run, so an editor can underline the same span. Notes
appear as a nested `notes` array.

## How many errors you get

One mistake should report once. Three things enforce that:

- **The parser recovers.** After an error it skips to the next `;`, `}` or
  keyword that can start a construct, so a missing semicolon does not produce
  an error for every token that follows it.
- **The lexer keeps going.** A stray character is skipped and reported;
  it does not end the token stream, which used to discard the rest of the
  file silently.
- **Duplicates are dropped** and diagnostics are sorted by position before
  printing, so the order matches the file rather than the pass that ran.

Every input file is parsed even when an earlier one failed, and the type
checker runs on whatever parsed, so a missing semicolon in one function and a
type error in another come out of the same run rather than one build apiece.

A declaration the parser could not read is one the checker never saw, so a
later use of that name reports as undeclared even though you wrote the
declaration. Rather than suppress those (which would throw away the ordinary
case, one syntax error and one unrelated type error, to tidy up the
pathological one), every undeclared-identifier error in a run with parse
errors says so on its own help line:

```
   = help: if this is declared above, a syntax error there may have hidden it; fix those first
```

So the output reads correctly without knowing the rule: the syntax errors
print first, and any undeclared error that might be their fallout announces
itself.

Sema *warnings* are dropped when the parse failed, for the same reason in
reverse: a warning is advice about code you mean to keep, and on a file that
does not parse it is as likely to be an artefact of the lost tree as a real
finding. Nobody acts on warnings in a build that already failed.

Printing stops after 100 errors and says so. `--max-errors=N` changes the
limit; `--max-errors=0` removes it.

## Adding a diagnostic

`C99.Common` holds the message type. Only a severity, a location and a
sentence are required; everything else is a combinator on top:

```haskell
emit
  . withHelp "did you mean 'counter'?"
  . withLabel "not found in this scope"
  . withCode "E0102"
  . withSnap name          -- move the caret onto this identifier
  . withLen (length name)
  $ diag Error loc ("undeclared identifier '" ++ name ++ "'")
```

`withSnap` exists because a declaration's location is the start of its *type*:
`int scratch` reported at the declaration would otherwise underline `int scr`.
The renderer looks for the name on the line as a whole word and moves the
caret there, falling back to the raw span when it cannot find it.

Add the code to `C99.Explain` at the same time. An entry is meaning, then an
example, then the fix, because the reader is stuck and wants the third part.

## Testing a diagnostic

`tests/run_suite.ps1` asserts with regexes, not golden files, so a case pins
the load-bearing content without breaking when a sentence is reworded:

```powershell
Need (Run-Diag "diag/undeclared" @("tests\diag\undeclared.c") $false `
  @("error\[E0102\]: undeclared identifier 'coutner'",
    "\^\^\^\^\^\^\^ not found in this scope",
    "= help: did you mean 'counter'\?") $null)
```

Asserting the caret run literally pins the column, the length and the label in
one pattern.

The last argument is what must *not* appear, and it is where the real work
happens. Two examples.

Testing that one mistake reports once needs a second, unrelated mistake in the
same file. Asserting only "one error" would pass just as well if the parser
gave up at the first one and never read the rest, which is the opposite of
recovering:

```powershell
Need (Run-Diag "diag/cascade" @("tests\diag\cascade.c") $false `
  @("--> .*cascade\.c:17:", "--> .*cascade\.c:21:", "due to 2 previous errors") `
  @("expected a declaration", "due to [3-9] previous"))
```

Testing a warning needs the shapes that must stay *quiet* more than the one
that fires. Every `ok_*` function in `tests/diag/flow.c` is a shape that once
warned falsely:

```powershell
Need (Run-Diag "diag/flow" @("tests\diag\flow.c") $true `
  @("warning: unreachable statement", "control can reach here without returning a value") `
  @("ok_switch_all_return", "ok_if_else", "ok_forever", "ok_noreturn_tail",
    "ok_void", "ok_label_after_return", "'main'"))
```
