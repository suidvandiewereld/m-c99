# Drive the real c99mtlc entry point on regression + gap tests.
# Usage: from repo root after build: powershell -File tests/run_suite.ps1 [scratch_dir]
$ErrorActionPreference = "Continue"
$Root = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
if (-not $Root) { $Root = Get-Location }
Set-Location $Root
$Scratch = $args[0]
if (-not $Scratch) {
  $Scratch = Join-Path $env:TEMP "c99mtlc-suite"
}
# Which compiler to exercise. Defaults to the freshly built bin\c99mtlc.exe;
# set C99MTLC to point at another build.
$CC = $env:C99MTLC
if (-not $CC) { $CC = Join-Path $Root "bin\c99mtlc.exe" }
if (-not (Test-Path $CC)) { Write-Error "missing $CC"; exit 1 }
Write-Host "Using compiler: $CC"

function Run-Case($Name, $ArgsList, $ExpectExit, $ExpectOutContains) {
  $dir = Join-Path $Scratch $Name
  New-Item -ItemType Directory -Force -Path $dir | Out-Null
  $exe = Join-Path $dir "out.exe"
  $log = Join-Path $dir "compile.log"
  $runlog = Join-Path $dir "run.log"
  $allArgs = @() + $ArgsList + @("-o", $exe)
  & $CC @allArgs 2>&1 | Tee-Object -FilePath $log
  if ($LASTEXITCODE -ne 0) {
    Write-Host "FAIL $Name compile exit $LASTEXITCODE"
    return $false
  }
  $out = & $exe 2>&1 | Out-String
  $code = $LASTEXITCODE
  $out | Set-Content -Path $runlog -NoNewline
  "exit=$code" | Add-Content $runlog
  if ($null -ne $ExpectExit -and $code -ne $ExpectExit) {
    Write-Host "FAIL $Name exit got $code want $ExpectExit"
    return $false
  }
  if ($ExpectOutContains -and ($out -notlike "*$ExpectOutContains*")) {
    Write-Host "FAIL $Name output missing '$ExpectOutContains'"
    return $false
  }
  Write-Host "PASS $Name"
  return $true
}

# Diagnostics are asserted with regexes rather than golden files, so a case
# pins the load-bearing content (the code, the caret run, the suggestion)
# without breaking every time a sentence is reworded.
#
# MustNotMatch is what actually pins cascade suppression: it is the only way to
# say "and nothing else was reported".
function Run-Diag($Name, $ArgsList, $ShouldCompile, $MustMatch, $MustNotMatch) {
  $dir = Join-Path $Scratch $Name
  New-Item -ItemType Directory -Force -Path $dir | Out-Null
  $exe = Join-Path $dir "out.exe"
  $allArgs = @() + $ArgsList + @("--color=never", "-o", $exe)
  # Width so console wrapping cannot break a multi-word match.
  $out = (& $CC @allArgs 2>&1 | Out-String -Width 4096)
  $code = $LASTEXITCODE
  $out | Set-Content -Path (Join-Path $dir "diag.log") -NoNewline
  if ($null -ne $ShouldCompile) {
    if ($ShouldCompile -and $code -ne 0) {
      Write-Host "FAIL $Name expected success, exit $code"; return $false
    }
    if ((-not $ShouldCompile) -and $code -eq 0) {
      Write-Host "FAIL $Name expected failure, exit 0"; return $false
    }
  }
  foreach ($p in $MustMatch) {
    if ($out -notmatch $p) { Write-Host "FAIL $Name missing /$p/"; return $false }
  }
  foreach ($p in $MustNotMatch) {
    if ($out -match $p) { Write-Host "FAIL $Name unexpected /$p/"; return $false }
  }
  Write-Host "PASS $Name"
  return $true
}

$fail = 0
function Need($ok) { if (-not $ok) { $script:fail++ } }

Need (Run-Case "regression/fib" @("tests\fib.c") 55 $null)
Need (Run-Case "regression/hello" @("tests\hello.c") 0 "Hi")
Need (Run-Case "regression/arith" @("tests\arith.c") 0 $null)
Need (Run-Case "regression/loop" @("tests\loop.c") 55 $null)

Need (Run-Case "gaps/pp_include" @("-I","tests\include","tests\pp_include.c") 42 $null)
Need (Run-Case "gaps/pp_macros" @("tests\pp_macros.c") 22 $null)
Need (Run-Case "gaps/pp_cond" @("tests\pp_cond.c") 42 $null)
Need (Run-Case "gaps/designated" @("tests\designated.c") 24 $null)
Need (Run-Case "gaps/switch_ft" @("tests\switch_ft.c") 11 $null)
Need (Run-Case "gaps/fptr" @("tests\fptr.c") 42 $null)
Need (Run-Case "gaps/variadic" @("tests\variadic.c") 0 $null)
Need (Run-Case "gaps/stack_array" @("tests\stack_array.c") 15 $null)
Need (Run-Case "gaps/ptr_arith" @("tests\ptr_arith.c") 42 $null)
Need (Run-Case "gaps/string_lit" @("tests\string_lit.c") 0 $null)
Need (Run-Case "gaps/bitfield" @("tests\bitfield.c") 22 $null)
Need (Run-Case "gaps/complex" @("tests\complex.c") 0 $null)
Need (Run-Case "gaps/vla" @("tests\vla.c") 15 $null)
Need (Run-Case "multifile" @("tests\multifile_a.c","tests\multifile_b.c") 42 $null)
Need (Run-Case "gaps/struct_assign" @("tests\struct_assign.c") 0 $null)
Need (Run-Case "gaps/struct_return" @("tests\struct_return.c") 7 $null)
Need (Run-Case "gaps/partial_zero" @("tests\partial_zero.c") 0 $null)
Need (Run-Case "gaps/global_agg" @("tests\global_agg.c") 7 $null)
Need (Run-Case "multifile/static_link" @("tests\static_a.c","tests\static_b.c") 0 $null)
Need (Run-Case "gaps/static_shadow" @("tests\static_shadow.c") 0 $null)
Need (Run-Case "gaps/storage_stack" @("tests\storage_stack.c") 0 $null)
Need (Run-Case "gaps/global_agg_init" @("tests\global_agg_init.c") 7 $null)
Need (Run-Case "gaps/global_array_init" @("tests\global_array_init.c") 6 $null)
Need (Run-Case "gaps/addr_taken_global_init" @("tests\addr_taken_global_init.c") 0 $null)

# ---- diagnostics quality ----

# The caret run is asserted literally, which pins the column, the length and
# the label in one pattern.
Need (Run-Diag "diag/undeclared" @("tests\diag\undeclared.c") $false `
  @("error\[E0102\]: undeclared identifier 'coutner'",
    "\^\^\^\^\^\^\^ not found in this scope",
    "= help: did you mean 'counter'\?",
    "--> .*undeclared\.c:6:12") $null)

# One diagnostic per real mistake, and no more. The file holds two unrelated
# problems: a missing ';' on line 14 and an undeclared name on line 21.
#
# The second one is the load-bearing half. Asserting only "one error" would
# also pass if the parser gave up at the first mistake and never read the rest
# of the file, which is the opposite of recovering. Requiring both proves it
# recovered; requiring exactly two proves it did not cascade.
Need (Run-Diag "diag/cascade" @("tests\diag\cascade.c") $false `
  @("--> .*cascade\.c:17:",
    "--> .*cascade\.c:21:",
    "due to 2 previous errors") `
  @("expected a declaration", "due to [3-9] previous"))

Need (Run-Diag "diag/unused" @("tests\diag\unused.c") $true `
  @("warning: unused variable 'scratch'",
    "\^\^\^\^\^\^\^ declared here and never read",
    "rename it to '_scratch'") `
  @("unused variable 'used'", "unused variable '_intentional'",
    "unused variable 'written'"))

# -Wno- silences it; -Werror promotes it and fails the build.
Need (Run-Diag "diag/unused_off" @("-Wno-unused", "tests\diag\unused.c") $true `
  $null @("unused variable"))
Need (Run-Diag "diag/unused_werror" @("-Werror", "tests\diag\unused.c") $false `
  @("unused variable 'scratch'", "due to 1 previous error") $null)

# A syntax error no longer hides the semantic errors behind it: both come out
# of one run. The must-not-match half pins the other side of the rule, that
# sema warnings are dropped when the parse failed.
Need (Run-Diag "diag/phases" @("tests\diag\phases.c") $false `
  @("error\[E0010\]: expected ;",
    "error\[E0102\]: undeclared identifier 'undeclared_thing'",
    "a syntax error there may have hidden it",
    "due to 2 previous errors") `
  @("unused variable", "due to [3-9] previous"))

# Flow warnings. The MustNotMatch list is the real test: every ok_* function in
# flow.c is a shape that must stay quiet, and they are the shapes that produced
# 118 false positives on the Mettle sources before switch and noreturn were
# handled.
Need (Run-Diag "diag/flow" @("tests\diag\flow.c") $true `
  @("warning: unreachable statement",
    "this will never run",
    "non-void function 'no_return_on_one_path' can end without returning",
    "control can reach here without returning a value") `
  @("ok_switch_all_return", "ok_if_else", "ok_forever", "ok_noreturn_tail",
    "ok_void", "ok_label_after_return", "'main'"))

Need (Run-Diag "diag/flow_off" @("-Wno-unreachable-code", "-Wno-missing-return", "tests\diag\flow.c") $true `
  $null @("unreachable statement", "can end without returning"))

Need (Run-Diag "diag/redefinition" @("tests\diag\redefinition.c") $false `
  @("error\[E0100\]: redefinition of 'x'",
    "note: previous declaration of 'x' is here",
    "--> .*redefinition\.c:2:1") $null)

# Lexing must continue past a stray character, so the later error still shows.
Need (Run-Diag "diag/stray_char" @("tests\diag\stray_char.c") $false `
  @("error\[E0001\]: unexpected character '@'",
    "undeclared_after_stray") $null)

Need (Run-Diag "diag/json" @("--error-format=json", "tests\diag\undeclared.c") $false `
  @('"severity":"error"', '"code":"E0102"', '"line":6', '"column":12', '"length":7') $null)

# --explain works without an input file at all.
$exp = (& $CC --explain e0102 2>&1 | Out-String -Width 4096)
if ($exp -match "error\[E0102\]" -and $exp -match "not in scope") {
  Write-Host "PASS diag/explain"
} else {
  Write-Host "FAIL diag/explain"; $fail++
}

# -E check
$Edir = Join-Path $Scratch "preprocess"
New-Item -ItemType Directory -Force -Path $Edir | Out-Null
$Ei = Join-Path $Edir "out.i"
& $CC -E -I tests\include tests\pp_include.c 2>$null | Set-Content $Ei
$etext = Get-Content $Ei -Raw
if ($etext -match "DOUBLE" -and $etext -notmatch "add_one") {
  Write-Host "WARN -E may be incomplete"
}
if ($etext -match "add_one") {
  Write-Host "PASS preprocess -E"
} else {
  Write-Host "FAIL preprocess -E"
  $fail++
}

if ($fail -gt 0) {
  Write-Host "FAILED $fail case(s). Scratch: $Scratch"
  exit 1
}
Write-Host "ALL PASS. Scratch: $Scratch"
exit 0
