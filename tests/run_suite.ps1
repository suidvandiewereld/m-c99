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
$CC = Join-Path $Root "bin\c99mtlc.exe"
if (-not (Test-Path $CC)) { Write-Error "missing $CC"; exit 1 }

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

$fail = 0
function Need($ok) { if (-not $ok) { $script:fail++ } }

Need (Run-Case "regression/fib" @("tests\fib.c") 55 $null)
Need (Run-Case "regression/hello" @("tests\hello.c") 0 "Hi")
Need (Run-Case "regression/arith" @("tests\arith.c") 0 $null)
Need (Run-Case "regression/loop" @("tests\loop.c") 55 $null)

Need (Run-Case "gaps/pp_include" @("-I","tests\include","tests\pp_include.c") 42 $null)
Need (Run-Case "gaps/pp_macros" @("tests\pp_macros.c") 22 $null)
Need (Run-Case "gaps/designated" @("tests\designated.c") 24 $null)
Need (Run-Case "gaps/switch_ft" @("tests\switch_ft.c") 11 $null)
Need (Run-Case "gaps/fptr" @("tests\fptr.c") 42 $null)
Need (Run-Case "gaps/variadic" @("tests\variadic.c") 0 $null)
Need (Run-Case "gaps/stack_array" @("tests\stack_array.c") 15 $null)
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
