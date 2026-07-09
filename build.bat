@echo off
setlocal
cd /d "%~dp0"

set CC=gcc
set CFLAGS=-std=c11 -Wall -Wextra -O2 -I src -I libmtlc\include
set SRC=src\common.c src\token.c src\lexer.c src\ast.c src\ctype.c src\parser.c src\sema.c src\lower.c src\preprocess.c src\main.c
set LIBS=libmtlc\lib\mtlc.lib -ldbghelp

if not exist bin mkdir bin

echo Building c99mtlc...
%CC% %CFLAGS% %SRC% %LIBS% -o bin\c99mtlc.exe
if errorlevel 1 (
  echo Build failed.
  exit /b 1
)
echo Built bin\c99mtlc.exe
endlocal
