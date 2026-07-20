@echo off
setlocal
cd /d "%~dp0"

rem The c99mtlc frontend (Haskell). Built with `ghc --make` rather than cabal:
rem every dependency is a GHC boot library, so no package index is needed.
rem GHC comes from GHCup (https://www.haskell.org/ghcup/).

set GHC=ghc
where %GHC% >nul 2>nul
if errorlevel 1 set GHC=%USERPROFILE%\.ghcup\bin\ghc.exe
if not exist "%GHC%" if not "%GHC%"=="ghc" (
  echo GHC not found. Install it with ghcup, or put ghc on PATH.
  exit /b 1
)

if not exist bin mkdir bin
if not exist build\hs mkdir build\hs

rem -A64m: a bigger GC nursery. The frontend allocates fast, and the default
rem 4 MB arena spends far too long collecting. 64 MB measured fastest on the
rem Mettle sweep: 128 MB was large enough to start thrashing the L2/L3 cache,
rem so a smaller-but-still-generous nursery wins.
echo Building bin\c99mtlc.exe ...
%GHC% --make -O2 -Wall ^
  -rtsopts "-with-rtsopts=-A64m" ^
  -isrc -iapp ^
  -outputdir build\hs ^
  -optc-Ilibmtlc\include ^
  app\Main.hs cbits\blob.c ^
  libmtlc\lib\mtlc.lib -ldbghelp ^
  -o bin\c99mtlc.exe
if errorlevel 1 (
  echo Build failed.
  exit /b 1
)
echo Built bin\c99mtlc.exe
endlocal
