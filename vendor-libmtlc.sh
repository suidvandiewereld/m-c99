#!/usr/bin/env bash
#
# Regenerate the vendored libmtlc backend (headers + static archive) from a
# MettleToolchain checkout, as a single atomic step.
#
# WHY THIS EXISTS
# ---------------
# libmtlc is vendored into this repo as a prebuilt archive (libmtlc/lib/mtlc.lib)
# plus its public headers (libmtlc/include/mtlc/*.h). The frontend's own C shims
# (cbits/blob.c) are compiled against those headers and then linked against the
# archive, so the two MUST come from the same MettleToolchain commit. If the
# headers lag the archive, MtlcType's field layout skews between the shim and the
# archive and codegen dereferences garbage -- the m-c99 #15 crash, where a field
# (address_space) added to type.h in one component but not the other shifted
# base_type by 8 bytes. Always regenerate both here, never one alone.
#
# USAGE
#   ./vendor-libmtlc.sh [path-to-MettleToolchain]
# Defaults to ../MettleToolchain. Needs gcc + ar on PATH (MSYS2/mingw on Windows).
# After running, rebuild the frontend so cbits recompiles against the new headers:
#   ghc will not relink on a .lib change alone -- delete the exe first:
#     rm -f bin/c99mtlc.exe && cmd.exe /c build.bat   (or: ./build.bat via a shell)

set -eu

FRONTEND_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="${1:-$FRONTEND_DIR/../MettleToolchain}"

if [ ! -d "$SRC_DIR/src" ] || [ ! -d "$SRC_DIR/include/mtlc" ]; then
  echo "error: '$SRC_DIR' is not a MettleToolchain checkout (no src/ or include/mtlc/)" >&2
  echo "usage: $0 [path-to-MettleToolchain]" >&2
  exit 1
fi
SRC_DIR="$(cd "$SRC_DIR" && pwd)"

echo "libmtlc source : $SRC_DIR"
echo "vendor target  : $FRONTEND_DIR/libmtlc"
COMMIT="$(cd "$SRC_DIR" && git rev-parse --short HEAD 2>/dev/null || echo '(not a git checkout)')"
echo "source commit  : $COMMIT"

# --- Build the archive -----------------------------------------------------
# Mirrors MettleToolchain's Makefile source list and flags. libmtlc excludes the
# reference frontend's own lowering TUs (this frontend does its own lowering).
# -O2, no -g: the vendored archive ships lean; add -g locally if you need to
# symbolicate a backend crash.
SRC=src
OUT="$(mktemp -d)"
trap 'rm -rf "$OUT"' EXIT
CFLAGS="-Wall -Wextra -std=c99 -O2 -D_GNU_SOURCE -I$SRC_DIR/src -I$SRC_DIR/include -fno-omit-frame-pointer -pthread"

LOWERING="ir/ir_lowering.c ir/ir_lower_address.c ir/ir_lower_defer.c \
ir/ir_lower_expr.c ir/ir_lower_stmt.c ir/ir_lower_support.c \
ir/ir_lower_switch_match.c ir/ir_lower_types.c"

cd "$SRC_DIR"
srcs=""
for f in $SRC/ir/*.c; do
  base=${f#$SRC/}; skip=0
  for l in $LOWERING; do [ "$base" = "$l" ] && skip=1; done
  [ $skip -eq 0 ] && srcs="$srcs $f"
done
srcs="$srcs $SRC/common.c $(ls $SRC/ir/optimizer/*.c)"
srcs="$srcs $SRC/codegen/binary_emitter.c $SRC/codegen/code_generator.c"
srcs="$srcs $SRC/codegen/elf_emitter.c $SRC/codegen/ptx_emitter.c $SRC/codegen/spirv_emitter.c"
srcs="$srcs $(ls $SRC/codegen/binary/*.c) $(ls $SRC/linker/*.c)"
srcs="$srcs $SRC/debug/debug_info.c $SRC/error/error_reporter.c"
srcs="$srcs $SRC/compiler/compiler_context.c $SRC/compiler/compiler_crash.c"
srcs="$srcs $SRC/mtlc_api.c $SRC/mtlc_build.c $SRC/mtlc_lib_fallbacks.c"

echo "compiling $(echo $srcs | wc -w) translation units ..."
for f in $srcs; do
  o="$OUT/$(echo "${f#$SRC/}" | tr '/' '_' | sed 's/\.c$/.o/')"
  gcc $CFLAGS -c "$f" -o "$o"
done

ARCHIVE="$OUT/mtlc.lib"
ar rcs "$ARCHIVE" "$OUT"/*.o

# --- Install headers + archive together ------------------------------------
DEST_INC="$FRONTEND_DIR/libmtlc/include/mtlc"
DEST_LIB="$FRONTEND_DIR/libmtlc/lib/mtlc.lib"
mkdir -p "$DEST_INC" "$(dirname "$DEST_LIB")"

# Headers first, then the archive, so an interrupted run never leaves an archive
# newer than the headers it must match.
cp "$SRC_DIR"/include/mtlc/*.h "$DEST_INC/"
cp "$ARCHIVE" "$DEST_LIB"

echo "installed headers -> libmtlc/include/mtlc/  ($(ls "$SRC_DIR"/include/mtlc/*.h | wc -l) files)"
echo "installed archive -> libmtlc/lib/mtlc.lib   ($(stat -c %s "$DEST_LIB") bytes, from $COMMIT)"
echo
echo "done. Now rebuild the frontend so cbits recompiles against the new headers:"
echo "    rm -f bin/c99mtlc.exe && cmd.exe /c build.bat"
