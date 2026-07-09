CC ?= gcc
CFLAGS ?= -std=c11 -Wall -Wextra -O2
CPPFLAGS += -Isrc -Ilibmtlc/include
SRCS = src/common.c src/token.c src/lexer.c src/ast.c src/ctype.c \
       src/parser.c src/sema.c src/lower.c src/preprocess.c src/main.c
OBJS = $(SRCS:src/%.c=build/%.o)

ifeq ($(OS),Windows_NT)
  EXE = bin/c99mtlc.exe
  LDLIBS = libmtlc/lib/mtlc.lib -ldbghelp
else
  EXE = bin/c99mtlc
  LDLIBS = libmtlc/lib/mtlc.lib
  # Prefer .a if present
  ifneq ($(wildcard libmtlc/lib/libmtlc.a),)
    LDLIBS = libmtlc/lib/libmtlc.a
  endif
endif

.PHONY: all clean test

all: $(EXE)

bin:
	mkdir -p bin

build:
	mkdir -p build

$(EXE): $(OBJS) | bin
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDLIBS)

build/%.o: src/%.c | build
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

clean:
	rm -rf build bin/c99mtlc bin/c99mtlc.exe

test: $(EXE)
	./$(EXE) -o bin/test_fib.exe tests/fib.c && bin/test_fib.exe
	./$(EXE) -o bin/test_hello.exe tests/hello.c && bin/test_hello.exe
