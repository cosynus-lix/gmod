SOURCES = mystdlib.ml lang.ml parser.mly lexer.mll interpreter.ml simple.ml gmod.ml
RESULT = gmod
ANNOTATE = true
DOC_FILES = stdlib.ml lang.ml simple.ml

all: build
	@dune exec ./gmod.exe test.c

build:
	@dune build
