SOURCES = mystdlib.ml lang.ml parser.mly lexer.mll interpreter.ml simple.ml gmod.ml
RESULT = gmod
ANNOTATE = true
DOC_FILES = stdlib.ml lang.ml simple.ml

all: dnc
	./gmod test.c

include OCamlMakefile
