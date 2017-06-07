SOURCES = dashed.ml
RESULT = dashed
INCDIRS =
LIBS =
ANNOTATE = true
USE_CAMLP4 =
DOC_FILES = $(SOURCES)
OCAMLDEP = ocamldep.opt
OCAMLOPT = ocamlopt.opt
OCAMLDOC = ocamldoc.opt
OCAMLDOCFLAGS = -sort

all: dncl

test: all
	$(OCAMLOPT) dashed.ml -o dashed

include OCamlMakefile
