SOURCES = glyph.ml common.ml globals.ml sig.mli algebra.ml \
interval.ml arc.ml mix.ml \
cube.ml torus.ml cylinder.ml \
area.mli \
areaOverCube.ml areaOverTorus.ml areaOverCylinder.ml \
halfLine.ml

RESULT = 
INCDIRS = -I ~/.opam/4.04.1/lib/ocamlgraph/
LIBS = 
ANNOTATE = true
USE_CAMLP4 =
DOC_FILES = $(SOURCES)
OCAMLDEP = ocamldep.opt
OCAMLOPT = ocamlopt.opt
OCAMLDOC = ocamldoc.opt
OCAMLDOCFLAGS = -sort

all: dncl

include OCamlMakefile
