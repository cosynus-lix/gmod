OCAMLMAKEFILE = $(SRC)OCamlMakefile
SRC = ../../
COMMON = $(SRC)common/
LEGACY_COMMON = $(SRC)legacy/trunk_common/
SOURCES = graph.ml graphOverInteger.ml
RESULT = main
ANNOTATE = true
DOC_FILES = graph.ml
PACKS = str

all: dnc

cleaned:
	make clean

include $(OCAMLMAKEFILE)

#$(LEGACY_COMMON)interval.mli $(LEGACY_COMMON)interval.ml \
#$(LEGACY_COMMON)cube.ml $(LEGACY_COMMON)cube.mli \
#$(LEGACY_COMMON)algebra.mli $(LEGACY_COMMON)algebra.ml \
#$(LEGACY_COMMON)areaOverCube.ml $(LEGACY_COMMON)areaOverCube.mli \
