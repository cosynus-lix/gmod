SRC = ../../
REGIONS = $(SRC)regions/
HALFLINE = $(REGIONS)/halfline/
GRAPH_REGIONS = $(REGIONS)graph/
OCAMLMAKEFILE = $(SRC)OCamlMakefile
SOURCES = $(REGIONS)bound.ml $(HALFLINE)halfLineRegion.ml \
$(GRAPH_REGIONS)graph.ml \
block.ml
RESULT = main
ANNOTATE = true
DOC_FILES = block.ml

all: dnc

cleaned:
	make clean
	rm -rf ../regions

include $(OCAMLMAKEFILE)
