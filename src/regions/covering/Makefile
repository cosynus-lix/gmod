OCAMLMAKEFILE = $(SRC)OCamlMakefile
SRC = ../../
REGIONS = $(SRC)regions/
BLOCK = $(REGIONS)block/
GRAPH = $(REGIONS)graph/
ON_HALFLINE = $(REGIONS)on_halfline/
ON_GRAPH = $(REGIONS)on_graph/
SOURCES = \
$(REGIONS)bound.ml $(ON_HALFLINE)onHalfLine.ml \
$(GRAPH)graph.ml $(ON_GRAPH)onGraph.ml \
$(BLOCK)block.ml \
covering.ml
RESULT = main
ANNOTATE = true
DOC_FILES = covering.ml

all: dnc

cleaned:
	make clean
	rm -rf ../regions

include $(OCAMLMAKEFILE)
