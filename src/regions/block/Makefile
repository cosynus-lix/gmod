SRC = ../../
REGIONS = $(SRC)regions/
HALFLINE = $(REGIONS)on_halfline/
GRAPH = $(REGIONS)graph/
ON_GRAPH = $(REGIONS)on_graph/
OCAMLMAKEFILE = $(SRC)OCamlMakefile
SOURCES = $(REGIONS)bound.ml $(HALFLINE)onHalfLine.ml \
$(GRAPH)graph.ml \
$(ON_GRAPH)onGraph.ml \
block.ml
RESULT = main
ANNOTATE = true
DOC_FILES = block.ml

all: dnc

cleaned:
	make clean
	rm -rf ../regions

include $(OCAMLMAKEFILE)
