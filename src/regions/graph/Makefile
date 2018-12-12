OCAMLMAKEFILE = ../../OCamlMakefile
COMMON = ../../common/
LEGACY_COMMON = ../../legacy/trunk_common/
GEOREAL = $(LEGACY_COMMON)/geometric_realization/
SOURCES = ../bound.ml ../nonEmptyInterval.ml ../halfLineRegion.ml graphRegion.ml \
$(COMMON)integer.ml ../halfLineRegionOverInteger.ml \
test.ml
RESULT = main
ANNOTATE = true
DOC_FILES = graphRegion.ml
PACKS = str ocamlgraph unix

all: dnc

cleaned:
	make clean
	rm *.d

include $(OCAMLMAKEFILE)

#$(LEGACY_COMMON)interval.mli $(LEGACY_COMMON)interval.ml \
#$(LEGACY_COMMON)cube.ml $(LEGACY_COMMON)cube.mli \
#$(LEGACY_COMMON)algebra.mli $(LEGACY_COMMON)algebra.ml \
#$(LEGACY_COMMON)areaOverCube.ml $(LEGACY_COMMON)areaOverCube.mli \
