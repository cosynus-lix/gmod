SRC = ../../
OCAMLMAKEFILE = $(SRC)OCamlMakefile
COMMON = $(SRC)common/
LEGACY_COMMON = $(SRC)legacy/trunk_common/
GEOREAL = $(LEGACY_COMMON)/geometric_realization/
SOURCES = $(SRC)/regions/bound.ml \
$(COMMON)integer.ml $(LEGACY_COMMON)common.ml  \
$(GEOREAL)ODA.ml \
nonEmptyInterval.ml halfLineRegion.ml \
halfLineRegionOverInteger.ml \
test.ml
RESULT = main
ANNOTATE = true
DOC_FILES = bound.ml halfLineRegion.ml
PACKS = str unix

all: dnc

cleaned:
	make clean
	rm -r ../common ../legacy ../regions

include $(OCAMLMAKEFILE)

#$(LEGACY_COMMON)interval.mli $(LEGACY_COMMON)interval.ml \
#$(LEGACY_COMMON)cube.ml $(LEGACY_COMMON)cube.mli \
#$(LEGACY_COMMON)algebra.mli $(LEGACY_COMMON)algebra.ml \
#$(LEGACY_COMMON)areaOverCube.ml $(LEGACY_COMMON)areaOverCube.mli \
