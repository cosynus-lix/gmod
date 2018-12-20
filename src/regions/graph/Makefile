SRC = ../../
REGIONS = $(SRC)regions/
HALFLINE = $(REGIONS)/halfline/
OCAMLMAKEFILE = $(SRC)OCamlMakefile
COMMON = $(SRC)common/
LEGACY_COMMON = $(SRC)legacy/trunk_common/
GEOREAL = $(LEGACY_COMMON)/geometric_realization/
SOURCES = $(REGIONS)bound.ml $(HALFLINE)nonEmptyInterval.ml $(HALFLINE)halfLineRegion.ml \
graphRegion.ml \
$(COMMON)integer.ml $(HALFLINE)halfLineRegionOverInteger.ml \
graphRegionOverInteger.ml \
parser.mly lexer.mll \
test.ml
RESULT = main
ANNOTATE = true
DOC_FILES = graphRegion.ml
PACKS = str unix

all: dnc

cleaned:
	make clean
	rm -rf ../common/ ../regions

include $(OCAMLMAKEFILE)

#$(LEGACY_COMMON)interval.mli $(LEGACY_COMMON)interval.ml \
#$(LEGACY_COMMON)cube.ml $(LEGACY_COMMON)cube.mli \
#$(LEGACY_COMMON)algebra.mli $(LEGACY_COMMON)algebra.ml \
#$(LEGACY_COMMON)areaOverCube.ml $(LEGACY_COMMON)areaOverCube.mli \
