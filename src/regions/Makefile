OCAMLMAKEFILE = ../OCamlMakefile
COMMON = ../common/
LEGACY_COMMON = ../legacy/trunk_common/
GEOREAL = $(LEGACY_COMMON)/geometric_realization/
SOURCES = bound.ml dashDot.ml \
$(COMMON)integer.ml $(LEGACY_COMMON)common.ml  \
interval2.ml dashDot2.ml \
$(GEOREAL)ODA.ml \
test.ml test2.ml
RESULT = main
ANNOTATE = true
DOC_FILES = bound.ml dashDot.ml
PACKS = str ocamlgraph unix

all: dnc

include $(OCAMLMAKEFILE)

#$(LEGACY_COMMON)interval.mli $(LEGACY_COMMON)interval.ml \
#$(LEGACY_COMMON)cube.ml $(LEGACY_COMMON)cube.mli \
#$(LEGACY_COMMON)algebra.mli $(LEGACY_COMMON)algebra.ml \
#$(LEGACY_COMMON)areaOverCube.ml $(LEGACY_COMMON)areaOverCube.mli \
