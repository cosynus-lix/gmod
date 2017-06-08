BASIS = ../
MAINSOURCE = ../../../../trunk_common/
PARSERS = 

GM = $(MAINSOURCE)geometric_model/

CFLAGS = -g -O2
LDFLAGS = 
OCAMLLDFLAGS = 
#$(MAINSOURCE)common.cmx \
#$(MAINSOURCE)globals.cmx half_line.cmx \
#$(MAINSOURCE)interval.cmx $(MAINSOURCE)cube.cmx $(MAINSOURCE)arc.cmx\
#$(MAINSOURCE)torus.cmx $(MAINSOURCE)algebra.cmx $(MAINSOURCE)areaOverCube.cmx $(MAINSOURCE)areaOverTorus.cmx \
#$(BASIS)ODA.cmx $(BASIS)DGROG.cmx $(BASIS)CPODGROG.cmx $(PARSERS)BuiltInWithoutParsers.cmx
INCDIRS = -I $(BASIS) -I $(MAINSOURCE) -I $(PARSERS) -I $(GM) -I ../ -I ../.generated_files/

OCAMLMAKEFILE = ../OCamlMakefile

THREADS =  yes
#PACKS = lablgtk2 lablgtk2.init lablgtk2.glade lablgtk2-gl.gtkgl ocamlgraph extlib str ulex
PACKS =  ocamlgraph str ulex
LIBS =  ulexing
#CLIBS = camlidl
CLIBS = 



ANNOTATE = yes
DOC_FILES = 
#DOC_FILES = half_line.mli interval.mli arc.mli oda.mli cube.mli tore.mli cyle.mli \
area.mli sig.mli
OCAMLDOCFLAGS = -sort -stars
DOC_DIR = ../doc


SOURCES = $(MAINSOURCE)/glyph.ml $(MAINSOURCE)common.ml $(BASIS)message.ml \
$(MAINSOURCE)sig.mli $(BASIS)AdditionalSig.mli $(MAINSOURCE)algebra.ml \
$(BASIS)ODA.ml $(BASIS)DGROG.ml \
$(BASIS)CPODGROG.ml $(MAINSOURCE) BuiltInWithoutParsers.ml Expression.ml \
parser_oda.mly lexer_oda.mll parser_oda_sheet.mly lexer_oda_sheet.mll \
sheet_solver_oda.ml

RESULT = oda
#RESULT = check

all: dnc

static:
	$(MAKE) nc OCAMLOPT="ocamlopt -ccopt -static"

doc: htdoc

include OCamlMakefile
#-include $(OCAMLMAKEFILE)

.PHONY: doc
