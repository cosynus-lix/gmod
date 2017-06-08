CFLAGS = -g -O2
LDFLAGS = 
INCDIRS =  /home/emmanuel/.opam/4.04.1/lib/lablgtk2
OCAMLLDFLAGS = 

OCAMLMAKEFILE = OCamlMakefile

THREADS = yes
PACKS =  lablgtk2 ocamlgraph str
LIBS =  lablglade
CLIBS = 

ANNOTATE = yes
DOC_FILES = $(shell find . -name \*.mli)
OCAMLDOCFLAGS = -sort -stars
#OCAML_GLADECC_FLAGS = -embed
DOC_DIR = ../doc

COMMON = ../../trunk_common/

ifneq "no" "no"
NUMERIX = geometric_model/halfLineWithNumerix.ml geometric_model/algebraWithNumerix.ml
endif

ifneq "yes" "no"
LABLGTK = gui/treeview_simple.ml gui/gui2.glade gui/display_with_gtk2.ml
ifneq "no" "no"
LABLGTK += gui/gtkgl.ml
endif
endif

ifneq "no" "no"
APRON = abstract_interpretation.ml
endif

ifneq "no" "no"
CIL = frontend/c/cil_parser.ml
endif

TRACE = geometric_model/trace_space/boolean.mli geometric_model/trace_space/boolean.ml \
	geometric_model/trace_space/simplicial.ml geometric_model/trace_space/simploid.mli geometric_model/trace_space/simploid.ml \
	geometric_model/trace_space/hypergraph.mli geometric_model/trace_space/hypergraph.ml \
	geometric_model/trace_space/trace_space.mli geometric_model/trace_space/trace_space.ml

GEOMETRIC_MODEL = $(COMMON)sig.mli $(COMMON)algebra.mli $(COMMON)halfLine.mli \
$(COMMON)interval.mli $(COMMON)arc.mli $(COMMON)mix.mli $(COMMON)cube.mli \
$(COMMON)torus.mli $(COMMON)cylinder.mli $(COMMON)area.mli \
$(COMMON)areaOverCube.mli $(COMMON)areaOverTorus.mli \
$(COMMON)areaOverCylinder.mli geometric_model/covering.mli \
$(COMMON)halfLine.ml $(COMMON)interval.ml $(COMMON)arc.ml $(COMMON)mix.ml \
$(COMMON)algebra.ml $(COMMON)cube.ml $(COMMON)torus.ml $(COMMON)cylinder.ml \
$(COMMON)areaOverCube.ml $(COMMON)areaOverTorus.ml $(COMMON)areaOverCylinder.ml \
geometric_model/covering.ml geometric_model/concur2005.ml \

FRONTEND = frontend/AST.ml frontend/pv/parser_pv.mly frontend/pv/lexer_pv.mll \
	frontend/geometric_model/parser_cubical_area_over_integer.mly frontend/geometric_model/lexer_cubical_area_over_integer.mll frontend/geometric_model/parser_torical_area_over_integer.mly frontend/geometric_model/lexer_torical_area_over_integer.mll frontend/geometric_model/parser_cylindrical_area_over_integer.mly frontend/geometric_model/lexer_cylindrical_area_over_integer.mll

SOURCES = $(COMMON)common.ml settings.mli settings.ml $(COMMON)glyph.mli $(COMMON)glyph.ml  \
	$(GEOMETRIC_MODEL) \
	interpreter.mli forbidden.mli flag.ml color.ml globals.ml \
	parser_config.mly lexer_config.mll option_user_manual.ml \
	AbstractSyntaxSignature.mli AbstractSyntax.mli \
	AbstractSyntax.ml interpreter.ml buzy_section.ml buzy_matrix.ml forbidden.ml geometric_model.ml type.ml \
	$(FRONTEND) \
	path_extract.ml	calculator.ml $(TRACE) analyzer.ml $(CIL) \
	display.ml $(LABLGTK) $(NUMERIX) $(APRON) \
	command_line_options.ml main.ml

RESULT = oplate

all: dnc

static:
	$(MAKE) nc OCAMLOPT="ocamlopt -ccopt -static"

test-forbidden: all
	for i in $(wildcard pv/*.pv); do ./oplate -tc-off -d frb $$i; done

doc: htdoc

-include $(OCAMLMAKEFILE)

.PHONY: doc
