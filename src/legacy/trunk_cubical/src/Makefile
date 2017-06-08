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

ifneq "no" "no"
NUMERIX = geometric_model/half_line_with_numerix.ml geometric_model/algebra_with_numerix.ml
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

GEOMETRIC_MODEL = geometric_model/sig.mli geometric_model/algebra.mli geometric_model/half_line.mli geometric_model/interval.mli geometric_model/arc.mli geometric_model/mix.mli geometric_model/Cube.mli geometric_model/Torus.mli geometric_model/Cylinder.mli geometric_model/Area.mli geometric_model/AreaOverCube.mli geometric_model/AreaOverTorus.mli geometric_model/AreaOverCylinder.mli geometric_model/covering.mli geometric_model/half_line.ml geometric_model/interval.ml geometric_model/arc.ml geometric_model/mix.ml geometric_model/algebra.ml geometric_model/Cube.ml geometric_model/Torus.ml geometric_model/Cylinder.ml geometric_model/AreaOverCube.ml geometric_model/AreaOverTorus.ml geometric_model/AreaOverCylinder.ml geometric_model/covering.ml geometric_model/concur2005.ml \

FRONTEND = frontend/AST.ml frontend/pv/parser_pv.mly frontend/pv/lexer_pv.mll \
	frontend/geometric_model/parser_cubical_area_over_integer.mly frontend/geometric_model/lexer_cubical_area_over_integer.mll frontend/geometric_model/parser_torical_area_over_integer.mly frontend/geometric_model/lexer_torical_area_over_integer.mll frontend/geometric_model/parser_cylindrical_area_over_integer.mly frontend/geometric_model/lexer_cylindrical_area_over_integer.mll

SOURCES = common.ml settings.mli settings.ml glyph.mli glyph.ml  \
	$(GEOMETRIC_MODEL) \
	interpreter.mli forbidden.mli flag.ml color.ml globals.ml \
	parser_config.mly lexer_config.mll option_user_manual.ml \
	semantics.ml interpreter.ml buzy_section.ml buzy_matrix.ml forbidden.ml geometric_model.ml type.ml \
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
