# Makefile for interpret

COMPILER = ocamlopt.opt	-g
PARSER_GENERATOR = ocamlyacc
LEXER_GENERATOR = ocamllex

SRC = ../
GEOREAL = $(SRC)geometric_realization/
EH = ../../../../people/ehaucourt/
SPEC = $(EH)speclist_maker/
FRONTEND = $(GEOREAL)frontend/
BACKEND = $(GEOREAL)backend/
BOOLALG = $(GEOREAL)boolean_algebra/

OPAM = ~/.opam/4.02.1/lib/
GEOMOD = $(SRC)geometric_model/
REQUIRED_CMXA = unix.cmxa str.cmxa graph.cmxa lablgtk.cmxa
OCAMLGRAPH = $(OPAM)ocamlgraph/
SYNTAX_PV = $(SRC)frontend/pv/
LABLGTK2 = $(OPAM)lablgtk2/
G = .generated_files/

REQUIRED_CMXA = unix.cmxa str.cmxa graph.cmxa lablgtk.cmxa

REQUIRED_CMX = $(G)algebra.cmx $(G)half_line.cmx \
	$(G)common.cmx $(G)SpeclistMaker.cmx $(G)glyph.cmx $(G)Message.cmx \
	$(G)parser_pv_flags.cmx \
	$(G)AbstractSyntax_without_parser.cmx $(G)parser_pv.cmx $(G)lexer_pv.cmx $(G)AbstractSyntax.cmx \
	$(G)interval.cmx $(G)arc.cmx $(G)Cube.cmx $(G)Torus.cmx \
	$(G)AreaOverCube.cmx $(G)AreaOverTorus.cmx \
	$(G)ODA.cmx $(G)DGROG.cmx \
	$(G)BooleanAlgebra.cmx \
	$(G)CPODGROG.cmx \
	$(G)BuiltInWithoutParsers.cmx \
	$(G)Expression.cmx \
	$(G)PV2CFG.cmx \
	$(G)ForbiddenExtension.cmx\
	$(G)Semantics.cmx \
	$(G)Interpreter.cmx \
	$(G)Main.cmx

OUTPUT = interpret

all: $(OUTPUT)

$(G):
	mkdir $@

$(G)sig.cmi: $(GEOMOD)sig.mli | $(G)
	$(COMPILER) -c -o $@ $<

$(G)AdditionalSig.cmi: $(GEOREAL)AdditionalSig.mli | $(G)
	$(COMPILER) -c -o $@ $<

$(G)AbstractSyntaxSignature.cmi: $(SRC)AbstractSyntaxSignature.mli $(G)common.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)AbstractSyntax_without_parser.cmi: $(SRC)AbstractSyntax_without_parser.mli $(G)common.cmx $(G)AbstractSyntaxSignature.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)AbstractSyntax_without_parser.cmx: $(SRC)AbstractSyntax_without_parser.ml $(G)AbstractSyntax_without_parser.cmi $(G)common.cmx $(G)AbstractSyntaxSignature.cmi $(G)glyph.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)AbstractSyntax.cmi: $(SRC)AbstractSyntax.mli $(G)common.cmx $(G)AbstractSyntaxSignature.cmi $(G)AbstractSyntax_without_parser.cmx $(G)parser_pv.cmx $(G)lexer_pv.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)AbstractSyntax.cmx: $(SRC)AbstractSyntax.ml $(G)AbstractSyntax.cmi $(G)common.cmx $(G)AbstractSyntaxSignature.cmi $(G)AbstractSyntax_without_parser.cmx $(G)glyph.cmx $(G)parser_pv.cmx $(G)lexer_pv.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)ODA.cmx: $(GEOREAL)ODA.ml $(G)sig.cmi $(G)AdditionalSig.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)glyph.cmi: $(SRC)glyph.mli | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)glyph.cmx: $(SRC)glyph.ml $(G)glyph.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)common.cmx: $(SRC)common.ml | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Message.cmx: $(GEOREAL)Message.ml $(G)common.cmx $(G)glyph.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)DGROG.cmx: $(GEOREAL)DGROG.ml $(G)ODA.cmx $(G)glyph.cmx $(G)common.cmx $(G)Message.cmx | $(G)
	$(COMPILER) -c -I $(G) -I $(OCAMLGRAPH) -o $@ $<

$(G)algebra.cmi: $(GEOMOD)algebra.mli $(G)sig.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)algebra.cmx: $(GEOMOD)algebra.ml $(G)sig.cmi $(G)algebra.cmi | $(G)
	$(COMPILER) -c -I $(G) -I $(OCAMLGRAPH) -o $@ $<

$(G)BooleanAlgebraSig.cmi: $(BOOLALG)BooleanAlgebraSig.mli | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)BooleanAlgebra.cmi: $(BOOLALG)BooleanAlgebra.mli $(G)BooleanAlgebraSig.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)BooleanAlgebra.cmx: $(BOOLALG)BooleanAlgebra.ml $(G)BooleanAlgebra.cmi $(G)algebra.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)CPODGROG.cmx: $(GEOREAL)CPODGROG.ml $(G)algebra.cmx $(G)Message.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)BuiltInWithoutParsers.cmx: $(FRONTEND)BuiltInWithoutParsers.ml $(G)ODA.cmx $(G)Message.cmx $(G)DGROG.cmx $(G)CPODGROG.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

#~ $(G)parser_oda.cmi: $(G)parser_oda.mli $(G)BuiltInWithoutParsers.cmx $(G)Expression.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)parser_oda.cmx: $(G)parser_oda.ml $(G)parser_oda.cmi | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)parser_oda.mli: $(FRONTEND)parser_oda.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_oda $<
#~
#~ $(G)parser_oda.ml: $(FRONTEND)parser_oda.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_oda $<
#~
#~ $(G)lexer_oda.ml: $(FRONTEND)lexer_oda.mll | $(G)
	#~ $(LEXER_GENERATOR) -o $@ $<
#~
#~ $(G)lexer_oda.cmx: $(G)lexer_oda.ml $(G)BuiltInWithoutParsers.cmx $(G)parser_oda.cmi | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)parser_dgrog.mli: $(FRONTEND)parser_dgrog.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_dgrog $<
#~
#~ $(G)parser_dgrog.ml: $(FRONTEND)parser_dgrog.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_dgrog $<

$(G)Expression.cmx: $(FRONTEND)Expression.ml $(G)BuiltInWithoutParsers.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

#~ $(G)parser_dgrog.cmi: $(G)parser_dgrog.mli $(G)Expression.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)parser_dgrog.cmx: $(G)parser_dgrog.ml $(G)parser_dgrog.cmi $(G)sheet_solver_oda.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)lexer_dgrog.ml: $(FRONTEND)lexer_dgrog.mll $(G)parser_oda.cmi | $(G)
	#~ $(LEXER_GENERATOR) -o $@ $<
#~
#~ $(G)lexer_dgrog.cmx: $(G)lexer_dgrog.ml $(G)parser_dgrog.cmi  $(G)lexer_oda.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)parser_cpodgrog_sheet.mli: $(FRONTEND)parser_cpodgrog_sheet.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_cpodgrog_sheet $<
#~
#~ $(G)parser_cpodgrog_sheet.ml: $(FRONTEND)parser_cpodgrog_sheet.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_cpodgrog_sheet $<
#~
#~ $(G)parser_cpodgrog.mli: $(FRONTEND)parser_cpodgrog.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_cpodgrog $<
#~
#~ $(G)parser_cpodgrog.ml: $(FRONTEND)parser_cpodgrog.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_cpodgrog $<
#~
#~ $(G)parser_cpodgrog.cmi: $(G)parser_cpodgrog.mli $(G)Expression.cmx $(G)sheet_solver_oda.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)parser_cpodgrog.cmx: $(G)parser_cpodgrog.ml $(G)parser_cpodgrog.cmi | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)lexer_cpodgrog.ml: $(FRONTEND)lexer_cpodgrog.mll | $(G)
	#~ $(LEXER_GENERATOR) -o $@ $<
#~
#~ $(G)lexer_cpodgrog.cmx: $(G)lexer_cpodgrog.ml $(G)BuiltInWithoutParsers.cmx $(G)parser_cpodgrog.cmx $(G)lexer_oda.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<

#~ $(G)BuiltInAuxiliary.cmx: $(FRONTEND)BuiltInAuxiliary.ml $(G)parser_dgrog.cmx $(G)lexer_dgrog.cmx $(G)parser_cpodgrog.cmx $(G)lexer_cpodgrog.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<

#~ $(G)parser_cpodgrog_sheet.cmi: $(G)parser_cpodgrog_sheet.mli $(G)BuiltInWithoutParsers.cmx $(G)Expression.cmx $(G)BuiltInAuxiliary.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)parser_cpodgrog_sheet.cmx: $(G)parser_cpodgrog_sheet.ml $(G)parser_cpodgrog_sheet.cmi | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)lexer_cpodgrog_sheet.ml: $(FRONTEND)lexer_cpodgrog_sheet.mll | $(G)
	#~ $(LEXER_GENERATOR) -o $@ $<
#~
#~ $(G)lexer_cpodgrog_sheet.cmx: $(G)lexer_cpodgrog_sheet.ml $(G)parser_cpodgrog_sheet.cmi | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)sheet_solver_cpodgrog.cmx: $(FRONTEND)sheet_solver_cpodgrog.ml $(G)BuiltInWithoutParsers.cmx $(G)parser_cpodgrog_sheet.cmi $(G)lexer_cpodgrog_sheet.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)parser_oda_sheet.mli: $(FRONTEND)parser_oda_sheet.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_oda_sheet $<
#~
#~ $(G)parser_oda_sheet.ml: $(FRONTEND)parser_oda_sheet.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_oda_sheet $<
#~
#~ $(G)parser_oda_sheet.cmi: $(G)parser_oda_sheet.mli $(G)BuiltInWithoutParsers.cmx $(G)Expression.cmx $(G)parser_oda.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)parser_oda_sheet.cmx: $(G)parser_oda_sheet.ml $(G)parser_oda_sheet.cmi $(G)parser_oda.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)lexer_oda_sheet.ml: $(FRONTEND)lexer_oda_sheet.mll | $(G)
	#~ $(LEXER_GENERATOR) -o $@ $<
#~
#~ $(G)lexer_oda_sheet.cmx: $(G)lexer_oda_sheet.ml $(G)parser_oda_sheet.cmi $(G)lexer_oda.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)sheet_solver_oda.cmx: $(FRONTEND)sheet_solver_oda.ml $(G)lexer_oda_sheet.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<

$(G)BuiltIn.cmx: BuiltIn.ml $(G)BuiltInWithoutParsers.cmx $(G)parser_dgrog.cmi $(G)lexer_dgrog.cmx $(G)parser_cpodgrog_sheet.cmx $(G)lexer_cpodgrog_sheet.cmx $(G)sheet_solver_cpodgrog.cmx $(G)parser_oda_sheet.cmi $(G)lexer_oda_sheet.cmx $(G)sheet_solver_oda.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)PV2CFG.cmx: $(BACKEND)PV2CFG.ml $(G)AbstractSyntax.cmx $(G)BuiltInWithoutParsers.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)parser_pv_flags.cmi: $(SYNTAX_PV)parser_pv_flags.mli | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)parser_pv_flags.cmx: $(SYNTAX_PV)parser_pv_flags.ml $(G)parser_pv_flags.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)parser_pv.mli: $(SYNTAX_PV)parser_pv.mly | $(G)
	$(PARSER_GENERATOR) -b$(G)parser_pv $<

$(G)parser_pv.ml: $(SYNTAX_PV)parser_pv.mly | $(G)
	$(PARSER_GENERATOR) -b$(G)parser_pv $<

$(G)parser_pv.cmi: $(G)parser_pv.mli $(G)AbstractSyntax_without_parser.cmx $(G)parser_pv_flags.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)parser_pv.cmx: $(G)parser_pv.ml $(G)parser_pv.cmi $(G)AbstractSyntax_without_parser.cmx $(G)parser_pv_flags.cmi $(G)parser_pv_flags.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)lexer_pv.ml: $(SYNTAX_PV)lexer_pv.mll $(G)parser_pv.cmx | $(G)
	$(LEXER_GENERATOR) -o $@ $<

$(G)lexer_pv.cmx: $(G)lexer_pv.ml $(G)parser_pv_flags.cmi $(G)parser_pv_flags.cmx  $(G)parser_pv.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)ForbiddenExtension.cmx: $(BACKEND)ForbiddenExtension.ml $(G)PV2CFG.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

#~ $(G)parser_dgrog_sheet.mli: $(FRONTEND)parser_dgrog_sheet.mly $(G)BuiltIn.cmx | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_dgrog_sheet $<
#~
#~ $(G)parser_dgrog_sheet.ml: $(FRONTEND)parser_dgrog_sheet.mly | $(G)
	#~ $(PARSER_GENERATOR) -b$(G)parser_dgrog_sheet $<
#~
#~ $(G)parser_dgrog_sheet.cmi: $(G)parser_dgrog_sheet.mli $(G)BuiltInWithoutParsers.cmx $(G)Expression.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)parser_dgrog_sheet.cmx: $(G)parser_dgrog_sheet.ml $(G)parser_dgrog_sheet.cmi | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)lexer_dgrog_sheet.ml: $(FRONTEND)lexer_dgrog_sheet.mll | $(G)
	#~ $(LEXER_GENERATOR) -o $@ $<
#~
#~ $(G)lexer_dgrog_sheet.cmx: $(G)lexer_dgrog_sheet.ml $(G)BuiltInWithoutParsers.cmx $(G)parser_dgrog_sheet.cmi | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<
#~
#~ $(G)sheet_solver_dgrog.cmx: $(FRONTEND)sheet_solver_dgrog.ml $(G)parser_dgrog_sheet.cmi $(G)lexer_dgrog_sheet.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<

$(G)Semantics.cmi: $(BACKEND)Semantics.mli $(G)AbstractSyntax.cmx $(G)PV2CFG.cmx $(G)ForbiddenExtension.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Semantics.cmx: $(BACKEND)Semantics.ml $(G)Semantics.cmi $(G)AbstractSyntax.cmx $(G)PV2CFG.cmx $(G)ForbiddenExtension.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

#~ $(G)Command.cmx: Command.ml $(G)AbstractSyntax.cmx $(G)AbstractSyntax.cmi $(G)parser_pv_flags.cmi $(G)parser_pv_flags.cmx $(G)ForbiddenExtension.cmx $(G)sheet_solver_dgrog.cmx $(G)Semantics.cmi $(G)Semantics.cmx | $(G)
	#~ $(COMPILER) -c -I $(G) -o $@ $<

$(G)half_line.cmi: $(GEOMOD)half_line.mli $(G)sig.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)half_line.cmx: $(GEOMOD)half_line.ml $(G)half_line.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)interval.cmi: $(GEOMOD)interval.mli $(G)sig.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)interval.cmx: $(GEOMOD)interval.ml $(G)interval.cmi $(G)sig.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)arc.cmi: $(GEOMOD)arc.mli $(G)sig.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)arc.cmx: $(GEOMOD)arc.ml $(G)arc.cmi $(G)sig.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Cube.cmi: $(GEOMOD)Cube.mli $(G)interval.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Cube.cmx: $(GEOMOD)Cube.ml $(G)Cube.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Torus.cmi: $(GEOMOD)Torus.mli $(G)arc.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Torus.cmx: $(GEOMOD)Torus.ml $(G)Torus.cmi | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Area.cmi: $(GEOMOD)Area.mli | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)AreaOverCube.cmi: $(GEOMOD)AreaOverCube.mli $(G)Area.cmi $(G)Cube.cmx $(G)algebra.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)AreaOverCube.cmx: $(GEOMOD)AreaOverCube.ml $(G)AreaOverCube.cmi $(G)algebra.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)AreaOverTorus.cmi: $(GEOMOD)AreaOverTorus.mli $(G)Area.cmi $(G)Torus.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)AreaOverTorus.cmx: $(GEOMOD)AreaOverTorus.ml $(G)AreaOverTorus.cmi $(G)algebra.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Parameter.cmx: Parameter.ml | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Test.cmx: Test.ml $(G)half_line.cmi $(G)interval.cmx $(G)arc.cmx $(G)Cube.cmx $(G)Torus.cmx $(G)AreaOverCube.cmx $(G)AreaOverTorus.cmx $(G)ODA.cmx $(G)BuiltInWithoutParsers.cmx $(G)sheet_solver_dgrog.cmx $(G)Command.cmx $(G)Parameter.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

#$(G)gui.cmx: $(GUI)gui.ml $(G)common.cmx $(G)Parameter.cmx $(G)sheet_solver_cpodgrog.cmx | $(G)
#	$(COMPILER) -c -I $(G) -I $(LABLGTK2) -o $@ $<

$(G)SpeclistMaker.cmx: $(SPEC)SpeclistMaker.ml $(G)common.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)CommandLineOptions.cmx: CommandLineOptions.ml $(G)Command.cmx $(G)Test.cmx $(G)SpeclistMaker.cmx | $(G)
#$(G)gui.cmx
	$(COMPILER) -c -I $(G) -o $@ $<

#$(G)BuzyMatrix.cmx: $(BACKEND)BuzyMatrix.ml | $(G)
#	$(COMPILER) -c -I $(G) -o $@ $<

$(G)InterpreterSignature.cmi: InterpreterSignature.mli $(G)AbstractSyntax.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Interpreter.cmi: Interpreter.mli $(G)InterpreterSignature.cmi $(G)Semantics.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Interpreter.cmx: Interpreter.ml $(G)Interpreter.cmi $(G)Semantics.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

$(G)Main.cmx: Main.ml $(G)Interpreter.cmx | $(G)
	$(COMPILER) -c -I $(G) -o $@ $<

#$(OUTPUT): $(G)Main.cmx $(G)parser_dgrog_sheet.cmx $(G)parser_cpodgrog.cmx $(G)parser_dgrog.cmx $(G)parser_oda_sheet.cmx $(G)parser_oda.cmx $(G)AreaOverTorus.cmx $(G)AreaOverCube.cmx $(G)Torus.cmx $(G)Cube.cmx $(G)arc.cmx $(G)interval.cmx $(G)half_line.cmx $(G)AbstractSyntax_without_parser.cmx  $(G)lexer_pv.cmx $(G)parser_pv.cmx $(G)AbstractSyntax.cmx $(G)algebra.cmx | $(G)
#	$(COMPILER) -I $(LABLGTK2) -I $(OCAMLGRAPH) -o $(OUTPUT) $(REQUIRED_CMXA) $(REQUIRED_CMX)

$(OUTPUT): $(REQUIRED_CMX) | $(G)
	$(COMPILER) -I $(LABLGTK2) -I $(OCAMLGRAPH) -o $(OUTPUT) $(REQUIRED_CMXA) $(REQUIRED_CMX)

clean:
	rm -rf $(G)
	rm -f $(OUTPUT)

