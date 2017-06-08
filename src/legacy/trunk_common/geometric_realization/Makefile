CMN = ../../../trunk_common/

SRC = ../

FE = frontend/

BE = backend/

PV = ../frontend/pv/

SOURCES = $(CMN)/glyph.ml $(CMN)common.ml message.ml \
$(CMN)sig.mli $(CMN)halfLine.ml $(CMN)algebra.ml \
AdditionalSig.mli ODA.ml DGROG.ml CPODGROG.ml $(CMN)area.mli \
$(CMN)interval.ml $(CMN)cube.ml $(CMN)areaOverCube.ml $(CMN)arc.ml \
$(CMN)torus.ml $(CMN)areaOverTorus.ml \
$(FE)BuiltInWithoutParsers.ml \
$(FE)Expression.ml \
$(FE)parser_oda_sheet.mly $(FE)parser_oda.mly $(FE)lexer_oda.mll \
$(FE)lexer_oda_sheet.mll \
$(FE)sheet_solver_oda.ml \
$(FE)parser_dgrog.mly $(FE)lexer_dgrog.mll \
$(FE)parser_cpodgrog.mly $(FE)lexer_cpodgrog.mll \
$(FE)BuiltInAuxiliary.ml \
$(FE)parser_cpodgrog_sheet.mly $(FE)lexer_cpodgrog_sheet.mll \
$(FE)sheet_solver_cpodgrog.ml \
BuiltIn.ml \
$(FE)parser_dgrog_sheet.mly $(FE)lexer_dgrog_sheet.mll \
$(FE)sheet_solver_dgrog.ml \
$(SRC)AbstractSyntaxSignature.mli \
$(SRC)AbstractSyntax_without_parser.ml \
$(PV)parser_pv_flags.ml $(PV)parser_pv.mly $(PV)lexer_pv.mll \
$(SRC)AbstractSyntax.ml \
$(BE)PV2CFG.ml \
$(BE)ForbiddenExtension.ml \
$(BE)Semantics.ml \
Parameter.ml \
Command.ml Test.ml \
$(CMN)SpeclistMaker.ml \
CommandLineOptions.ml \
Main.ml
#INCDIRS = I ~/.opam/4.04.1/lib/ocamlgraph/
INCDIRS = I ~/Bureau/wip/cosynus/alcool/sources/trunk_isothetic/src/geometric_realization/frontend/
LIBS =  ulexing
PACKS = ocamlgraph str ulex unix

RESULT = main

all: nc

include OCamlMakefile
