SOURCES = generator.ml
RESULT = generator
INSTALL_TARGET = ~/bin/

test: all
	./generator --philosophers 3

all: $(SOURCES)
	ocamlopt str.cmxa generator.ml -o generator

install: $(SOURCES)
	cp generator $(INSTALL_TARGET)
