VERSION = 0.1
DISTDIR = oplate-$(VERSION)

all static clean doc:
	$(MAKE) -C src $@

dist: clean
	mkdir -p $(DISTDIR)/src
	cp -r bootstrap configure.ac configure Makefile m4 $(DISTDIR)
	cp $(wildcard src/*.ml src/*.mli src/*.mll src/*.mly src/*.glade) $(DISTDIR)/src
	cp src/Makefile.in src/OCamlMakefile $(DISTDIR)/src
	cp -r src/icons $(DISTDIR)/src
	cp -r src/pv $(DISTDIR)/src
	tar zcvf $(DISTDIR).tar.gz $(DISTDIR) --exclude=.svn
	$(RM) -rf $(DISTDIR)

cil:
	if [ ! -e cil ]; then git clone git://cil.git.sourceforge.net/gitroot/cil/cil; fi
	cd cil; git pull
	if [ ! -e cil/Makefile ]; then cd cil; ./configure; fi
	cd cil; make
	#Remove modules that we also define
	rm -f cil/obj/*/main.cmi

.PHONY: doc cil
