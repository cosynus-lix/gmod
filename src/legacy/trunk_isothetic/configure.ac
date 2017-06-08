AC_INIT(oplate,0.1)
VERSION=$PACKAGE_VERSION
AC_MSG_RESULT(configuring $PACKAGE_STRING)

AC_PROG_CC

# Add prefix to compilation variables
# if passed
if test "x$prefix" != "xNONE"; then
        CFLAGS="$CFLAGS -I$prefix/include"
        LDFLAGS="$LDFLAGS -L$prefix/lib"
        CPPFLAGS="$CPPFLAGS -I$prefix/include"
fi

# Check for OCaml compilers

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_OCAMLLEX
AC_PROG_OCAMLYACC

if test "$OCAMLOPT" = "no" ; then
    BEST=byte
else
    BEST="byte opt"
fi

AC_ARG_ENABLE([debugging],
   AC_HELP_STRING(
      [--enable-debugging],
      [compile with debugging information (backtrace printing in particular)]))

if test "x$enable_debugging" = "xyes" ; then
  OCAMLFLAGS="$OCAMLFLAGS -g"
fi

# ocamlfind
AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

# lablgtk/lablgl
AC_ARG_WITH([lablgl_dir],AC_HELP_STRING([--with-lablgl-dir],[path for LablGL library]))
AC_ARG_WITH([lablgtk2_dir],AC_HELP_STRING([--with-lablgtk2-dir],[path for LablGTK2 library]))
AC_CHECK_OCAML_PKG([lablgtk2])
AC_ARG_ENABLE([gui],AC_HELP_STRING([--disable-gui],["don't build the the GUI"]))
if test "$enable_gui" = "no"; then
   OCAML_PKG_lablgtk2="no"
fi
if test "z$with_lablgtk2_dir" = "z" -a "$OCAML_PKG_lablgtk2" != "no"; then
   if test `$OCAMLFIND query lablgtk2.init`; then
     # We are on a Debian/Ubuntu

     # Do we have glade support?
     if test `$OCAMLFIND query lablgtk2.glade`; then
        PACKS="$PACKS lablgtk2 lablgtk2.init lablgtk2.glade"
	if test `$OCAMLFIND query lablgtk2-gl.gtkgl`; then
	    PACKS="$PACKS lablgtk2-gl.gtkgl"
	    OCAML_PKG_gtkgl="lablgtk2-gl.gtkgl"
	else
	    OCAML_PKG_gtkgl="no"
	fi
     else
        AC_MSG_WARN([Glade support is not present!])
	OCAML_PKG_lablgtk2="no"
        OCAML_PKG_gtkgl="no"
     fi
   else
     PACKS="$PACKS lablgtk2"
     LIBS="$LIBS lablglade"
     OCAMLINCDIRS="$OCAMLINCDIRS `ocamlfind query lablgtk2`"
     OCAML_PKG_lablgtk2="yes"
     # TODO: add manual support for gtkgl
     OCAML_PKG_gtkgl="no"
   fi
else
  OCAML_PKG_lablgtk2="no"
  OCAML_PKG_gtkgl="no"
  if test "z$with_lablgtk2_dir" = "z"; then with_lablgtk2_dir="+lablgtk2"; fi
  AC_CHECK_OCAML_MODULE([has_lablgtk2],[lablgtk2],[GMain],[$with_lablgtk2_dir])
  AC_CHECK_OCAML_MODULE([has_lablglade],[lablgtk2(glade)],[Glade],[$with_lablgtk2_dir])

  if test "$has_lablgtk" != "no" -a "$has_lablglade" != "no"; then
    OCAML_PKG_lablgtk2="lablgtk2"
    OCAMLINCDIRS="$OCAMLINCDIRS $has_lablgtk2"
    LIBS="$LIBS lablgtk lablglade"

    if test "z$with_lablgl_dir" = "z"; then with_lablgl_dir="+lablgl"; fi
    AC_CHECK_OCAML_MODULE([has_lablgl],[lablgl],[Gl],[$with_lablgl_dir])
    AC_CHECK_OCAML_MODULE([has_lablgtkgl],[lablgtk2(gl)],[GlGtk],[$with_lablgtk2_dir])
    if test "$has_lablgl" != "no" -a "$has_lablgtkgl" != "no"; then
      OCAML_PKG_gtkgl="lablgtkgl"
      OCAMLINCDIRS="$OCAMLINCDIRS $has_lablgl"
      LIBS="$LIBS lablgl lablgtkgl"
    fi
  fi
fi

# ocamlgraph
AC_ARG_WITH([ocamlgraph_dir],AC_HELP_STRING([--with-ocamlgraph-dir],[path for ocamlgraph library]))
if test "z$with_ocamlgraph_dir" != "z"; then
   OCAML_PKG_ocamlgraph="no"
   AC_CHECK_OCAML_MODULE([has_ocamlgraph],[ocamlgraph],[Graph],[$with_ocamlgraph_dir])
   if test "$has_ocamlgraph" != no; then
     OCAML_PKG_ocamlgraph="ocamlgraph"
     OCAMLINCDIRS="$OCAMLINCDIRS $has_ocamlgraph"
     LIBS="$LIBS graph"
   fi
else
   AC_CHECK_OCAML_PKG([ocamlgraph])
   if test "$OCAML_PKG_ocamlgraph" != "no"; then
      PACKS="$PACKS ocamlgraph"
   fi
fi
if test "$OCAML_PKG_ocamlgraph" = "no"; then
   AC_MSG_ERROR([You must install the ocamlgraph OCaml library])
fi

# apron
AC_CHECK_OCAML_MODULE([apron],[Apron],[Apron],[+apron])
if test "$apron" != "no"; then
        OCAML_PKG_apron="apron"
	OCAMLINCDIRS="$OCAMLINCDIRS +apron"
        LIBS="$LIBS gmp apron"
        CLIBS="$CLIBS boxMPQ_caml boxMPQ octMPQ_caml octMPQ mpfr itvMPQ apron_caml apron camlidl"
else
        OCAML_PKG_apron="no"
fi

# batteries
AC_CHECK_OCAML_PKG([batteries])
if test "$OCAML_PKG_batteries" != "no"; then
        PACKS="$PACKS batteries"
fi

# numerix
AC_CHECK_OCAML_PKG([numerix])
if test "$OCAML_PKG_numerix" != "no"; then
        PACKS="$PACKS numerix"
fi

# cil
AC_CHECK_OCAML_MODULE(OCAML_CIL,cil,Cil,cil/obj/*)
if test "$OCAML_CIL" != "no"; then
   OCAMLINCDIRS="$OCAMLINCDIRS ../$OCAML_CIL"
   LIBS="$LIBS cil"
   OCAML_CIL=cil
fi

# substitutions to perform
AC_SUBST(VERSION)
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLFLAGS)
AC_SUBST(BEST)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
AC_SUBST(OCAMLBEST)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLBIN)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLMKTOP)
AC_SUBST(OCAMLFIND)
AC_SUBST(OCAMLFIND_LDCONF)
AC_SUBST(OCAMLCP)
AC_SUBST(CAMLLIBPATH)
AC_SUBST(OCAMLINCDIRS)
AC_SUBST(OCAMLLDFLAGS)
AC_SUBST(PACKS)
AC_SUBST(LIBS)
AC_SUBST(CLIBS)
AC_SUBST(OCAML_PKG_lablgtk2)
AC_SUBST(OCAML_PKG_gtkgl)
AC_SUBST(OCAML_PKG_ocamlgraph)
AC_SUBST(OCAML_PKG_apron)
AC_SUBST(OCAML_PKG_batteries)
AC_SUBST(OCAML_PKG_numerix)
AC_SUBST(OCAML_CIL)

# Finally create the Makefile and samples
AC_CONFIG_FILES([src/Makefile],[chmod a-w src/Makefile])
AC_OUTPUT

cat <<EOMSG
 -------------------------------------------------------------------- Features
 * OCamlGraph: $OCAML_PKG_ocamlgraph
 * LablGtk2:   $OCAML_PKG_lablgtk2
 * LablGtkGL:  $OCAML_PKG_gtkgl
 * Apron:      $OCAML_PKG_apron
 * Numerix:    $OCAML_PKG_numerix
 * CIL:        $OCAML_CIL
 --------------------------------------------------------------------
EOMSG
# * Batteries:  $OCAML_PKG_batteries
