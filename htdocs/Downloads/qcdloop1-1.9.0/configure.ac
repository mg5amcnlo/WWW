#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ([2.63])

# Package version number
# Major.Minor goes into symbols version
m4_define([libqcd_major_version], [1])
m4_define([libqcd_minor_version], [9])
m4_define([libqcd_micro_version], [0])

# Libtool interface number ( current : revision : age )
# - If the library source code has changed at all since the last update,
#   then increment revision
# - If any interfaces have been added, removed, or changed since the last update,
#   increment current, and set revision to 0.
# - If any interfaces have been added since the last public release,
#   then increment age.
# - If any interfaces have been removed or changed since the last public release,
#   then set age to 0.
m4_define([libqcd_interface_current],  [0])
m4_define([libqcd_interface_revision], [0])
m4_define([libqcd_interface_age],      [0])

m4_define([libqcd_version], m4_defn([libqcd_major_version]).m4_defn([libqcd_minor_version]).m4_defn([libqcd_micro_version]))

AC_INIT([qcdloop1], m4_defn([libqcd_version]))

LIBQCD_MAJOR=libqcd_major_version
LIBQCD_MINOR=libqcd_minor_version
LIBQCD_MICRO=libqcd_micro_version

LIBQCD_CURRENT=libqcd_interface_current
LIBQCD_REVISION=libqcd_interface_revision
LIBQCD_AGE=libqcd_interface_age

LIBQCD_VERSION=$LIBQCD_MAJOR.$LIBQCD_MINOR.$LIBQCD_MICRO
LIBQCD_VERSION_INFO=$LIBQCD_CURRENT:$LIBQCD_REVISION:$LIBQCD_AGE

AC_CONFIG_SRCDIR([ql/qlinit.f])

AC_CONFIG_AUX_DIR([auxfiles])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall silent-rules foreign subdir-objects])

AC_CONFIG_HEADERS([config.h])

m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
LT_INIT

# Checks for programs.
AC_PROG_CC
AC_PROG_F77
# AC_PROG_RANLIB
AC_PROG_LIBTOOL

AC_PROG_LD
AC_PROG_CPP
AC_CHECK_TOOL(SED, sed, :)
#AC_PROG_LN_S

# Checks for libraries.

# Checks for header files.

# Set language
AC_LANG([Fortran 77])

# Checks for typedefs, structures, and compiler characteristics.
#AC_HEADER_STDBOOL

# Checks for library functions.
#AC_CHECK_FUNCS([sqrt])

AC_MSG_CHECKING([if libraries can be versioned])
# Special case for PE/COFF platforms: ld reports
# support for version-script, but doesn't actually
# do anything with it.
case $host in
*cygwin* | *mingw32* )
    have_ld_version_script=no
    AC_MSG_RESULT(no)
;;
* )
GLD=`$LD --help < /dev/null 2>/dev/null | grep version-script`
if test "$GLD"; then
    have_ld_version_script=yes
    AC_MSG_RESULT(yes)
else
    have_ld_version_script=no
    AC_MSG_RESULT(no)
    AC_MSG_WARN(*** You have not enabled versioned symbols.)
fi
;;
esac

AM_CONDITIONAL(HAVE_LD_VERSION_SCRIPT, test "$have_ld_version_script" = "yes")

if test "$have_ld_version_script" = "yes"; then
    AC_MSG_CHECKING([for symbol prefix])
    SYMBOL_PREFIX=`echo "PREFIX=__USER_LABEL_PREFIX__" \
                   | ${CPP-${CC-gcc} -E} - 2>&1 \
                   | ${EGREP-grep} "^PREFIX=" \
                   | ${SED-sed} "s:^PREFIX=::"`
    AC_SUBST(SYMBOL_PREFIX)
    AC_MSG_RESULT($SYMBOL_PREFIX)
fi

# ------------------------- # ------------------------- # -------------------------

AC_ARG_WITH([maple],
            [AS_HELP_STRING([--with-maple],
            [build maple wrapper interface @<:@default=no@:>@.
@<:@manual@:>@ assumes that CPPFLAGS and LDFLAGS are set to find maplec.h and maplec.so.
@<:@fullpath@:>@ assumes that fullpath points to maple binary.
@<:@no@:>@ disables building maple interface.
])],
            [test "x$with_maple" == "x" && with_maple=auto],
            [with_maple=no])

# ------------------------- # -------------------------

case $with_maple in
no) ;;
auto)
  AC_MSG_CHECKING([for working maple installation])
  AC_PATH_PROGS_FEATURE_CHECK([MAPLE], [maple maple14.s maple13.s maple12.s],
  [[
    rm -f maple.conftest
    echo 'writeline(default,kernelopts(mapledir),kernelopts(bindir)):' | $ac_path_MAPLE -qs > maple.conftest
    if test "x$?" == "x0" && test -s maple.conftest ; then
      maple_incdir=`sed -n 1p maple.conftest`/extern/include
      maple_libdir=`sed -n 2p maple.conftest`
      ac_cv_path_MAPLE=$ac_path_MAPLE ac_path_MAPLE_found=:
      with_maple=$ac_path_MAPLE
    fi
    rm -f maple.conftest
  ]], [with_maple=no])
  AC_MSG_RESULT($with_maple)
;;
manual)
  AC_LANG_PUSH([C])
  AC_CHECK_HEADER([maplec.h],have_maplec_h=yes)
  AC_CHECK_HEADER([mplshlib.h],have_mplshlib_h=yes)
  AC_LANG_POP([C])
  test "x$have_maplec_h" != "xyes" && test "x$have_mplshlib_h" != "xyes" && with_maple=no
;;
*)
  AC_MSG_CHECKING([for working maple installation])
  if test -x $with_maple ; then
    rm -f maple.conftest
    echo 'writeline(default,kernelopts(mapledir),kernelopts(bindir)):' | $with_maple -qs > maple.conftest
    if test "x$?" == "x0" && test -s maple.conftest ; then
      maple_incdir=`sed -n 1p maple.conftest`/extern/include
      maple_libdir=`sed -n 2p maple.conftest`
      AC_MSG_RESULT(yes)
    else
      with_maple=no
      AC_MSG_RESULT(no)
    fi
    rm -f maple.conftest
  else
    with_maple=no
    AC_MSG_RESULT(no)
  fi
;;
esac

# ------------------------- # ------------------------- # -------------------------

F77_PROG_NAME=`basename $F77`
case "$F77_PROG_NAME" in
    g77|ifort)
        test "x$enable_f2c" == "x" && enable_f2c=yes
    ;;
esac

# ------------------------- # ------------------------- # -------------------------

if test "x$with_maple" != "xno" ; then
  AC_SUBST([MAPLEINC], [$maple_incdir])
  AC_SUBST([MAPLELIB], [$maple_libdir])
fi
AM_CONDITIONAL(HAVE_MAPLE, test "x$with_maple" != "xno")
AM_CONDITIONAL(HAVE_MAPLE_AUTO, test "x$have_maplec_h" != "xyes" && test "x$have_mplshlib_h" != "xyes")

AC_ARG_ENABLE(f2c,[AS_HELP_STRING([--enable-f2c],
            [Use g77 and ifort calling conventions @<:@default=no@:>@])],
            [], [test "x$enable_f2c" == "x" && enable_f2c=no])
test "x$enable_f2c" == "xyes" && AC_DEFINE(USE_F2C,1,"Use f2c conventions for complex return values")

# ------------------------- # ------------------------- # -------------------------

# Substitutions for .in files
AC_SUBST(LIBQCD_MAJOR)
AC_SUBST(LIBQCD_MINOR)
AC_SUBST(LIBQCD_MICRO)
AC_SUBST(LIBQCD_CURRENT)
AC_SUBST(LIBQCD_REVISION)
AC_SUBST(LIBQCD_AGE)
AC_SUBST(LIBQCD_VERSION_INFO)

AC_CONFIG_FILES([Makefile qcdloop1.pc])

AC_OUTPUT
