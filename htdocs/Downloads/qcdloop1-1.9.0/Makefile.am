## Process this file with automake to produce Makefile.in
ACLOCAL_AMFLAGS = -I m4

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = qcdloop1.pc

lib_LTLIBRARIES = libqcdloop1.la
if HAVE_MAPLE
    lib_LTLIBRARIES += libmwrap_qcdloop1.la
endif
libmwrap_qcdloop1_la_SOURCES = mwrap_qcdloop1.c
libmwrap_qcdloop1_la_LIBADD = libqcdloop1.la -lmaplec
if HAVE_MAPLE_AUTO
    libmwrap_qcdloop1_la_CFLAGS = -I$(MAPLEINC)
    libmwrap_qcdloop1_la_LDFLAGS = -L$(MAPLELIB)
endif

libqcdloop1_la_SOURCES = ff/ffxc0.f ff/ffxd0.f \
	ff/fftran.f ff/ffxd0h.f ff/ffdel2.f ff/ffdel3.f ff/ffdel4.f \
	ff/ff2dl2.f ff/ffabcd.f ff/ffxc0i.f ff/ffxc0p.f ff/ffxd0p.f ff/ffxdbd.f \
	ff/ffxli2.f ff/ffcli2.f ff/ffcxs3.f ff/ffcxs4.f ff/ffxxyz.f ff/ffcxyz.f \
	ff/ffcc0p.f ff/ffcxr.f ff/ffcrr.f ff/ffdxc0.f ff/ffdcc0.f ff/ffxd0i.f \
	ff/ffcel2.f ff/ffcel3.f ff/ffdcxs.f \
	\
	ql/qlinit.f ql/qlI1.f ql/qlI2.f ql/qlI3.f ql/qlI4.f \
	ql/qlI2fin.f ql/qlI3fin.f ql/qlI4fin.f \
	ql/qlI3sub.f ql/qlI4array.f ql/qlI4sub0m.f ql/qlI4sub1m.f \
	ql/qlI4sub2m.f ql/qlI4sub2ma.f ql/qlI4sub2mo.f ql/qlI4sub3m.f \
	ql/auxCD.f ql/qlzero.f ql/qltrisort.f ql/qlsnglsort.f ql/qlfndd.f \
	ql/qlkfn.f ql/qllnrat.f ql/qlYcalc.f ql/ddilog.f \
	ql/qltri1.f ql/qltri2.f ql/qltri3.f ql/qltri4.f ql/qltri5.f ql/qltri6.f \
	ql/qlbox1.f ql/qlbox2.f ql/qlbox3.f ql/qlbox4.f ql/qlbox5.f ql/qlbox6.f \
	ql/qlbox7.f ql/qlbox8.f ql/qlbox9.f ql/qlbox10.f \
	ql/qlbox11.f ql/qlbox12.f ql/qlbox13.f ql/qlbox14.f ql/qlbox15.f ql/qlbox16.f \
	ql/qlfunctions.f ql/qlratgam.f ql/qlratreal.f ql/qlspencer.f ql/qlI4DNS41.f ql/qlxpicheck.f \
	ql/qlcLi2omx2.f ql/qlcLi2omx3.f ql/qlLi2omprod.f ql/qlLi2omrat.f ql/qlLi2omx.f ql/qlLi2omx2.f

nodist_libqcdloop1_la_SOURCES = ffinit_auto.f
EXTRA_libqcdloop1_la_SOURCES = ff/ffinit_mine.f ff/ff.h ff/aa.h ql/qlconstants.f ql/qlonshellcutoff.f
libqcdloop1_la_LDFLAGS = -no-undefined -export-dynamic \
	-version-info @LIBQCD_VERSION_INFO@

EXTRA_DIST=libqcdloop1.vers wrapper.mpl test.log wrapper.log
if HAVE_LD_VERSION_SCRIPT
  # Versioned symbols and restricted exports
  libqcdloop1_la_LDFLAGS += -Wl,--version-script=${srcdir}/libqcdloop1.vers
  libqcdloop1_la_DEPENDENCIES = ${srcdir}/libqcdloop1.vers
else
  # Only restricted exports when possible
  libqcdloop1_la_LDFLAGS += -export-symbols libqcdloop1.sym
  libqcdloop1_la_DEPENDENCIES = libqcdloop1.sym
endif

include_HEADERS = qcdloop1.h
nobase_dist_data_DATA = ff/fferr.dat ff/ffwarn.dat

check_PROGRAMS = test

test_LDADD = libqcdloop1.la
test_SOURCES = test.f

CLEANFILES=ffinit_auto.f ff_auto.h aa_auto.h libqcdloop1.sym

ffinit_auto.f: ff/ffinit_mine.f ff_auto.h aa_auto.h
	rm -f $@ $@.new
	$(SED) 's|/usr/local/share/ff/|${datarootdir}/ff/|' $< | \
	$(SED) "s|^\([\t ]\{1,5\}\)\(.\{66\}\)\(.\{1,\}\)$$|\1\2\n     +\3|" | \
	$(SED) "s|include 'ff.h'|include 'ff_auto.h'|"  | \
	$(SED) "s|include 'aa.h'|include 'aa_auto.h'|"  >> $@.new
	mv $@.new $@

ff_auto.h: ff/ff.h
	$(LN_S) $< $@

aa_auto.h: ff/aa.h
	$(LN_S) $< $@

AN = '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
libqcdloop1.sym: qcdloop1.h
	rm -f $@ $@.new
	$(CPP) $(CPPFLAGS) -DQL_BUILDSYMS $^ | \
		$(SED) -n -e \
		's|^.*QL_EXPORT_MARK[ 	]*\([$(AN)]*\).*$$|$(SYMBOL_PREFIX)\1|p' \
			> $@.new
	mv $@.new $@

