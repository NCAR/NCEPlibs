# src/Makefile.  Generated from Makefile.in by configure.
include ../../../../macros.make

# src/ level makefile template

# Package-specific substitution variables
package = nceplibs landsfcutil
version = v2.1.0
precision = d
tarname   = landsfcutil_$(version)_$(precision)
distdir   = $(tarname)_$(version)_$(precision)

# Prefix-specific substitution variables
prefix      = ../ 

# Mixed package and prefix substitution variables
installdir  = $(prefix)/$(distdir)

# Compiler substitution variables
FC       = $(FCserial)
FCFLAGS  = $(LAND_FFLAGS)
AR       = ar
ARFLAGS  = $(LAND_ARFLAGS) 
RANLIB   = ranlib
INSTALL         = /usr/bin/install -c
INSTALL_DATA    = ${INSTALL} -m 644

# The library name
LIBRARY = ../../../../lib$(tarname).a

# The file definitions. This include must occur before targets.
include make.filelist

# The targets
all: library

$(LIBRARY): $(OBJ_FILES)
	$(AR) $(ARFLAGS) $@ $(OBJ_FILES)
	$(RANLIB) $@
	mkdir -p ../../../../include/ 
	cp *.mod ../../../../include/
library: $(LIBRARY)

clean:
	-rm -f *.o *.mod *.a

distclean:
	-rm Makefile

check: library
	@echo "***THIS IS WHERE THE UNIT TEST INVOCATION GOES***"

# ...Gnu-style installation
install:
	$(INSTALL) -d $(DESTDIR)$(installdir)/include
	$(INSTALL_DATA) *.mod $(DESTDIR)$(installdir)/include
	$(INSTALL) -d $(DESTDIR)$(installdir)/lib
	$(INSTALL_DATA) $(LIBRARY) $(DESTDIR)$(installdir)/lib
	$(INSTALL_DATA) ../config.log $(DESTDIR)$(installdir)

# ...Universal uninstallation
uninstall:
	-rm -fr $(DESTDIR)$(installdir) >/dev/null 2>&1

# Specify targets that do not generate filesystem objects
.PHONY: all clean distclean check install uninstall

# Dependency include file
include make.dependencies

# Suffix rules
.SUFFIXES:
.SUFFIXES: .F .f .o
.F.o:
	$(FC) $(EXTRA_FCFLAGS) $(FCFLAGS) $(FPPFLAGS) $<

.f.o:
	$(FC) $(EXTRA_FCFLAGS) $(FCFLAGS) $<
