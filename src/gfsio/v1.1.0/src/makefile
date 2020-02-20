SHELL=/bin/sh
include ../../../../macros.make

INCMOD = ../../../../include/
LIB    = ../../../../libgfsio_4.a

FFLAGS  = $(GFSIO_FFLAGS) -I$(INCMOD)
AR      = ar
ARFLAGS = -rv

$(LIB): $(LIB)(gfsio_module.o)
.f.a:
	$(FC) -c $(FFLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	cp *.mod $(INCMOD)
	$(RM) $*.o

clean:
	$(RM) $(LIB)

