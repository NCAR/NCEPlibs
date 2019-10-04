SHELL=/bin/sh
include ../../../../macros.make

LIB    = ../../../../libgfsio_4.a
FFLAGS  = $(GFSIO_FFLAGS)
AR      = ar
ARFLAGS = -rv

$(LIB): $(LIB)(gfsio_module.o)
.f.a:
	$(FC) -c $(FFLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	$(RM) $*.o

clean:
	$(RM) $(LIB)

