SHELL=/bin/sh
include ../../../../macros.make

LIB    = ../../../../libg2tmpl_v1.5.0.a
INCMOD = ../../../../include/

FFLAGS=$(G2TMPL_FFLAGS)
CFLAGS=$(G2TMPL_CFLAGS)
AR     =ar
ARFLAGS=

$(LIB): $(LIB)(process_4dot2.o) $(LIB)(grib2_all_tables_module.o) $(LIB)(gdtsec3.o)  \
        $(LIB)(g2sec4_temp1.o) $(LIB)(g2sec4_temp11.o) $(LIB)(g2sec4_temp12.o)

$(LIB)(process_4dot2.o): process_4dot2.c
	$(CC) -O3 -c $(CFLAGS) $<
	$(AR) $(ARFLAGS) -ruv $@ $*.o
	rm -f $*.o

.f.a:
	$(FC) -c $(FFLAGS) $<
	$(AR) $(ARFLAGS) -ruv $@ $*.o 
	cp *.mod $(INCMOD)
	rm -f $*.o

clean:
	rm -f process_4dot2.o

realclean:      clean
	rm -f test_* core

