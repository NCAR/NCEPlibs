include ../../../../macros.make

LIB	= ../../../../libsfcio_v1.0.0_4.a
INCMOD	= ../../../../include/sfcio_v1.0.0_4


$(LIB):	$(LIB)(sfcio_module.o)
.f.a:
	$(FC) -c $(SFCIO_FFLAGS) $<
	$(AR) $(SFCIO_ARFLAGS) $@ $*.o
	rm -f $*.o
	mv *.mod $(INCMOD)
