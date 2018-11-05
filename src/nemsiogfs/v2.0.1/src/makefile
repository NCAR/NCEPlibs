include ../../../../macros.make

VER        = v2.0.1
LIB        = ../../../../libnemsiogfs_$(VER).a
INCMOD     = ../../../../include/

FFLAGS  = $(NEMSIOGFS_FFLAGS)
AR      = ar
ARFLAGS = -rvu

MODULES  = nemsio_gfs.o

$(LIB): $(MODULES)
	$(AR) $(ARFLAGS) $@ $(MODULES)
	mkdir -p $(INCMOD)
	mv *.mod $(INCMOD)/
	rm -f *.o

clean:
	rm -f *.o *.mod *.a $(LIB)

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) $(FFLAGS) -I$(INCMOD) -c $*.f90

