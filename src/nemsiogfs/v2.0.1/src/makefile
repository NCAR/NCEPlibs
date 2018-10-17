
LIB     = $(LIBDIR)/libnemsiogfs_$(VER).a
INCMOD  = $(LIBDIR)/$(INC)

FC      = $(FCOMP)
FFLAGS  = $(FCFLAGS)
AR      = ar
ARFLAGS = -rvu

MODULES  = nemsio_gfs.o

$(LIB): $(MODULES)
	$(AR) $(ARFLAGS) $@ $(MODULES)
	mkdir -p $(INCMOD)
	mkdir -p $(INCMOD)/nemsiogfs_$(VER)
	mv *.mod $(INCMOD)/nemsiogfs_$(VER)
	rm -f *.o

clean:
	mkdir -p $(INCMOD)/nemsiogfs_$(VER)
	rm -f *.o *.mod *.a $(LIB) -r $(INCMOD)

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) $(FFLAGS) -c $*.f90

