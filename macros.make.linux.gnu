# Settings for LIBRARY BUILD ONLY: linux.gnu
#
# Flags common to all
RM         = rm -f
AR         = ar
ARFLAGS    =
#FC         = mpif90
#FCserial   = gfortran
#CC         = gcc
CPP        = cpp

ifeq ($(OPENMP),1)
  OMPFLAGS= -fopenmp
  OMPCPPFLAGS= -DOPENMP
else
  OMPFLAGS=
  OMPCPPFLAGS=
endif

# Number of parallel tasks for gmake
GMAKEMINUSJ = -j24

# Flags for bacio library
BACIO_FFLAGS  = $(OMPFLAGS) -O3 -fbacktrace -fPIC
BACIO_CFLAGS  = $(OMPFLAGS) -O3 -DUNDERSCORE -DLINUX -fPIC

# Flags for g2 library
G2_FFLAGS  = $(OMPFLAGS) -c -O3 -fconvert=big-endian -fno-second-underscore -frecord-marker=4 -fno-range-check -fbacktrace
G2_CFLAGS  = $(OMPFLAGS) -O3 -g -I${JASPER_INC} -I${PNG_INC} -DLINUX

# Flags for g2tmpl library
G2TMPL_FFLAGS  = $(OMPFLAGS) -O3 -ffree-form -fbacktrace
G2TMPL_CFLAGS  = $(OMPFLAGS) -O3 -g -DUNDERSCORE

# Flags for gfsio library
GFSIO_FFLAGS  = $(OMPFLAGS) -O3 -c -ffree-form -ffree-line-length-none -fconvert=big-endian -fno-second-underscore -frecord-marker=4 -fno-range-check -fbacktrace
GFSIO_ARFLAGS = -rv

# Flags for ip library
IP_FFLAGS     = $(OMPFLAGS) -ffree-form -ffree-line-length-none -fconvert=big-endian -fno-second-underscore -frecord-marker=4 -DCOMMCODE -DLINUX -DUPPLITTLEENDIAN  -fno-range-check -O3 -c
IP_FPPFLAGS   = -cpp -DLSIZE=4
IP_ARFLAGS    = -ruv

# Flags for landsfcutil library
LAND_FFLAGS   = $(OMPFLAGS) -fdefault-real-8  -O3 -ffree-form -c
LAND_ARFLAGS  = crvs

# Flags for nemsio library
NEMSIO_FFLAGS  = $(OMPFLAGS) -O -g -fPIC
NEMSIO_ARFLAGS = -rvu

# Flags for nemsiogfs library
NEMSIOGFS_FFLAGS  = $(OMPFLAGS) -O3 -ffree-form

# Flags for sfcio library
SFCIO_FFLAGS = -O2 -g -fbacktrace -fconvert=big-endian -I$(INCMOD) -ffree-form
SFCIO_ARFLAGS = -ruv

# Flags for sigio library
SIGIO_FFLAGS  = $(OMPFLAGS) -O0 -g -fbacktrace -ffree-form -fconvert=big-endian -c -fPIC
SIGIO_ARFLAGS = crvs

# Flags for sp library
SP_FFLAGS  = $(OMPFLAGS) -O3 -fdefault-real-8 -fconvert=big-endian -cpp -DLINUX -fPIC $(OMPCPPFLAGS)
SP_ARFLAGS = -ruv

# Flags for w3emc library
W3EMC_FFLAGS = $(OMPFLAGS) -O2 -g -fdefault-real-8 -fbacktrace -ffixed-form -fno-range-check -c -fPIC
W3EMC_4_FFLAGS = $(OMPFLAGS) -O3 -g -fconvert=big-endian -fno-second-underscore -frecord-marker=4 -fno-range-check -c
W3EMC_ARFLAGS = ruv

# Flags for w3nco library
W3NCO_FFLAGS  = $(OMPFLAGS) -O0 -g -fdefault-real-8 -fno-range-check -ffixed-form -fPIC
W3NCO_4_FFLAGS  = $(OMPFLAGS) -O3 -g -fconvert=big-endian -fno-second-underscore -frecord-marker=4 -fno-range-check
W3NCO_CFLAGS  = $(OMPFLAGS) -O0 -DLINUX -fPIC
W3NCO_ARFLAGS = -ruv

# Flags for wrfio library
WRFIO_FFLAGS   = $(OMPFLAGS) -ffree-form -ftree-vectorize -funroll-loops -fconvert=big-endian -frecord-marker=4
WRFIO_ARFLAGS  = ru

