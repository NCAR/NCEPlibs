# libsrc Makefile

include ./macros.make

# DH* move nemsio to end of all list
nompi: core

core:
	$(MAKE) $(GMAKEMINUSJ) -C src/bacio/v2.0.1/src # GLOBAL,SAR,UPP
	$(MAKE) $(GMAKEMINUSJ) -C src/ip/v3.0.0/sorc   # GLOBAL,SAR
	$(MAKE) $(MAKEMINUSJ) -C src/sp/v2.0.2/src     # GLOBAL,SAR,UPP
	$(MAKE) $(MAKEMINUSJ) -C src/sigio/v2.0.1/src  # GLOBAL,SAR,UPP
	$(MAKE) $(MAKEMINUSJ) -C src/w3emc/v2.2.0/src  # GLOBAL,SAR,UPP; Depends on sigio 2.0.1
	$(MAKE) $(MAKEMINUSJ) -C src/w3nco/v2.0.6/src  # GLOBAL,SAR,UPP

# nemsio gets separate stanza since it does not support serial build
nemsio:
	$(MAKE) $(MAKEMINUSJ) -C src/nemsio/v2.2.3/src # GLOBAL,SAR,UPP

# sfcio gets separate stanza since it is used by both SAR and UPP
sfcio:
	$(MAKE) $(MAKEMINUSJ) -C src/sfcio/v1.0.0/src       # SAR,UPP

# Makerule for libraries needed for Stand-Alone Regional (SAR) FV3
sar: core nemsio sfcio sarlibs
sarlibs:
	$(MAKE) $(MAKEMINUSJ) -C src/landsfcutil/v2.1.0/src # SAR
	$(MAKE) $(MAKEMINUSJ) -C src/nemsiogfs/v2.0.1/src   # SAR; Depends on nemsio 2.0.1

# Makerule for the Unified Post-Processor (UPP)
upp: core nemsio sfcio upplibs
upplibs:
	$(MAKE) $(MAKEMINUSJ) -C src/g2/v3.1.0/src          # UPP
	$(MAKE) $(MAKEMINUSJ) -C src/g2tmpl/v1.5.0/src      # UPP
	$(MAKE) $(MAKEMINUSJ) -C src/gfsio/v1.1.0/src       # UPP; Depends on w3emc
	$(MAKE) $(MAKEMINUSJ) -C src/wrfio/v1.1.1/src       # UPP

# Makerule for original (global) libraries
global: core nemsio

# Makerule for building all libraries
all: core nemsio sfcio sarlibs upplibs

clean:
	$(MAKE) -C src/bacio/v2.0.1/src  clean
	$(MAKE) -C src/ip/v3.0.0/sorc clean
	$(MAKE) -C src/sp/v2.0.2/src  clean
	$(MAKE) -C src/sigio/v2.0.1/src  clean
	$(MAKE) -C src/w3emc/v2.2.0/src clean # Depends on sigio 2.0.1
	$(MAKE) -C src/w3nco/v2.0.6/src clean
	$(MAKE) -C src/nemsio/v2.2.3/src clean
	$(MAKE) -C src/landsfcutil/v2.1.0/src clean
	$(MAKE) -C src/sfcio/v1.0.0/src clean
	$(MAKE) -C src/nemsiogfs/v2.0.1/src clean # Depends on nemsio 2.0.1
	$(MAKE) -C src/g2/v3.1.0/src clean
	$(MAKE) -C src/gfsio/v1.1.0/src clean
	$(MAKE) -C src/wrfio/v1.1.1/src clean
