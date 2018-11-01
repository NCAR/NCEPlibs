# libsrc Makefile

include ./macros.make

# DH* move nemsio to end of all list
some:
	$(MAKE) $(GMAKEMINUSJ) -C src/bacio/v2.0.1/src
	$(MAKE) $(GMAKEMINUSJ) -C src/ip/v3.0.0/sorc
	$(MAKE) $(MAKEMINUSJ) -C src/sp/v2.0.2/src
	$(MAKE) $(MAKEMINUSJ) -C src/sigio/v2.0.1/src
	$(MAKE) $(MAKEMINUSJ) -C src/w3emc/v2.2.0/src # Depends on sigio 2.0.1
	$(MAKE) $(MAKEMINUSJ) -C src/w3nco/v2.0.6/src
	$(MAKE) $(MAKEMINUSJ) -C src/nemsio/v2.2.3/src

all: some
	$(MAKE) $(MAKEMINUSJ) -C src/landsfcutil/v2.1.0/src
	$(MAKE) $(MAKEMINUSJ) -C src/sfcio/v1.0.0/src
	$(MAKE) $(MAKEMINUSJ) -C src/nemsiogfs/v2.0.1/src # Depends on nemsio 2.0.1

clean:
	$(MAKE) -C src/bacio/v2.0.1/src  clean
	$(MAKE) -C src/ip/v3.0.0/sorc clean
	$(MAKE) -C src/sp/v2.0.2/src  clean
	$(MAKE) -C src/sigio/v2.0.1/src  clean
	$(MAKE) -C src/w3emc/v2.2.0 clean/src # Depends on sigio 2.0.1
	$(MAKE) -C src/w3nco/v2.0.6/src clean
	$(MAKE) -C src/nemsio/v2.2.3/src clean
	$(MAKE) -C src/landsfcutil/v2.1.0/src clean
	$(MAKE) -C src/sfcio/v1.0.0/src clean
	$(MAKE) -C src/nemsiogfs/v2.0.1/src clean # Depends on nemsio 2.0.1

