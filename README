# Original version: Julie Schramm, NOAA
# Last update: Dom Heinzeller, NOAA, 20180420

NCEP libraries for FV3 (NEMSfv3gfs trunk, April 2018)

The library sources are available from following websites:

http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/bacio/v2.0.1/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/ip/v2.0.0/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/nemsio/v2.2.3/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/sigio/v2.0.1/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/sp/v2.0.2/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/w3nco/v2.0.6/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/w3emc/v2.2.0/

Several files in w3nco/v2.0.6/src had to be patched for thread-safety,
(OpenMP compilers), and for using the GNU compilers in general. Files in
bacio/v2.0.1/src had to be extended with preprocessor flags for MACOSX.

Building:

To build these libraries, users are advised to load the modules
and set the environment variables as in the respective module file
of NEMSfv3gfs (NEMSfv3gfs_top_dir/modulefiles/MACHINE.COMPILER/fv3)
and in addition set the environment variable OPENMP to 1 for building
thread-safe versions of all libraries (or 0 if OpenMP will not be
used when compiling FV3):

> # for bash
> . ${NEMSfv3gfs_top_dir}/modulefiles/MACHINE.COMPILER/fv3
> export OPENMP=1 # or 0

The libraries are built with

> ./make_ncep_libs.sh MACHINE.COMPILER 2>&1 | tee log.make

Cleaning:

> ./make_ncep_libs.sh clean 

will clean all *.a, *.mod and *.o files and remove the exec_* directories. 

mat