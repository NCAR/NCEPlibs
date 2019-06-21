# NCEP libraries for FV3 (NEMSfv3gfs trunk, April 2018)

#### Original version: Julie Schramm, NOAA
#### Last update: Dom Heinzeller, NOAA, 2019/06/21

The original library sources are available from following websites:

http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/bacio/v2.0.1/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/ip/v3.0.0/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/nemsio/v2.2.3/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/sigio/v2.0.1/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/sp/v2.0.2/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/w3nco/v2.0.6/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/w3emc/v2.2.0/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/landsfcutil/v2.1.0/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/sfcio/v1.0.0/
http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/lib/nemsiogfs/v2.0.1/

Several files in w3nco/v2.0.6/src had to be patched for thread-safety,
(OpenMP compilers), and for using the GNU compilers in general. Files in
bacio/v2.0.1/src had to be extended with preprocessor flags for MACOSX.
nemsio_gfs had to be modified to exclude non-standard code.

#### Building:

To build these libraries, users are advised to load the same modules and
set the environment variables (compilers and flags) as in the respective
module file of NEMSfv3gfs (NEMSFV3GFS_DIR/modulefiles/SYSTEM.COMPILER/fv3).

The libraries are built and installed with

> ./make_ncep_libs.sh -s MACHINE -c COMPILER -d NCEPLIBS_DIR -o OPENMP [-m mpi] [-a COMPILEALL]

It is recommended to install the NCEPlibs into their own directory, which must be created before running the installer. Further information on the command line arguments can be obtained with

> ./make_ncep_libs.sh -h

The include files and libraries are installed to NCEPLIBS_DIR/{include,lib}

To remove the installed libraries, simply delete the contents of NCEPLIBS_DIR.
