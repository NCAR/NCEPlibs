#!/bin/sh --login

#-----------------------------------------------------
# Build the nemsiogfs library on wcoss phase 1/2 or
# wcoss-cray.  Invoke with no arguments:
#
# $> build.wcoss.sh
#-----------------------------------------------------

#set -x

export VER="v2.0.1"
module purge

mac=$(hostname -f)

case $mac in

  g????.ncep.noaa.gov | t????.ncep.noaa.gov)  # wcoss phase 1/2

    echo BUILD WITH INTEL COMPILER. 

    module load ics
    module load nemsio/v2.2.2
    module list

    export LIBDIR='..'
    export INC='include'
    export FCOMP=ifort
    export FCFLAGS='-O3 -FR -I$(NEMSIO_INC)'

    make clean
    make;;

  llogin? | slogin?)  # wcoss cray

    echo BUILD WITH INTEL COMPILER. 

    module load PrgEnv-intel
    module load craype-sandybridge
    module load nemsio-intel/2.2.2
    module list

    export LIBDIR='../intel'
    export INC='include'
    export FCOMP=ftn
    export FCFLAGS='-O3 -FR -I$(NEMSIO_INC) -axCore-AVX2 -craype-verbose'

    make clean
    make 
 
    echo BUILD WITH CRAY COMPILER.

    module swap PrgEnv-intel PrgEnv-cray
    module swap craype-sandybridge craype-haswell
    module swap nemsio-intel/2.2.2 nemsio-cray-haswell/2.2.2
    module list

    export LIBDIR='../cray'
    export INC='include'
    export FCOMP=ftn
    export FCFLAGS='-O2 -ffree -I$(NEMSIO_INC) -craype-verbose'

    make clean
    make ;;

tfe??)  # theia

    echo BUILD WITH INTEL COMPILER. 

    module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
    module load nemsio

    module load intel

    export LIBDIR='..'
    export INC='include'
    export FCOMP=ifort
    export FCFLAGS='-O3 -FR -I$(NEMSIO_INC)'

    make clean
    make ;;

esac

exit
