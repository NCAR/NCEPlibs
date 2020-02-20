#!/bin/sh

case ${COMP:?} in
  intel)
    #export FC=${1:-ifort}
    export FC=${1:-ftn}
    export flagOpt="-O3 -axCore-AVX2"
    export flagFort="-traceback -FR -convert big_endian -module"
  ;;
  cray)
    export FC=${1:-ftn}
    export flagOpt="-O2"
    export flagFort="-G2 -ffree -hbyteswapio -J"
  ;;
  *)
    >&2 echo "Don't know how to build lib under $COMP compiler"
    exit 1
  ;;
esac

export LIB=${GFSIO_LIB4:-../${COMP}/libgfsio_4.a}
export MODDIR=${GFSIO_INC4:-../${COMP}/include/gfsio_4}
mkdir -p $(dirname $LIB) $MODDIR
make -f makefile_4

