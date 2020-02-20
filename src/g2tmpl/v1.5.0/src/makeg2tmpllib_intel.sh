#!/bin/sh

case ${COMP:?} in
  intel)
    export FC=${FC:-ftn}
    export CC=${CC:-cc}
    export flagOpt="-O3 -axCore-AVX2 -g"
    export flagFort="-free -module"
  ;;
  cray)
    export FC=${FC:-ftn}
    export CC=${CC:-cc}
    export flagOpt="-O2 -G2"
    export flagFort="-f free -J"
  ;;
  *)
    >&2 echo "Don't know how to build lib under $COMP compiler"
    exit 1
  ;;
esac

export LIB=${G2TMPL_LIB:-../${COMP}/libg2tmpl_${version}.a}
export INCMOD=${G2TMPL_INC:-../${COMP}/include}
mkdir -p $(dirname $LIB) $INCMOD
make -f makefile_cray
