#case ${COMP:?} in
#  intel)
#    export FC=${1:-ifort}
#  ;;
#  cray)
#    export FC=${1:-ftn}
#  ;;
#  *)
#    >&2 echo "Don't know how to build lib under $COMP compiler"
#    exit 1
#  ;;
#esac
export FC=${1:-ftn}

export LIB=${SFCIO_LIB4:-../${COMP}/libsfcio_${SFCIO_VER}_4.a}
export INCMOD=${SFCIO_INC4:-../${COMP}/include/sfcio_${SFCIO_VER}_4}
mkdir -p $INCMOD

make -f makefile_4
