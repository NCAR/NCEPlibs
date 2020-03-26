#!/bin/bash
#==========================================================================
#
# Description: This script builds the NCEP libraries used by NEMSfv3gfs v1
#
# Usage: see function usage below
#
# Examples:
#     > ./make_ncep_libs.sh -h
#     > ./make_ncep_libs.sh -s hera -c intel -d /scratch4/home/USERNAME/NCEPlibs-20180401 -o 1
#     > ./make_ncep_libs.sh -s cheyenne -c pgi -d /glade/p/work/USERNAME/NCEPlibs-20180401 -o 0
#     > ./make_ncep_libs.sh -s macosx -c gnu -d /usr/local/NCEPlibs-20180401 -o 1
#     > ./make_ncep_libs.sh -s cheyenne -c gnu -d /glade/p/work/USERNAME/NCEPlibs-20180401 -o 0 -a sar
#
#==========================================================================

# Define functions.
function fail    { [ -n "$1" ] && printf "\n%s\n" "$1"; exit 1; }

function usage   { 
  echo "Usage: "
  echo "$THIS_FILE -s system -c compiler -d installdir -o openmp [-m mpi] [-a application] | -h"
  echo "    Where: system      [required] can be : ${validsystems[@]}"
  echo "           compiler    [required] can be : ${validcompilers[@]}"
  echo "           installdir  [required] is the installation destination (must exist)"
  echo "           openmp      [required] is an OpenMP build flag and can be ${validopenmpflags[@]}"
  echo "           mpi         [optional] is an MPI build flag and can be ${validmpiflags[@]} "
  echo "                                  (default 1; if 0, nemsio is not built and application must be 'global')"
  echo "           application [optional] is a flag to build certain sets of libraries for specific applications"
  echo "                                  The default is 'global', the full set of options is: ${validapplicationflags[@]}"
  exit 1
}

NCEPLIBS_SRC_DIR=`pwd`

THIS_FILE=$(basename "$0" )

#--------------------------------------------------------------
# Define available options
#--------------------------------------------------------------
validsystems=( hera theia jet gaea cheyenne macosx linux )
validcompilers=( intel pgi gnu )
validopenmpflags=( 0 1 )
validmpiflags=( 0 1 )
validapplicationflags=( global sar upp all )
#--------------------------------------------------------------
# Parse command line arguments
#--------------------------------------------------------------
while getopts :s:c:d:o:m:a:help opt; do
  case $opt in
    s) SYSTEM=$OPTARG ;;
    c) COMPILER=$OPTARG ;;
    d) NCEPLIBS_DST_DIR=$OPTARG ;;
    o) OPENMP=$OPTARG ;;
    m) MPI=$OPTARG ;;
    a) APP=$OPTARG ;;
    h) usage ;;
    *) usage ;;
  esac
done

# Check if all mandatory arguments are provided
if [ -z $SYSTEM ] ; then echo "ERROR: system argument is required"; echo""; usage; fi
if [ -z $COMPILER ] ; then echo "ERROR: compiler argument is required"; echo""; usage; fi
if [ -z $NCEPLIBS_DST_DIR ] ; then echo "ERROR: installdir argument is required"; echo""; usage; fi
if [ -z $OPENMP ] ; then echo "ERROR: openmp argument is required"; echo""; usage; fi
if [ -z $MPI ] ; then MPI=1; fi
if [ -z $APP ] ; then APP=1; fi #APP is an optional argument

# For back compatability, allow APP to be 0 (global) or 1 (all)
if [ "$APP" == "0" ]; then
  APP="global"
elif [ "$APP" == "1" ]; then
  APP="all"
fi

# Ensure value ($2) of variable ($1) is contained in list of validvalues ($3)
function checkvalid {
  if [ $# -lt 3 ]; then
    echo $FUNCNAME requires at least 3 arguments: stopping
    exit 1
  fi

  var_name=$1 && shift
  input_val=$1 && shift
  valid_vars=($@)

  for x in ${valid_vars[@]}; do
    if [ "$input_val" == "$x" ]; then
      echo "${var_name}=${input_val} is valid."
      return
    fi
  done
  echo "ERROR: ${var_name}=${input_val} is invalid. Valid values are: ${valid_vars[@]}"
  exit 1
}

checkvalid SYSTEM ${SYSTEM} ${validsystems[@]}
checkvalid COMPILER ${COMPILER} ${validcompilers[@]}
checkvalid OPENMP ${OPENMP} ${validopenmpflags[@]}
checkvalid MPI ${MPI} ${validmpiflags[@]}
checkvalid APP ${APP} ${validapplicationflags[@]}

# Consistency check: if MPI is zero, APP must be zero, too
if [ "$MPI" == "0" ]; then
  if [ "$APP" != "global" ]; then
    echo "ERROR: Option -a (application) must be 'global' if MPI is disabled."
    exit 1
  fi
fi

# Application check: if compiling for UPP, openmp must be disabled
if [ "$OPENMP" == "1" ]; then
  if [ "$APP" == "upp" ]; then
    echo "ERROR: Openmp must be disabled (-o 0) when compiling libraries for UPP"
    exit 1
  fi
fi

# Make sure the destination directory exists
if [ -d ${NCEPLIBS_DST_DIR} ]; then
  echo "Destination directory ${NCEPLIBS_DST_DIR} exists."
else
  echo "ERROR: Destination directory ${NCEPLIBS_DST_DIR} does not exist."
  exit 1
fi

#--------------------------------------------------------------
# Check that all libraries are available on this platform
#--------------------------------------------------------------
if [ "$APP" == "all" ]; then
  if [ "${SYSTEM}" != "cheyenne" -a "${SYSTEM}" != "macosx" -a "${SYSTEM}" != "theia" -a "${SYSTEM}" != "hera" ]; then
    echo "ERROR: Compile all option only supported for 'cheyenne', 'macosx', and 'hera' at this time"
    exit 1
  fi
fi
if [ "$APP" == "upp" ] || [ "$APP" == "sar" ]; then
  if [ "${SYSTEM}" != "cheyenne" -a "${SYSTEM}" != "macosx" -a "${SYSTEM}" != "hera" -a "${SYSTEM}" != "linux" ]; then
    echo "ERROR: upp and sar library sets are only supported for 'cheyenne', 'macosx', 'hera', and linux at this time"
    exit 1
  fi
fi

#--------------------------------------------------------------
# For generic Linux/MacOSX systems, check compiler environment
# variables CC, F90, MPIF90, or use default values.
#--------------------------------------------------------------
if [ "${SYSTEM}" == "macosx" -o "${SYSTEM}" == "linux" ] && [ -z "${NOCOMPILERCHOICE}" ]; then
  echo "Checking environment variable CC to overwrite default 'gcc' ..."
  export CC=${CC:-gcc}
  echo "Checking environment variable F90 to overwrite default 'gfortran' ..."
  export FCserial=${F90:-gfortran}
  if [ "$MPI" == "0" ]; then
    export FC=${FCserial}
  else
    echo "Checking environment variable MPIF90 to overwrite default 'mpif90' ..."
    export FC=${MPIF90:-mpif90}
  fi
  echo "Compiler setttings:"
  echo "  CC       = ${CC}"
  echo "  FCserial = ${FCserial}"
  echo "  FC       = ${FC}"
  while true; do
    read -p "Proceed? (y/n) " yn
    case $yn in
      [Yy]* ) break;;
      [Nn]* ) exit;;
      * ) echo "Please answer yes or no.";;
    esac
  done
fi
#--------------------------------------------------------------
# For grib2 libraries, need JASPER and PNG library include files
if [ "$APP" == "upp" ] || [ "$APP" == "all" ]; then
  if [ -z "$JASPER_INC" ] || [ -z "$PNG_INC" ]; then
    echo "ERROR: You must define locations of JASPER (JASPER_INC) and PNG (PNG_INC) library include files to compile upp libraries"
    exit 1
  elif [ ! -d "$JASPER_INC" ] || [ ! -d "$PNG_INC" ]; then
    echo "ERROR: Check your include paths; one or both of these paths does not exist:"
    echo $JASPER_INC
    echo $PNG_INC
    exit 2
  fi
# For wrfio libraries, need NETCDF libs and includes
  if [ -z "$NETCDF_LIB" ] || [ -z "$NETCDF_INC" ]; then
    if [ -z "$NETCDF" ]; then
      echo "ERROR: You must define the locations of NETCDF library (NETCDF_LIB) and include (NETCDF_INC) files to compile upp libraries"
      exit 1
    else
      export NETCDF_LIB="$NETCDF/lib"
      export NETCDF_INC="$NETCDF/include"
    fi
  fi
  if [ ! -d "$NETCDF/lib" ] || [ ! -d "$NETCDF/include" ]; then
    echo "ERROR: Check your NETCDF location; one or both of these paths does not exist:"
    echo $NETCDF/lib
    echo $NETCDF/include
    exit 2
  fi
fi

echo "NETCDF_INC=$NETCDF_INC"
echo "NETCDF_LIB=$NETCDF_LIB"
#--------------------------------------------------------------
# Get the build root directory
#--------------------------------------------------------------
export BUILD_DIR="${NCEPLIBS_SRC_DIR}/exec_${SYSTEM}.${COMPILER}"
echo
echo "Building NCEP libraries in ${BUILD_DIR} ..."
echo

#--------------------------------------------------------------
# Copy appropriate macros.make file
#--------------------------------------------------------------
MACROS_FILE=${NCEPLIBS_SRC_DIR}/macros.make
if [ -f ${MACROS_FILE} ]; then
  rm -rf ${MACROS_FILE}
fi
cp -v ${MACROS_FILE}.${SYSTEM}.${COMPILER} ${MACROS_FILE}
 
#--------------------------------------------------------------
# Copy library source to BUILD_DIR and build
#--------------------------------------------------------------
if [ "$OPENMP" == "1" ]; then
  export OPENMP=${OPENMP}
fi
rsync -a macros.make Makefile src ${BUILD_DIR}
cd ${BUILD_DIR}
if [ "$APP" == "all" ]; then
   make all || fail "An error occurred building the NCEP libraries"
elif [ "$MPI" == "0" ]; then
   make nompi || fail "An error occurred building the NCEP libraries"
elif [ "$APP" == "upp" ]; then
   make upp || fail "An error occurred building the NCEP libraries"
elif [ "$APP" == "sar" ]; then
   make sar || fail "An error occurred building the NCEP libraries"
else
   make global || fail "An error occurred building the NCEP libraries"
fi
export -n OPENMP

#--------------------------------------------------------------
# Install to NCEPLIBS_DST_DIR
#--------------------------------------------------------------
echo
echo "Installing to ${NCEPLIBS_DST_DIR} ..."
echo
rm -fr ${NCEPLIBS_DST_DIR}/lib
rm -fr ${NCEPLIBS_DST_DIR}/include
mkdir ${NCEPLIBS_DST_DIR}/lib
mkdir ${NCEPLIBS_DST_DIR}/include
cp -av ${BUILD_DIR}/include/* ${NCEPLIBS_DST_DIR}/include/
cp -av ${BUILD_DIR}/lib*.a ${NCEPLIBS_DST_DIR}/lib/

if [ "$APP" == "upp" ]; then
   echo "To build UPP, set environment variable NCEPLIBS_DIR to ${NCEPLIBS_DST_DIR}"
else
   echo "To build FV3, set environment variable NCEPLIBS_DIR to ${NCEPLIBS_DST_DIR}"
fi
echo
