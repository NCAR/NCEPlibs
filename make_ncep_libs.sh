#!/bin/bash
#==========================================================================
#
# Description: This script builds the NCEP libraries used by FV3 V0
#
# Necessary input parameters: host, compiler
#
# Optional input parameters:
#          clean     - cleans *all* exec* directories and library build
#          help      - display valid builds and options
#
# Usage: see function usage below
#
# Examples:
#     > make_ncep_libs_fv3 help
#     > make_ncep_libs clean    
#     > makefv3 theia.intel
#     > makefv3 jet.pgi
#
# set -x     # Uncomment for debugging
#==========================================================================

# Define functions.

function clean {
  cmd="rm -rf exec_*"
  echo "Executing ${cmd}..."
  ${cmd}
  rm -rf ${NCEPLIBS_SRC_DIR}/macros.make
  exit 0
}

function fail    { [ -n "$1" ] && printf "\n%s\n" "$1"; exit 1; }

function usage   { 
  echo "Usage: "
  echo "$THIS_FILE host.compiler | clean | help "
  echo "    Where: host     [required] can be : ${validhosts[@]}"
  echo "           compiler [required] can be : ${validcompilers[@]}"
  echo "           clean    [optional] removes exec directories and generated files"
  echo "           help     [optional] display valid builds and options "
  exit 1
}

function checkvalid {
# Ensure value ($2) of variable ($1) is contained in list of validvalues ($3)
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

NCEPLIBS_SRC_DIR=`pwd`

if [[ $1 = "clean" ]]; then clean; fi
if [[ $1 = "help" ]] ; then usage; fi

THIS_FILE=$(basename "$0" )

#--------------------------------------------------------------
# Define available options
#--------------------------------------------------------------
ARCH=""

validhosts=( theia cheyenne macbook )
validcompilers=( intel pgi gnu )

#--------------------------------------------------------------
# Parse command line arguments
#--------------------------------------------------------------
INPUT_ARGS=$@
for x in ${INPUT_ARGS[@]} 
do
  case $x in
    "clean")    clean        ;;
    "help")     usage        ;;
    *)          ARCH=$x      ;;
  esac
done

if [ -z $ARCH ] ; then usage; fi

#--------------------------------------------------------------
# Get the host and compiler
#--------------------------------------------------------------
number_of_dots=`echo "$ARCH" | grep -o "\." | wc -l`
if [ $number_of_dots -ne 1 ] ; then usage; fi

export MY_HOST=`echo $ARCH | cut -f 1 -d"."`
export COMPILER=`echo $ARCH | cut -f 2 -d"."`

#--------------------------------------------------------------
# Verify valid input for settings easily checked
#--------------------------------------------------------------
checkvalid MY_HOST $MY_HOST ${validhosts[@]}
checkvalid COMPILER $COMPILER ${validcompilers[@]}

#--------------------------------------------------------------
# Look for build configuration file
#--------------------------------------------------------------
echo "Building NCEP libraries for $ARCH..."
echo "  HOST     = $MY_HOST"
echo "  COMPILER = $COMPILER"
echo

#--------------------------------------------------------------
# Get the build root directory
#--------------------------------------------------------------
BUILD_SUFFIX=exec_${ARCH}
export EXEC_DIR="${NCEPLIBS_SRC_DIR}/${BUILD_SUFFIX}"

echo "  Building NCEP libraries for ${ARCH} ${COMP} `date`"

#--------------------------------------------------------------
# Copy appropriate macros.make file in nceplibs dir
#--------------------------------------------------------------
MACROS_FILE=${NCEPLIBS_SRC_DIR}/macros.make
if [ -f ${MACROS_FILE} ]; then
  rm -rf ${MACROS_FILE}
fi
  
cp ${MACROS_FILE}.${MY_HOST}.${COMPILER} ${MACROS_FILE}
 
#--------------------------------------------------------------
# Copy library source to EXEC_DIR and build
#--------------------------------------------------------------
rsync -a --exclude ".svn" macros.make Makefile src ${EXEC_DIR} || fail "rsync of nceplibs"
cd ${EXEC_DIR} || exit 1
make || exit 2
