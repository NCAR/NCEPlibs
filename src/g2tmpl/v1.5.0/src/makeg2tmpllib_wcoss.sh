#!/bin/sh

export LIB=${G2TMPL_LIB:-../libg2tmpl_${version}.a}
export INCMOD=${G2TMPL_INC:-../include/g2tmpl_${version}}
mkdir -p $(dirname $LIB) $INCMOD
make -f makefile_wcoss
