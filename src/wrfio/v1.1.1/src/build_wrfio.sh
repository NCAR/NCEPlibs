SHELL=/bin/sh

module purge
set -x
mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)

if [ $mac = v -o $mac = m  ] ; then   # For Dell
 machine=wcoss_dell_p3
 . $MODULESHOME/init/bash
 module load ips/18.0.1.163
 module load impi/18.0.1
 module load NetCDF/3.6.3
 export  mySFC=ifort
 
elif [ $mac = l -o $mac = s ] ; then   #    wcoss_c (i.e. luna and surge)
 export machine=cray-intel
 module load PrgEnv-intel
 module load craype-sandybridge
 module switch intel intel/15.0.3.187
 module load craype/2.3.0
 module load NetCDF-intel-sandybridge/3.6.3
 export  mySFC=ftn
fi

module list
make clean
make
