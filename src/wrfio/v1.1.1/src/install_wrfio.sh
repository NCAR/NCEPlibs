SHELL=/bin/sh

set +x
mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)

export version=v1.1.1
echo "========================================"
echo "= install lib ($version)               ="
echo "========================================"

if [ $mac = v -o $mac = m  ] ; then   # For Dell
  machine=wcoss_dell_p3
  export libdir=../ips/18.0.1
  mkdir -p -m 775 $libdir
  cp -p libwrfio.a  $libdir/libwrfio_$version.a
elif [ $mac = l -o $mac = s ] ; then   #    wcoss_c (i.e. luna and surge)
  machine=wcoss_cray
  export libdir=../intel
  mkdir -p -m 775 $libdir
  cp -p libwrfio.a  $libdir/libwrfio_$version.a 
fi
ls -l $libdir/libwrfio_$version.a
