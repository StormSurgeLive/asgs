#!/bin/bash

OPT=${1:-$ASGS_INSTALL_PATH}
COMPILER=${2:-intel}

VERSION=3.1.2

if [ $COMPILER == "clean" ]; then
  make clean
  rm -rfv grib2
  rm -fv  wgrib2-${VERSION}.tgz 
  exit
fi

pushd $SCRIPTDIR

if [ ! -e wgrib2${VERSION}.tgz ]; then
  wget -4 https://ftp.cpc.ncep.noaa.gov/wd51we/wgrib2/wgrib2.tgz.v${VERSION} -O wgrib2-${VERSION}.tgz
fi

rm -rf grib2 > /dev/null 2>&1
tar zxvf wgrib2-${VERSION}.tgz 

if [ "$COMPILER" = "gfortran" ]; then
  export CC=gcc
  export FC=gfortran
  export COMP_SYS=gnu_linux

elif [ "$COMPILER" = "intel" ]; then
  export CC=icc
  export FC=ifort
  export COMP_SYS=intel_linux

elif [ "$COMPILER" = "intel-oneapi" ]; then
  if command -v mpiifx >/dev/null 2>&1; then
    export CC=icx
    export FC=ifx
    export COMP_SYS=oneapi_linux
  else
    echo "mpiifx not found; falling back to Intel classic toolchain"
    export CC=icc
    export FC=ifort
    export COMP_SYS=intel_linux
  fi

else
  echo "ERROR: Unknown compiler '$COMPILER'"
  exit 1
fi

make -j 1 NETCDFPATH=$OPT NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable MACHINE_NAME=$ASGS_MACHINE_NAME compiler=$COMPILER

cp $SCRIPTDIR/grib2/wgrib2/wgrib2 $SCRIPTDIR/bin > /dev/null 2>&1

rm -rfv grib2
rm -fv  wgrib2-${VERSION}.tgz

popd
