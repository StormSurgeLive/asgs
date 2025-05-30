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
  #wget https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/wgrib2-${VERSION}.tgz -O wgrib2-${VERSION}.tgz
  wget https://ftp.cpc.ncep.noaa.gov/wd51we/wgrib2/wgrib2.tgz.v${VERSION} -O wgrib2-${VERSION}.tgz
fi

rm -rf grib2 > /dev/null 2>&1
tar zxvf wgrib2-${VERSION}.tgz 

if [ "$compiler" == "gfortran" ]; then
  CC=gcc
  FC=gfortran
  COMP_SYS=gnu_linux
fi
if [ "$compiler" == "intel" ]; then
  CC=icc
  FC=ifort
  COMP_SYS=intel_linux
fi
if [ "$compiler" == "intel-oneapi" ]; then
  CC=icx
  FC=ifx
  COMP_SYS=oneapi_linux
fi

make -j 1 NETCDFPATH=$OPT NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable MACHINE_NAME=$ASGS_MACHINE_NAME compiler=$COMPILER

cp $SCRIPTDIR/grib2/wgrib2/wgrib2 $SCRIPTDIR/bin > /dev/null 2>&1

rm -rfv grib2
rm -fv  wgrib2-${VERSION}.tgz

popd
