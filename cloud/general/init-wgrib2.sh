#!/bin/bash

OPT=${1:-$ASGS_INSTALL_PATH}
COMPILER=${2:-intel}

VERSION=-3.1.1

if [ $COMPILER == "clean" ]; then
  make clean
  rm -rfv grib2
  rm -fv  wgrib2${VERSION}.tgz 
  exit
fi

pushd $SCRIPTDIR

export COMP_SYS=gnu_linux
export CC=gcc
export FC=gfortran
export CXX=g++


if [ ! -e wgrib2${VERSION}.tgz ]; then
  #wget https://www.ftp.cpc.ncep.noaa.gov/wd51we/wgrib2/wgrib2.tgz
  wget https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/wgrib2${VERSION}.tgz
fi

rm -rf grib2
tar zxvf wgrib2${VERSION}.tgz

cd grib2
make compiler=gfortran
cp $SCRIPTDIR/grib2/wgrib2/wgrib2 $SCRIPTDIR/bin

popd
