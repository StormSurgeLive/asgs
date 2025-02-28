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

if [ ! -e wgrib2${VERSION}.tgz ]; then
  wget https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/wgrib2${VERSION}.tgz
fi

rm -rf grib2 > /dev/null 2>&1
tar zxvf wgrib2${VERSION}.tgz > /dev/null 2>&1

pwd
make -j 1 NETCDFPATH=$asgs_install_path NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable MACHINE_NAME=$asgs_machine_name compiler=$COMPILER
cp $SCRIPTDIR/grib2/wgrib2/wgrib2 $SCRIPTDIR/bin > /dev/null 2>&1

rm -rfv grib2
rm -fv  wgrib2${VERSION}.tgz

popd
