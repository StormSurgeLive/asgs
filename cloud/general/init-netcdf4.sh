#!/bin/bash

OPT=${1:-$ASGS_INSTALL_PATH}
COMPILER=${2:-intel}
JOBS=${3:-1}
NETCDF4_C_VERSION=${4:-"4.9.2"}
NETCDF4_F_VERSION=${5:-"4.6.2"};

mkdir -p $OPT 2> /dev/null

HDF5_USE_FILE_LOCKING=FALSE

_ASGS_TMP=${ASGS_TMPDIR:-/tmp/${USER}-asgs}

if [ $2 == "clean" ]; then
  echo Cleaning NetCDF libraries and utilities
  cd $OPT/bin
  rm -fv nc-config ncdump ncgen3 nccopy ncgen nf-config
  cd $OPT/lib
  rm -fv libnetcdf*
  cd $OPT/include
  rm -fv netcdf*
  rm -rvf $_ASGS_TMP/netcdf-${NETCDF4_C_VERSION}*
  rm -rvf $_ASGS_TMP/netcdf-fortran-${NETCDF_F_VERSION}*
  cd $OPT/share/info
  rm -rvf netcfg*
  cd $OPT/share/man/man1
  rm -rvf nc*
  cd $OPT/share/man/man3
  rm -rvf netcdf*
  exit
fi

if [ $COMPILER == "intel" ]; then
  export CC=icc
  export FC=ifort
  export CXX=icpc
fi
if [ $COMPILER == "intel-oneapi" ]; then
  export CC=icx
  export FC=ifx
  export CXX=icx
fi
if [ $COMPILER == "gfortran" ]; then
  export CC=gcc
  export FC=gfortran
  export CXX=g++
fi

mkdir -p $_ASGS_TMP 2> /dev/null
chmod 700 $_ASGS_TMP
cd $_ASGS_TMP

if [ ! -e netcdf-${NETCDF4_C_VERSION}.tar.gz ]; then
  echo wget --no-check-certificate https://github.com/Unidata/netcdf-c/archive/refs/tags/v${NETCDF4_C_VERSION}.tar.gz
  wget --no-check-certificate https://github.com/Unidata/netcdf-c/archive/refs/tags/v${NETCDF4_C_VERSION}.tar.gz
fi

if [ ! -e netcdf-fortran-${NETCDF4_F_VERSION}.tar.gz ]; then
  echo wget --verbose https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v${NETCDF4_F_VERSION}.tar.gz
  wget --verbose https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v${NETCDF4_F_VERSION}.tar.gz
fi

mkdir -p $OPT 2> /dev/null

if [ ! -e $OPT/bin/nc-config ]; then
  tar zxvf v${NETCDF4_C_VERSION}.tar.gz
  cd netcdf-c-${NETCDF4_C_VERSION}
  make clean
  ./configure --enable-netcdf-4 --disable-dap --disable-byterange --disable-remote-tests --disable-libxml2 --prefix $OPT
  make -j $JOBS install
  cd ..
fi

if [ ! -e $OPT/bin/nc-config ]; then
  echo something went wrong with NETCDF
  exit 1
else
  rm -rvf $_ASGS_TMP/netcdf-${NETCDF4_C_VERSION}*
fi

if [[ ! -L $OPT/lib/libnetcdff.so || ! -e $(readlink -f  $OPT/lib/libnetcdff.so) ]]; then
  tar zxvf v${NETCDF4_F_VERSION}.tar.gz
  cd netcdf-fortran-${NETCDF4_F_VERSION}
  make clean
  ./configure --enable-netcdf-4 --disable-dap --disable-byterange --disable-remote-tests --disable-libxml2 --prefix $OPT
  make -j $JOBS install
  cd ..
fi

if [[ ! -L $OPT/lib/libnetcdff.so || ! -e $(readlink -f  $OPT/lib/libnetcdff.so) ]]; then
  echo something went wrong with NETCDF Fortran support
  exit 1
else
  rm -rvf $_ASGS_TMP/netcdf-fortran-${NETCDF_F_VERSION}*
fi
