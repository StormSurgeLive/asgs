#!/bin/bash

OPT=${1:-$ASGS_INSTALL_PATH}
COMPILER=${2:-intel}
JOBS=${3:-1}
HDF5_VERSION=${4:-1.12.2}

# may need to make this available in asgs generally
HDF5_USE_FILE_LOCKING=FALSE

_TMPDIR=${TMPDIR:-/tmp/${USER}-asgs}
_ASGS_TMP=${ASGS_TMPDIR:-$_TMPDIR}

if [ $2 == "clean" ]; then
  echo Cleaning HDF5 libraries and utilities
  cd $OPT/bin
  rm -fv gif2h5 h5cc h5debug h5dump h5import h5ls h5perf_serial h5repack h5stat h52gif h5copy h5diff h5fc h5jam h5mkgrp h5redeploy h5repart h5unjam
  cd $OPT/lib
  rm -fv libhd5* libhdf5*
  cd $OPT/include
  rm -fv H5* h5* hd5* HD5* hdf* typesizes.mod
  rm -rvf $_ASGS_TMP/hdf5-${HDF5_VFERSION}*
  cd $OPT/share
  rm -rvf hdf5_examples
  cd $_ASGS_TMP
  rm -rvf hdf5-${HDF5_VERSION}.tar.gz hdf5-${HDF5_VERSION}
  exit
fi

if [ $COMPILER == "intel" ]; then
  echo Compiling HDF5 with Intel suite
  export CC=icc
  export FC=ifort
  export CXX=icpc
fi
if [ $COMPILER == "gfortran" ]; then
  echo Compiling HDF5 with GCC suite
  export CC=gcc
  export FC=gfortran
  export CXX=g++
fi

mkdir -p $_ASGS_TMP 2> /dev/null
chmod 700 $_ASGS_TMP
cd $_ASGS_TMP

if [ ! -e hdf5-${HDF5_VERSION}.tar.gz ]; then
  wget --no-check-certificate https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/hdf5-${HDF5_VERSION}.tar.gz
fi

mkdir -p $OPT 2> /dev/null

if [ ! -e $OPT/bin/h5diff ]; then
  tar zxvf hdf5-${HDF5_VERSION}.tar.gz
  cd hdf5-${HDF5_VERSION}
  make clean
  ./configure --prefix $OPT --enable-fortran
  make -j $JOBS install
  cd ../
fi

if [ ! -e $OPT/bin/h5diff ]; then
  echo something went wrong with HD5
  exit 1
else
  # no errors, so clean up
  echo cleaning build scripts and downloads
  cd $_ASGS_TMP
  rm -rvf hdf5-${HDF5_VERSION}.tar.gz hdf5-${HDF5_VERSION}
fi
