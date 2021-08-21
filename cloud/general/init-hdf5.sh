#!/bin/bash

OPT=${1-$HOME/opt}
COMPILER=${2-intel}
JOBS=${3-1}

if [ $2 == "clean" ]; then
  echo Cleaning HDF5 libraries and utilities
  cd $OPT/bin
  rm -fv gif2h5 h5cc h5debug h5dump h5import h5ls h5perf_serial h5repack h5stat h52gif h5copy h5diff h5fc h5jam h5mkgrp h5redeploy h5repart h5unjam
  cd $OPT/lib
  rm -fv libhd5* libhdf5*
  cd $OPT/include
  rm -fv H5* h5* hd5* HD5* hdf* typesizes.mod
  rm -rvf $_ASGS_TMP/hdf5-1.8.12*
  cd $OPT/share
  rm -rvf hdf5_examples
  exit
fi

if [ $COMPILER == "intel" ]; then
  export CC=icc
  export FC=ifort
  export CXX=icpc
fi
if [ $COMPILER == "gfortran" ]; then
  export CC=gcc
  export FC=gfortran
  export CXX=g++
fi

mkdir -p $_ASGS_TMP 2> /dev/null
chmod 700 $_ASGS_TMP
cd $_ASGS_TMP

if [ ! -e hdf5-1.8.12.tar.gz ]; then
  wget --verbose https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/hdf5-1.8.12.tar.gz
fi

mkdir -p $OPT 2> /dev/null

if [ ! -e $OPT/bin/h5diff ]; then
  tar zxvf hdf5-1.8.12.tar.gz
  cd hdf5-1.8.12
  make clean
  ./configure --prefix $OPT --enable-fortran
  make -j $JOBS install
  cd ../
fi

if [ ! -e $OPT/bin/h5diff ]; then
  echo something went wrong with HD5
  exit 1
fi
