#!/bin/bash

TMP=$HOME/tmp
OPT=${1-$HOME/opt}
COMPILER=${2-intel}
JOBS=${3-1}

if [ $2 == "clean" ]; then 
  echo Cleaning HDF5 and NetCDF libraries and utilities
  cd $OPT/bin
  rm -fv gif2h5 h5cc h5debug h5dump h5import h5ls h5perf_serial h5repack h5stat nc-config ncdump ncgen3 h52gif h5copy h5diff h5fc h5jam h5mkgrp h5redeploy h5repart h5unjam nccopy ncgen nf-config
  cd $OPT/lib
  rm -fv libhd5* libhdf5 libnetcdf*
  cd $OPT/include
  rm -fv netcdf* hd5* HD5* hdf*
  rm -rvf $TMP/hdf5-1.8.12*
  rm -rvf $TMP/netcdf-4.2.1.1*
  rm -rvf $TMP//netcdf-fortran-4.2*
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

mkdir -p $TMP
cd $TMP

if [ ! -e hdf5-1.8.12.tar.gz ]; then
  wget --verbose https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/hdf5-1.8.12.tar.gz
  wget --verbose https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/netcdf-4.2.1.1.tar.gz
  wget --verbose https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/netcdf-fortran-4.2.tar.gz
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

if [ ! -e $OPT/bin/nc-config ]; then
  tar zxvf netcdf-4.2.1.1.tar.gz
  cd netcdf-4.2.1.1
  make clean
  ./configure --prefix $OPT
  make -j $JOBS install
  cd ..
fi

if [ ! -e $OPT/bin/nc-config ]; then
  echo something went wrong with NETCDF
  exit 1
fi

if [  ! -e $OPT/bin/nf-config ] && [ -z "$($OPT/bin/nf-config --all  | grep '\-\-has\-f90   \-> yes')" ]; then
  tar zxvf netcdf-fortran-4.2.tar.gz
  cd netcdf-fortran-4.2
  make clean
  ./configure --prefix $OPT
  make -j $JOBS install
  cd ..
fi

if [  ! -e $OPT/bin/nf-config ] && [ -z "$($OPT/bin/nf-config --all  | grep '\-\-has\-f90   \-> yes')" ]; then
  echo something went wrong with NETCDF Fortran support
  exit 1
fi
