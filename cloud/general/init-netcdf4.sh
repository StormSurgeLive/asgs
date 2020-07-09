#!/bin/bash

OPT=${1-$HOME/opt}
COMPILER=${2-intel}
JOBS=${3-1}
TMP=/tmp/$USER-asgs

if [ $2 == "clean" ]; then 
  echo Cleaning NetCDF libraries and utilities
  cd $OPT/bin
  rm -fv nc-config ncdump ncgen3 nccopy ncgen nf-config
  cd $OPT/lib
  rm -fv libnetcdf*
  cd $OPT/include
  rm -fv netcdf*
  rm -rvf $TMP/netcdf-4.2.1.1*
  rm -rvf $TMP/netcdf-fortran-4.2*
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
if [ $COMPILER == "gfortran" ]; then 
  export CC=gcc
  export FC=gfortran
  export CXX=g++
fi

mkdir -p $TMP 2> /dev/null
chmod 700 $TMP
cd $TMP

if [ ! -e netcdf-4.2.1.1.tar.gz ]; then
  wget --verbose https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/netcdf-4.2.1.1.tar.gz
fi

if [ ! -e netcdf-fortran-4.2.tar.gz ]; then
  wget --verbose https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/netcdf-fortran-4.2.tar.gz
fi

mkdir -p $OPT 2> /dev/null

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
