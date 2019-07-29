#!/bin/sh

export CC=icc
export FC=ifort
export CXX=icpc
#export CPP=

TMP=$HOME/tmp
OPT=$HOME/opt/usr

export LD_LIBRARY_PATH=$OPT/lib:$LD_LIBRARY_PATH
export LD_INCLUDE_PATH=$OPT/lib:$LD_INCLUDE_PATH
export CPPFLAGS=-I$OPT/include
export LDFLAGS=-L$OPT/lib

mkdir -p $TMP
cd $TMP

if [ ! -e hdf5-1.8.12.tar.gz ]; then
  wget https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/hdf5-1.8.12.tar.gz
  wget https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/netcdf-4.2.1.1.tar.gz
  wget https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/netcdf-fortran-4.2.tar.gz
fi

mkdir -p $OPT 2> /dev/null

if [ ! -e $OPT/bin/h5diff ]; then
  tar zxvf hdf5-1.8.12.tar.gz
  cd hdf5-1.8.12
  ./configure --prefix $OPT --enable-fortran
  make -j4 install
  cd ../
fi

if [ ! -e $OPT/bin/h5diff ]; then
  echo something went wrong with HD5
  exit 1
fi

if [ ! -e $OPT/bin/nc-config ]; then
  tar zxvf netcdf-4.2.1.1.tar.gz
  cd netcdf-4.2.1.1
  ./configure --prefix $OPT
  make -j4 install
  cd ..
fi

if [ ! -e $OPT/bin/nc-config ]; then
  echo something went wrong with NETCDF
  exit 1
fi

if [  ! -e $OPT/bin/nf-config ] && [ -z "$($OPT/bin/nf-config --all  | grep '\-\-has\-f90   \-> yes')" ]; then
  tar zxvf netcdf-fortran-4.2.tar.gz
  cd netcdf-fortran-4.2
  ./configure --prefix $OPT
  make -j4 install
  cd ..
fi

if [  ! -e $OPT/bin/nf-config ] && [ -z "$($OPT/bin/nf-config --all  | grep '\-\-has\-f90   \-> yes')" ]; then
  echo something went wrong with NETCDF Fortran support
  exit 1
fi
