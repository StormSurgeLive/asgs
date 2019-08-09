#!/usr/bin/bash

TMP=$HOME/tmp
OPT=${1-$HOME/opt}
COMPILER=${2-intel}

OPENMPI_VERSION=openmpi-1.8.1

if [ $2 == "clean" ]; then 
  echo Cleaning OpenMPI libraries and utilities
  rm -rfv $OPT/$OPENMPI_VERSION-$COMPILER
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
mkdir -p $OPT/$OPENMPI_VERSION-$COMPILER
cd $TMP

wget https://www.open-mpi.org/software/ompi/v1.8/downloads/${OPENMPI_VERSION}.tar.gz
tar -xvf $OPENMPI_VERSION.tar.gz
cd $OPENMPI_VERSION 

./configure --prefix=$OPT
make
make install
