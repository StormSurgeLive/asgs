#!/usr/bin/bash

TMP=$HOME/tmp 2> /dev/null
mkdir -p $TMP
PREFIX=$HOME/opt/openmpi-1.8.1 2> /dev/null
mkdir -p $PREFIX
cd $TMP
wget https://www.open-mpi.org/software/ompi/v1.8/downloads/openmpi-1.8.1.tar.gz
tar -xvf openmpi-1.8.1.tar.gz
cd openmpi-1.8.1
./configure --prefix=$PREFIX
make
make install
