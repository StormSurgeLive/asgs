#!/usr/bin/bash

if [ ! -d $HOME/adcirc-cg/work ]; then
  git clone https://github.com/adcirc/adcirc-cg.git
fi
cd $HOME/adcirc-cg/work
export PATH=$PATH:$HOME/opt/openmpi-1.8.1/bin
make all compiler=gfortran NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable MACHINENAME=jason-desktop
