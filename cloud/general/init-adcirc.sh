#!/usr/bin/env bash

# meant to be run under the asgs-brew.pl environment

if [ ! -d $HOME/adcirc-cg/work ]; then
  read -p "Can't find ADCIRC directory in $HOME/adcirc-cg. Should we attempt to get it from GitHub? [y|Y|n|N]? " yesOrNo
  if [ "$yesOrNo" = "y" ] || [ "$yesOrNo" = "Y" ]; then
    cd $HOME
    git clone https://github.com/adcirc/adcirc-cg.git
    cd $HOME//adcirc-cg
    git checkout v53release
  else
    echo "Make ADCIRC source available $HOME/adcirc-cg, then try again..."
    exit 1
  fi
fi

TODO=${1-make};

cd $HOME/adcirc-cg
cd $HOME/adcirc-cg/work
if [ "$TODO" = "clean" ]; then
  make clean clobber
else
  #make clean clobber
  make all adcswan padcswan hstime aswip compiler=$ADCCOMPILER NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable NETCDFHOME=$NETCDFHOME MACHINENAME=$MACHINENAME
fi
