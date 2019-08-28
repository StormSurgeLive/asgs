#!/bin/bash

if [ ! -d $HOME/asgs ]; then
  git clone https://github.com/jasonfleming/asgs.git
fi

cd ./asgs
git checkout master
./cloud/general/asgs-brew.pl --install-path=$HOME/opt --compiler=gfortran --machinename=vagrant --make-jobs=2
