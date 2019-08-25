#!/bin/bash

if [ ! -d $HOME/asgs ]; then
  git clone https://github.com/wwlwpd/asgs.git
fi

cd ./asgs
git checkout issue-107-vagrant
./cloud/general/asgs-brew.pl --install-path=$HOME/opt --compiler=gfortran --machinename=vagrant --make-jobs=2
