#!/usr/bin/env bash

PWD=$(pwd)
PWD=${PWD##*/}
BKPTGZ=${1:-$HOME/ASGS-SAVE-${PWD}.tgz}

# untar/gunzip
tar zxvf $BKPTGZ

# verify checksums 
# ...
