#!/usr/bin/env bash

PWD=$(pwd)
PWD=${PWD##*/}
BKPTGZ=${1:-$HOME/ASGS-SAVE-${PWD}.tgz}

# purge directory of anything not in the git repo
git clean -x -d -f

tar zxvf $BKPTGZ

