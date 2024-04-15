#!/usr/bin/env bash

read -p "Type in the word 'DELETE' if you really wish to delete all non-repo files: " PURGE

if [ "$PURGE" != "DELETE" ]; then
  echo PURGE cancelled...
  exit
fi

PWD=$(pwd)
PWD=${PWD##*/}
BKPTGZ=${1:-$HOME/ASGS-SAVE-${PWD}.tgz}

# NOTE: purges directory of anything not in the git repo
git clean -x -d -f

# remove directories that are known to contain git repos
rm -rvf git/
rm -rvf opt/models/adcircs
