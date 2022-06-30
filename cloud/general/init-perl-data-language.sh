#!/usr/bin/env bash

# Currently just assumes cpanm, installs modules listed
# below and their dependencies

COMPILER=${2}

if [[ "$COMPILER" == "clean" || "$COMPILER" == "rebuild" ]]; then 
  cpanm -U PDL
  if [[ "$COMPILER" == "clean" ]]; then 
    exit
  fi
fi

cpanm --verbose PDL
