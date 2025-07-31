#!/usr/bin/env bash

for A in $(list adcirc | awk '{print $2}'); do
  load adcirc "$A"
  verify adcirc
  ERR=$?
  if [ $ERR != 0 ]; then
    echo error verifying "$A"
    exit $ERR
  fi 
done
