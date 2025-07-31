#!/usr/bin/env bash

#
# this script runs through and builds all supported ADCIRC
# versions for testing
#

for V in v53release v55.01-5bc04d6 v55.02 v56.0.2 v56.0.4; do
  init-adcirc.sh -b -N $V
  ERR=$?
  if [ "$ERR" != 0 ]; then
    exit $ERR
  fi
done
