#!/usr/bin/env bash

if [ -z "$(which adcirc)" ]; then
  echo ADCIRC not found - runt "'initadcirc'" to build ADCIRC
  echo or "'list adcirc'" to find an ADCIRC build to load using "'load adcirc <adcirc-build-name>'"
  echo exiting test ...
  exit
fi

mkdir -p /tmp/$$-adcirc-test
pushd /tmp/$$-adcirc-test
wget https://www.dropbox.com/s/1wk91r67cacf132/NetCDF_shinnecock_inlet.tar.bz2 > /dev/null 2>&1
bunzip2 NetCDF_shinnecock_inlet.tar.bz2 > /dev/null 2>&1
tar xvf NetCDF_shinnecock_inlet.tar > /dev/null 2>&1
# simple check, so just run for 0.5 days
sed -i 's/5.0                                      ! RNDAY/0.1                                      ! RNDAY/g' ./fort.15
adcirc
if [ 0 -eq $? ]; then
  echo ok adcirc appears to work
else
  echo not ok - something went wrong with adcirc
fi

for n in 2; do
  adcprep<<EOF
2
1
fort.14
EOF
  adcprep<<EOF
$n
2
EOF
  mpirun -n $n padcirc
done
popd
rm -rvf /tmp/$$-adcirc-test > /dev/null
