#!/usr/bin/env bash

which adcirc > /dev/null 2>&1
if [ $? -gt 0 ]; then
  echo ADCIRC not found - run "'build adcirc'" to build ADCIRC
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
adcirc > /dev/null
if [ 0 -eq $? ]; then
  echo ok adcirc appears to work
else
  echo not ok - something went wrong with adcirc, skipping adcprep and padcirc test
  exit
fi

for n in 2; do
  adcprep<<EOF > /dev/null
2
1
fort.14
EOF
  adcprep<<EOF > /dev/null
$n
2
EOF
if [ 0 -eq $? ]; then
  echo ok adcprep appears to work ... attempting to run padcirc
  IFS=
  PADCIRCOUT=$(mpirun -n $n padcirc 2>&1)
  if [ 0 -eq $? ]; then
    echo ok padcirc appears to work
  else
    echo "'mpirun -n $n padcirc' failed, but it could be due to head node restrictions (check error)."
    echo $PADCIRCOUT
  fi
else
  echo not ok - something went wrong with adcprep
fi
done
popd
rm -rvf /tmp/$$-adcirc-test > /dev/null
