#!/usr/bin/env bash

while getopts "k" optname; do
   case $optname in
      k) KEEP=1
         ;;
   esac
done


which adcirc > /dev/null 2>&1
if [ $? -gt 0 ]; then
  echo ADCIRC not found - run "'build adcirc'" to build ADCIRC
  echo or "'list adcirc'" to find an ADCIRC build to load using "'load adcirc <adcirc-build-name>'"
  echo exiting test ...
  exit 1 
fi

echo
mkdir -p /tmp/$$-adcirc-test
pushd /tmp/$$-adcirc-test
echo

wget -4 https://www.dropbox.com/s/1wk91r67cacf132/NetCDF_shinnecock_inlet.tar.bz2 > /dev/null 2>&1
bunzip2 NetCDF_shinnecock_inlet.tar.bz2 > /dev/null 2>&1
tar xvf NetCDF_shinnecock_inlet.tar > /dev/null 2>&1

# simple check, so just run for 0.5 days
sed -i 's/5.0                                      ! RNDAY/0.1                                      ! RNDAY/g' ./fort.15
adcirc && ok=1
if [ "$ok" == 1 ]; then
  echo ok adcirc appears to work
else
  echo not ok - something went wrong with adcirc, skipping adcprep and padcirc test
  exit 1
fi

ok=0

adcprep<<EOF
2
1
fort.14
EOF
adcprep<<EOF
2
2
EOF

if [[ -d "./PE0000" && -d "./PE0001" ]]; then
  echo ok adcprep worked ... attempting to run padcirc
# look for mpiexec
  mpirun -np 2 padcirc && ok=1
  if [ "$ok" == 1 ]; then
    echo
    echo ok padcirc appears to work
  else
    echo "'mpirun -np 2 padcirc' failed, but it could be due to head node restrictions (check error)."
    echo $PADCIRCOUT
  fi
else
  echo not ok - something went wrong with adcprep
fi

popd > /dev/null

if [[ -z "$KEEP" ]]; then
  rm -rvf /tmp/$$-adcirc-test > /dev/null
else
  echo
  echo "Run directory has been retained, to inspect:"
  echo
  echo "  cd /tmp/$$-adcirc-test"
  echo
fi
