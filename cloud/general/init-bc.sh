#!/usr/bin/env bash

OPT=${1:-$ASGS_INSTALL_PATH}
COMPILER=$2
JOBS=${3:-1}

_ASGS_TMP=${ASGS_TMPDIR:-/tmp/${USER}-asgs}

mkdir -p $_ASGS_TMP 2> /dev/null
chmod 700 $_ASGS_TMP

if [[ "$COMPILER" == "clean" || "$COMPILER" == "rebuild" ]]; then
  echo cleaning bc
  pushd $OPT > /dev/null 2>&1
  rm -vf ./bin/bc
  rm -vf ./bin/dc
  popd $OPT > /dev/null 2>&1
  echo cleaning bc build scripts and downloads
  cd $_ASGS_TMP
  rm -vrf ${BC_DIR}* > /dev/null 2>&1

  # stop here if 'clean', proceed if 'rebuild'
  if [ "$COMPILER" == "clean" ]; then
    exit
  fi
fi

if [ -x ${OPT}/bin/bc ]; then
  printf "(warn) 'bc' was found in $OPT/bin/bc; to rebuild run,\n\n\tbuild bc rebuild\n\n"
  exit
fi

BC_VERSION=1.08.2
BC_DIR=bc-${BC_VERSION}
BC_TGZ=${BC_DIR}.tar.gz
cd $_ASGS_TMP

if [ ! -e ${BC_TGZ} ]; then
  wget -4 https://ftp.gnu.org/gnu/bc/${BC_TGZ}
fi

rm -rf ./$BC_DIR 2> /dev/null

tar zxvf ./$BC_TGZ

cd $BC_DIR
./configure --prefix $ASGS_INSTALL_PATH
make -C lib        && \
make -C bc install && \
make -C dc install

# no errors, so clean up
if [ "$?" == 0 ]; then
  echo ...cleaning build scripts and downloads
  cd $_ASGS_TMP
  rm -rfv ${BC_DIR}* > /dev/null 2>&1
  echo
  echo "Installation of 'bc' appears to have gone well"
  printf "Output of 'which bc':\n\n\t%s\n" $(which bc)
  echo
fi
