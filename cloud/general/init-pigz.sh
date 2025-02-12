#!/usr/bin/env bash

OPT=${1:-$ASGS_INSTALL_PATH}
COMPILER=$2
JOBS=${3:-1}

_ASGS_TMP=${ASGS_TMPDIR:-/tmp/${USER}-asgs}

mkdir -p $_ASGS_TMP 2> /dev/null
chmod 700 $_ASGS_TMP

if [[ "$COMPILER" == "clean" || "$COMPILER" == "rebuild" ]]; then
  echo cleaning pigz
  pushd $OPT > /dev/null 2>&1
  rm -vf ./bin/pigz ./bin/unpigz
  popd > /dev/null 2>&1

  # stop here if 'clean', proceed if 'rebuild'
  if [ "$COMPILER" == "clean" ]; then
    exit
  fi
fi

if [ -x ${OPT}/bin/pigz ]; then
  printf "(warn) 'pigz' was found in $OPT/bin/pigz; to rebuild run,\n\n\tbuild pigz rebuild\n\n"
  exit
fi

PIGZ_VERSION=2.8
PIGZ_DIR=pigz
PIGZ_TGZ=${PIGZ_DIR}-${PIGZ_VERSION}.tar.gz
cd $_ASGS_TMP

#e.g., https://zlib.net/pigz/pigz-2.8.tar.gz
if [ ! -e ${PIGZ_TGZ} ]; then
  wget --no-check-certificate https://zlib.net/${PIGZ_DIR}/${PIGZ_TGZ}
fi

rm -rf ./${PIGZ_DIR}-${PIGZ_VERSION} 2> /dev/null

tar zxvf ./${PIGZ_TGZ}

cd ${PIGZ_DIR}-${PIGZ_VERSION}
make        && \
cp -v ./pigz ./unpigz $ASGS_INSTALL_PATH/bin

# no errors, so clean up
if [ "$?" == 0 ]; then
  echo ...cleaning build scripts and downloads
  cd $_ASGS_TMP
  rm -rfv ${PIGZ_DIR}* > /dev/null 2>&1
  echo
  echo "Installation of 'pigz' appears to have gone well"
  echo
  printf "Output of 'which pigz':\n\n\t%s\n" $(which pigz)
  echo
  printf "Output of 'which unpigz':\n\n\t%s\n" $(which unpigz)
  echo
fi
