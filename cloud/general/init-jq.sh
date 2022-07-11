#!/usr/bin/env bash

OPT=${1-$ASGS_INSTALL_PATH}
COMPILER=$2
JOBS=${3-1}

if [[ "$COMPILER" == "clean" || "$COMPILER" == "rebuild" ]]; then
  echo cleaning jq
  pushd $OPT > /dev/null 2>&1
  rm -vf ./share/man/man1/jq.1
  rm -vf ./share/doc/jq
  rm -vf ./lib/libjq.la
  rm -vf ./lib/libjq.so.1
  rm -vf ./lib/libjq.so.1.0.4
  rm -vf ./lib/libjq.a
  rm -vf ./lib/libjq.so
  rm -vf ./bin/jq
  rm -vf ./include/jq.h
  popd $OPT > /dev/null 2>&1
  echo cleaning jq build scripts and downloads
  cd $ASGS_TMPDIR
  rm -rfv ${JQ_DIR}* > /dev/null 2>&1

  # stop here if 'clean', proceed if 'rebuild'
  if [ "$COMPILER" == "clean" ]; then
    exit
  fi
fi

if [ -x ${OPT}/bin/jq ]; then
  printf "(warn) 'jq' was found in $OPT/bin/jq; to rebuild run,\n\n\tbuild jq rebuild\n\n"
  exit
fi

JQ_VERSION=1.6
JQ_DIR=jq-${JQ_VERSION}
JQ_TGZ=${JQ_DIR}.tar.gz
cd $ASGS_TMPDIR

if [ ! -e ${JQ_TGZ} ]; then
  wget --no-check-certificate https://github.com/stedolan/jq/releases/download/${JQ_DIR}/${JQ_TGZ}
fi

rm -rf ./$JQ_DIR 2> /dev/null

tar zxvf ./$JQ_TGZ

cd $JQ_DIR
./configure --prefix $ASGS_INSTALL_PATH --without-oniguruma
make         && \
make install

# no errors, so clean up
if [ "$?" == 0 ]; then
  echo ...cleaning build scripts and downloads
  cd $ASGS_TMPDIR
  rm -rfv ${JQ_DIR}* > /dev/null 2>&1
  echo
  echo "Installation of 'jq' appears to have gone well"
  printf "Output of 'which jq':\n\n\t%s\n" $(which jq)
  echo
fi
