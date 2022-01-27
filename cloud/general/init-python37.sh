#!/usr/bin/env bash

OPT=${1-$ASGS_INSTALL_PATH}
COMPILER=$2
JOBS=${3-1}

PYTHON3VERSION=3.10.2
PYTHON3DIR=Python-${PYTHON3VERSION}
PYTHON3TGZ=${PYTHON3DIR}.tgz

if [[ "$COMPILER" == "clean" || "$COMPILER" == "rebuild" ]]; then 
  echo cleaning python37 
  cd $OPT/bin
  rm -rvf pip3.10 pip3 python3.10 python3.10-config pydoc3.10 idle3.10 2to3-3.10 python3-config python3 pydoc3 idle3 2to3
  cd $OPT/lib
  rm -rvf libpython3.10.a python3.10 pkgconfig
  cd $OPT/include
  rm -rvf ./python3.10
  cd $OPT/share/man/man1
  rm -rvf python3.10.1 python3.1
  cd $_ASGS_TMP
  rm -rfv ${PYTHON3DIR}* > /dev/null 2>&1
  exit

  # stop here if 'clean', proceed if 'rebuild'
  if [ "$COMPILER" == "clean" ]; then
    exit
  fi
fi

if [ -x ${OPT}/bin/python37 ]; then
  printf "(warn) 'python37' was found in $OPT/bin/python37; to rebuild run,\n\n\tbuild python37 rebuild\n\n"
  exit
fi

cd $_ASGS_TMP

if [ ! -e ${PYTHON3TGZ} ]; then
  wget --no-check-certificate https://www.python.org/ftp/python/3.10.2/Python-3.10.2.tgz 
fi

rm -rf ./$PYTHON3DIR 2> /dev/null

tar zxvf ./$PYTHON3TGZ

cd $PYTHON3DIR
./configure --prefix $ASGS_INSTALL_PATH
make         && \
make install

exit;

# no errors, so clean up
if [ "$?" == 0 ]; then
  echo ...cleaning build scripts and downloads
  cd $_ASGS_TMP
  rm -rfv ${PYTHON3DIR}* > /dev/null 2>&1
  echo
  echo "Installation of 'python37' appears to have gone well"
  printf "Output of 'which python37':\n\n\t%s\n" $(which python37)
  echo
fi
