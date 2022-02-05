#!/usr/bin/env bash

ACTION=${1-install}
OPT=${2-$ASGS_INSTALL_PATH}
JOBS=${3-1}

PYTHON3VERSION=3.7.9
PYTHON3DIR=Python-${PYTHON3VERSION}
PYTHON3TGZ=${PYTHON3DIR}.tgz

if [[ "$ACTION" == "clean" || "$ACTION" == "rebuild" ]]; then 
  echo cleaning python3 
  cd $OPT/bin
  rm -rvf pyvenv-3.7 pyvenv python3-config python3.7m-config python3.7m python3.7-config python3.7 python3 \
          t pydoc3.7 pydoc3 pip3.7 pip3 pip idle3.7 idle3 easy_install-3.7 2to3-3.7 2to3
  cd $OPT/lib
  rm -rvf *python* pkgconfig/*python* 
  cd $OPT/include
  rm -rvf ./python3.7m
  cd $OPT/share/man/man1
  rm -rvf python3.1 python3.7
  cd $_ASGS_TMP
  rm -rfv ${PYTHON3DIR}* > /dev/null 2>&1

  # stop here if 'clean', proceed if 'rebuild'
  if [ "$ACTION" == "clean" ]; then
    exit
  fi
fi

if [ -x ${OPT}/bin/python3.7 ]; then
  printf "(warn) 'python3' was found in $OPT/bin/python3; to rebuild run,\n\n\tbuild python3 rebuild\n\n"
  exit
fi

cd $_ASGS_TMP

if [ ! -e ${PYTHON3TGZ} ]; then
  wget --no-check-certificate https://www.python.org/ftp/python/${PYTHON3VERSION}/${PYTHON3TGZ}
fi

rm -rf ./$PYTHON3DIR 2> /dev/null

tar zxvf ./$PYTHON3TGZ

cd $PYTHON3DIR
./configure --prefix $OPT
make         && \
make install

# upgrade pip3 and install things we need
python3.7 -m pip install --upgrade pip
pip3 install netCDF4
pip3 install pika

# no errors, so clean up
if [ "$?" == 0 ]; then
  echo ...cleaning build scripts and downloads
  cd $_ASGS_TMP
  rm -rfv ${PYTHON3DIR}* > /dev/null 2>&1
  echo
  echo "Installation of 'python 3.7' appears to have gone well"
  printf "Output of 'which python 3.7':\n\n\t%s\n" $(which python3.7)
  echo
fi
