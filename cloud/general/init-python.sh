#!/bin/bash

ACTION=${1-install}
OPT=${2-$PYTHONPATH}
PYTHON_VERSION=${3-2.7.18}

TMP=$HOME/tmp

if [ "$ACTION" == "clean" ]; then
  # remove installed binaries from upstream build
  rm -rfv $OPT
  # remove modules and local pip directory
  rm -rfv $HOME/.local
  if [ -e "$TMP/Python-${PYTHON_VERSION}" ]; then
    rm -rfv $TMP/Python-${PYTHON_VERSION} $TMP/Python-${PYTHON_VERSION}.tgz
  fi
  echo
  echo Run again without clean flag to install
  echo
  exit
fi

mkdir -p $TMP 2> /dev/null
cd $TMP

if [ ! -e ./Python-${PYTHON_VERSION}.tgz ]; then
  wget https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tgz
fi
if [ $? != 0 ]; then
  echo Error downloading Python ${PYTHON_VERSION}. Please check access to the URL:
  echo      https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tgz
  exit $?
fi

rm -rf ./Python-${PYTHON_VERSION} 2> /dev/null

if [ ! -e $PYTHONPATH/bin/python ]; then
  tar zxvf ./Python-${PYTHON_VERSION}.tgz && \
    cd Python-${PYTHON_VERSION}
  mkdir -p $OPT/python/${PYTHON_VERSION} 2> /dev/null
  ./configure --prefix=$PYTHONPATH && \
  make -j 8 && \
    make install
fi

_install_asgs_python_modules () {
   cd $TMP
   wget https://bootstrap.pypa.io/get-pip.py -O ./get-pip.py
   python - --user < ./get-pip.py
   pip install --user 'pika==1.1.0'         -I --force-reinstall
   # version constrain is due to Unidata's drop in support of Python 2.7 in v1.5.4 of the netCDF4 module
   pip install --user 'numpy==1.16.6'       -I --force-reinstall
   # NOTE: installation of this netCDF4 installs numpy 1.16.6 if not specified before
   pip install --user 'netCDF4==1.5.2'      -I --force-reinstall
   pip install --user python-pptx           -I --force-reinstall
}

_install_asgs_python_modules
