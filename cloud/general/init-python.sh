#!/bin/bash

ACTION=${1-install}
OPT=${2-$PYTHONPATH}
PYTHON_VERSION=${3-2.7.18}
PIP_URL=https://bootstrap.pypa.io/pip/2.7/get-pip.py

if [ "$ACTION" == "clean" ]; then
  # remove installed binaries from upstream build
  rm -rfv $OPT
  if [ -e "$_ASGS_TMP/Python-${PYTHON_VERSION}" ]; then
    rm -rfv $_ASGS_TMP/Python-${PYTHON_VERSION} $_ASGS_TMP/Python-${PYTHON_VERSION}.tgz
  fi
  echo
  echo Run again without clean flag to install
  echo
  exit
fi

mkdir -p $_ASGS_TMP 2> /dev/null
chmod 700 $_ASGS_TMP
cd $_ASGS_TMP

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
   cd $_ASGS_TMP
 
   # this must use a deprecated and archived version of the install pip script
   wget $PIP_URL -O ./get-pip.py
   python - --prefix=$PYTHONPATH < ./get-pip.py
   pip install 'pika==1.1.0'    -I --force-reinstall --ignore-installed --prefix=$PYTHONPATH
   # version constrain is due to Unidata's drop in support of Python 2.7 in v1.5.4 of the netCDF4 module
   pip install 'numpy==1.16.6'  -I --force-reinstall --ignore-installed --prefix=$PYTHONPATH
   # NOTE: installation of this netCDF4 installs numpy 1.16.6 if not specified before
   pip install 'netCDF4==1.5.2' -I --force-reinstall --ignore-installed --prefix=$PYTHONPATH
   pip install python-pptx      -I --force-reinstall --ignore-installed --prefix=$PYTHONPATH
}

_install_asgs_python_modules
