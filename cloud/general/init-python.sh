#!/bin/bash

ACTION=${1-install}
OPT=${2-$HOME/opt}
PYTHON_VERSION=2.7.17
TMP=$HOME/tmp

if [ "$ACTION" == "clean" ]; then
  rm -rfv /home/estrabd/opt/python/2.7.17 $HOME/.local
  rm -rfv $PYTHONPATH
  echo
  echo Run again without clean flag to install
  echo
  exit
fi

mkdir -p $TMP 2> /dev/null
cd $TMP

if [ ! -e ./Python-2.7.17.tgz ]; then
  wget https://www.python.org/ftp/python/2.7.17/Python-2.7.17.tgz
fi

rm -rf ./Python-2.7.17 2> /dev/null

if [ ! -e $PYTHONPATH/bin/python ]; then
  tar zxvf ./Python-2.7.17.tgz && \
    cd Python-2.7.17
  mkdir -p $OPT/python/2.7.17 2> /dev/null
  ./configure --prefix=$PYTHONPATH && \
  make -j 8 && \
    make install
fi

_install_asgs_python_modules () {
which python
echo $PATH
echo $PYTHONPATH
   cd $TMP
   wget https://bootstrap.pypa.io/get-pip.py -O ./get-pip.py
   python - --user < ./get-pip.py
   pip install --user numpy -I
   pip install --user pika -I
   pip install --user netCDF4 -I
   pip install --user python-pptx -I
}

_install_asgs_python_modules
