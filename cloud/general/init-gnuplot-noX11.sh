#!/usr/bin/env bash

OPT=${1:-$ASGS_INSTALL_PATH}
COMPILER=$2
JOBS=${3:-1}
_ASGS_TMP=${ASGS_TMPDIR:-/tmp/${USER}-asgs}

mkdir -p $_ASGS_TMP 2> /dev/null
chmod 700 $_ASGS_TMP

if [ "$COMPILER" == "clean" ]; then
  echo cleaning gnuplot
  cd $OPT/bin
  rm -rvf gnuplot
  cd $OPT/share
  rm -rvf gnuplot
  cd $OPT/share/man/man1
  rm -rvf gnuplot.1
  cd $OPT/share/man/man3
  cd $_ASGS_TMP
  rm -rfv gnuplot*
  exit
fi

GNUP_VERSION=5.4.1
GNUP_DIR=gnuplot-${GNUP_VERSION}
GNUP_TGZ=${GNUP_DIR}.tar.gz
cd $_ASGS_TMP

if [ ! -e ${GNUP_TGZ} ]; then
  wget -4 https://asgs-static-assets.sfo2.digitaloceanspaces.com/lib/${GNUP_TGZ}
fi

rm -rf ./$GNUP_DIR 2> /dev/null

tar zxvf ./$GNUP_TGZ

cd $GNUP_DIR
./configure --prefix=$OPT --without-qt --without-cairo --disable-wxwidgets --without-libcerf --without-latex
make -j $JOBS
make install

# no errors, so clean up
if [ "$?" == 0 ]; then
  echo cleaning build scripts and downloads
  cd $_ASGS_TMP
  rm -rfv gnuplot*
fi
