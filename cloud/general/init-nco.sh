#!/usr/bin/env bash

OPT=${1:-$ASGS_INSTALL_PATH}
COMPILER=$2
JOBS=${3:-1}

_ASGS_TMP=${ASGS_TMPDIR:-/tmp/${USER}-asgs}

mkdir -p $_ASGS_TMP 2> /dev/null
chmod 700 $_ASGS_TMP

if [ "$COMPILER" == "clean" ]; then
  echo cleaning nco
  cd $OPT/include
  rm -rvf libnco_c++.hh nco_dmn.hh nco_hgh.hh nco_var.hh nco_att.hh nco_fl.hh nco_utl.hh
  cd $OPT/lib
  rm -rvf libnco.so libnco.la libnco.a libnco-4.9.8.so libnco_c++.so libnco_c++.la libnco_c++.a libnco_c++-4.9.8.so
  cd $OPT/bin
  rm -rvf ncwa ncrename ncremap ncrcat ncra ncpdq ncks ncflint nces ncecat ncea ncdiff ncclimo ncbo ncatted
  cd $OPT/share/man/man1
  rm -rvf ncap2.1 ncatted.1 ncbo.1 ncclimo.1 nces.1 ncecat.1 ncflint.1 ncks.1 nco.1 ncra.1 ncremap.1 ncrename.1 ncpdq.1 ncrcat.1 ncwa.1
  cd $_ASGS_TMP
  rm -rfv nco-${NCO_VERSION} ${NCO_VERSION}.tar.gz
  exit
fi

NCO_VERSION=4.9.8
NCO_DIR=nco-${NCO_VERSION}
NCO_TGZ=${NCO_VERSION}.tar.gz
cd $_ASGS_TMP

if [ ! -e ${NCO_TGZ} ]; then
  wget --no-check-certificate https://github.com/nco/nco/archive/refs/tags/${NCO_TGZ}
fi

rm -rf ./$NCO_DIR 2> /dev/null

if [ ! -e $OPT/bin/ncks ]; then
   echo $OPT/bin/ncks was not found
   tar zxvf ./$NCO_TGZ
   cd $NCO_DIR
   ./configure --prefix=$OPT --enable-gsl=no
   make -j $JOBS
   make install
fi

# no errors, so clean up
if [ -e $OPT/bin/ncks ]; then
  echo cleaning build scripts and downloads
  cd $_ASGS_TMP
  rm -rfv nco* $NCO_TGZ
else
  echo something went wrong with installing nco
  exit 1
fi
