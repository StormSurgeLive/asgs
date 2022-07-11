#!/usr/bin/env bash

OPT=${1-$ASGS_INSTALL_PATH}
COMPILER=${2-gfortran}
JOBS=${3-1}

if [ "$COMPILER" == "clean" ]; then
  echo cleaning NASM and ffmpeg libraries and utilities
  cd $OPT/bin
  rm -rfv ffmpeg ffprobe nasm ndisasm
  cd $OPT/lib
  rm -rfv libavfilter.a libavdevice.a libswscale.a libswresample.a libavformat.a libavcodec.a libavutil.a
  cd $OPT/lib/pkgconfig
  rm -rfv libswscale.pc libswresample.pc libavutil.pc libavformat.pc libavfilter.pc libavdevice.pc libavcodec.pc
  cd $OPT/include
  rm -rfv libswscale libswresample libavutil libavformat libavfilter libavdevice libavcodec
  cd $OPT/share
  rm -rfv ffmpeg
  cd $OPT/share/man/man1
  rm -rfv ndisasm.1 nasm.1 ffprobe-all.1 ffprobe.1 ffmpeg-utils.1 ffmpeg-scaler.1 ffmpeg-resampler.1 ffmpeg-protocols.1 \
    ffmpeg-formats.1 ffmpeg-filters.1 ffmpeg-devices.1 ffmpeg-codecs.1 ffmpeg-bitstream-filters.1 ffmpeg-all.1 ffmpeg.1
  cd $OPT/share/man/man3
  rm -rfv libswscale.3 libswresample.3 libavutil.3 libavformat.3 libavfilter.3 libavdevice.3 libavcodec.3
  cd $ASGS_TMPDIR
  rm -rfv nasm* ffmpeg*
  exit
fi

NASM_VERSION=2.15.05
NASM_TGZ=nasm-${NASM_VERSION}.tar.gz
NASM_DIR=nasm-${NASM_VERSION}

FFMPEG_VERSION=4.4
FFMPEG_TAR=ffmpeg-${FFMPEG_VERSION}.tar
FFMPEG_BZ2=${FFMPEG_TAR}.bz2
FFMPEG_DIR=ffmpeg-${FFMPEG_VERSION}

cd $ASGS_TMPDIR

# requires NASM
if [ ! -e ${NASM_TGZ} ]; then
  wget --no-check-certificate https://www.nasm.us/pub/nasm/releasebuilds/${NASM_VERSION}/${NASM_TGZ}
fi

rm -rf $NASM_DIR 2> /dev/null
tar zxvf $NASM_TGZ
cd $NASM_DIR
./configure --prefix=$OPT
make
make install

# now fetch and build/install ffmpeg
cd $ASGS_TMPDIR

if [ ! -e ${FFMPEG_BZ2} ]; then
  wget --no-check-certificate https://ffmpeg.org/releases/${FFMPEG_BZ2}
fi

rm -rf $FFMPEG_DIR 2> /dev/null
bunzip2 $FFMPEG_BZ2
tar xvf $FFMPEG_TAR
cd $FFMPEG_DIR
# Note:, LD_LIBRARY_PATH and LD_INCLUDE_PATH are standardized based on $OPT
# and do not need to be appended for this next step; ffpeg will find nasm as-is
./configure --prefix=$OPT # this 'configure' seems sensitive, needing the '=' after flags like '--prefix'
make -j $JOBS
make install

# no errors, so clean up
if [ "$?" == 0 ]; then
  echo cleaning build scripts and downloads
  cd $ASGS_TMPDIR
  rm -rfv nasm* ffmpeg*
fi
