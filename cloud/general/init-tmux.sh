#/usr/bin/env bash

OPT=${1:-$ASGS_INSTALL_PATH}
COMPILER=$2
JOBS=${3:-1}

_ASGS_TMP=${ASGS_TMPDIR:-/tmp/${USER}-asgs}

mkdir -p $_ASGS_TMP 2> /dev/null
chmod 700 $_ASGS_TMP

if [[ "$COMPILER" == "clean" || "$COMPILER" == "rebuild" ]]; then
  echo cleaning tmux 
  pushd $OPT > /dev/null 2>&1
  # ...rm -vf ./include/jq.h
  popd $OPT > /dev/null 2>&1
  echo cleaning tmux build scripts and downloads
  cd $_ASGS_TMP
  rm -rfv ${TMUX_DIR}* > /dev/null 2>&1

  # stop here if 'clean', proceed if 'rebuild'
  if [ "$COMPILER" == "clean" ]; then
    exit
  fi
fi

cd $_ASGS_TMP

# libevent dependencies
wget https://github.com/libevent/libevent/releases/download/release-2.1.11-stable/libevent-2.1.11-stable.tar.gz
tar zxvf libevent-2.1.11-stable.tar.gz
cd libevent-2.1.11-stable
mkdir -p $HOME/local
# install libevent under $HOME/local
./configure --prefix=$ASGS_INSTALL_PATH
make -j 3 && make install

cd $HOME/tmp
wget http://ftp.gnu.org/pub/gnu/ncurses/ncurses-6.1.tar.gz
tar zxvf ncurses-6.1.tar.gz
cd ncurses-6.1/
./configure --prefix=$ASGS_INSTALL_PATH
make -j3 && make install

cd $HOME/tmp
wget https://github.com/tmux/tmux/releases/download/2.9/tmux-2.9.tar.gz
tar zxvf tmux-2.9.tar.gz
cd tmux-2.9
# the CPPFLAGS and LDFLAGS are important, make sure you have written them correctly, or the build will fail
./configure --prefix=$ASGS_INSTALL_PATH \
    CPPFLAGS="-I$ASGS_INSTALL_PATH/include -I$ASGS_INSTALL_PATH/include/ncurses" \
    LDFLAGS="-L$ASGS_INSTALL_PATH/lib"
make -j 3 && make install
