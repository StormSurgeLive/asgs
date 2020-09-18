#/usr/bin/env bash

read -p "This will download and install tmux (and dependencies). Proceed? [N] " proceed

if [[ -z "$proceed" || "N" = "$proceed" ]]; then
  echo cancelling tmux installation ...
  exit
fi

mkdir -p $HOME/tmp

cd $HOME/tmp
wget https://github.com/libevent/libevent/releases/download/release-2.1.11-stable/libevent-2.1.11-stable.tar.gz
tar zxvf libevent-2.1.11-stable.tar.gz
cd libevent-2.1.11-stable
mkdir -p $HOME/local
# install libevent under $HOME/local
./configure --prefix="$HOME/local"
make -j 3 && make install

cd $HOME/tmp
wget http://ftp.gnu.org/pub/gnu/ncurses/ncurses-6.1.tar.gz
tar zxvf ncurses-6.1.tar.gz
cd ncurses-6.1/
./configure --prefix="$HOME/local"
make -j3 && make install

cd $HOME/tmp
wget https://github.com/tmux/tmux/releases/download/2.9/tmux-2.9.tar.gz
tar zxvf tmux-2.9.tar.gz
cd tmux-2.9
# the CPPFLAGS and LDFLAGS are important, make sure you have written them correctly, or the build will fail
./configure --prefix=$HOME/local \
    CPPFLAGS="-I$HOME/local/include -I$HOME/local/include/ncurses" \
    LDFLAGS="-L$HOME/local/lib"
make -j 3 && make install
