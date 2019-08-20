#!/bin/bash

if [ ! -d $HOME/adcirc-cg/work ]; then
  cd $HOME
  git clone https://github.com/adcirc/adcirc-cg.git
fi

export PERLBREW_PERL=perl-5.28.2
export PERLBREW_ROOT=/home/vagrant/perl5/perlbrew
export PERLBREW_PATH=/home/vagrant/perl5/perlbrew/bin:/home/vagrant/perl5/perlbrew/perls/perl-5.28.2/bin
export PERLBREW_HOME=/home/vagrant/.perlbrew
export NETCDFHOME=/home/vagrant/opt
export LD_INCLUDE_PATH=/home/vagrant/opt/include
export PATH=/home/vagrant/perl5/perlbrew/perls/perl-5.28.2/bin:/home/vagrant/opt/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin
export LD_LIBRARY_PATH=/home/vagrant/opt/lib
export PERLBREW_MANPATH=/home/vagrant/perl5/perlbrew/perls/perl-5.28.2/man

cd $HOME/adcirc-cg/work
make clean && make all compiler=gfortran NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable NETCDFHOME=$NETCDFHOME MACHINENAME=jason-desktop
