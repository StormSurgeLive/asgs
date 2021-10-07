#!/bin/bash

PERLBREW_ROOT=${1-"$HOME/perl5"}
export PERLBREW_ROOT
ACTION=${2-"install"}
PERL_VERSION=${3-"perl-5.34.0"}

if [ "$ACTION" == "clean" ]; then

  # remove local directories
  rm -rfv $PERLBREW_ROOT
  echo
  echo This also deleted all Perl modules installed with this perlbrew
  echo
  echo Run again without clean flag to install
  echo
  exit
fi

CURRENT_PERL=$(which perl)
echo Current perl is: $CURRENT_PERL

if [ 1 -eq "$(echo $CURRENT_PERL | grep -c perl5)" ]; then
  echo a perlbrew managed perl is already set, unset with
  echo \"perlbrew off\", then rerun
  exit
fi

if [ ! -e "$PERLBREW_ROOT/bin/perlbrew" ]; then
  curl -sL https://install.perlbrew.pl | bash
else
  echo perlbrew seems to be already set up and avaiable via PATH
fi

# source for this session
source $PERLBREW_ROOT/etc/bashrc > /dev/null 2>&1

if [ ! -e "$PERLBREW_ROOT/perls/$PERL_VERSION/bin/perl" ]; then

  # --notest is just to increase the speed of the installation
  perlbrew --verbose --notest install $PERL_VERSION  # -D useshrplib #<- to build libperl.so rather than libperl.a

  if [ $? -ne 0 ]; then
    echo perlbrew failed to install perl $PERL_VERSION
  fi

  NOW_PERL=$(which perl)
  echo Currently perl is now: $NOW_PERL
  echo It will switch back to $CURRENT_PERL after this script ends,
  echo see notes when this script finishes. 

else

  echo found $PERL_VERSION has already been installed by perlbrew...

fi

if [ ! -e "$PERLBREW_ROOT/perls/$PERL_VERSION/bin/perl" ]; then
  echo $PERL_VERSION failed to build
  exit
fi

echo
echo The installed perl will be available through asgsh, the ASGS Shell
echo
