#!/usr/bin/env bash

# unconditionally attempts to install the Perl modules that are required

PERLBREW_ROOT=${1:-"${ASGS_HOME}/perl5/perlbrew"}  # ASGS_HOME is not exported by init-asgs.sh or asgs-brew.pl ...
source $PERLBREW_ROOT/etc/bashrc

# Note: not available via asgs-brew.pl at this time
if [ "${2}" = "clean" ]; then
  # uninstall tempermental modules first
  for module in $(cat ./PERL-MODULES.notest); do
    echo Y | cpanm --uninstall $module
  done

  # uninstall the rest
  for module in $(cat ./PERL-MODULES); do
    echo Y | cpanm --uninstall $module
  done

  # cleans out an remaining build directory left by cpanm
  rm -rfv $PERLBREW_ROOT/.cpanm
  exit
fi

if [ ! -e $PERLBREW_ROOT/bin/cpanm ]; then
  echo installing cpanm...
  perlbrew install-cpanm
fi

echo Installing Perl modules required for ASGS

# ensure CPAN over non-SSL
MIRROR="https://www.cpan.org/"

# install tempermental modules first
for module in $(cat ./PERL-MODULES.notest); do
  cpanm --from $MIRROR --notest $module
done

# install another class of tempermental modules that require
OLDIFS=$IFS
IFS=$'\n'
for line in $(cat ./PERL-MODULES.wget); do
echo $line
  module=$(echo $line | awk '{print $1}');
  mURL=$(echo $line   | awk '{print $2}');
  base=$(echo $line   | awk '{print $3}');
  ext=$(echo $line    | awk '{print $4}');
  pushd /tmp
  tgz=$base.$ext
  echo Fetching $module via $mURL/$tgz
  wget -4 $mURL/$tgz
  cpanm --verbose $tgz
  rm -v $tgz
  popd
done
IFS=$OLDIFS

# manual fetch via wget
for module in $(cat ./PERL-MODULES.notest); do
  cpanm --from $MIRROR --notest $module
done

# run tests on these installations
for module in $(cat ./PERL-MODULES); do
  cpanm --from $MIRROR $module || exit 1 # forces script to exit with error, asgs-brew.pl will catch and report this
done

# copy over ./PERL/perltidyrc to $HOME/.perltidyrc
cp ./PERL/perltidyrc $HOME/.perltidyrc

# clean up the build directory created by .cpanm
rm -rfv $PERLBREW_ROOT/.cpanm
