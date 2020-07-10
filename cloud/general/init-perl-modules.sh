#!/bin/bash

# unconditionally attempts to install the Perl modules that are required
#for the operation of ASGS.

PERLBREW_ROOT=${1-$HOME/perl5/perlbrew}

source $PERLBREW_ROOT/etc/bashrc

if [ ! -e $PERLBREW_ROOT/bin/cpanm ]; then
  echo installing cpanm...
  perlbrew install-cpanm
fi

echo Installing Perl modules required for ASGS

# due to failing tests that do not affect usefullness
# the module 
cpanm install --notest Date::Format
cpanm install --notest Date::Handler

for module in $(cat ./PERL-MODULES); do
  cpanm install $module || exit 1 # forces script to exit with error, asgs-brew.pl will catch and report this
done

# copy over ./PERL/perltidyrc to $HOME/.perltidyrc
cp ./PERL/perltidyrc $HOME/.perltidyrc

# interactive (selects "p" option for "pure pure"), skips testing
#echo Installing Date::Pcalc using --force and --interactive due to known issue
#cpanm --force --interactive Date::Pcalc <<EOF
#p
#EOF
## crude check for install (Date::Pcalc will be deprecated from ASGS soon, so this will go away)
#perldoc -l Date::Pcalc > /dev/null 2>&1 || exit 1
