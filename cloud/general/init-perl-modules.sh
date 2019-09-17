#!/bin/bash

# unconditionally attempts to install the Perl modules that are required
#for the operation of ASGS.

source ~/perl5/perlbrew/etc/bashrc

if [ ! -e $HOME/perl5/perlbrew/bin/cpanm ]; then
  perlbrew install-cpanm
fi

echo Installing Perl modules required for ASGS
for module in $(cat ./PERL-MODULES); do
  cpanm install $module || exit 1 # forces script to exit with error, asgs-brew.pl will catch and report this
done

# interactive (selects "p" option for "pure pure"), skips testing
#echo Installing Date::Pcalc using --force and --interactive due to known issue
#cpanm --force --interactive Date::Pcalc <<EOF
#p
#EOF
## crude check for install (Date::Pcalc will be deprecated from ASGS soon, so this will go away)
#perldoc -l Date::Pcalc > /dev/null 2>&1 || exit 1
