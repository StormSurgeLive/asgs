#!/bin/bash

# unconditionally attempts to install the Perl modules that are required
#for the operation of ASGS.

source ~/perl5/perlbrew/etc/bashrc

perlbrew --force install-cpanm
cpanm --local-lib=~/perl5 local::lib && eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)

echo Installing Perl modules required for ASGS
cpanm Date::Format Date::Handler DateTime DateTime::Format::Builder IO::Socket::SSL HTTP::Tiny List::Util        \
      Math::Trig Net::FTP Params::Validate Time::Local Email::Sender::Simple Email::Sender::Transport::SMTP::TLS \
      Email::Simple::Creator Config::Tiny Try::Tiny Date::Calc # this could be extended to use a MANIFEST type file

# interactive (selects "p" option for "pure pure"), skips testing
echo Installing Date::Pcalc using --force and --interactive due to known issue
cpanm --force --interactive Date::Pcalc <<EOF
p
EOF

