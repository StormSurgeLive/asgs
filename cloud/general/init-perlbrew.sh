#!/bin/bash

PERLBREW_ROOT=${1:-"$HOME/perl5"}
export PERLBREW_ROOT
ACTION=${2:-"install"}
PERL_VERSION=${3:-"perl-5.42.0"}

if [ "$ACTION" == "clean" ]; then

  # remove local directories
  rm -rfv "$PERLBREW_ROOT"
  echo
  echo This also deleted all Perl modules installed with this perlbrew
  echo
  echo Run again without clean flag to install
  echo
  exit
fi

CURRENT_PERL=$(which perl)
echo Current perl is: "$CURRENT_PERL"

if [ 1 -eq "$(echo "$CURRENT_PERL" | grep -c perl5)" ]; then
  echo a perlbrew managed perl is already set, unset with
  echo \"perlbrew off\", then rerun
  exit
fi

#
# Determine which curl options this platform supports.
#
CURL_OPTS="-4"

if command -v curl >/dev/null 2>&1; then
  if curl --help all 2>/dev/null | grep -q -- '--http1.1'; then
    CURL_OPTS="$CURL_OPTS --http1.1"
  fi
fi

#
# Install perlbrew.
#
if [ ! -e "$PERLBREW_ROOT/bin/perlbrew" ]; then

  if command -v curl >/dev/null 2>&1; then
    curl $CURL_OPTS -L https://install.perlbrew.pl | bash

  elif command -v wget >/dev/null 2>&1; then
    wget -qO- https://install.perlbrew.pl | bash

  else
    echo "ERROR: Neither curl nor wget is available."
    exit 1
  fi

  if [ ! -e "$PERLBREW_ROOT/bin/perlbrew" ]; then
    echo "ERROR: perlbrew failed to install."
    exit 1
  fi

else
  echo perlbrew seems to be already set up and available via PATH
fi

# source for this session
if [ -e "$PERLBREW_ROOT/etc/bashrc" ]; then
  source "$PERLBREW_ROOT/etc/bashrc" > /dev/null 2>&1
else
  echo "ERROR: perlbrew bashrc was not created:"
  echo "       $PERLBREW_ROOT/etc/bashrc"
  exit 1
fi

#
# Update perlbrew's curl download options.
#
# Force IPv4 on platforms where that is needed. Also force HTTP/1.1 when
# supported by the installed curl. Older curl versions, such as the one on
# Frontera, do not recognize --http1.1.
#
PB=$(which perlbrew)

if [ -z "$PB" ]; then
  echo "ERROR: perlbrew is not available after installation."
  exit 1
fi

if echo "$CURL_OPTS" | grep -q -- '--http1.1'; then
  perl -pi -e "s/download => '--silent/download => '-4 --http1.1 --silent/g" "$PB"
else
  perl -pi -e "s/download => '--silent/download => '-4 --silent/g" "$PB"
fi

if [ ! -e "$PERLBREW_ROOT/perls/$PERL_VERSION/bin/perl" ]; then

  #
  # If CC was supplied by the environment, use its basename as Perl's C
  # compiler. Otherwise default to gcc.
  #
  if [ -n "${CC:-}" ]; then
    PERL_COMPILER=$(basename "$CC")
  else
    PERL_COMPILER=gcc
  fi

  echo "Building $PERL_VERSION with C compiler: $PERL_COMPILER"

  # --notest is just to increase the speed of the installation
  perlbrew -Dcc="$PERL_COMPILER" --verbose --notest install "$PERL_VERSION" \
    --mirror https://www.cpan.org
  # -D useshrplib #<- to build libperl.so rather than libperl.a

  if [ $? -ne 0 ]; then
    echo perlbrew failed to install perl "$PERL_VERSION"
  fi

  NOW_PERL=$(which perl)
  echo Currently perl is now: "$NOW_PERL"
  echo It will switch back to "$CURRENT_PERL" after this script ends,
  echo see notes when this script finishes.

else

  echo found "$PERL_VERSION" has already been installed by perlbrew...

fi

if [ ! -e "$PERLBREW_ROOT/perls/$PERL_VERSION/bin/perl" ]; then
  echo "$PERL_VERSION" failed to build
  exit 1
fi

echo
echo The installed perl will be available through asgsh, the ASGS Shell
echo
