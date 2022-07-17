#!/usr/bin/env bash

OPT=${1:-$ASGS_INSTALL_PATH}
COMPILER=$2
JOBS=${3:-1}

if [[ "$COMPILER" == "clean" || "$COMPILER" == "rebuild" ]]; then
  echo cleaning replaycli
  pushd $OPT > /dev/null 2>&1
  rm -vf ./bin/replaycli
  popd $OPT > /dev/null 2>&1

  # stop here if 'clean', proceed if 'rebuild'
  if [ "$COMPILER" == "clean" ]; then
    exit
  fi
fi

if [ -x ${OPT}/bin/replaycli ]; then
  printf "(warn) 'replaycli' was found in $OPT/bin/replaycli; to reinstall run,\n\n\tbuild replaycli rebuild\n\n"
  exit
fi

REPLAYCLI_PATH=$ASGS_INSTALL_PATH/bin/replaycli

# requires Util::H2O::More
echo installing or updating Util::H2O::More
cpanm Util::H2O::More

# download script into final destination
echo Downloading latest replaycli from Github
curl -s https://raw.githubusercontent.com/StormSurgeLive/storm-replay-client/master/bin/replaycli > $REPLAYCLI_PATH

ERR=$?
if [ $ERR -ne 0 ]; then
  echo Error has occurred ...
  exit $ERR
fi

# make it executable
echo Making $REPLAYCLI_PATH executable
chmod 700 $REPLAYCLI_PATH

ERR=$?
if [ $ERR -ne 0 ]; then
  echo Error has occurred ...
  exit $ERR
fi

echo "'$REPLAYCLI_PATH' has been successfully installed"
