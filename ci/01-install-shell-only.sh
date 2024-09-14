#!/usr/bin/env bash

./init-asgs.sh -b -x "--update-shell"

if [ ! -e ./asgsh ]; then
  echo "01 not ok - can't find ./asgsh" >&2
  exit 1
fi

echo "01 ok - found ./asgsh"

if [ ! -e ./update-asgs ]; then
  echo "02 not ok - can't find ./update-asgs" >&2
  exit 1
fi

echo "02 ok - found ./update-asgs"
