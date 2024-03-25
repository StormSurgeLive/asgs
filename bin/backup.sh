#!/usr/bin/env bash

PWD=$(pwd)
PWD=${PWD##*/}
BKPTGZ=${1:-$HOME/ASGS-SAVE-${PWD}.tgz}

# create manifest
git clean -n -x -d | awk '{print $3}' | grep -v repository > ASGS-MANIFEST

tar zcvf $BKPTGZ -T ./ASGS-MANIFEST | grep -v '\/$' | xargs md5sum | tee $BKPTGZ.MD5 

