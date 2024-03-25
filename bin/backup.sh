#!/usr/bin/env bash

PWD=$(pwd)
PWD=${PWD##*/}
BKPTGZ=${1:-$HOME/ASGS-SAVE-${PWD}.tgz}
BKPMD5=${1:-$HOME/ASGS-SAVE-${PWD}.MD5}

# create manifest
git clean -n -x -d | awk '{print $3}' | grep -v repository > ASGS-MANIFEST

# add directories that are also git repos
echo "opt/models/adcircs" >> ASGS-MANIFEST
echo "git/"               >> ASGS-MANIFEST

# tar/gzip + generation of the contents, each file is checksum'd with MD5
tar zcvf $BKPTGZ -T ./ASGS-MANIFEST | grep -v '\/$' | xargs md5sum | tee $BKPMD5

# compress MD5 manifest
gzip $BKPMD5

