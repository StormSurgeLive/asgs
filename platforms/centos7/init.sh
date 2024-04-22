#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export WORK=${WORK:-$HOME/work}
export SCRATCH=${SCRATCH:-$HOME/scratch}
export DEFAULT_COMPILER=gfortran
export HPCENV=cent7.local
export HPCENVSHORT=cent7
export QUEUESYS=mpiexec
export QCHECKCMD=${QCHECKCMD:-"ps -aux | grep mpiexec "}
export SUBMITSTRING=mpirun
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export OPENDAPPOST=opendap_post2.sh
export TDS=()
export MAKEJOBS=8

# tell init-asgs.sh to prompt for QUEUESYS, QCHECKCMD, SUBMITSTRING
export QSYSASK=NO
