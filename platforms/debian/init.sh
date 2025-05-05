#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export WORK=${WORK:-$HOME/work}
export SCRATCH=${SCRATCH:-$HOME/scratch}
export DEFAULT_COMPILER=gfortran
export HPCENV=debian.local
export HPCENVSHORT=debian
export QUEUESYS=mpiexec
export QCHECKCMD="ps -aux | grep mpiexec "
export HOSTFILE=$SCRATCHDIR/hostfile.txt
export SUBMITSTRING="mpiexec --hostfile $HOSTFILE "
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export OPENDAPPOST=opendap_post2.sh
export TDS=()
export MAKEJOBS=2
