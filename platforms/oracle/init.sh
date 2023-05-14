#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export WORK=${WORK:-/work}
export SCRATCH=${SCRATCH:-/scratch}
export DEFAULT_COMPILER=gfortran
export HPCENV=oracle.local
export HPCENVSHORT=oracle
export QUEUESYS=mpiexec
export QCHECKCMD="ps -aux | grep mpiexec "
export SUBMITSTRING="mpiexec "
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export OPENDAPPOST=opendap_post2.sh
export TDS=()
export MAKEJOBS=2
