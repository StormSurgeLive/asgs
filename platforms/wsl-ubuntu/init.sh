#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export WORK=${WORK:-$HOME/work}
export SCRATCH=${SCRATCH:-$HOME/scratch}
export DEFAULT_COMPILER=gfortran
export HPCENV=wsl-ubuntu.local
export HPCENVSHORT=wsl-ubuntu
export QUEUESYS=mpiexec
export QCHECKCMD="ps -aux | grep mpiexec "
export SUBMITSTRING="mpirun "
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export OPENDAPPOST=opendap_post2.sh
export TDS=()
export MAKEJOBS=2
