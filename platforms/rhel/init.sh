#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export WORK=${WORK:-$(pwd)}
export SCRATCH=${SCRATCH:-$(pwd)}
export DEFAULT_COMPILER=gfortran
export HPCENV=rhel.local
export HPCENVSHORT=rhel
export QUEUESYS=${QUEUESYS:-mpiexec}
export QCHECKCMD=${QCHECKCMD:-"ps -aux | grep mpiexec "}
export SUBMITSTRING=${SUBMITSTRING:-"mpiexec "}
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export OPENDAPPOST=opendap_post2.sh
export TDS=()
export MAKEJOBS=8

# tell init-asgs.sh to prompt for QUEUESYS, QCHECKCMD, SUBMITSTRING
export QSYSASK=YES
