#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export HPCENV=kitt.local
export HPCENVSHORT=kitt
export PPN=16
export QUEUENAME=asgsq15  # compute cores
export SERQUEUE=asgsq1    # for serial jobs
export QSUMMARYCMD=null
export QUOTACHECKCMD=null
export ALLOCCHECKCMD=null
export QSCRIPTTEMPLATE="${SCRIPTDIR}/qscript.template"
export QSCRIPTGEN=qscript.pl
export JOBLAUNCHER='mpiexec -n %totalcpu% '
export ACCOUNT=null
export OPENDAPPOST=opendap_post2.sh #<~ $SCRIPTDIR/output/ assumed
export WORK=${WORK:-$HOME/work}
export SCRATCH=${SCRATCH:-$HOME/scratch}
export DEFAULT_COMPILER=gfortran
export QUEUESYS=nq
export QCHECKCMD="ls -l /tmp/asgsq*"
export SUBMITSTRING="nq "
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export OPENDAPPOST=opendap_post2.sh
export TDS=()
export MAKEJOBS=2