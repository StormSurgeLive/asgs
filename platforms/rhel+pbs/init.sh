#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export HPCENV=rhel+pbs.local
export HPCENVSHORT=rhel+pbs
export WORK=${WORK:-$(pwd)}
export SCRATCH=${SCRATCH:-$(pwd)}
export DEFAULT_COMPILER=gfortran
export QUEUESYS=PBS
export QCHECKCMD=qstat
export QSUMMARYCMD=showq
export QUOTACHECKCMD=showquota
export ALLOCCHECKCMD=showquota
export SUBMITSTRING=qsub
export QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
export QSCRIPTGEN=qscript.pl # asgs looks in $SCRIPTDIR for this
export QUEUENAME=workq
export SERQUEUE=single
export DEFAULT_COMPILER=intel

export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export OPENDAPPOST=opendap_post2.sh
export TDS=()
export MAKEJOBS=2
