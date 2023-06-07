#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export WORK=${WORK:-$(pwd)}
export SCRATCH=${SCRATCH:-$(pwd)}
export DEFAULT_COMPILER=gfortran
export ASGS_TMP=${TMPDIR:-$WORK/asgs-build-tmp}

# replacement for set_hpc() function in platforms.sh
export HPCENV=pod.penguincomputing.com
export HPCENVSHORT=penguin

# Needed for ASGS
export QUEUESYS=PBS
export QCHECKCMD=qstat
export ACCOUNT=null
export QSUMMARYCMD=null
export QUOTACHECKCMD=null
export ALLOCCHECKCMD=null
export QUEUENAME=B30
export SERQUEUE=B30
export PPN=28
export SUBMITSTRING=qsub
export QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
export QSCRIPTGEN=qscript.pl # asgs looks in $SCRIPTDIR for this
export JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
export local THIS="platforms.sh>env_dispatch()>init_penguinpod()"
export SSHKEY=~/.ssh/id_rsa.pub
export RESERVATION=null
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export TDS=()
export MAKEJOBS=8
