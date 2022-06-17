#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export WORK=${WORK:-/work/$USER}
export SCRATCH=${SCRATCH:-$WORK}
export DEFAULT_COMPILER=intel

# replacement for set_hpc() function in platforms.sh
export HPCENV=mike.hpc.lsu.edu
export HPCENVSHORT=mike

export RMQMessaging_Enable="off"
export HPCENV=mike.hpc.lsu.edu
export QUEUESYS=SLURM
export QCHECKCMD=squeue
export QSUMMARYCMD=squeue
export QUOTACHECKCMD=null
export ALLOCCHECKCMD=null
export QUEUENAME=workq
export SERQUEUE=single
export ACCOUNT=null
export SUBMITSTRING=sbatch
export JOBLAUNCHER='srun -N %nnodes%'
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export SSHKEY=~/.ssh/id_rsa.pub
export QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
export QSCRIPTGEN=qscript.pl
export OPENDAPPOST=opendap_post2.sh
export PPN=64
export CONSTRAINT=null
export RESERVATION=null
export REMOVALCMD="rm"
export TDS=( lsu_tds )
export MAKEJOBS=8
