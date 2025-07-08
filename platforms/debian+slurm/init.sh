#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export WORK=${WORK:-$HOME/work}
export SCRATCH=${SCRATCH:-$HOME/scratch}
export DEFAULT_COMPILER=gfortran
export HPCENV=debian.cluster
export HPCENVSHORT=debian
export QUEUESYS=SLURM
export QCHECKCMD=squeue
export SUBMITSTRING=sbatch
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export OPENDAPPOST=opendap_post2.sh
export TDS=()
export MAKEJOBS=2

export QSUMMARYCMD=squeue
export QUOTACHECKCMD=null
export ALLOCCHECKCMD=null
export QUEUENAME=general
export SERQUEUE=general
export ACCOUNT=null
export JOBLAUNCHER='srun -N %nnodes%'
export PPN=28 # this will vary wildly from cluster to cluster
