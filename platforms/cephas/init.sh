#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export WORK=${WORK:-$HOME/work}
export SCRATCH=${SCRATCH:-$HOME/scratch}
export DEFAULT_COMPILER=intel-oneapi
export HPCENV=cephas
export HPCENVSHORT=cephas
export QUEUESYS=SLURM
export QCHECKCMD=squeue
export SUBMITSTRING=sbatch
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export OPENDAPPOST=opendap_post2.sh
export TDS=()
export MAKEJOBS=16

export QSUMMARYCMD=squeue
export QUOTACHECKCMD=null
export ALLOCCHECKCMD=null
export QUEUENAME=general
export SERQUEUE=general
export SERQUEUE_NTASKS=1
export ACCOUNT=null

export QSCRIPTTEMPLATE=$SCRIPTDIR/platforms/cephas/qscript.template.cephas
export JOBLAUNCHER='srun --mpi=pmi2 -n %totalcpu% --distribution=block:block --cpu-bind=cores --mem-bind=local --kill-on-bad-exit=1'
export PPN=40
