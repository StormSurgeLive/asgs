#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export WORK=${WORK:-$HOME/work}
export SCRATCH=${SCRATCH:-$HOME/scratch}
export DEFAULT_COMPILER=intel-oneapi
export HPCENV=cephas
export HPCENVSHORT=cephas
export QUEUESYS=SLURM
export QCHECKCMD=squeue
export QSCRIPTTEMPLATE=$SCRIPTDIR/platforms/cephas/qscript.template
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
export ACCOUNT=null
# export JOBLAUNCHER='srun -N %nnodes%'  # use if cluster assigns whole nodes
#export JOBLAUNCHER='srun -n %totalcpu%' # assuming slurm is set up to share nodes/cores
export JOBLAUNCHER='srun --mpi=pmi2 -N %nnodes% -n %totalcpu% --ntasks-per-node=%ppn% --cpu-bind=cores --mem-bind=local --distribution=block:block --kill-on-bad-exit=1'
export PPN=40
