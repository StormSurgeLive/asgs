#!/usr/bin/env bash

export PPN=64
export HPCENVSHORT=qbd
export HPCENV=qbd.loni.org
export QUEUESYS=SLURM
export QCHECKCMD=sacct
export QUEUENAME=workq
export SERQUEUE=single
export SERQUEUE_NTASKS=3 # 12G for slurm, $SERQUEUE_NTASKS * 4G, applied to --ntasks
export WORK=${WORK:-/work/$USER}
export SCRATCH=${SCRATCH:-/work/$USER}
export JOBLAUNCHER='srun '
# ASGS_SINGULARITY_CMD works with ADCIRC_SINGULARITY_SIF, which is
# defined by ADCIRC_SINGULARITY_SIF defined in a registered
# ADCIRC's metadata file of the version that's been loaded
export ASGS_SINGULARITY_CMD='singularity run -B /ddnB/work,/work,/scratch,/project '
export SUBMITSTRING=sbatch
export QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
export QSCRIPTGEN=qscript.pl
export DEFAULT_COMPILER=intel
export OPENDAPPOST=opendap_post2.sh #<~ $SCRIPTDIR/output/ assumed
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export TDS=(lsu_tds)
export MAKEJOBS=8
