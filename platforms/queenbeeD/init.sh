#!/usr/bin/env bash

export PPN=64
export HPCENVSHORT=qbd
export HPCENV=qbd.loni.org
export QUEUESYS=SLURM
export QCHECKCMD=sacct
export QUEUENAME=workq
export SERQUEUE=single
export WORK=${WORK:-/work/$USER}
export SCRATCH=${SCRATCH:-/work/$USER}
export JOBLAUNCHER='srun '
export SUBMITSTRING=sbatch
export QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
export QSCRIPTGEN=qscript.pl
export DEFAULT_COMPILER=intel-oneapi
export OPENDAPPOST=opendap_post2.sh #<~ $SCRIPTDIR/output/ assumed
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export TDS=(lsu_tds)
export MAKEJOBS=8
