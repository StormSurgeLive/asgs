#!/usr/bin/env bash

WORK=${WORK:-/work/$USER}
SCRATCH=${SCRATCH:-/work/$USER}
HPCENV=qbd.loni.org
QUEUESYS=SLURM
QCHECKCMD=sacct
QUEUENAME=workq
SERQUEUE=single
PPN=64
JOBLAUNCHER='srun '
SUBMITSTRING=sbatch
QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
QSCRIPTGEN=qscript.pl
OPENDAPPOST=opendap_post2.sh #<~ $SCRIPTDIR/output/ assumed
ARCHIVE=enstorm_pedir_removal.sh
ARCHIVEBASE=$SCRATCH
ARCHIVEDIR=$SCRATCH
TDS=( lsu_tds )
MAKEJOBS=8
