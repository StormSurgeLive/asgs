#!/usr/bin/env bash

PPN=64
HPCENVSHORT=qbd1
HPCENV=qbd1.loni.org
QUEUESYS=SLURM
QCHECKCMD=sacct
QUEUENAME=workq
SERQUEUE=single
WORK=${WORK:-/work/$USER}
SCRATCH=${SCRATCH:-/work/$USER}
JOBLAUNCHER='srun '
SUBMITSTRING=sbatch
QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
QSCRIPTGEN=qscript.pl
OPENDAPPOST=opendap_post2.sh #<~ $SCRIPTDIR/output/ assumed
ARCHIVE=enstorm_pedir_removal.sh
ARCHIVEBASE=$SCRATCH
ARCHIVEDIR=$SCRATCH
TDS=(lsu_tds)
MAKEJOBS=8
