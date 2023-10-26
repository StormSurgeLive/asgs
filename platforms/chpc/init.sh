#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export PPN=64
export WORK=${WORK:-/twater/$USER}
export SCRATCH=${SCRATCH:-/twater/$USER}
export DEFAULT_COMPILER=gfortran
export HPCENV=chpc.ua.edu
export HPCENVSHORT=chpc
export QUEUESYS=SLURM
export QCHECKCMD=squeue
export SUBMITSTRING=sbatch
export JOBLAUNCHER='srun '
export QSCRIPTTEMPLATE="${SCRIPTDIR}/qscript.template"
export QSCRIPTGEN=qscript.pl
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export OPENDAPPOST=opendap_post2.sh
export TDS=()
export MAKEJOBS=2
