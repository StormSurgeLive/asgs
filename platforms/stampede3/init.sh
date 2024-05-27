#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export PPN=48
export HPCENV=stampede3.tacc.utexas.edu
export HPCENVSHORT=stampede3
export QUEUESYS=SLURM
export QUEUENAME=skx
export SERQUEUE=skx
export QSUMMARYCMD=null
export QUOTACHECKCMD=null
export ALLOCCHECKCMD=null
export QSCRIPTTEMPLATE="${SCRIPTDIR}/qscript.template"
export QSCRIPTGEN=qscript.pl
export DEFAULT_COMPILER=intel-llvm
export GROUP="G-803086"
export WORK=${WORK:-$HOME}
export SCRATCH=${SCRATCH:-$HOME}
export ASGS_TMP=${TMPDIR:-$ASGS_HOME/tmp}
export CONSTRAINT=null
export RESERVATION=null
export QCHECKCMD=squeue
export JOBLAUNCHER='ibrun '
export ACCOUNT=null
export SUBMITSTRING=sbatch
export SSHKEY=id_rsa_lonestar5
export OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
export UMASK=006
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=/corral-tacc/utexas/hurricane/ASGS
export ARCHIVEDIR=2020
export TDS=(tacc_tds3)
export MAKEJOBS=8
