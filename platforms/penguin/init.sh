#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
WORK=${WORK:-$HOME}
SCRATCH=${SCRATCH:-$HOME}
DEFAULT_COMPILER=gfortran
ASGS_TMP=${TMPDIR:-$WORK/asgs-build-tmp}

# replacement for set_hpc() function in platforms.sh
HPCENV=pod.penguincomputing.com
HPCENVSHORT=peguin

# Needed for ASGS
QUEUESYS=PBS
QCHECKCMD=qstat
ACCOUNT=null
QSUMMARYCMD=null
QUOTACHECKCMD=null
ALLOCCHECKCMD=null
QUEUENAME=B30
SERQUEUE=B30
PPN=28
SUBMITSTRING=qsub
QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
QSCRIPTGEN=qscript.pl # asgs looks in $SCRIPTDIR for this
if [[ ${RMQMessaging_Enable} == "on" ]]; then
  RMQMessaging_LocationName="Penguin"
  RMQMessaging_ClusterName="POD"
  RMQMessaging_NcoHome="$HOME/local"
fi
JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
local THIS="platforms.sh>env_dispatch()>init_penguinpod()"
SSHKEY=~/.ssh/id_rsa.pub
RESERVATION=null
ARCHIVE=enstorm_pedir_removal.sh
ARCHIVEBASE=$SCRATCH
ARCHIVEDIR=$SCRATCH
TDS=()
MAKEJOBS=8
