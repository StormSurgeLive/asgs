#!/usr/bin/env bash

# Needed for ./init-asgs.sh, bin/guess
export PPN=null
export HPCENV=hatteras.renci.org
export HPCENVSHORT=hatteras
export QUEUESYS=SLURM
export QUEUENAME=batch
export SERQUEUE=batch
export QSUMMARYCMD=null
export QUOTACHECKCMD="df -h /projects/ncfs"
export ALLOCCHECKCMD=null
export QSCRIPTTEMPLATE="${SCRIPTDIR}/qscript.template"
export QSCRIPTGEN=qscript.pl
export OPENDAPPOST=opendap_post2.sh
export DEFAULT_COMPILER=intel
export WORK=${WORK:-/projects/ncfs/$USER}
export SCRATCH=${SCRATCH:-/projects/ncfs/data}
export ASGS_TMPDIR=${ASGS_HOME}/tmp
export CONSTRAINT=null
export RESERVATION=null
export PARTITION=null
export WALLTIMEFORMAT="minutes"
export QCHECKCMD=sacct
export JOBLAUNCHER='srun '
export ACCOUNT=null
export SUBMITSTRING=sbatch
export SSHKEY=id_rsa_lonestar5
export OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
export UMASK=006
export ARCHIVE=enstorm_pedir_removal.sh
export ARCHIVEBASE=$SCRATCH
export ARCHIVEDIR=$SCRATCH
export TDS=(renci_tds)
export MAKEJOBS=8
export MATLABEXE=script # "script" means just execute matlab (don't use mex files)
export RMQMessaging_Enable="on"      # "on"|"off" 
export RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
export RMQMessaging_NcoHome="/home/ncfs" 
export RMQMessaging_LocationName="RENCI"
export RMQMessaging_ClusterName="Hatteras"
module load icc/2022.0.2  mvapich2/2.3.7-intel
