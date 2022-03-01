#!/bin/bash
#----------------------------------------------------------------
#
# $THIS: This file contains functions required for initializing
# variables that are architecture (platform) dependent.
# It is sourced by asgs_main.sh and any other shell script that
# is platform dependent.
#
#----------------------------------------------------------------
# Copyright(C) 2012--2019 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------
#
# initialization subroutines for the various machines/architectures
#
# Suggested aliases to support the Operator's tasks. Add these
# to .bashrc, .bash_profile or similar
#
# alias lsta='ls -lth *.state | head'
#

source ${SCRIPTDIR}/monitoring/logging.sh

init_supermike()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_supermike()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=mike.hpc.lsu.edu
  QUEUESYS=PBS
  QCHECKCMD=qstat
  QUEUENAME=workq
  SERQUEUE=single
  ACCOUNT=null
  SUBMITSTRING=qsub
  JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=$SCRIPTDIR/input/machines/supermike/supermike.template.pbs
  MATLABEXE=mex
  MCRROOT=/usr/local/packages/license/matlab/r2017a # for matlab mex files
  QSCRIPTGEN=tezpur.pbs.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  PPN=16
  TDS=(lsu_tds)
  MAKEJOBS=8
}
#
init_queenbee()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_queenbee()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=queenbee.loni.org
  QUEUESYS=PBS
  PPN=20
  QCHECKCMD=qstat
  QSUMMARYCMD=showq
  QUOTACHECKCMD=showquota
  ALLOCCHECKCMD=showquota
  QUEUENAME=workq
  SERQUEUE=single
  SUBMITSTRING=qsub
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl # asgs looks in $SCRIPTDIR for this
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  if [[ ${RMQMessaging_Enable} == "on" ]]; then
    RMQMessaging_LocationName="LONI"
    RMQMessaging_ClusterName="Queenbee"
    RMQMessaging_NcoHome="$HOME/local"
  fi
  JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
  ACCOUNT=null
  TDS=( lsu_tds )
  SSHKEY=~/.ssh/id_rsa.pub
  REMOVALCMD="rmpurge"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  # @jasonfleming: for ~/.bashrc: Prevent git push from opening up a graphical
  # dialog box to ask for a password; it will interactively ask for
  # a password instead
  unset SSH_ASKPASS
  MAKEJOBS=8
}
#
init_rostam()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_rostam()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=rostam.cct.lsu.edu
  QUEUESYS=SLURM
  QCHECKCMD=squeue
  QSUMMARYCMD=squeue
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  QUEUENAME=marvin  # same as SLURM partition
  SERQUEUE=marvin
  ACCOUNT=null
  SUBMITSTRING=sbatch
  JOBLAUNCHER='srun -N %nnodes%'
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  PPN=40
  CONSTRAINT=null
  RESERVATION=null
  REMOVALCMD="rm"
  TDS=( lsu_tds )
  MAKEJOBS=8
}
init_supermic()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_supermic()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=supermic.hpc.lsu.edu
  QUEUESYS=PBS
  PPN=20
  QCHECKCMD=qstat
  QSUMMARYCMD=showq
  QUOTACHECKCMD=showquota
  ALLOCCHECKCMD=showquota
  QUEUENAME=workq
  SERQUEUE=single
  SUBMITSTRING=qsub
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  if [[ ${RMQMessaging_Enable} == "on" ]]; then
    RMQMessaging_LocationName="LSU"
    RMQMessaging_ClusterName="SuperMIC"
    RMQMessaging_NcoHome="$HOME/local"
  fi
  JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
  ACCOUNT=null
  PERL5LIB=${PERL5LIB}:${SCRIPTDIR}/PERL
  local THIS="platforms.sh>env_dispatch()>init_supermic()"
  SSHKEY=~/.ssh/id_rsa.pub
  REMOVALCMD="rmpurge"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  TDS=( lsu_tds )
  MAKEJOBS=8
}

# Note 0: This entry is meant to model a minimal entry
# Note 1: We're moving to placing conditional environements based on user or operator
# into $HOME/.asgsh_profile; e.g., setting MATLABEXE ('mex' or 'script' should go there)
# Note 3: Anything that is initialized without a value or as "null" should not be in here, but
# the variables should be documented somewhere
# Note 4: init-asgsh.sh should be run to ensure $WORK and $SCRATCH is set properly so that it
# propagates in the ASGS Shell environment

init_queenbeeC()
{ local THIS="platforms.sh>env_dispatch()>init_queenbeeC()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=qbc.loni.org
  QUEUESYS=SLURM
  QCHECKCMD=sacct
  QUEUENAME=workq
  SERQUEUE=single
  PPN=48
  JOBLAUNCHER='srun '
  SUBMITSTRING=sbatch
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  if [[ ${RMQMessaging_Enable} == "on" ]]; then
    RMQMessaging_LocationName="LONI"
    RMQMessaging_ClusterName="QueenbeeC"
    RMQMessaging_NcoHome=$WORK/local
  fi
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  TDS=( lsu_tds )
  MAKEJOBS=8
}

init_pod()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_pod()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=pod.penguincomputing.com
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=null
  QSUMMARYCMD=null
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  QUEUENAME=B30
  SERQUEUE=B30
  SUBMITSTRING=qsub
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl # asgs looks in $SCRIPTDIR for this
  if [[ ${RMQMessaging_Enable} == "on" ]]; then
    RMQMessaging_LocationName="Penguin"
    RMQMessaging_ClusterName="POD"
    RMQMessaging_NcoHome="$HOME/local"
  fi
  JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
  local THIS="platforms.sh>env_dispatch()>init_pod()"
  SSHKEY=~/.ssh/id_rsa.pub
  RESERVATION=null
  PPN=28
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  TDS=(renci_tds)
  MAKEJOBS=8
}
init_hatteras()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_hatteras()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=hatteras.renci.org
  QUEUESYS=SLURM
  QUEUENAME=batch # <---<< PARTITION synonym on slurm
  SERQUEUE=batch
  PPN=null
  CONSTRAINT=null      # ivybridge or sandybridge
  RESERVATION=null    # ncfs or null, causes job to run on dedicated cores
  PARTITION=null
  QCHECKCMD=sacct
  JOBLAUNCHER='srun '
  ACCOUNT=null
  SUBMITSTRING=sbatch
  SCRATCH=/projects/ncfs/data
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  WALLTIMEFORMAT="minutes"
  QSUMMARYCMD=null
  QUOTACHECKCMD="df -h /projects/ncfs"
  ALLOCCHECKCMD=null
  TDS=( renci_tds )
  #
  MATLABEXE=script # "script" means just execute matlab (don't use mex files)
  #
  RMQMessaging_Enable="on"      # "on"|"off"
  RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
  RMQMessaging_NcoHome="/home/ncfs"
  RMQMessaging_LocationName="RENCI"
  RMQMessaging_ClusterName="Hatteras"

  #
  # specify location of platform- and Operator-specific scripts to
  # set up environment for different types of jobs
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  MAKEJOBS=8
}
#
init_frontera()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_frontera()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=frontera.tacc.utexas.edu
  QUEUESYS=SLURM
  QUEUENAME=normal
  SERQUEUE=small
  PPN=56    # if this changes, also need to change the "small" queue defn in asgs_main.sh
  CONSTRAINT=null
  RESERVATION=null
  QOS=null
  QCHECKCMD=sacct
  JOBLAUNCHER='ibrun '
  ACCOUNT=null
  SUBMITSTRING=sbatch
  SSHKEY=~/.ssh/id_rsa_frontera
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  GROUP="G-803086"
  QSUMMARYCMD=null
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  #
  if [[ ${RMQMessaging_Enable} == "on" ]]; then
    RMQMessaging_LocationName="TACC"
    RMQMessaging_ClusterName="Frontera"
    RMQMessaging_NcoHome=$WORK/local
  fi
  MATLABEXE=script # "script" means just execute matlab (don't use mex files)
  # specify location of platform- and Operator-specific scripts to
  # set up environment for different types of jobs
  local THIS="platforms.sh>env_dispatch()>init_frontera()"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=/corral-tacc/utexas/hurricane/ASGS
  ARCHIVEDIR=2020 # is this used?
  TDS=( tacc_tds )
  MAKEJOBS=8
}
#
init_stampede2()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_stampede2()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=stampede2.tacc.utexas.edu
  QUEUESYS=SLURM
  QUEUENAME=skx-normal # same as SLURM partition
  SERQUEUE=skx-normal
  PPN=48
  CONSTRAINT=null
  RESERVATION=null
  QOS=null
  QCHECKCMD=sacct
  JOBLAUNCHER='ibrun '
  ACCOUNT=null
  SUBMITSTRING=sbatch
  SSHKEY=~/.ssh/id_rsa_stampede2
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  GROUP="G-803086"
  QSUMMARYCMD=null
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  if [[ ${RMQMessaging_Enable} == "on" ]]; then
    RMQMessaging_LocationName="TACC"
    RMQMessaging_ClusterName="Stampede2"
    RMQMessaging_NcoHome=$WORK/local
  fi
  local THIS="platforms.sh>env_dispatch()>init_stampede2()"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=/corral-tacc/utexas/hurricane/ASGS
  ARCHIVEDIR=2020
  TDS=( tacc_tds )
  MAKEJOBS=8
}
#
init_lonestar5()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_lonestar5()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=lonestar5.tacc.utexas.edu
  QUEUESYS=SLURM
  QUEUENAME=normal # same as SLURM partition
  SERQUEUE=normal
  CONSTRAINT=null
  RESERVATION=null
  QCHECKCMD=squeue
  JOBLAUNCHER='ibrun '
  ACCOUNT=null
  PPN=24
  SUBMITSTRING=sbatch
  SSHKEY=id_rsa_lonestar5
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  UMASK=006
  GROUP="G-803086"
  QSUMMARYCMD=null
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  # matlab
  MATLABEXE=script # "script" means just execute matlab (don't use mex files)
  #
  if [[ ${RMQMessaging_Enable} == "on" ]]; then
    RMQMessaging_LocationName="TACC"
    RMQMessaging_ClusterName="Lonestar5"
    RMQMessaging_NcoHome=$WORK/local
  fi
  local THIS="platforms.sh>env_dispatch()>init_lonestar5()"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=/corral-tacc/utexas/hurricane/ASGS
  ARCHIVEDIR=2020
  TDS=(tacc_tds)
  MAKEJOBS=8
}

# docker bootstrap
init_docker()
{
  local THIS="platforms.sh>env_dispatch()>init_docker()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=docker.local
  QUEUESYS=mpiexec
  QCHECKCMD="ps -aux | grep mpiexec "
  SUBMITSTRING="mpiexec "
  SCRATCH=${SCRATCH:-/scratch/$USER}
  SSHKEY=id_rsa_docker
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  TDS=()
  MAKEJOBS=2
}

# placeholder for vagrant bootstrap
init_vagrant() {
  MAKEJOBS=2
}

init_desktop()
{
  local THIS="platforms.sh>env_dispatch()>init_desktop()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=jason-desktop.seahorsecoastal.com
  QUEUESYS=mpiexec
  QCHECKCMD="ps -aux | grep mpiexec "
  SUBMITSTRING="mpiexec "
  SCRATCH=/srv/asgs
  SSHKEY=id_rsa_jason-desktop
  ADCOPTIONS='compiler=gfortran MACHINENAME=jason-desktop'
  SWANMACROSINC=macros.inc.gfortran
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  TDS=()
  MAKEJOBS=1
}

init_desktop_serial() # changed from init_desktop-serial due to bash complaints
{
  local THIS="platforms.sh>env_dispatch()>init_desktop-serial()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=jason-desktop-serial
  QUEUESYS=serial
  QCHECKCMD="ps -aux | grep adcirc "
  SUBMITSTRING="./"
  SCRATCH=/srv/asgs
  SSHKEY=id_rsa_jason-desktop-serial
  ADCOPTIONS='compiler=gfortran MACHINENAME=jason-desktop-serial'
  SWANMACROSINC=macros.inc.gfortran
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  TDS=(renci_tds)
  MAKEJOBS=1
}

init_Poseidon()
{
  HPCENV=poseidon.vsnet.gmu.edu
  QUEUESYS=mpiexec
  QCHECKCMD="ps -aux | grep mpiexec "
  SUBMITSTRING="mpiexec -n"
  SCRATCH=/home/fhrl/Documents/asgs_processing
  SSHKEY=id_rsa_jason-desktop
  ADCOPTIONS='compiler=gfortran MACHINENAME=jason-desktop'
  SWANMACROSINC=macros.inc.gfortran
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  MAKEJOBS=1
}
init_penguin()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_penguin()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=pod.penguincomputing.com
  #HOSTNAME=login-29-45.pod.penguincomputing.com
  QUEUESYS=PBS
  QCHECKCMD=qstat
  SCRATCH=/home/$USER
  SUBMITSTRING="mpirun"
  QSCRIPT=penguin.template.pbs
  QSCRIPTGEN=penguin.pbs.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  PPN=40
  MAKEJOBS=8
}
init_test()
{ #<- can replace the following with a custom script
  QUEUESYS=Test
  NCPU=-1
  MAKEJOBS=1
}
#
# Writes properties related to the combination of the HPC platform, the Operator,
# and the THREDDS data server the results are to be posted to.
writeTDSProperties()
{
   local THIS="platforms.sh>writeTDSProperties()"
   SERVER=$1
   RUNPROPERTIES=run.properties
   if [[ $# -eq 2 ]]; then
      RUNPROPERTIES=$2
   fi
   scenarioMessage "$THIS: Setting platforms-specific parameters for ${SERVER}."
   operator=$USER
   CATALOGPREFIX=""    # after thredds/catalog
   DOWNLOADPREFIX=""   # after thredds/fileServer
   case $SERVER in
   "renci_tds")
      # THREDDS Data Server (TDS, i.e., OPeNDAP server) at RENCI
      # http://tds.renci.org:8080/thredds/fileServer/DataLayers/asgs/tc/nam/2018070806/ec_95d/pod.penguin.com/podtest/namforecast/maxele.63.nc
      # http://tds.renci.org:8080/thredds/dodsC/     DataLayers/asgs/tc/nam/2018070806/ec_95d/pod.penguin.com/podtest/namforecast/maxele.63.nc
      # http://tds.renci.org:8080/thredds/catalog/                   tc/nam/2018070806/ec_95d/pod.penguin.com/podtest/namforecast/catalog.html
      THREDDSHOST=tds.renci.org # WWW hostname for emailed links
      OPENDAPHOST=renci_tds     # alias in $HOME/.ssh/config
      OPENDAPPORT=":8080"
      OPENDAPPROTOCOL="http"
      OPENDAPBASEDIR=/projects/ncfs/opendap/data
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> $RUNPROPERTIES
      echo "post.opendap.${SERVER}.copyablehosts : ( hatteras )" >> $RUNPROPERTIES
      #DOWNLOADPREFIX="http://tds.renci.org:8080/thredds/fileServer/DataLayers/asgs/"
      #CATALOGPREFIX="http://tds.renci.org:8080/thredds/DataLayers/asgs/"
      #OPENDAPBASEDIR=/projects/ees/DataLayers/asgs/
      ;;

   # THREDDS Data Server (TDS, i.e., OPeNDAP server) at LSU
   "lsu_tds")
      THREDDSHOST=fortytwo.cct.lsu.edu
      OPENDAPHOST=lsu_tds
      OPENDAPPORT=":443"
      OPENDAPPROTOCOL="https"
      OPENDAPBASEDIR=/data/opendap
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> $RUNPROPERTIES
      echo "post.opendap.${SERVER}.copyablehosts : ( null )" >> $RUNPROPERTIES
      ;;

   # THREDDS Data Server (TDS, i.e., OPeNDAP server) at LSU Center for Coastal Resiliency
   "lsu_ccr_tds")
      THREDDSHOST=chenier.cct.lsu.edu # WWW hostname for emailed links
      OPENDAPHOST=lsu_ccr_tds         # alias in $HOME/.ssh/config
      OPENDAPPORT=":8080"
      OPENDAPPROTOCOL="http"
      CATALOGPREFIX=/asgs/ASGS-2019
      DOWNLOADPREFIX=/asgs/ASGS-2019
      OPENDAPBASEDIR=/data/thredds/ASGS/ASGS-2019
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> $RUNPROPERTIES
      echo "post.opendap.${SERVER}.copyablehosts : ( null )" >> $RUNPROPERTIES
      ;;
   #
   # THREDDS Data Server (TDS, i.e., OPeNDAP server) at Texas
   # Advanced Computing Center (TACC)
   "tacc_tds")
      THREDDSHOST=adcircvis.tacc.utexas.edu # WWW hostname for emailed links
      OPENDAPHOST=tacc_tds                  # alias in $HOME/.ssh/config
      OPENDAPPORT=":8080"
      OPENDAPPROTOCOL="http"
      DOWNLOADPREFIX=/asgs
      CATALOGPREFIX=/asgs
      OPENDAPBASEDIR=/corral-tacc/utexas/hurricane/ASGS
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> $RUNPROPERTIES
      echo "post.opendap.${SERVER}.copyablehosts : ( lonestar5 stampede2 frontera )" >> $RUNPROPERTIES
      ;;
   "tacc_tds2")
      THREDDSHOST=chg-1.oden.tacc.utexas.edu # WWW hostname for emailed links
      OPENDAPHOST=tacc_tds2                 # alias in $HOME/.ssh/config
      OPENDAPPORT=":80"                     # ':80' can be an empty string, but for clarity it's here
      OPENDAPPROTOCOL="http"
      DOWNLOADPREFIX=/asgs
      CATALOGPREFIX=/asgs
      OPENDAPBASEDIR=/corral-tacc/utexas/hurricane/ASGS
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> $RUNPROPERTIES
      echo "post.opendap.${SERVER}.copyablehosts : ( lonestar5 stampede2 frontera )" >> $RUNPROPERTIES
      ;;
   *)
      echo "$THIS: ERROR: THREDDS Data Server $SERVER was not recognized."
   esac
   # now write properties
   echo "post.opendap.${SERVER}.opendaphost : $OPENDAPHOST" >> $RUNPROPERTIES
   echo "post.opendap.${SERVER}.threddshost : $THREDDSHOST" >> $RUNPROPERTIES
   echo "post.opendap.${SERVER}.downloadprefix : $OPENDAPPROTOCOL://$THREDDSHOST$OPENDAPPORT/thredds/fileServer$DOWNLOADPREFIX" >> $RUNPROPERTIES
   echo "post.opendap.${SERVER}.catalogprefix : $OPENDAPPROTOCOL://$THREDDSHOST$OPENDAPPORT/thredds/catalog$CATALOGPREFIX" >> $RUNPROPERTIES
   echo "post.opendap.${SERVER}.opendapbasedir : $OPENDAPBASEDIR" >> $RUNPROPERTIES
   # if the Operator has an asgs-global.conf file, assume that a perl mail client capability is
   # set up and ready to use
   echo "notification.opendap.email.opendapmailserver : aws" >> $RUNPROPERTIES
}
#
# set the values of HPCENV and HPCENVSHORT
set_hpc() {
   local THIS="platforms.sh>set_hpc()"
   echo "$THIS: Setting the values of HPCENV and HPCENVSHORT."
   fqdn=$(hostname --long)
   echo "$THIS: The fully qualified domain name is ${fqdn}."
   HPCENV=null
   HPCENVSHORT=null
   if [[ ${fqdn:(-18)} = "rostam.cct.lsu.edu" ]]; then
      HPCENV=${fqdn:(-18)}
      HPCENVSHORT=rostam
      return
   fi
   if [[ ${fqdn:(-25)} = "stampede2.tacc.utexas.edu" ]]; then
      HPCENV=${fqdn:(-25)}
      HPCENVSHORT=stampede2
      return
   fi
   if [[ ${fqdn:(-19)} = "ls5.tacc.utexas.edu" ]]; then
      HPCENV=lonestar5.tacc.utexas.edu
      HPCENVSHORT=lonestar5
      return
   fi
   if [[ ${fqdn:(-24)} = "frontera.tacc.utexas.edu" ]]; then
      HPCENV=frontera.tacc.utexas.edu
      HPCENVSHORT=frontera
      return
   fi
   if [ 1 -eq $(hostname --fqdn | grep -c qb1) ]; then
      HPCENV=queenbee.loni.org
      HPCENVSHORT=queenbee
   fi
   if [ 1 -eq $(hostname --fqdn | grep -c qb2) ]; then
      HPCENV=queenbee.loni.org
      HPCENVSHORT=queenbee
   fi
   if [ 1 -eq $(hostname --fqdn | grep -c qbc) ]; then
      HPCENV=qbc.loni.org
      HPCENVSHORT=queenbeeC
   fi
   if [[ ${fqdn:0:4} == "smic" ]]; then
      HPCENV=supermic.hpc.lsu.edu
      HPCENVSHORT=supermic
   fi
   if [[ ${fqdn:0:2} == "ht" ]]; then
      HPCENV=hatteras.renci.org
      HPCENVSHORT=hatteras
   fi
   if [[ ${fqdn:0:5} == "jason" ]]; then
      HPCENV=desktop.seahorsecoastal.com
      HPCENVSHORT=desktop
   fi
   if [ 1 -eq $(hostname --fqdn | grep -c soldier) ]; then
      HPCENV=soldier.seahorsecoastal.com
      HPCENVSHORT=desktop
   fi
   if [[ "${ASGS_MACHINE_NAME}" = "docker" ]]; then
      HPCENV=docker.local
      HPCENVSHORT=docker
   fi
   # this whole function will be replaced with guess, but for now ...
   if [[ $HPCENVSHORT = "null" ]]; then
      plat=$($SCRIPTDIR/bin/guess platform)
      HPCENVSHORT=$plat
      HPCENV=$plat
   fi
   echo "$THIS: The value of HPCENV is ${HPCENV}."
   echo "$THIS: The value of HPCENVSHORT is ${HPCENVSHORT}."
}
#
# used to dispatch environmentally sensitive actions
env_dispatch() {
 HPCENVSHORT=$1
 local THIS="platforms.sh>env_dispatch()"
 scenarioMessage "$THIS: Initializing settings for ${HPCENVSHORT}."
 echo "(info)    $THIS: Initializing settings for ${HPCENVSHORT}."
 case $HPCENVSHORT in
  "pod") allMessage "$THIS: POD (Penguin) configuration found."
          init_pod
          ;;
  "hatteras") allMessage "$THIS: Hatteras (RENCI) configuration found."
          init_hatteras
          ;;
  "supermike") allMessage "$THIS: Supermike (LSU) configuration found."
          init_supermike
          ;;
  "queenbee") allMessage "$THIS: Queenbee (LONI) configuration found."
          init_queenbee
          ;;
  "supermic") allMessage "$THIS: SuperMIC (LSU HPC) configuration found."
          init_supermic
          ;;
  "queenbeeC") allMessage "$THIS: QueenbeeC (LONI) configuration found."
          init_queenbeeC
          ;;
  "lonestar5") allMessage "$THIS: Lonestar (TACC) configuration found."
          init_lonestar5
          ;;
  "stampede2") allMessage "$THIS: Stampede2 (TACC) configuration found."
          init_stampede2
          ;;
  "frontera") allMessage "$THIS: Frontera (TACC) configuration found."
          init_frontera
          ;;
  "desktop") allMessage "$THIS: desktop configuration found."
          init_desktop
           ;;
  "desktop-serial") consoleMessage "$THIS: desktop-serial configuration found."
          init_desktop-serial
           ;;
  "docker") allMessage "$THIS: docker configuration found."
          init_docker
           ;;
  "poseidon") allMessage "$THIS: Poseidon configuration found."
          init_Poseidon
           ;;
  "penguin") allMessage "$THIS: Penguin configuration found."
          init_penguin
           ;;
  "rostam") allMessage "$THIS: rostam configuration found."
          init_rostam
           ;;
  "vagrant") allMessage "$THIS: vagrant configuration found."
          init_vagrant
           ;;
  "docker") allMessage "$THIS: docker configuration found."
          init_docker
           ;;
  "test") allMessage "$THIS: test environment (default) configuration found."
          init_test
           ;;
  *) fatal "$THIS: '$HPCENVSHORT' is not a supported environment; currently supported options: stampede2, lonestar5, supermike, queenbee, supermic, hatteras, desktop, desktop-serial, docker, su_tds, lsu_ccr_tds, renci_tds, tacc_tds, tacc_tds2"
     ;;
  esac

  # support arbitrarily $USER customizations after platforms.sh has been utilized
  # this file will also be replacing config/operator_defaults.sh in the near future
  if [ -e $HOME/.asgsh_profile ]; then
    source $HOME/.asgsh_profile
  fi
}

# This bash function uses:
# * local variables
# * doesn't affect environmenta "by reference" (implicitly)
# * echo's "return" so it can be captured by called using $() syntax
# e.g.,
#   QUEUENAME=$(HPCQueueHints "$QUEUENAME" "$HPCENV" "$QOS" "$CPUREQUEST")
HPC_Queue_Hint()
{
   # default, returned if conditions not met
   local DEFAULT_QUEUENAME=$1
   local HPCENV=$2
   local QOS=$3
   local CPUREQUEST=$4
   case "$HPCENV" in
   "frontera.tacc.utexas.edu")
     # on frontera, if a job uses only 1 or 2 nodes, it must be submitted to the
     # "small" queue ... this includes wind-only parallel jobs ... the PPN
     # for frontera is 56, so this hack would have to be updated if that changes
     if [[ $CPUREQUEST -eq 1 ]]; then
       echo "small"
     elif [[ $CPUREQUEST -le 112 ]]; then
       echo "small"
     else
       echo $DEFAULT_QUEUENAME
     fi
   ;;
   "qbc.loni.org")
     if [[ $CPUREQUEST -eq 1 ]]; then
       echo "single"
     elif [[ $CPUREQUEST -le 48 ]]; then
       echo "single"
     else
       echo $DEFAULT_QUEUENAME
     fi
   ;;
   *)
     echo $DEFAULT_QUEUENAME
   ;;
   esac
}

# encapsulated potentially hairy logic for adjusting PPN for
# certain platforms and based on more than one variable
HPC_PPN_Hint()
{
   local QUEUEKIND=$1
   local QUEUENAME=$2
   local HPCENV=$3
   local QOS=$4
   local DEFAULT_PPN=$5 # default, returned if conditions not met
   case "$HPCENV" in
   "supermic.hpc.lsu.edu")
     if [[ "$QUEUENAME" == "priority" && "$QUEUEKIND" == "serial" ]]; then
       echo 20
     else
       echo $DEFAULT_PPN
     fi
   ;;
   "queenbee.loni.org")
     if [[ "$QUEUENAME" == "priority" && "$QUEUEKIND" == "serial" ]]; then
       echo 20
     else
       echo $DEFAULT_PPN
     fi
   ;;
   *)
     echo $DEFAULT_PPN
   ;;
   esac
}


# This bash function uses:
# * local variables
# * doesn't affect environments "by reference" (implicitly)
# * echo's "return" so it can be captured by called using $() syntax
# e.g.,
#   RESERVATION=$(HPC_Reservation_Hint "$RESERVATION" "$HPCENV" "$QOS" "$CPUREQUEST")
HPC_Reservation_Hint()
{
   # default, returned if conditions not met
   local DEFAULT_RESERVATION=$1
   local HPCENV=$2
   local QOS=$3
   local CPUREQUEST=$4
   case "$HPCENV" in
   "frontera.tacc.utexas.edu")
     # on frontera, if a job uses only 1 or 2 nodes, it must be submitted to the
     # "small" queue ... this includes wind-only parallel jobs ... the PPN
     # for frontera is 56, so this hack would have to be updated if that changes
     # ... a reservation on Frontera that was created for parallel jobs will
     # not work on the "small" queue i.e., jobs (whether serial or parallel)
     # that use only 1 or 2 compute nodes
     if [[ $CPUREQUEST -eq 1 ]]; then
       echo "null"
     elif [[ $CPUREQUEST -le 112 ]]; then
       echo "null"
     else
       echo $DEFAULT_RESERVATION  # this is the reservation created by TACC for the Operator to use for parallel jobs
     fi
   ;;
   *)
     echo $DEFAULT_RESERVATION
   ;;
   esac
}

