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

#
# D O  N O T   M A N U A L L Y  A D D  P L A T F O R M S  H E R E  A N Y M O R E
# See ./platforms/README
#

source ${SCRIPTDIR:-.}/monitoring/logging.sh

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
  JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
  ACCOUNT=null
  TDS=( lsu_tds )
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
init_supermic()
{ #<- can replace the following with a custom script
  local THIS="platforms.sh>env_dispatch()>init_supermic()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=supermic.hpc.lsu.edu
  QUEUESYS=SLURM
  PPN=20
  QCHECKCMD=sacct
  QUOTACHECKCMD=showquota
  ALLOCCHECKCMD=showquota
  QUEUENAME=workq
  SERQUEUE=single
  SUBMITSTRING=sbatch
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  JOBLAUNCHER='srun '
  PERL5LIB=${PERL5LIB}:${SCRIPTDIR}/PERL
  local THIS="platforms.sh>env_dispatch()>init_supermic()"
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
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCH
  ARCHIVEDIR=$SCRATCH
  TDS=( lsu_tds )
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
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  OPENDAPPOST=opendap_post.sh #<~ $SCRIPTDIR/output/ assumed
  GROUP="G-803086"
  QSUMMARYCMD=null
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  MATLABEXE=script # "script" means just execute matlab (don't use mex files)
  # specify location of platform- and Operator-specific scripts to
  # set up environment for different types of jobs
  local THIS="platforms.sh>env_dispatch()>init_frontera()"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=/corral-tacc/utexas/hurricane/ASGS
  ARCHIVEDIR=2020 # is this used?
  TDS=( tacc_tds3 )
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

#                                                                #
# D O  N O T  A D D  N E W  T H R E D D S  S E R V E R S H E R E #
#          please us the ./ssh-servers directory                 #
#                                                                #
writeTDSProperties()
{
   local THIS="platforms.sh>writeTDSProperties()"
   SERVER=$1
   local RUNPROPERTIES=run.properties
   if [[ $# -eq 2 ]]; then
      local RUNPROPERTIES=$2
   fi
   scenarioMessage "$THIS: Setting platforms-specific parameters for ${SERVER}."
   operator=$USER
   CATALOGPREFIX=""    # after thredds/catalog
   DOWNLOADPREFIX=""   # after thredds/fileServer
   case $SERVER in
   "renci_tds")
      THREDDSHOST=tds.renci.org # WWW hostname for emailed links
      OPENDAPINDEX=catalog.html
      OPENDAPHOST=renci_tds     # alias in $HOME/.ssh/config
      OPENDAPPORT=":80"
      OPENDAPPROTOCOL="http"
      OPENDAPBASEDIR=/projects/ncfs/opendap/data
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> $RUNPROPERTIES
      echo "post.opendap.${SERVER}.copyablehosts : ( hatteras )" >> $RUNPROPERTIES
      ;;

   # THREDDS Data Server (TDS, i.e., OPeNDAP server) at LSU
   "lsu_tds")
      THREDDSHOST=fortytwo.cct.lsu.edu
      OPENDAPINDEX=""           # this is just a directory listing, no index file
      OPENDAPHOST=lsu_tds
      OPENDAPPORT=":443"
      OPENDAPPROTOCOL="https"
      OPENDAPBASEDIR=/data/opendap
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> $RUNPROPERTIES
      echo "post.opendap.${SERVER}.copyablehosts : ( null )" >> $RUNPROPERTIES
      ;;
   *) # if not found, look in $SCRIPTDIR/ssh-servers where we add new servers now
      local SERVERDEF="${SCRIPTDIR}/ssh-servers/${SERVER}.sh"
      # if not found in ./ssh-servers and ASGS_LOCAL_DIR is defined, look in $ASGS_LOCAL_DIR/ssh-servers/
      local LOCALSERVERDEF="${ASGS_LOCAL_DIR}/ssh-servers/${SERVER}.sh"
      if [ -e "$SERVERDEF" ]; then
        consoleMessage "$I Found THREDDS Data Server $SERVER in ${SERVERDEF}.sh"
        source "$SERVERDEF"
      elif [[ -d "${ASGS_LOCAL_DIR}" && -e "${LOCALSERVERDEF}" ]]; then
        consoleMessage "$I Found THREDDS Data Server $SERVER in ${LOCALSERVERDEF}.sh"
        source "$LOCALSERVERDEF"
      else
        consoleMessage "$W ERROR: THREDDS Data Server $SERVER was not recognized."
      fi
      ;;
   esac
   # now write properties
   echo "post.opendap.${SERVER}.opendaphost : $OPENDAPHOST" >> $RUNPROPERTIES
   echo "post.opendap.${SERVER}.threddshost : $THREDDSHOST" >> $RUNPROPERTIES
   echo "post.opendap.${SERVER}.downloadprefix : $OPENDAPPROTOCOL://$THREDDSHOST$OPENDAPPORT/thredds/fileServer$DOWNLOADPREFIX" >> $RUNPROPERTIES
   echo "post.opendap.${SERVER}.catalogprefix : $OPENDAPPROTOCOL://$THREDDSHOST$OPENDAPPORT/thredds/catalog$CATALOGPREFIX" >> $RUNPROPERTIES
   echo "post.opendap.${SERVER}.opendapbasedir : $OPENDAPBASEDIR" >> $RUNPROPERTIES
   echo "post.opendap.${SERVER}.opendapindex : $OPENDAPINDEX" >> $RUNPROPERTIES
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
   # this whole function will be replaced with guess, but for now ...
   if [[ $HPCENVSHORT = "null" ]]; then
      plat=$($SCRIPTDIR/bin/guess platform)
      HPCENVSHORT=$plat
      HPCENV=$plat
   fi
   if [[ $HPCENVSHORT == "null" ]]; then
      echo "$THIS: FATAL: Could not determine what platform the ASGS is running on."
      exit 1
   fi
   echo "$THIS: The value of HPCENV is ${HPCENV}."
   echo "$THIS: The value of HPCENVSHORT is ${HPCENVSHORT}."
}

# general init function for platforms defined using the
# the init script as the first argument
init_platform()
{
  local INIT=${1:-"$PLATFORM_INIT"}
  local STATUSLOG=$RUNDIR/status.log
  if [[ $RUNDIR == "null" || -z $RUNDIR ]]; then
     if [[ ! -d $WORK/log ]]; then
        mkdir -p $WORK/log
     fi
     STATUSLOG=$WORK/log/status.log
  fi
  if [ -z "${INIT}" ]; then
    consoleMessage "${W} platform init script must be specified as the first argument! No platform init.sh known."
    return
  fi
  if [ ! -e "${INIT}" ]; then
    logMessage "${W} Can't find init script, '$INIT'! No platform selected."
    consoleMessage "${W} Can't find init script, '$INIT'! No platform selected."
    return
  fi
  echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] attempting to load '$INIT' ... " >> $STATUSLOG
  source $INIT
  if [ $? -eq 0 ]; then
    echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] OK" >> $STATUSLOG
  else
    echo
    logMessage "${W} Failed to load '$INIT'! No platform selected."
    consoleMessage "${W} Failed to load '$INIT'! No platform selected."
  fi
  return
}

#
# used to dispatch environmentally sensitive actions
env_dispatch() {
 HPCENVSHORT=$1
 local THIS="platforms.sh>env_dispatch()"
 scenarioMessage "$THIS: Initializing settings for ${HPCENVSHORT}."
 case $HPCENVSHORT in
  "queenbee") logMessage "$I Queenbee (LONI) configuration found."
          init_queenbee
          ;;
  "supermic") logMessage "$I SuperMIC (LSU HPC) configuration found."
          init_supermic
          ;;
  "queenbeeC") logMessage "$I QueenbeeC (LONI) configuration found."
          init_queenbeeC
          ;;
  "frontera") logMessage "$I Frontera (TACC) configuration found."
          init_frontera
          ;;

  "test") consoleMessage "$I test environment (default) configuration found."
          init_test
          ;;
  *) # fallback for new method of initializing a platform
          init_platform
          ;;
  esac

  # support arbitrarily $USER customizations after platforms.sh has been utilized
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
   "mike.hpc.lsu.edu")
     if [[ $CPUREQUEST -eq 1 ]]; then
       echo "single"
     elif [[ $CPUREQUEST -le 64 ]]; then
       echo "single"
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
   local CPUREQUEST=$6
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
   "mike.hpc.lsu.edu")
   if [[ "$QUEUENAME" == "single" && "$CPUREQUEST" -lt 64 ]]; then
       echo $CPUREQUEST
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

