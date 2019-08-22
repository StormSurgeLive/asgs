#!/bin/bash
#----------------------------------------------------------------
#
# config_defaults.sh: This script provides the default 
# configuration parameters for the ASGS.
#
#----------------------------------------------------------------
# Copyright(C) 2014--2019 Jason Fleming
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
THIS="config/config_defaults.sh"
   INSTANCENAME=nullInstanceName
   BACKGROUNDMET=on
   TIDEFAC=off
   TROPICALCYCLONE=off
   WAVES=off
   VARFLUX=off
   MINMAX=continuous
   REINITIALIZESWAN=no
   USERIVERFILEONLY=no
   STORMNAME=stormname
   RIVERSITE=ftp.nssl.noaa.gov
   RIVERDIR=/projects/ciflow/adcirc_info
   RIVERUSER=null
   RIVERDATAPROTOCOL=null
   ELEVSTATIONS=null
   VELSTATIONS=null
   METSTATIONS=null
   GRIDFILE=fort.14
   GRIDNAME=fort14
   OUTPUTOPTIONS=
   ARCHIVEBASE=/dev/null
   ARCHIVEDIR=null
   FORECASTCYCLE="00,06,12,18"
   TRIGGER="rss"
   LASTADVISORYNUM=0
   ADVISORY=0
   FORECASTLENGTH=84
   ALTNAMDIR=null
   HOTSTARTCOMP=fulldomain
   HINDCASTWALLTIME="10:00:00"
   ADCPREPWALLTIME="00:30:00"
   NOWCASTWALLTIME="02:00:00"
   FORECASTWALLTIME="05:00:00"
   WALLTIMEFORMAT="hh:mm:ss" # "hh:mm:ss" or "minutes"
   TIMESTEPSIZE=1.0
   SWANDT=600
   UMASK=002
   GROUP=""
   STORM=0
   YEAR=null
   CSDATE=null
   HOTORCOLD=coldstart
   LASTSUBDIR=null
   FTPSITE=null
   ADCIRCDIR=null
   SCRATCHDIR=null
   MAILINGLIST=null
   QUEUESYS=null
   QUEUENAME=null
   SERQUEUE=null
   QCHECKCMD=null
   NCPU=null
   JOBTYPE=null
   NUMWRITERS=0
   ACCOUNT=desktop
   SUBMITSTRING=null
   NOTIFYUSER=null
   RUNDIR=null
   INPUTDIR=null
   HOTSTARTFORMAT=null
   STORMDIR=stormdir
   SSHKEY=null
   PPN=1
   VELOCITYMULTIPLIER=1.0
   HOTSWAN=off
   ONESHOT=no      # yes if ASGS is launched by cron
   NCPUCAPACITY=2  # total number of CPUs available to run jobs
   si=0       # storm index for forecast ensemble; -1 indicates non-forecast
   STATEFILE=null
   ENSTORM=hindcast
   CYCLETIMELIMIT="05:00:00"
   IMAGEMAGICKBINPATH=null
   VORTEXMODEL=GAHM
   STORMTRACKOPTIONS="--strengthPercent null"
   PSEUDOSTORM=n
   MESHPROPERTIES=null
   CONTROLPROPERTIES=null 
   NAPROPERTIES=null
   EMAILNOTIFY=no # set to yes to have host platform email notifications
   NOTIFY_SCRIPT=null_notify.sh
   ACTIVATE_LIST=null
   NEW_ADVISORY_LIST=null
   POST_INIT_LIST=null
   POST_LIST=null
   JOB_FAILED_LIST=null
   NOTIFYUSER=null
   RESERVATION=null # for SLURM
   CONSTRAINT=null # for SLURM
   QOS=null
   ASGSADMIN=ASGSADMIN
   PERIODICFLUX=null
   SPATIALEXTRAPOLATIONRAMP=yes
   SPATIALEXTRAPOLATIONRAMPDISTANCE=1.0
   declare -a NOWCASTPOST=( null_post.sh )
   declare -a INITPOST=( null_post.sh )
   declare -a POSTPROCESS=( null_post.sh ) 
   declare -a JOBENV=( null )  # array of shell scripts to 'source' for compute job
   JOBENVDIR=null
   declare -a subshellPIDs  # list of process IDs of subshells
   declare -a logFiles      # list of log files to be tailed onto scenario.log
   PYTHONVENV=null # path to python virtual environment, e.g., ~/asgs/asgspy/venv
# RMQMessaging defaults
   RMQMessaging_Enable="off"   # "on"|"off"
   RMQMessaging_Transmit="off" #  enables message transmission ("on" | "off")
   RMQMessaging_Script="${SCRIPTDIR}/monitoring/asgs-msgr.py"
   RMQMessaging_StartupScript="${SCRIPTDIR}/monitoring/asgs-msgr_startup.py"
   RMQMessaging_NcoHome="/set/RMQMessaging_NcoHome/in/asgs/config"
   RMQMessaging_Python="/set/RMQMessaging_Python/in/asgs/config"
   namedot=${HPCENVSHORT}.
   RMQMessaging_LocationName=${HPCENV#$namedot}
   RMQMessaging_ClusterName=$HPCENVSHORT
