#!/bin/bash
#----------------------------------------------------------------
# variables_init.sh: Called when ASGS first starts up, to declare
# and initialize variables.
#----------------------------------------------------------------
# Copyright(C) 2021--2023 Jason Fleming
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
variables_init()
{
# Initialize variables accessed from config.sh to reasonable values
   HPCENVSHORT=${HPCENVSHORT:-null}
   HPCENV=${HPCENV:-null}
   BACKGROUNDMET=on
   # tides
   TIDEFAC=off
   selfAttractionEarthLoadTide="notprovided"
   LOADTIDEURL=null
   #
   TROPICALCYCLONE=off
   GET_ATCF_SCRIPT=${GET_ATCF_SCRIPT:-"$SCRIPTDIR/get_atcf.pl"}
   WAVES=off
   VARFLUX=off
   HPCENV=${HPCENV:-"null"}
   HPCENVSHORT=${HPCENVSHORT:-"null"}
   MINMAX=reset
   REINITIALIZESWAN=no
   USERIVERFILEONLY=${USERIVERFILEONLY:-no}
   STORMNAME=${STORMNAME:-stormname}
   RIVERSITE=${RIVERSITE:-"ftp.nssl.noaa.gov"}
   RIVERDIR=${RIVERDIR:-"/projects/ciflow/adcirc_info"}
   RIVERUSER=${RIVERUSER:-null}
   RIVERDATAPROTOCOL=${RIVERDATAPROTOCOL:-null}
   # GFS
   declare -g -A gfsDomain
   declare -g -A gfsLatLonGrid
   GFSBACKSITE=null  # domain name of the site to download data from
   GFSBACKDIR=null   # path to data on remote server
   #
   ELEVSTATIONS=null
   VELSTATIONS=null
   METSTATIONS=null
   GRIDFILE=fort.14
   GRIDNAME=fort14
   OUTPUTOPTIONS=
   NSCREEN=${NSCREEN:-"-1000"}
   ARCHIVEBASE=/dev/null
   ARCHIVEDIR=null
   FORECASTCYCLE="00,06,12,18"
   TRIGGER="rss"
   LASTADVISORYNUM=0
   ADVISORY=0
   FORECASTLENGTH=84
   ALTNAMDIR=null
   HOTSTARTCOMP=fulldomain
   HINDCASTWALLTIME=${HINDCASTWALLTIME:-"10:00:00"}
   ADCPREPWALLTIME=${ADCPREPWALLTIME:-"00:30:00"}
   NOWCASTWALLTIME=${NOWCASTWALLTIME:-"02:00:00"}
   FORECASTWALLTIME=${FORECASTWALLTIME:-"05:00:00"}
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
   ADCIRCDIR=${ADCIRCDIR:-null} # will respect ADCIRCDIR if already sent in the environment
   # if not set, try to set SCRATCHDIR to SCRATCH (if set); otherwise default to "null"
   # "SCRATCH" is set on TACC platforms in a USER's default environment; init-asgs.sh sets
   # it for all others to provide some consistency
   if [ -z "$SCRATCHDIR" ]; then
     SCRATCHDIR=${SCRATCH:-null}
   fi
   MAILINGLIST=null
   INTENDEDAUDIENCE=${INTENDEDAUDIENCE:-"developers-only"} # "general" | "developers-only" | "professional"
   QUEUESYS=${QUEUESYS:-null}
   QUEUENAME=${QUEUENAME:-null}
   SERQUEUE=${SERQUEUE:-null}
   ACCOUNT=${ACCOUNT:-desktop}
   PPN=${PPN:-1}
   QCHECKCMD=null
   NCPU=null
   JOBTYPE=null
   NUMWRITERS=0
   SUBMITSTRING=null
   RUNDIR=${RUNDIR:-null}
   INPUTDIR=$SCRIPTDIR/input/meshes/null
   OUTPUTDIR=$SCRIPTDIR/output
   HOTSTARTFORMAT=null
   STORMDIR=stormdir
   SSHKEY=null
   VELOCITYMULTIPLIER=1.0
   HOTSWAN=off
   ONESHOT=no      # yes if ASGS is launched by cron
   NCPUCAPACITY=2  # total number of CPUs available to run jobs
   si=0       # scenario index for ; -1 indicates non-forecast
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
   RESERVATION=null # for SLURM
   CONSTRAINT=null # for SLURM
   QOS=null
   PERIODICFLUX=null
   SPATIALEXTRAPOLATIONRAMP=yes
   SPATIALEXTRAPOLATIONRAMPDISTANCE=1.0
   STATICOFFSET=null # (m), nonzero assumes unit offset file is available
   UNITOFFSETFILE=null
   ENSEMBLESIZE=null # deprecated in favor of SCENARIOPACKAGESIZE
   SCENARIOPACKAGESIZE=null
   # TODO: write all this configuration to the run.properties file
   declare -g -a INITPOST=( null_post.sh )
   declare -g -a subshellPIDs  # list of process IDs of subshells
   declare -g -a logFiles      # list of log files to be tailed onto scenario.log
   PYTHONVENV=null # path to python virtual environment, e.g., ~/asgs/asgspy/venv
   # mesh coordinate system
   zNorth="northpole"
   #
   #  H O O K S
   #
   # init and exit hooks
   initHooks=( START_INIT FINISH_INIT )
   # spinup hooks
   spinupHooks=( START_SPINUP_STAGE BUILD_SPINUP SUBMIT_SPINUP )
   spinupHooks+=( FINISH_SPINUP_SCENARIO HOT_SPINUP FINISH_SPINUP_STAGE )
   # nowcast hooks
   nowcastHooks=( START_NOWCAST_STAGE NOWCAST_POLLING NOWCAST_TRIGGERED BUILD_NOWCAST_SCENARIO )
   nowcastHooks+=( SUBMIT_NOWCAST_SCENARIO FINISH_NOWCAST_SCENARIO FINISH_NOWCAST_STAGE )
   # forecast hooks
   forecastHooks=( START_FORECAST_STAGE INITIALIZE_FORECAST_SCENARIO CAPACITY_WAIT )
   forecastHooks+=( BUILD_FORECAST_SCENARIO SUBMIT_FORECAST_SCENARIO FINISH_FORECAST_STAGE )
   # status properties
   declare -g -A hooksTimes      # time each hook is executed
   declare -g -A hooksScripts    # space-delimited string of scripts to execute at each hook
   declare -g -a allHooks
   allHooks=( ${initHooks[@]} ${spinupHooks[@]} ${nowcastHooks[@]} ${forecastHooks[@]} EXIT_STAGE )
   statusDir="null"
   previousStatusFile="null"
   previousHookStatusFile="null"
   hookStatusURL="null"
   previousHookStatusURL="null"
   asgsInstanceStatusURL="null"
   latestHook="null"
   enablePostStatus="no"     # yes if asgs instance status should be posted to local opendap
   enableStatusNotify="no"   # turn asgs instance status notification on and off
   statusNotify="null"       # comma separated list of email addresses
   notifyNow="no"            # used to limit notification to just the FINISH_INIT hook

   stage="SPINUP"  # modelling phase : SPINUP, NOWCAST, or FORECAST

   SCENARIO="null"
   SCENARIODIR="null"
   OPENDAPADDROOT=""
}
