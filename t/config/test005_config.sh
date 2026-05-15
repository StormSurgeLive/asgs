#!/bin/sh
#-------------------------------------------------------------------
# config.sh: This file is read at the beginning of the execution of the ASGS to
# set up the runs  that follow. It is reread at the beginning of every cycle,
# every time it polls the datasource for a new advisory. This gives the user
# the opportunity to edit this file mid-storm to change config parameters
# (e.g., the name of the queue to submit to, the addresses on the mailing list,
# etc)
#-------------------------------------------------------------------
#
# Copyright(C) 2025 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# ASGS is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------

# Fundamental

INSTANCENAME=Shinnecock_al132020_kitt_jgf # "name" of this ASGS process

# Input files and templates

GRIDNAME=Shinnecock
parameterPackage=default
createWind10mLayer="yes"
source $SCRIPTDIR/config/mesh_defaults.sh

# Physical forcing (defaults set in config/forcing_defaults.sh)

TIDEFAC=on               # tide factor recalc
   HINDCASTLENGTH=1.0    # length of initial hindcast, from cold (days)
BACKGROUNDMET=off        # NAM/GFS download/forcing
   FORECASTCYCLE="00,06,12,18"
TROPICALCYCLONE=on       # tropical cyclone forcing
   STORM=18              # storm number, e.g. 05=ernesto in 2006
   YEAR=2012             # year of the storm
   FDIR=$WORK
   HDIR="$FDIR"
   RSSSITE=filesystem
   FTPSITE=filesystem
WAVES=on                 # wave forcing
   REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off              # variable river flux forcing
#
CYCLETIMELIMIT="99:00:00"

# Computational Resources (related defaults set in platforms.sh)

NCPU=3                 # number of compute CPUs for all simulations
NCPUCAPACITY=9999
NUMWRITERS=1

# Post processing and publication

INTENDEDAUDIENCE=general    # "general" | "developers-only" | "professional"
OPENDAPPOST=opendap_post2.sh
#POSTPROCESS=( includeWind10m.sh createOPeNDAPFileList.sh $OPENDAPPOST )
#hooksScripts[FINISH_SPINUP_SCENARIO]=" output/createOPeNDAPFileList.sh output/$OPENDAPPOST "
#hooksScripts[FINISH_NOWCAST_SCENARIO]=" output/includeWind10m.sh output/createOPeNDAPFileList.sh output/$OPENDAPPOST "
OPENDAPNOTIFY="jason.fleming@stormsurge.live"

# Monitoring

enablePostStatus="no"
enableStatusNotify="no"
statusNotify="null"

# Initial state (overridden by STATEFILE after ASGS gets going)

COLDSTARTDATE=2020092818
HOTORCOLD=coldstart
LASTSUBDIR=null

#
# Scenario package
#
case $si in
   -2)
       ENSTORM=hindcast
       OPENDAPNOTIFY="null"  # <-<< do not notify CERA
       ;;
   -1)
       # do nothing ... this is not a forecast
       ENSTORM=nowcast
       OPENDAPNOTIFY="null"  # <-<< do not notify CERA
       ;;
    0)
       ENSTORM=cooperative17
       ;;
    *)
       echo "CONFIGURATION ERROR: Unknown scenario number: '$si'."
       ;;
esac
#
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz
