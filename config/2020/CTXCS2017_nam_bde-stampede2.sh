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
# Copyright(C) 2020 Jason Fleming
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

INSTANCENAME=CTXCS2017_nam_bde     # "name" of this ASGS process
ACCOUNT=DesignSafe-CERA
QOS=vip7000 # for priority during a storm
QUEUENAME=skx-normal # same as SLURM partition
SERQUEUE=skx-normal
PPN=48
GROUP="G-803086"
ASGSADMIN="asgsnotifications@opayq.com"

# Input files and templates

GRIDNAME=CTXCS2017
source $SCRIPTDIR/config/mesh_defaults.sh

# Initial state (overridden by STATEFILE after ASGS gets going)

COLDSTARTDATE=2020081718
HOTORCOLD=coldstart
LASTSUBDIR=null

RMQMessaging_Enable="on"
RMQMessaging_Transmit="on"

#FTPSITE=ftp.nhc-replay.stormsurge.email
#RSSSITE=nhc-replay.stormsurge.email

# Physical forcing (defaults set in config/forcing_defaults.sh)

TIDEFAC=on               # tide factor recalc
   HINDCASTLENGTH=30.0   # length of initial hindcast, from cold (days)
BACKGROUNDMET=on         # NAM download/forcing
   FORECASTCYCLE="00,06,12,18"
TROPICALCYCLONE=off      # tropical cyclone forcing
   STORM=19              # storm number, e.g. 05=ernesto in 2006
   YEAR=2020             # year of the storm
WAVES=off                # wave forcing
   REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off              # variable river flux forcing
#STATICOFFSET=0.30
#
CYCLETIMELIMIT="99:00:00"

# Computational Resources (related defaults set in platforms.sh)

NCPU=2016         # number of compute CPUs for all simulations
NCPUCAPACITY=9999
NUMWRITERS=1

# Post processing and publication

INTENDEDAUDIENCE=general    # "general" | "developers-only" | "professional"
#POSTPROCESS=( accumulateMinMax.sh createMaxCSV.sh cpra_slide_deck_post.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
POSTPROCESS=( createMaxCSV.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,asgsnotifications@opayq.com,rluettich1@gmail.com,asgsnotes4ian@gmail.com,cera.asgs.tk@gmail.com,clint@oden.utexas.edu,amin.kiaghadi2013@gmail.com"
NOTIFY_SCRIPT=ut-nam-notify.sh
TDS=( tacc_tds lsu_tds )

#
# Scenario package
SCENARIOPACKAGESIZE=2
case $si in
   -2)
       ENSTORM=hindcast
       ;;
   -1)
       # do nothing ... this is not a forecast
       ENSTORM=nowcast
       ;;
    0)
       ENSTORM=namforecastWind10m
       ;;
    1)
       ENSTORM=namforecast
       ;;
    *)
       echo "CONFIGURATION ERROR: Unknown ensemble member number: '$si'."
      ;;
esac
source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
#
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz
