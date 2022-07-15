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
# Copyright(C) 2019 Jason Fleming
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
RMQMessaging_Enable="on"
RMQMessaging_Transmit="on"
QSCRIPTTEMPLATE=$SCRIPTDIR/config/2022/ncfs-dev/qscript.template-bridges2-RM-Shared

#INSTANCENAME=ec95d-nam-bob-da-nowcast      # "name" of this ASGS process
INSTANCENAME=ec95d-al01-bob-psc      # "name" of this ASGS process
SCRATCHDIR="$HOME/results/${INSTANCENAME}"

# Source file paths
HM=/jet/home/bblanton
#ADCIRCDIR="$HM/ADCIRC/fixedblendandContEqOffset_adcv53-dev-238-g62e8042-modified/work"  # ADCIRC executables
ADCIRCDIR="$HM/ADCIRC/v53release/work"  # ADCIRC executables
SWANDIR="$ADCIRCDIR/../swan/"    # SWAN executables
SCRIPTDIR="$HM/asgs.renci-2021"    # ASGS executables
INPUTDIR=$SCRIPTDIR/input/meshes/ec95d     # grid and other input files
OUTPUTDIR=${SCRIPTDIR}/output              # post processing scripts


# Input files and templates

GRIDNAME=ec95d
source $SCRIPTDIR/config/mesh_defaults.sh

# Initial state (overridden by STATEFILE after ASGS gets going)

COLDSTARTDATE=2022050100  # calendar year month day hour YYYYMMDDHH24
HOTORCOLD=coldstart       # "hotstart" or "coldstart"
LASTSUBDIR=null

# Physical forcing (defaults set in config/forcing_defaults.sh)

TIDEFAC=on               # tide factor recalc
   HINDCASTLENGTH=32.0   # length of initial hindcast, from cold (days)
BACKGROUNDMET=off         # NAM download/forcing
   FORECASTCYCLE="00,06,12,18"
TROPICALCYCLONE=on      # tropical cyclone forcing
   STORM=01              # storm number, e.g. 05=ernesto in 2006
   YEAR=2022             # year of the storm
WAVES=on                 # wave forcing
   REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off              # variable river flux forcing
   RIVERSITE=data.disaster.renci.org
   RIVERDIR=/opt/ldm/storage/SCOOP/RHLRv9-OKU
   RIVERUSER=bblanton
   RIVERDATAPROTOCOL=scp
CYCLETIMELIMIT="99:00:00"

# Computational Resources (related defaults set in platforms.sh)

NCPU=16                    # number of compute CPUs for all simulations
NCPUCAPACITY=128
NUMWRITERS=0
#ACCOUNT=null
QUEUENAME="RM-shared"

# Post processing and publication

INTENDEDAUDIENCE=developers-only    # "general" | "developers-only" | "professional"

FINISH_NOWCAST_SCENARIO=( output/opendap_post_nowcast.sh output/opendap_post_nowcast_k8.sh ) # output/run_adda.sh )
#POSTPROCESS=( accumulateMinMax.sh createMaxCSV.sh cpra_slide_deck_post.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
POSTPROCESS=( createOPeNDAPFileList.sh opendap_post.sh opendap_post_k8.sh transmit_rps.sh )
#POSTPROCESS=( includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh transmit_rps.sh )

#OPENDAPNOTIFY="asgs.cera.lsu@gmail.com jason.g.fleming@gmail.com"
OPENDAPNOTIFY="bblanton@renci.org"
NOTIFY_SCRIPT=ncfs_nam_notify.sh

# Scenario package

#PERCENT=default
SCENARIOPACKAGESIZE=1
case $si in
   -2) 
       ENSTORM=hindcast
       ;;
   -1)      
       # do nothing ... this is not a forecast
       ENSTORM=nowcast
       ;;
    0)
       ENSTORM=nhcOfcl
       ;;
    1)
       ENSTORM=veerLeft100
       PERCENT=-100
       ;;
    2)
       ENSTORM=veerRight100
       PERCENT=100
       ;;
    *)   
       echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
      ;;
esac

PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz
