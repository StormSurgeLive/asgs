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
# Copyright(C) 2021 Jason Fleming
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
# The defaults for parameters that can be reset in this config file 
# are preset in the following scripts:
# {SCRIPTDIR/platforms.sh               # also contains Operator-specific info
# {SCRIPTDIR/config/config_defaults.sh
# {SCRIPTDIR/config/mesh_defaults.sh
# {SCRIPTDIR/config/forcing_defaults.sh
# {SCRIPTDIR/config/io_defaults.sh
# {SCRIPTDIR/config/operator_defaults.sh
#-------------------------------------------------------------------

# Fundamental

INSTANCENAME=LAv20a_al092021_jgf_10kcms  # "name" of this ASGS process

# Input files and templates

GRIDNAME=LAv20a

CONTROLTEMPLATE=LAv20a_10kcms.15.template # <---<<< default is LA_v20a-WithUpperAtch.15.template in $SCRIPTDIR/config/mesh_defaults.sh
source $SCRIPTDIR/config/mesh_defaults.sh

# Physical forcing (defaults set in config/forcing_defaults)

TIDEFAC=on            # tide factor recalc
HINDCASTLENGTH=30.0   # length of initial hindcast, from cold (days)
BACKGROUNDMET=off      # NAM download/forcing
FORECASTCYCLE="06"
   forecastSelection="strict"
TROPICALCYCLONE=on   # tropical cyclone forcing
STORM=09             # storm number, e.g. 05=ernesto in 2006
YEAR=2021            # year of the storm
WAVES=on             # wave forcing
#STATICOFFSET=0.1524
REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off           # variable river flux forcing
CYCLETIMELIMIT="99:00:00"

# Computational Resources (related defaults set in platforms.sh)

NCPU=1919                     # number of compute CPUs for all simulations
NUMWRITERS=1
NCPUCAPACITY=9999

# Post processing and publication

INTENDEDAUDIENCE=general    # can also be "developers-only" or "professional"
POSTPROCESS=( includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh transmit_rps.sh )
OPENDAPNOTIFY="asgs.cera.lsu@gmail.com"
RMQMessaging_Enable="on"
RMQMessaging_Transmit="on"

# Initial state (overridden by STATEFILE after ASGS gets going)

COLDSTARTDATE=2021072800
HOTORCOLD=coldstart      # "hotstart" or "coldstart"
LASTSUBDIR=null

# Scenario package 

#PERCENT=default
SCENARIOPACKAGESIZE=10 # <====<<!!TEN TOTAL!! # number of scenarios
case $si in
 -2)
   ENSTORM=hindcast
   ;;
-1)
   # do nothing ... this is not a forecast
   ENSTORM=nowcast
   ;;
0)
   ENSTORM=veerRight100Wind10m
   PERCENT=100
   source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
   ;;
1)
   ENSTORM=veerRight100
   PERCENT=100
   ;;
2)
   ENSTORM=veerLeft100Wind10m
   PERCENT=-100
   source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
   ;;
3)
   ENSTORM=veerLeft100
   PERCENT=-100
   ;;
4)
   ENSTORM=nhcConsensusWind10m
   source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
   ;;
5)
   ENSTORM=nhcConsensus
   ;;
6)
   ENSTORM=veerLeft50Wind10m
   PERCENT=-50
   source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
   ;;
7)
   ENSTORM=veerLeft50
   PERCENT=-50
   ;;
8)
   ENSTORM=veerRight50Wind10m
   PERCENT=50
   source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
   ;;
9)
   ENSTORM=veerRight50
   PERCENT=50
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown scenario number: '$si'."
   ;;
esac

PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz
