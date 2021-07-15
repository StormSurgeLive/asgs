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

#-- testing
#QSCRIPTTEMPLATE="$SCRIPTDIR/qscript.template-test"

# Fundamental
INSTANCENAME=NCv999_al052021_bde     # "name" of this ASGS process
ASGSADMIN="asgsnotifications@opayq.com"

ACCOUNT=loni_cera_2020b
#QUEUENAME=priority # same as SLURM partition
QUEUENAME=workq
SERQUEUE=single
PPN=20

# Initial conditions
COLDSTARTDATE=$(get-coldstart-date) 

# Input files and templates

GRIDNAME=NCv999
source $SCRIPTDIR/config/mesh_defaults.sh

# Physical forcing (defaults set in config/forcing_defaults.sh)

TIDEFAC=on               # tide factor recalc
   HINDCASTLENGTH=30.0   # length of initial hindcast, from cold (days)
BACKGROUNDMET=off         # NAM download/forcing
   FORECASTCYCLE="06"
TROPICALCYCLONE=on      # tropical cyclone forcing
   STORM=05              # storm number, e.g. 05=ernesto in 2006
   YEAR=2021             # year of the storm
WAVES=off                # wave forcing
   REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off              # variable river flux forcing
#STATICOFFSET=0.30
#
CYCLETIMELIMIT="99:00:00"

# Computational Resources (related defaults set in platforms.sh)

NCPU=1099          # number of compute CPUs for all simulations
NUMWRITERS=1       # number of writers, usually 1
NCPUCAPACITY=9999  # total max number of CPUs used concurrently

# Post processing and publication

INTENDEDAUDIENCE=general    # "general" | "developers-only" | "professional"
POSTPROCESS=( createMaxCSV.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,asgsnotifications@opayq.com,cera.asgs.tk@gmail.com,asgsnotes4ian@gmail.com,janelle.fleming@seahorsecoastal.com"
NOTIFY_SCRIPT=ut-nam-notify.sh
TDS=( lsu_tds )

# Initial state (overridden by STATEFILE after ASGS gets going)

#
# Scenario package
#
#PERCENT=default
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
       ENSTORM=nhcConsensusWinds10m
       ;;
    1)
       ENSTORM=nhcConsensus
       ;;
    *)
       echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
      ;;
esac
source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz
