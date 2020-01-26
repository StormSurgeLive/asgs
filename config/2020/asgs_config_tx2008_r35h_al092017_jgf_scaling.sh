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

INSTANCENAME=tx2008_r35h_al092017_jgf_scaling  # "name" of this ASGS process

# Input files and templates

GRIDNAME=tx2008_r35h
source $SCRIPTDIR/config/mesh_defaults.sh

# Physical forcing (defaults set in config/forcing_defaults.sh)

TIDEFAC=on               # tide factor recalc
   HINDCASTLENGTH=30.0   # length of initial hindcast, from cold (days)
BACKGROUNDMET=off        # NAM download/forcing
   FORECASTCYCLE="06"
TROPICALCYCLONE=on       # tropical cyclone forcing
   STORM=09              # storm number, e.g. 05=ernesto in 2006
   YEAR=2017             # year of the storm
WAVES=on                 # wave forcing
   REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off              # variable river flux forcing
#STATICOFFSET=0.30
#
CYCLETIMELIMIT="99:00:00"

# set up for scaling test for harvey with Texas mesh
RSSSITE=filesystem
FTPSITE=filesystem
FDIR=$WORK/asgs/jasonfleming/asgs/input/sample_advisories/2017
HDIR=${FDIR}
# made the following symbolic links in $FDIR
#lrwxrwxrwx 1 jgflemin G-803086    16 Jan 21 13:20 bal092017.dat -> 16.bal092017.dat
#lrwxrwxrwx 1 jgflemin G-803086    22 Jan 21 14:12 index-at.xml -> 16.092017.index-at.xml

# Computational Resources (related defaults set in platforms.sh)

NCPU=2000                 # number of compute CPUs for all simulations
NCPUCAPACITY=99999
NUMWRITERS=1
if [[ $HPCENVSHORT = "hatteras" ]]; then
   NCPU=639 # max on hatteras
fi
# QOS=vip # does not work on Frontera
ADCIRCDIR=/home1/00976/jgflemin/adcirc-cg/work
SWANDIR=/home1/00976/jgflemin/adcirc-cg/swan

# Post processing and publication

INTENDEDAUDIENCE=developers-only    # "general" | "developers-only" | "professional"
PSEUDOSTORM=y
#POSTPROCESS=( accumulateMinMax.sh createMaxCSV.sh cpra_slide_deck_post.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
POSTPROCESS=( createMaxCSV.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
#OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,rluettich1@gmail.com"
OPENDAPNOTIFY="jason.g.fleming@gmail.com,rluettich1@gmail.com,brett.estrade@protonmail.com,akheir1@lsu.edu"
NOTIFY_SCRIPT=ut-nam-notify.sh
TDS=( tacc_tds lsu_tds renci_tds )

# Initial state (overridden by STATEFILE after ASGS gets going)

COLDSTARTDATE=2017072412 # 30 days prior to harvey advisory 12
HOTORCOLD=coldstart
LASTSUBDIR=null
#
# Scenario package
#
#PERCENT=default
SCENARIOPACKAGESIZE=15
#
case $si in
   -2)
       ENSTORM=hindcast
       ;;
   -1)
       # do nothing ... this is not a forecast
       ENSTORM=nowcast
       ;;
    #-----------------------------------------------------------------
    #                   W I N D   1 0 M  
    #-----------------------------------------------------------------
    0)
       # only the first one will be sent to CERA, so only the first one
       # needs to have the Wind10m met-only scenario
       ENSTORM=nhcConsensus0100Wind10m
       # sets met-only mode based on "Wind10m" suffix
       source $SCRIPTDIR/config/io_defaults.sh 
       ;;
    #-----------------------------------------------------------------
    #               W I T H O U T    S W A N  
    #-----------------------------------------------------------------
    1)
       ENSTORM=nhcConsensus0100
       NCPU=100
       NUMWRITERS=1
       WAVES=off
       # only notify CERA of the first one
       OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,rluettich1@gmail.com,brett.estrade@protonmail.com,akheir1@lsu.edu"
       ;;
    2)
       ENSTORM=nhcConsensus0200
       NCPU=200
       NUMWRITERS=1
       WAVES=off
       ;;
    3)
       ENSTORM=nhcConsensus0400
       NCPU=400
       NUMWRITERS=1
       WAVES=off
       ;;
    4)
       ENSTORM=nhcConsensus0800
       NCPU=800
       NUMWRITERS=1
       WAVES=off
       ;;
    5)
       ENSTORM=nhcConsensus1000
       NCPU=1000
       NUMWRITERS=1
       WAVES=off
       ;;
    6)
       ENSTORM=nhcConsensus2000
       NCPU=2000
       NUMWRITERS=1
       WAVES=off
       ;;
    7)
       ENSTORM=nhcConsensus4000
       NCPU=4000
       NUMWRITERS=1
       WAVES=off
       ;;
    8)
       ENSTORM=nhcConsensus8000
       NCPU=8000
       NUMWRITERS=1
       WAVES=off
       ;;
    #   
    #  effect of dedicated writers (none or 20), still without SWAN,
    #  only looking at cases with 100, 1000, 8000 compute cores
    #
    9)
       ENSTORM=nhcConsensus0100W0
       NCPU=100
       NUMWRITERS=0
       WAVES=off
       ;;
    10)
       ENSTORM=nhcConsensus0100W20
       NCPU=100
       NUMWRITERS=20
       WAVES=off
       ;;
    11)
       ENSTORM=nhcConsensus1000W0
       NCPU=1000
       NUMWRITERS=0
       WAVES=off
       ;;
    12)
       ENSTORM=nhcConsensus1000W20
       NCPU=1000
       NUMWRITERS=20
       WAVES=off
       ;;
    13)
       ENSTORM=nhcConsensus8000W0
       NCPU=8000
       NUMWRITERS=0
       WAVES=off
       ;;
    14)
       ENSTORM=nhcConsensus8000W20
       NCPU=8000
       NUMWRITERS=20
       WAVES=off
       ;;
    *)
       echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
      ;;
esac
#
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz
