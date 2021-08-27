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

INSTANCENAME=hsofs-al09-bob     # "name" of this ASGS process
SCRATCHDIR=/projects/ncfs-dev/${INSTANCENAME}
RMQMessaging_Transmit=on
QSCRIPTTEMPLATE=$SCRIPTDIR/config/2021/ncfs-dev/qscript.template.renci

# Input files and templates

GRIDNAME=hsofs
source $SCRIPTDIR/config/mesh_defaults.sh

#--------------------------------------------------------------------------
#  changes for 0.2286m sea_surface_height_above_geoid
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
# The default values of the following parameters are set in
# config/mesh_defaults.sh, so these settings have to come after the
# sourcing of the mesh_defaults.sh script.
CONTROLTEMPLATE=hsofs_explicit.15.template
CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
NAFILE=hsofs.13
NAPROPERTIES=${NAFILE}.properties
STATICOFFSET=0.0
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#  changes for 0.2286m sea_surface_height_above_geoid
#--------------------------------------------------------------------------

# Initial state (overridden by STATEFILE after ASGS gets going)

#COLDSTARTDATE=2020121500  #  2020080100  # calendar year month day hour YYYYMMDDHH24
#HOTORCOLD=coldstart       # "hotstart" or "coldstart"
#LASTSUBDIR=null
COLDSTARTDATE=auto
HOTORCOLD=hotstart      # "hotstart" or "coldstart"
#LASTSUBDIR=https://tds.renci.org:8080/thredds/fileServer/2021/nam/2021082618/hsofs/hatteras.renci.org/hsofs-nam-bob-2021/namforecast
LASTSUBDIR=/projects/ncfs-dev/hsofs-nam-bob-2021/asgs6711/2021082700/

# Physical forcing (defaults set in config/forcing_defaults.sh)

TIDEFAC=on                # tide factor recalc
   HINDCASTLENGTH=18    # length of initial hindcast, from cold (days)
BACKGROUNDMET=off          # NAM download/forcing
   FORECASTCYCLE="00,06,12,18"
TROPICALCYCLONE=on       # tropical cyclone forcing
   STORM=09               # storm number, e.g. 05=ernesto in 2006
   YEAR=2021              # year of the storm
WAVES=on                 # wave forcing
   REINITIALIZESWAN=no    # used to bounce the wave solution
VARFLUX=off               # variable river flux forcing
   RIVERSITE=data.disaster.renci.org
   RIVERDIR=/opt/ldm/storage/SCOOP/RHLRv9-OKU
   RIVERUSER=bblanton
   RIVERDATAPROTOCOL=scp
CYCLETIMELIMIT="99:00:00"

# !! NHC ftp site seems to be unreachable
FTPSITE="filesystem"       # hindcast/nowcast ATCF formatted files
HDIR="/home/ncfs-dev/asgs.master/input/sample_advisories/2021/al09"                 # hindcast dir on nhc ftp site

# Computational Resources (related defaults set in platforms.sh)

NCPU=638                     # number of compute CPUs for all simulations
NCPUCAPACITY=9999
#NCPU=511                     # number of compute CPUs for all simulations
#NCPUCAPACITY=512
NUMWRITERS=2
ACCOUNT=null
PARTITION=ncfs

# Post processing and publication

INTENDEDAUDIENCE="developers-only" # "general" # ( | "developers-only" | "professional"
#POSTPROCESS=( accumulateMinMax.sh createMaxCSV.sh cpra_slide_deck_post.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
#POSTPROCESS=( includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh transmit_rps.sh )
POSTPROCESS=( createOPeNDAPFileList.sh opendap_post.sh transmit_rps.sh )
#OPENDAPNOTIFY="asgs.cera.lsu@gmail.com jason.g.fleming@gmail.com"
#OPENDAPNOTIFY="bblanton@renci.org, asgs.cera.lsu@gmail.com, rluettich1@gmail.com, jason.g.fleming@gmail.com, asgsnotifications@opayq.com, cera.asgs.tk@gmail.com, asgsnotes4ian@gmail.com, janelle.fleming@seahorsecoastal.com"
OPENDAPNOTIFY="bblanton@renci.org"
NOTIFY_SCRIPT=ncfs_cyclone_notify.sh

# Scenario package

#PERCENT=default
SCENARIOPACKAGESIZE=6
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
    3)
       ENSTORM=nhcOfclWind10m
       source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
       ;;
    4)
       ENSTORM=veerRight100Wind10m
       PERCENT=100
       source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
       ;;
    5)
       ENSTORM=veerLeft100Wind10m
       PERCENT=-100
       source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
       ;;
    *)   
       echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
      ;;
esac

PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz
