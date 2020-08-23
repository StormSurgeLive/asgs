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
# Copyright(C) 2019-2020 Jason Fleming
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

INSTANCENAME=LAv20a_nam_akheir     # "name" of this ASGS process

# Input files and templates

GRIDNAME=LA_v20a-WithUpperAtch_chk
source $SCRIPTDIR/config/mesh_defaults.sh

# Physical forcing (defaults set in config/forcing_defaults.sh)

#jgf20200721 : new template file with Matt's boundary condition
CONTROLTEMPLATE=LAv20a_12kcms.15.template # <---<<< default is LA_v20a-WithUpperAtch.15.template in $SCRIPTDIR/config/mesh_defaults.sh

TIDEFAC=on            # tide factor recalc
   HINDCASTLENGTH=30.0       # length of initial hindcast, from cold (days)
BACKGROUNDMET=on      # NAM download/forcing
   FORECASTCYCLE="00,06,12,18"
TROPICALCYCLONE=off   # tropical cyclone forcing
#   STORM=99                         # storm number, e.g. 05=ernesto in 2006
#   YEAR=2016                        # year of the storm
WAVES=off              # wave forcing
   REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off           # variable river flux forcing
CYCLETIMELIMIT="99:00:00"

# Computational Resources (related defaults set in platforms.sh)

NCPU=959                     # number of compute CPUs for all simulations
NCPUCAPACITY=9999
NUMWRITERS=1

# Post processing and publication

INTENDEDAUDIENCE=general    # can also be "developers-only" or "professional"
POSTPROCESS=( createMaxCSV.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
OPENDAPNOTIFY="kheirkhahan@gmail.com,asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,cera.asgs.tk@gmail.com,asgsnotes4ian@gmail.com,mbilskie@uga.edu"
TDS=(lsu_tds)

# Initial state (overridden by STATEFILE after ASGS gets going)

COLDSTARTDATE=2020071800   # calendar year month day hour YYYYMMDDHH24
HOTORCOLD=coldstart        # "hotstart" or "coldstart"
LASTSUBDIR=null


#COLDSTARTDATE=auto
#HOTORCOLD=hotstart     # "hotstart" or "coldstart"
#LASTSUBDIR=https://fortytwo.cct.lsu.edu/thredds/fileServer/2020/nam/2020073106/LA_v20a-WithUpperAtch_chk/supermic.hpc.lsu.edu/LAv20a_nam_akheir/namforecast

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
       ENSTORM=namforecastWind10m
       source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
       ;;
    1)
       ENSTORM=namforecast
       ;;
    *)   
       echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
      ;;
esac

PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz

