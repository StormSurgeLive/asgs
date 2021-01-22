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

INSTANCENAME=hsofs_nam_akheir_25cm     # "name" of this ASGS process

# Input files and templates

GRIDNAME=hsofs
source $SCRIPTDIR/config/mesh_defaults.sh
#**********************************
# jgf20200919: After calling 
# mesh_defaults.sh, set the 
# following parameters:
#**********************************
NAFILE=hsofs_25cm.13
NAPROPERTIES=${NAFILE}.properties # <---<<<  jgf20201004: this is needed for correct metadata
CONTROLTEMPLATE=hsofs_explicit_25cm.15.template
CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties


#--------------------------------------------------------------------------
#  changes for 0.2286m sea_surface_height_above_geoid 
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
# The default values of the following parameters are set in
# config/mesh_defaults.sh, so these settings have to come after the
# sourcing of the mesh_defaults.sh script. 
#CONTROLTEMPLATE=hsofs_explicit_sshag.15.template
#CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
#NAFILE=hsofs_2286.13
#NAPROPERTIES=${NAFILE}.properties
#  ----> commented out static offset
#STATICOFFSET=0.1524
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#  changes for 0.2286m sea_surface_height_above_geoid 
#--------------------------------------------------------------------------

# Physical forcing (defaults set in config/forcing_defaults.sh)

TIDEFAC=on            # tide factor recalc
   HINDCASTLENGTH=30.0       # length of initial hindcast, from cold (days)
BACKGROUNDMET=on      # NAM download/forcing
   FORECASTCYCLE="06"
TROPICALCYCLONE=off   # tropical cyclone forcing
   STORM=99                         # storm number, e.g. 05=ernesto in 2006
   YEAR=2016                        # year of the storm
WAVES=on              # wave forcing
   REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off           # variable river flux forcing
CYCLETIMELIMIT="99:00:00"

#STATICOFFSET=0.30        # (m), assumes a unit offset file is available

# Computational Resources (related defaults set in platforms.sh)

NCPU=959                     # number of compute CPUs for all simulations
NCPUCAPACITY=3648
NUMWRITERS=1
QUEUENAME=priority    # queenbee2 and supermic
SERQUEUE=priority     # queenbee2 and supermic

# Post processing and publication

INTENDEDAUDIENCE=general    # can also be "developers-only" or "professional"
POSTPROCESS=( createMaxCSV.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
OPENDAPNOTIFY="kheirkhahan@gmail.com,asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,cera.asgs.tk@gmail.com,asgsnotes4ian@gmail.com"
TDS=(lsu_tds)

# Initial state (overridden by STATEFILE after ASGS gets going)

COLDSTARTDATE=2020091900   # calendar year month day hour YYYYMMDDHH24
HOTORCOLD=coldstart        # "hotstart" or "coldstart"
LASTSUBDIR=null

#COLDSTARTDATE=auto
#HOTORCOLD=hotstart
#LASTSUBDIR=http://fortytwo.cct.lsu.edu:8080/thredds/catalog/2019/nam/2019092506/hsofs/queenbee.loni.org/hsofs_nam_jgf/namforecast

# Scenario package

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
