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
# Copyright(C) 2018--2020 Jason Fleming
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

INSTANCENAME=HSOFS_nam_bde_25cm  # "name" of this ASGS process
ACCOUNT=ASC20001
QOS=vippj_p3000 # for priority during a storm
QUEUENAME=normal # same as SLURM partition
SERQUEUE=normal
PPN=56
GROUP="G-803086"
ASGSADMIN="asgsnotifications@opayq.com"

# Input files and templates

GRIDNAME=HSOFS
source $SCRIPTDIR/config/mesh_defaults.sh
NAFILE=hsofs_25cm.13
NAPROPERTIES=${NAFILE}.properties
CONTROLTEMPLATE=hsofs_explicit_25cm.15.template
CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties 

# Physical forcing (defaults set in config/forcing_defaults)
TIDEFAC=on                  # tide factor recalc
HINDCASTLENGTH=30.0         # length of initial hindcast, from cold (days)
BACKGROUNDMET=on            # NAM download/forcing
FORECASTCYCLE="00,06,12,18" # 
TROPICALCYCLONE=off         # tropical cyclone forcing
STORM=22                    # storm number, e.g. 05=ernesto in 2006
YEAR=2020                   # year of the storm
WAVES=on                    # wave forcing (on|off)
REINITIALIZESWAN=no         # used to bounce the wave solution
VARFLUX=off                 # variable river flux forcing
CYCLETIMELIMIT="99:00:00"

# Computational Resources (related defaults set in platforms.sh)
NCPU=1919                    # number of compute CPUs for all simulations
NUMWRITERS=1
NCPUCAPACITY=99999 

INTENDEDAUDIENCE=general    # can also be "developers-only" or "professional"
POSTPROCESS=( includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,rluettich1@gmail.com,cera.asgs.tk@gmail.com,asgsnotes4ian@gmail.com,asgsnotifications@opayq.com"
TDS=( lsu_tds tacc_tds )

# Initial state (overridden by STATEFILE after ASGS gets going)
COLDSTARTDATE=auto #2020092000 
HOTORCOLD=hotstart    # "hotstart" or "coldstart"
LASTSUBDIR=https://fortytwo.cct.lsu.edu/thredds/fileServer/2020/nam/2020102506/HSOFS/supermic.hpc.lsu.edu/HSOFS_nam_jgf_25cm/namforecast

# Scenario package 
#PERCENT=default
SCENARIOPACKAGESIZE=2 # number of storms in the ensemble

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
   echo "CONFIGRATION ERROR: Unknown scenario number: '$si'."
   ;;
esac
source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz

