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

INSTANCENAME=LAv20a_al142020_jgf  # "name" of this ASGS process

# Input files and templates

GRIDNAME=LA_v20a-WithUpperAtch_chk
source $SCRIPTDIR/config/mesh_defaults.sh

# Physical forcing (defaults set in config/forcing_defaults)

#CONTROLTEMPLATE=LAv20a_26kcms.15.template # <---<<< default is LA_v20a-WithUpperAtch.15.template in $SCRIPTDIR/config/mesh_defaults.sh
#CONTROLTEMPLATE=LAv20a_23kcms.15.template # <---<<< default is LA_v20a-WithUpperAtch.15.template in $SCRIPTDIR/config/mesh_defaults.sh

#jgf20200721 : new template file with Matt's boundary condition
CONTROLTEMPLATE=LAv20a_12kcms.15.template # <---<<< default is LA_v20a-WithUpperAtch.15.template in $SCRIPTDIR/config/mesh_defaults.sh

TIDEFAC=on            # tide factor recalc
HINDCASTLENGTH=30.0   # length of initial hindcast, from cold (days)
BACKGROUNDMET=off      # NAM download/forcing
FORECASTCYCLE="00,06,12,18" # <---<<< #jgf20200721: was just 06 
TROPICALCYCLONE=on   # tropical cyclone forcing
STORM=14             # storm number, e.g. 05=ernesto in 2006
YEAR=2020            # year of the storm
WAVES=on            # wave forcing
#STATICOFFSET=0.1524
REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off           # variable river flux forcing
CYCLETIMELIMIT="99:00:00"

# Computational Resources (related defaults set in platforms.sh)

NCPU=959                     # number of compute CPUs for all simulations
NUMWRITERS=1
NCPUCAPACITY=9999 
ACCOUNT=loni_cera_2020
PARTITION=null
#QUEUENAME=priority    # queenbee2 and supermic
#SERQUEUE=priority     # queenbee2 and supermic
#QUEUENAME=workq   # not priority
#SERQUEUE=single   # not priority

#QOS=vip               # stampede2 and lonestar5
#
if [[ $USER = jgflemin ]]; then
   ADCIRCDIR=/work/jgflemin/adcirc-cg/work
   SWANDIR=/work/jgflemin/adcirc-cg/swan
   if [[ $HPCENVSHORT = queenbeeC ]]; then 
      ADCIRCDIR=/work/jgflemin/adcirc-cg-v53release-qbc-intel/work
      SWANDIR=/work/jgflemin/adcirc-cg-v53release-qbc-intel/swan
      QUEUENAME=workq   # not priority
      SERQUEUE=single   # not priority
   fi
   if [[ $HPCENVSHORT = frontera ]]; then
      ADCIRCDIR=$WORK/adcirc-cg/work
      SWANDIR=$WORK/adcirc-cg/swan
      QOS=vippj_p3000       # frontera
   fi
   if [[ $HPCENVSHORT = lonestar5 ]]; then
      NCPU=1399
      ADCIRCDIR=$WORK/adcirc-cg-v53release-intel/work
      SWANDIR=$WORK/adcirc-cg-v53release-intel/swan
      QOS=vip7000
      ACCOUNT=ADCIRC
   fi
   if [[ $HPCENVSHORT = supermic ]]; then
      ADCIRCDIR=/work/jgflemin/adcirc-cg-v53release-intel/work
      SWANDIR=/work/jgflemin/adcirc-cg-v53release-intel/swan
      ACCOUNT=hpc_lsu_ccr_20
      SERQUEUE=priority
      QUEUENAME=priority
   fi
   if [[ $HPCENVSHORT = queenbee ]]; then
      ADCIRCDIR=/work/jgflemin/adcirc-cg-v53release-intel/work
      SWANDIR=/work/jgflemin/adcirc-cg-v53release-intel/swan
      ACCOUNT=hpc_lsu_ccr_20
      SERQUEUE=priority
      QUEUENAME=priority
   fi
fi

# Post processing and publication

INTENDEDAUDIENCE=general    # can also be "developers-only" or "professional"
#POSTPROCESS=( createMaxCSV.sh cpra_slide_deck_post.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
POSTPROCESS=( createMaxCSV.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
#OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,mbilskie@uga.edu,rluettich1@gmail.com,shagen@lsu.edu,jikeda@lsu.edu,fsanti1@lsu.edu"
OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,mbilskie@uga.edu,shagen@lsu.edu,pbacopoulos@lsu.edu,rluettich1@gmail.com,cera.asgs.tk@gmail.com,asgsnotes4ian@gmail.com,asgsnotifications@opayq.com"
TDS=( lsu_tds )
if [[ $HPCENVSHORT = frontera || $HPCENVSHORT = stampede2 || $HPCENVSHORT = lonestar5 ]]; then
   TDS=( tacc_tds )
fi

# Initial state (overridden by STATEFILE after ASGS gets going)

COLDSTARTDATE=2020071800  
HOTORCOLD=hotstart     # "hotstart" or "coldstart"
LASTSUBDIR=/work/jgflemin/asgs253623/initialize

if [[ $HPCENVSHORT = supermic || $HPCENVSHORT = lonestar5 ]]; then
   COLDSTARTDATE=auto
   HOTORCOLD=hotstart     # "hotstart" or "coldstart"
   LASTSUBDIR=https://fortytwo.cct.lsu.edu/thredds/fileServer/2020/al14/07/LA_v20a-WithUpperAtch_chk/qbc.loni.org/LAv20a_al142020_jgf/nhcConsensus
fi

# Scenario package 

#PERCENT=default
SCENARIOPACKAGESIZE=6 # number of storms in the ensemble
case $si in
 -2)
   ENSTORM=hindcast
   ;;
-1)
   # do nothing ... this is not a forecast
   ENSTORM=nowcast
   ;;
 0)
   ENSTORM=veerLeft100Wind10m
   PERCENT=-100
   source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
   ;;
 1)
   ENSTORM=veerLeft100
   PERCENT=-100
   ;;
 2)
   ENSTORM=veerRight100Wind10m
   PERCENT=100
   source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
   ;;
 3)
   ENSTORM=veerRight100
   PERCENT=100
   ;;
 4)
   ENSTORM=nhcConsensusWind10m
   source $SCRIPTDIR/config/io_defaults.sh # sets met-only mode based on "Wind10m" suffix
   ;;
 5)
   ENSTORM=nhcConsensus
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown scenario number: '$si'."
   ;;
esac

PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz

