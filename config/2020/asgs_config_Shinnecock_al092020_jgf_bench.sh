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

INSTANCENAME=Shinnecock_al092020_jgf_bench  # "name" of this ASGS process

# Input files and templates

GRIDNAME=Shinnecock
source $SCRIPTDIR/config/mesh_defaults.sh


# Physical forcing (defaults set in config/forcing_defaults)

TIDEFAC=on            # tide factor recalc
HINDCASTLENGTH=5.0   # length of initial hindcast, from cold (days)
BACKGROUNDMET=off      # NAM download/forcing
   FORECASTCYCLE="18"
TROPICALCYCLONE=on   # tropical cyclone forcing
   STORM=09           # storm number, e.g. 05=ernesto in 2006
   YEAR=2020          # year of the storm
WAVES=off              # wave forcing
#STATICOFFSET=0.1524
REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off           # variable river flux forcing
CYCLETIMELIMIT="99:00:00"

# Computational Resources (related defaults set in platforms.sh)

NCPU=2                    # number of compute CPUs for all simulations
NUMWRITERS=1
NCPUCAPACITY=4
#
# jason-desktop
ADCIRCDIR=/srv/asgs/adcirc-cg-v53release-gfortran/work
SWANDIR=/srv/asgs/adcirc-cg-v53release-gfortran/swan

# Post processing and publication

INTENDEDAUDIENCE=general    # can also be "developers-only" or "professional"
#POSTPROCESS=( createMaxCSV.sh cpra_slide_deck_post.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
POSTPROCESS=(  )
#OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,mbilsk3@lsu.edu,shagen@lsu.edu,jikeda@lsu.edu,fsanti1@lsu.edu,rluettich1@gmail.com"
OPENDAPNOTIFY="jason.g.fleming@gmail.com"
TDS=( )
RMQMessaging_Enable="off"      # "on"|"off"
RMQMessaging_Transmit="off"    #  enables message transmission ("on" | "off")

# Initial state (overridden by STATEFILE after ASGS gets going)

COLDSTARTDATE=2020072800
HOTORCOLD=coldstart      # "hotstart" or "coldstart"
LASTSUBDIR=null

# Scenario package 

#PERCENT=default
SCENARIOPACKAGESIZE=3 # number of storms in the ensemble
case $si in
 -2)
   ENSTORM=hindcast
   ;;
-1)
   # do nothing ... this is not a forecast
   ENSTORM=nowcast
   ;;
   #---------------------------------------------------------------   
   #
   # Wind10m, without dedicated writers
0)
   # 1 core serial 
   ENSTORM=nhcConsensusNCPU1NW0_Wind10m
   source $SCRIPTDIR/config/io_defaults.sh
   NCPU=1 
   NUMWRITERS=0
   NCPUCAPACITY=1
   ;;
1)
   # 2 cores parallel
   ENSTORM=nhcConsensusNCPU2NW0_Wind10m
   # sets met-only mode based on "Wind10m" suffix   
   source $SCRIPTDIR/config/io_defaults.sh 
   # need to set these after sourcing io_defaults.sh
   NCPU=2
   NUMWRITERS=0
   NCPUCAPACITY=2
   ;;
2)
   # 3 cores parallel
   ENSTORM=nhcConsensusNCPU3NW0_Wind10m
   # sets met-only mode based on "Wind10m" suffix   
   source $SCRIPTDIR/config/io_defaults.sh 
   # need to set these after sourcing io_defaults.sh
   NCPU=3
   NUMWRITERS=0
   NCPUCAPACITY=3
   ;;
3) 
   # 4 cores parallel
   ENSTORM=nhcConsensusNCPU4NW0_Wind10m
   source $SCRIPTDIR/config/io_defaults.sh
   NCPU=4 
   NUMWRITERS=0
   NCPUCAPACITY=4
   ;;
   #---------------------------------------------------------------
   #
   # Wind10m, with dedicated writer
4)
   # 2 cores parallel
   ENSTORM=nhcConsensusNCPU2NW1_Wind10m
   # sets met-only mode based on "Wind10m" suffix   
   source $SCRIPTDIR/config/io_defaults.sh 
   NCPU=2
   NUMWRITERS=1
   NCPUCAPACITY=3
   ;;
5)
   # 3 cores parallel
   ENSTORM=nhcConsensusNCPU3NW1_Wind10m
   source $SCRIPTDIR/config/io_defaults.sh
   NCPU=3
   NUMWRITERS=1
   NCPUCAPACITY=4  
   ;;
   #---------------------------------------------------------------
   #
   # Full physics, without dedicated writer
6)
   # 1 core serial 
   ENSTORM=nhcConsensusNCPU1NW0
   NCPU=1
   NUMWRITERS=0
   NCPUCAPACITY=1
   ;;
7)
   # 2 core parallel
   ENSTORM=nhcConsensusNCPU2NW0
   NCPU=2
   NUMWRITERS=0
   NCPUCAPACITY=2  
   ;;
8)
   # 3 core parallel
   ENSTORM=nhcConsensusNCPU3NW0
   NCPU=3
   NUMWRITERS=0
   NCPUCAPACITY=3  
   ;;  
9)
   # 4 core parallel
   ENSTORM=nhcConsensusNCPU4NW0
   NCPU=4
   NUMWRITERS=0
   NCPUCAPACITY=4  
   ;;     
   #---------------------------------------------------------------
   #
   # Full physics, with dedicated writer
10)
   # 2 core parallel
   ENSTORM=nhcConsensusNCPU2NW1
   NCPU=2
   NUMWRITERS=1
   NCPUCAPACITY=3  
   ;;
11)
   # 3 core parallel
   ENSTORM=nhcConsensusNCPU3NW1
   NCPU=3
   NUMWRITERS=1
   NCPUCAPACITY=4  
   ;;     
   #---------------------------------------------------------------
   #
   # Full physics, without i/o
12)
   # 1 core serial
   ENSTORM=nhcConsensusNCPU1noIO
   NCPU=1
   NUMWRITERS=0
   NCPUCAPACITY=1 
   #
   # water surface elevation station output
   FORT61="--fort61freq 864000.0 --fort61netcdf"
   # water current velocity station output
   FORT62="--fort62freq 864000.0"
   # full domain water surface elevation output
   FORT63="--fort63freq 864000.0 --fort63netcdf"
   # full domain water current velocity output
   FORT64="--fort64freq 864000.0 --fort64netcdf"
   # met station output
   FORT7172="--fort7172freq 864000.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 864000.0 --fort7374netcdf"
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   ;;
13)
   # 2 core parallel
   ENSTORM=nhcConsensusNCPU2noIO
   NCPU=2
   NUMWRITERS=0
   NCPUCAPACITY=4  
   #
   # water surface elevation station output
   FORT61="--fort61freq 864000.0 --fort61netcdf"
   # water current velocity station output
   FORT62="--fort62freq 864000.0"
   # full domain water surface elevation output
   FORT63="--fort63freq 864000.0 --fort63netcdf"
   # full domain water current velocity output
   FORT64="--fort64freq 864000.0 --fort64netcdf"
   # met station output
   FORT7172="--fort7172freq 864000.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 864000.0 --fort7374netcdf"
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   ;;
14)
   # 3 core parallel
   ENSTORM=nhcConsensusNCPU3noIO
   NCPU=3
   NUMWRITERS=0
   NCPUCAPACITY=3 
   #
   # water surface elevation station output
   FORT61="--fort61freq 864000.0 --fort61netcdf"
   # water current velocity station output
   FORT62="--fort62freq 864000.0"
   # full domain water surface elevation output
   FORT63="--fort63freq 864000.0 --fort63netcdf"
   # full domain water current velocity output
   FORT64="--fort64freq 864000.0 --fort64netcdf"
   # met station output
   FORT7172="--fort7172freq 864000.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 864000.0 --fort7374netcdf"
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   ;;  
15)
   # 4 core parallel
   ENSTORM=nhcConsensusNCPU4noIO
   NCPU=4
   NUMWRITERS=0
   NCPUCAPACITY=4 
   #
   # water surface elevation station output
   FORT61="--fort61freq 864000.0 --fort61netcdf"
   # water current velocity station output
   FORT62="--fort62freq 864000.0"
   # full domain water surface elevation output
   FORT63="--fort63freq 864000.0 --fort63netcdf"
   # full domain water current velocity output
   FORT64="--fort64freq 864000.0 --fort64netcdf"
   # met station output
   FORT7172="--fort7172freq 864000.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 864000.0 --fort7374netcdf"
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   ;;     

   #---------------------------------------------------------------
   #
   # meteorology only, without i/o
16)
   # 1 core serial
   ENSTORM=nhcConsensusNCPU1noIO_Wind10m
   source $SCRIPTDIR/config/io_defaults.sh 
   # need to set these after sourcing io_defaults.sh
   NCPU=1
   NUMWRITERS=0
   NCPUCAPACITY=1
   # met station output
   FORT7172="--fort7172freq 864000.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 864000.0 --fort7374netcdf"
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   ;;     
17)
   # 2 core parallel
   ENSTORM=nhcConsensusNCPU2noIO_Wind10m
   source $SCRIPTDIR/config/io_defaults.sh 
   # need to set these after sourcing io_defaults.sh
   NCPU=2
   NUMWRITERS=0
   NCPUCAPACITY=2 
   # met station output
   FORT7172="--fort7172freq 864000.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 864000.0 --fort7374netcdf"
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   ;;     
18)
   # 3 core parallel
   ENSTORM=nhcConsensusNCPU3noIO_Wind10m
   source $SCRIPTDIR/config/io_defaults.sh 
   # need to set these after sourcing io_defaults.sh
   NCPU=3
   NUMWRITERS=0
   NCPUCAPACITY=3 
   # met station output
   FORT7172="--fort7172freq 864000.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 864000.0 --fort7374netcdf"
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   ;;     
19)
   # 4 core parallel
   ENSTORM=nhcConsensusNCPU4noIO_Wind10m
   source $SCRIPTDIR/config/io_defaults.sh 
   # need to set these after sourcing io_defaults.sh
   NCPU=4
   NUMWRITERS=0
   NCPUCAPACITY=4
   # met station output
   FORT7172="--fort7172freq 864000.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 864000.0 --fort7374netcdf"
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   ;;     
*)
   echo "CONFIGRATION ERROR: Unknown scenario number: '$si'."
   ;;
esac

PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz
