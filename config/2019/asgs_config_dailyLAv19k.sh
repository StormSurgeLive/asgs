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
# Copyright(C) 2018--2019 Jason Fleming
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

INSTANCENAME=dailyLAv19kmaster  # "name" of this ASGS process

# Input files and templates

GRIDNAME=LA_v19k-WithUpperAtch_chk
source $SCRIPTDIR/config/mesh_defaults.sh

# Physical forcing (defaults set in config/forcing_defaults)

TIDEFAC=on            # tide factor recalc
HINDCASTLENGTH=30.0   # length of initial hindcast, from cold (days)
BACKGROUNDMET=on      # NAM download/forcing
TROPICALCYCLONE=off   # tropical cyclone forcing
#STORM=07             # storm number, e.g. 05=ernesto in 2006
#YEAR=2018            # year of the storm
WAVES=off             # wave forcing
REINITIALIZESWAN=no   # used to bounce the wave solution
VARFLUX=off           # variable river flux forcing
CYCLETIMELIMIT="99:00:00"

# Computational Resources (related defaults set in platforms.sh)

NCPU=959                     # number of compute CPUs for all simulations
NUMWRITERS=1
NCPUCAPACITY=3600

# Post processing and publication

INTENDEDAUDIENCE=general    # can also be "developers-only" or "professional"
POSTPROCESS=( accumulateMinMax.sh createMaxCSV.sh cpra_slide_deck_post.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )

# Initial state (overridden by STATEFILE after ASGS gets going)

COLDSTARTDATE=auto  
HOTORCOLD=hotstart       # "hotstart" or "coldstart"
LASTSUBDIR=/work/jgflemin/asgs19009/2019081200  # path to previous execution (if HOTORCOLD=hotstart)

# Scenario package 

#PERCENT=default
ENSEMBLESIZE=2 # number of storms in the ensemble
case $si in
-1)
      # do nothing ... this is not a forecast
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

# Computational Resources

# (defaults for the following set in config/mesh_defaults.sh)
#TIMESTEPSIZE
#HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
#ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
#NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
#FORECASTWALLTIME="07:00:00" # forecast wall clock time
#CYCLETIMELIMIT="06:00:00"
#SWANDT=1200                 # swan time step size (seconds)
# (defaults for the following set in platforms.sh)
#SERQUEUE=priority
#QUEUENAME=priority
#ACCOUNT=loni_lsu_ccr_18

# External data sources : Tropical cyclones

#RSSSITE=filesystem
#FTPSITE=filesystem
#FDIR=~/asgs/branches/2014stable/input/sample_advisories/2018
#HDIR=${FDIR}
# default values as set in config/forcing_defaults.sh
#STORM=07                         # storm number, e.g. 05=ernesto in 2006
#YEAR=2018                        # year of the storm
#TRIGGER=rssembedded              # either "ftp" or "rss"
#RSSSITE=www.nhc.noaa.gov         # site information for retrieving advisories
#FTPSITE=ftp.nhc.noaa.gov         # hindcast/nowcast ATCF formatted files
#FDIR=/atcf/afst                  # forecast dir on nhc ftp site
#HDIR=/atcf/btk                   # hindcast dir on nhc ftp site

# External data sources : Background Meteorology

# defaults set in config/forcing_defaults.sh
#FORECASTCYCLE="06"
#BACKSITE=ftp.ncep.noaa.gov          # NAM forecast data from NCEP
#BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
#FORECASTLENGTH=84                   # hours of NAM forecast to run (max 84)
#PTFILE=ptFile_oneEighth.txt         # the lat/lons for the OWI background met
#ALTNAMDIR="/projects/ncfs/data/asgs5463","/projects/ncfs/data/asgs14174"
#VELOCITYMULTIPLIER=1.0

# External data sources : River Flux

#RIVERSITE=ftp.nssl.noaa.gov
#RIVERDIR=/projects/ciflow/adcirc_info

# Input files and templates

#GRIDNAME=LA_v19k-WithUpperAtch_chk
#TIMESTEPSIZE=1.0           # adcirc time step size (seconds)
#GRIDFILE=LA_v19k-WithUpperAtch_chk.grd   # mesh (fort.14) file
#MESHPROPERTIES=${GRIDFILE}.properties
#CONTROLTEMPLATE=LA_v19k-WithUpperAtch.15.template
#CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
#ELEVSTATIONS=combined_stations_20190327.txt
#VELSTATIONS=combined_stations_20190327.txt
#METSTATIONS=combined_stations_20190327.txt
#NAFILE=LA_v19k-WithUpperAtch_chk.13
#NAPROPERTIES=${NAFILE}.properties
#SWANTEMPLATE=LA_v19k-WithUpperAtch.26.template   # only used if WAVES=on
#RIVERINIT=null                           # this mesh has no rivers ...
#RIVERFLUX=null
#HINDCASTRIVERFLUX=null

# Output files

# (use defaults set in config/io_defaults.sh)
# water surface elevation station output
#FORT61="--fort61freq 300.0 --fort61netcdf" 
# water current velocity station output
#FORT62="--fort62freq 0"                    
# full domain water surface elevation output
#FORT63="--fort63freq 3600.0 --fort63netcdf" 
# full domain water current velocity output
#FORT64="--fort64freq 3600.0 --fort64netcdf" 
# met station output
#FORT7172="--fort7172freq 300.0 --fort7172netcdf"           
# full domain meteorological output
#FORT7374="--fort7374freq 3600.0 --fort7374netcdf"           
#SPARSE="--sparse-output"
#SPARSE=""
#NETCDF4="--netcdf4"
#OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
# fulldomain or subdomain hotstart files
#HOTSTARTCOMP=fulldomain                    
# binary or netcdf hotstart files
#HOTSTARTFORMAT=netcdf                      
# "continuous" or "reset" for maxele.63 etc files
#MINMAX=reset                               

# Notification

#NOTIFY_SCRIPT=corps_cyclone_notify.sh
# (the following are set in config/operator_defaults.sh)
#EMAILNOTIFY=yes       
#ACTIVATE_LIST=null
#NEW_ADVISORY_LIST=null
#POST_INIT_LIST=null
#POST_LIST=null
#JOB_FAILED_LIST="jason.fleming@seahorsecoastal.com"
#NOTIFYUSER=jason.fleming@seahorsecoastal.com
#ASGSADMIN=jason.fleming@seahorsecoastal.com

# RMQ Messaging

# (the following are set in platforms.sh and config/operator_defaults.sh)
#RMQMessaging_Enable="on"   # "on"|"off" 
#RMQMessaging_Transmit="on" #  enables message transmission ("on" | "off")     
#RMQMessaging_Script="/set/RMQMessaging_Script/in/asgs/config"     
#RMQMessaging_NcoHome=$HOME     
#RMQMessaging_Python=/usr/bin/python     
#RMQMessaging_LocationName="Seahorse"     
#RMQMessaging_ClusterName="jason-desktop-serial"

# opendap

#TDS=(lsu_tds renci_tds)                     # default set in platforms.sh
#OPENDAPNOTIFY="jason.g.fleming@gmail.com"   # default set in config/operator_defaults.sh

# Archiving

# ( use default values as set in platforms.sh )
#ARCHIVE=enstorm_pedir_removal.sh
#ARCHIVEBASE=/work/jgflemin
#ARCHIVEDIR=${ARCHIVEBASE}/asgs_archive

