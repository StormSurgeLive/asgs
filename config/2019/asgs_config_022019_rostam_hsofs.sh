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
# Copyright(C) 2017--2018 Jason Fleming
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

INSTANCENAME=022019hsofs    # "name" of this ASGS process
# COLDSTARTDATE=auto
COLDSTARTDATE=2019050300  # YYYYMMDDHH24 or "auto" to extract from hotstart file
HOTORCOLD=hotstart      # "hotstart" or "coldstart"
LASTSUBDIR=e.g./work/jgflemin/asgs6337/2019060412  # path to previous execution (if HOTORCOLD=hotstart)
# LASTSUBDIR=https://forttwo ...
HINDCASTLENGTH=30.0      # length of initial hindcast, from cold (days)
REINITIALIZESWAN=no      # used to bounce the wave solution

# Source file paths
# < fill in own paths>
ADCIRCDIR=~/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
SWANDIR=~/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
SCRIPTDIR=~/asgs/jasonfleming/2014stable           # ASGS executables
INPUTDIR=${SCRIPTDIR}/input/meshes/hsofs # grid and other input files
OUTPUTDIR=${SCRIPTDIR}/output # post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL    # DateCale.pm perl module

# Physical forcing

BACKGROUNDMET=off     # NAM download/forcing
TIDEFAC=on           # tide factor recalc
TROPICALCYCLONE=on  # tropical cyclone forcing
WAVES=off           # wave forcing
VARFLUX=off          # variable river flux forcing

# Computational Resources

TIMESTEPSIZE=2.0            # adcirc time step size (seconds)
SWANDT=1200                 # swan time step size (seconds)
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="10:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="05:00:00" # forecast wall clock time
NCPU=159                    # number of compute CPUs for all simulations
NUMWRITERS=1
NCPUCAPACITY=160 # total number of cpus to use (e.g. 1200) 
CYCLETIMELIMIT="99:00:00" # amount of time to wait for next cycle
QUEUENAME=workq
SERQUEUE=single

# External data sources : Tropical cyclones

STORM=02                         # storm number, e.g. 05=ernesto in 2006
YEAR=2019                        # year of the storm
TRIGGER=rssembedded              # either "ftp" or "rss"
#RSSSITE=filesystem
#FTPSITE=filesystem
#FDIR=~/asgs/2014stable/input/sample_advisories/isaac
#HDIR=$FDIR
RSSSITE=www.nhc.noaa.gov         # site information for retrieving advisories
FTPSITE=ftp.nhc.noaa.gov         # hindcast/nowcast ATCF formatted files
FDIR=/atcf/afst                  # forecast dir on nhc ftp site
HDIR=/atcf/btk                   # hindcast dir on nhc ftp site

# External data sources : Background Meteorology

FORECASTCYCLE="06"
BACKSITE=ftp.ncep.noaa.gov          # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours of NAM forecast to run (max 84)
PTFILE=ptFile_hsofs.txt         # the lat/lons for the OWI background met
SPATIALEXTRAPOLATIONRAMP=yes
SPATIALEXTRAPOLATIONRAMPDISTANCE=5.0
ALTNAMDIR="/projects/ncfs/data/asgs5463","/projects/ncfs/data/asgs14174"

# External data sources : River Flux

RIVERSITE=ftp.nssl.noaa.gov
RIVERDIR=/projects/ciflow/adcirc_info

# Input files and templates (looks in INPUTDIR=$SCRIPTDIR/input/meshes/hsofs)
#https://asgs-static-assets.sfo2.digitaloceanspaces.com/meshes/hsofs.14.xz
#https://asgs-static-assets.sfo2.digitaloceanspaces.com/nodal-attributes/hsofs.13.xz

GRIDFILE=hsofs.14  # mesh (fort.14) file
GRIDNAME=hsofs
MESHPROPERTIES=${GRIDFILE}.ng.properties     
CONTROLTEMPLATE=hsofs_explicit.15.template
CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
ELEVSTATIONS=hsofs_stations_20180907.txt
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
NAFILE=hsofs.13
NAPROPERTIES=${NAFILE}.properties
#SWANTEMPLATE=fort.26.template # only used if WAVES=on
SWANTEMPLATE=fort.26.nolimiter.template # need to use this with ADCIRC+SWAN v53
RIVERINIT=null                          # this mesh has no rivers ...
RIVERFLUX=null
HINDCASTRIVERFLUX=null
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz

# Output files

# water surface elevation station output
FORT61="--fort61freq 300.0 --fort61netcdf" 
# water current velocity station output
FORT62="--fort62freq 0"                    
# full domain water surface elevation output
FORT63="--fort63freq 3600.0 --fort63netcdf" 
# full domain water current velocity output
FORT64="--fort64freq 3600.0 --fort64netcdf" 
# met station output
FORT7172="--fort7172freq 300.0 --fort7172netcdf"           
# full domain meteorological output
FORT7374="--fort7374freq 3600.0 --fort7374netcdf"           
#SPARSE="--sparse-output"
SPARSE=""
NETCDF4="--netcdf4"
OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
# fulldomain or subdomain hotstart files
HOTSTARTCOMP=fulldomain                    
# binary or netcdf hotstart files
HOTSTARTFORMAT=netcdf                      
# "continuous" or "reset" for maxele.63 etc files
MINMAX=reset                              

# Notification

EMAILNOTIFY=yes         # yes to have host HPC platform email notifications
NOTIFY_SCRIPT=corps_nam_notify.sh  # wording of emails
ACTIVATE_LIST=null                 # when ASGS is turned on 1st time
NEW_ADVISORY_LIST=null             # email for every new advisory
POST_INIT_LIST=null                # when post processing has started
POST_LIST=null                     # when results are posted
JOB_FAILED_LIST="jason.g.fleming@gmail.com" # when a job fails
NOTIFYUSER="jason.g.fleming@gmail.com"      # default email address
ASGSADMIN="jason.g.fleming@gmail.com"       # in queue script
# RMQ Messaging
RMQMessaging_Enable="on"      #  enables message generation ("on" | "off")
RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
RMQMessaging_Script="${SCRIPTDIR}/asgs-msgr.py"
RMQMessaging_NcoHome="/home/jgflemin/"
RMQMessaging_Python="/usr/local/packages/python/2.7.12-anaconda/bin/python"
RMQMessaging_LocationName="LSU CCT"
RMQMessaging_ClusterName="Rostam"

# Post processing and publication

INTENDEDAUDIENCE=general # or developers-only or professional
INITPOST=null_init_post.sh
POSTPROCESS=cera_post.sh
POSTPROCESS2=null_post.sh

# opendap

TDS=(lsu_tds)
TARGET=rostam       # used in post processing to pick up HPC platform config
if [[ $OPENDAPHOST = "fortytwo.cct.lsu.edu" ]]; then
   OPENDAPUSER=alireza  
fi
# OPENDAPNOTIFY is used by opendap_post.sh and could be regrouped with the 
# other notification parameters above. 
OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,<al email address here>"

# Archiving

ARCHIVE=enstorm_pedir_removal.sh        # clean PE0001, PE0002, etc
ARCHIVEBASE=/work/jgflemin
ARCHIVEDIR=${ARCHIVEBASE}/asgs_archive

# Forecast scenarios

RMAX=default
PERCENT=default
ENSEMBLESIZE=2  # number of scenarios (e.g., official track, veer left 50% etc)
NCPUCAPACITY=999999
# 
case $si in
-1)
      # do nothing ... this is not a forecast
   ;;
0)
   # set NCPUCAPACITY above if you want these to run at same time
   # (19c + 1w namforecastWind10m)  + (159c + 1w namforecast) = 180 
   # wind only with no surface roughness, i.e., 10m wind
   ENSTORM=nhcConsensusWind10m  # name of scenario
   ADCPREPWALLTIME="00:20:00"  # adcprep wall clock time, including partmesh
   FORECASTWALLTIME="00:20:00" # forecast wall clock time
   CONTROLTEMPLATE=hsofs.nowindreduction.15.template  # fort.15 template
   CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
   TIMESTEPSIZE=300.0    # 15 minute time steps
   NCPU=19               # dramatically reduced resource requirements
   NUMWRITERS=1          # multiple writer procs might collide
   WAVES=off             # deactivate wave forcing 
   # turn off water surface elevation station output
   FORT61="--fort61freq 0"
   # turn off water current velocity station output
   FORT62="--fort62freq 0"
   # turn off full domain water surface elevation output
   FORT63="--fort63freq 0"
   # turn off full domain water current velocity output
   FORT64="--fort64freq 0"
   # met station output
   FORT7172="--fort7172freq 300.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 3600.0 --fort7374netcdf"
   #SPARSE="--sparse-output"
   SPARSE=""
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   INTENDEDAUDIENCE=general
   # prevent collisions in prepped archives
   PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
   POSTPROCESS=null_post.sh
   ;;
1)
   # our adcirc run for cera including water levels etc, uses parameters listed
   # above as-is
   ENSTORM=nhcConsensus # should be nhcTrack   # official forecast track
   # repeat
   NCPU=159
   NUMWRITERS=1
   ;;

2)
   # set NCPUCAPACITY above if you want these to run at same time
   # (19c + 1w namforecastWind10m)  + (159c + 1w namforecast) = 180 
   # wind only with no surface roughness, i.e., 10m wind
   ENSTORM=veerRight100Wind10m  # name of scenario
   ADCPREPWALLTIME="00:20:00"  # adcprep wall clock time, including partmesh
   FORECASTWALLTIME="00:20:00" # forecast wall clock time
   CONTROLTEMPLATE=hsofs.nowindreduction.15.template  # fort.15 template
   CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
   TIMESTEPSIZE=300.0    # 15 minute time steps
   NCPU=19               # dramatically reduced resource requirements
   NUMWRITERS=1          # multiple writer procs might collide
   WAVES=off             # deactivate wave forcing 
   # turn off water surface elevation station output
   FORT61="--fort61freq 0"
   # turn off water current velocity station output
   FORT62="--fort62freq 0"
   # turn off full domain water surface elevation output
   FORT63="--fort63freq 0"
   # turn off full domain water current velocity output
   FORT64="--fort64freq 0"
   # met station output
   FORT7172="--fort7172freq 300.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 3600.0 --fort7374netcdf"
   #SPARSE="--sparse-output"
   SPARSE=""
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   INTENDEDAUDIENCE=general
   # prevent collisions in prepped archives
   PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
   POSTPROCESS=null_post.sh
   ;;
3)
   ENSTORM=veerRight100  # along right edge of cone of uncertainty
   PERCENT=100
   # repeat
   NCPU=159
   NUMWRITERS=1
   ;;
4)
   # set NCPUCAPACITY above if you want these to run at same time
   # (19c + 1w namforecastWind10m)  + (159c + 1w namforecast) = 180 
   # wind only with no surface roughness, i.e., 10m wind
   ENSTORM=veerLeft100Wind10m  # name of scenario
   ADCPREPWALLTIME="00:20:00"  # adcprep wall clock time, including partmesh
   FORECASTWALLTIME="00:20:00" # forecast wall clock time
   CONTROLTEMPLATE=hsofs.nowindreduction.15.template  # fort.15 template
   CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
   TIMESTEPSIZE=300.0    # 15 minute time steps
   NCPU=19               # dramatically reduced resource requirements
   NUMWRITERS=1          # multiple writer procs might collide
   WAVES=off             # deactivate wave forcing 
   # turn off water surface elevation station output
   FORT61="--fort61freq 0"
   # turn off water current velocity station output
   FORT62="--fort62freq 0"
   # turn off full domain water surface elevation output
   FORT63="--fort63freq 0"
   # turn off full domain water current velocity output
   FORT64="--fort64freq 0"
   # met station output
   FORT7172="--fort7172freq 300.0 --fort7172netcdf"
   # full domain meteorological output
   FORT7374="--fort7374freq 3600.0 --fort7374netcdf"
   #SPARSE="--sparse-output"
   SPARSE=""
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   INTENDEDAUDIENCE=general
   # prevent collisions in prepped archives
   PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
   POSTPROCESS=null_post.sh
   ;;
5)
   ENSTORM=veerLeft100  # along left edge of cone of uncertainty
   PERCENT=-100
   # repeat
   NCPU=159
   NUMWRITERS=1
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac
