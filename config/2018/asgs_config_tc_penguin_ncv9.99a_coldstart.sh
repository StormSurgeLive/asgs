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
# Copyright(C) 2006--2016 Jason Fleming
# Copyright(C) 2006, 2007 Brett Estrade 
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

INSTANCENAME=pod_florence_ncv9.99wr  # name of this ASGS process (Change this for every new instance)
COLDSTARTDATE=2018081000 # (date to start cold start from )
HOTORCOLD=coldstart       # "hotstart" or "coldstart" 
#LASTSUBDIR=/home/bblanton/asgs-scratch/kickstart_ncv9.99/
LASTSUBDIR=null
HINDCASTLENGTH=30.0       # length of initial hindcast, from cold (days)  
REINITIALIZESWAN=no       # used to bounce the wave solution

# Source file paths

H="/home/bblanton/"
ADCIRCDIR=${H}/adcirc-cg-52release/work
SCRIPTDIR=${H}/asgs       # ASGS scripts/executables  
INPUTDIR=${SCRIPTDIR}/input/meshes/nc_v9.99_w_rivers   # dir containing grid and other input files 
OUTPUTDIR=${SCRIPTDIR}/output # dir containing post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL    # dir with DateCale.pm perl module

# Physical forcing

BACKGROUNDMET=off    # [de]activate NAM download/forcing 
TIDEFAC=on           # [de]activate tide factor recalc 
TROPICALCYCLONE=on   # [de]activate tropical cyclone forcing (temp. broken)
WAVES=on            # [de]activate wave forcing 
VARFLUX=on          # [de]activate variable river flux forcing

# Computational Resources

TIMESTEPSIZE=0.5
SWANDT=1200
HINDCASTWALLTIME="24:00:00"   # river inital and tidal spinup time in machine
ADCPREPWALLTIME="01:00:00"
NOWCASTWALLTIME="06:00:00"  # must have leading zero, e.g., 05:00:00
FORECASTWALLTIME="06:00:00" # must have leading zero, e.g., 05:00:00
NCPU=480
NUMWRITERS=0
NCPUCAPACITY=480
CYCLETIMELIMIT="06:00:00"
QUEUENAME=S30
SERQUEUE=S30
ACCOUNT=DhsCrcTesting
SCRATCHDIR=/home/bblanton/asgs-scratch

# External data sources : Tropical cyclones

STORM=06  # storm number, e.g. 05=ernesto in 2006 
YEAR=2018 # year of the storm (useful for historical storms) 
TRIGGER=rssembedded    # either "ftp" or "rss"
RSSSITE=filesystem # www.nhc.noaa.gov 
#FTPSITE=ftp.nhc.noaa.gov  # real anon ftp site for hindcast/forecast files
FTPSITE=filesystem  # real anon ftp site for hindcast/forecast files
#FDIR=/atcf/fst     # forecast dir on nhc ftp site 
FDIR=/home/bblanton/asgs-advisories/     # forecast dir on local filesystem 
#HDIR=/atcf/btk      # hindcast dir on nhc ftp site 
HDIR=/home/bblanton/asgs-advisories/      # hindcast dir on nhc ftp site 

# External data sources : Background Meteorology

FORECASTCYCLE="00,06,12,18" # (cycle of forecasts, needed)
BACKSITE=ftp.ncep.noaa.gov          # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours of NAM forecast to run (max 84)
PTFILE=ptFile_oneEighth.txt         # the lat/lons for the OWI background met
ALTNAMDIR="/scratch/bblanton"

# External data sources : River Flux

#RIVERSITE=ftp.nssl.noaa.gov
#RIVERDIR=/projects/ciflow/adcirc_info

RIVERSITE=data.disaster.renci.org
RIVERDIR=/opt/ldm/storage/SCOOP/RHLRv9-OKU
RIVERUSER=ldm
RIVERDATAPROTOCOL=scp

# Input files and templates

GRIDNAME=nc_inundation_v9.99a_w_rivers
GRIDFILE=${GRIDNAME}.14
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=nc_9.99wrivers_vortex_fort.15.template
CONTROLPROPERTIES=fort.15.properties
ELEVSTATIONS=ncv999_stations_20180907.txt # ncv999_combined_station_list_cera.20161105_ncem20180329.txt
VELSTATIONS=ncv999_stations_20180907.txt # ncv999_combined_station_list_cera.20161105_ncem20180329.txt
METSTATIONS=ncv999_stations_20180907.txt # ncv999_combined_station_list_cera.20161105_ncem20180329.txt
NAFILE=${GRIDNAME}.13
NAPROPERTIES=${NAFILE}.properties
SWANTEMPLATE=fort.26.template  
RIVERINIT=v6brivers.88
RIVERFLUX=v6brivers_fort.20_default
HINDCASTRIVERFLUX=v6brivers_fort.20_hc_default
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz   # prep all_
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz

# Output files

# water surface elevation station output
FORT61="--fort61freq 300.0 --fort61netcdf"
# water current velocity station output
FORT62="--fort62freq 300.0  --fort62netcdf"
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
OUTPUTOPTIONS="${SPARSE} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374} ${NETCDF4}" 
# fulldomain or subdomain hotstart files
HOTSTARTCOMP=fulldomain
# binary or netcdf hotstart files
HOTSTARTFORMAT=binary
# "continuous" or "reset" for maxele.63 etc files
MINMAX=reset

# Notification

EMAILNOTIFY=yes # set to yes to have host platform email notifications
ems="bblanton@renci.org"
NOTIFY_SCRIPT=pod_tc_notify.sh
ACTIVATE_LIST=   #  "$ems"
NEW_ADVISORY_LIST=   #  "$ems"
POST_INIT_LIST="$ems"
POST_LIST="$ems"
JOB_FAILED_LIST="$ems" 
NOTIFYUSER="$ems"
ASGSADMIN="$ems"

# RMQ Messaging

RMQMessaging_Script="${SCRIPTDIR}/asgs-msgr.py"
RMQMessaging_NcoHome="/home/bblanton/"
RMQMessaging_Python="/home/bblanton/asgs/asgspy/bin/python"
RMQMessaging_LocationName="Penguin"
RMQMessaging_ClusterName="POD"

# Post processing and publication

INTENDEDAUDIENCE="tc_testrun" # meta data audience
INITPOST=null_init_post.sh
POSTPROCESS=pod_post.sh
#POSTPROCESS=null_post.sh
POSTPROCESS2=null_post.sh

TDS=(renci_tds)
TARGET=pod              # used in post processing to pick up HPC platform config
OPENDAPUSER=bblanton         # default value that works for RENCI opendap 
# OPENDAPNOTIFY is used by opendap_post.sh and could be regrouped with the 
# other notification parameters above. 
OPENDAPNOTIFY="bblanton@renci.org"

# Archiving

ARCHIVE=pod_archive.sh # if null no data gets save
ARCHIVEBASE=/home/bblanton/scratch/
ARCHIVEDIR=archive

# Forecast ensemble members

RMAX=default
PERCENT=default
ENSEMBLESIZE=0 # number of storms in the ensemble
case $si in
-1)
      # do nothing ... this is not a forecast
   ;;
0)
   ENSTORM=nhcConsensus
   ;;
1)
   ENSTORM=veerLeft100
   PERCENT=-100
   ;;
2)
   ENSTORM=veerLeft50
   PERCENT=-50
   ;;
3)
   ENSTORM=rMax20
   RMAX=scaled
   PERCENT=20
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac
