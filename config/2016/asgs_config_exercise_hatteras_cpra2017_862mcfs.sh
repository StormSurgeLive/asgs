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
# Copyright(C) 2006--2015 Jason Fleming
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
#
# Fundamental 
#
INSTANCENAME=cpra862mcfs # name of this ASGS process
COLDSTARTDATE=2015120200
HOTORCOLD=coldstart        # "hotstart" or "coldstart" 
LASTSUBDIR=null
HINDCASTLENGTH=30.0        # length of initial hindcast, from cold (days)
REINITIALIZESWAN=no        # used to bounce the wave solution

# Source file paths

ADCIRCDIR=~/adcirc/v51release/work # ADCIRC executables 
SCRIPTDIR=~/asgs/2014stable        # ASGS scripts/executables  
INPUTDIR=${SCRIPTDIR}/input/meshes/cpra2017 # dir containing grid and other input files 
OUTPUTDIR=${SCRIPTDIR}/output # dir containing post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL    # dir with DateCale.pm perl module

# Physical forcing

BACKGROUNDMET=off    # [de]activate NAM download/forcing 
TIDEFAC=on           # [de]activate tide factor recalc 
TROPICALCYCLONE=on   # [de]activate tropical cyclone forcing (temp. broken)
WAVES=off            # [de]activate wave forcing 
VARFLUX=off          # [de]activate variable river flux forcing

# Computational Resources

TIMESTEPSIZE=1.0
SWANDT=1200
HINDCASTWALLTIME="24:00:00"
ADCPREPWALLTIME="00:30:00"
NOWCASTWALLTIME="05:00:00"  # must have leading zero, e.g., 05:00:00
FORECASTWALLTIME="05:00:00" # must have leading zero, e.g., 05:00:00
NCPU=480
NCPUCAPACITY=480
CYCLETIMELIMIT="05:00:00"
# queue
QUEUENAME=null
SERQUEUE=null
SCRATCHDIR=/projects/ncfs/data # for the NCFS on blueridge
ACCOUNT=batch # or "ncfs" on hatteras to use pre-empt capability

# External data sources : Tropical cyclones

PSEUDOSTORM=y
STORM=89  # storm number, e.g. 05=ernesto in 2006 
YEAR=2015 # year of the storm (useful for historical storms) 
TRIGGER=rssembedded    # either "ftp" or "rss"
RSSSITE=filesystem 
FTPSITE=filesystem  # real anon ftp site for hindcast/forecast files
FDIR=~/asgs/advisories # forecast dir on nhc ftp site 
HDIR=~/asgs/advisories # hindcast dir on nhc ftp site 
#RSSSITE=www.nhc.noaa.gov 
#FTPSITE=ftp.nhc.noaa.gov  # real anon ftp site for hindcast/forecast files
#FDIR=/atcf/afst     # forecast dir on nhc ftp site 
#HDIR=/atcf/btk      # hindcast dir on nhc ftp site 

# External data sources : Background Meteorology

FORECASTCYCLE="00,12"
BACKSITE=ftp.ncep.noaa.gov          # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours of NAM forecast to run (max 84)
PTFILE=ptFile_oneEighth.txt         # the lat/lons for the OWI background met
ALTNAMDIR="/projects/ncfs/data/asgs16441"

# External data sources : River Flux

#RIVERSITE=ftp.nssl.noaa.gov
#RIVERDIR=/projects/ciflow/adcirc_info

RIVERSITE=data.disaster.renci.org
RIVERDIR=/opt/ldm/storage/SCOOP/RHLRv9-OKU
RIVERUSER=ldm
RIVERDATAPROTOCOL=scp

# Input files and templates

GRIDFILE=cpra_2017_v07a_chk.grd
GRIDNAME=cpra_2017_v07a_chk
CONTROLTEMPLATE=cpra_2017_v07a_862mcfs.15.template
ELEVSTATIONS=cpra2017v07_stations.txt
VELSTATIONS=null
METSTATIONS=cpra2017v07_stations.txt
NAFILE=cpra_2017_v07a.13
SWANTEMPLATE=cpra_2017_v07a.26.template
RIVERINIT=null
RIVERFLUX=null
HINDCASTRIVERFLUX=null
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz

# Output files

# water surface elevation station output
FORT61="--fort61freq 900.0 --fort61netcdf"
# water current velocity station output
FORT62="--fort62freq 900.0 --fort62netcdf"
# full domain water surface elevation output
FORT63="--fort63freq 3600.0 --fort63netcdf"
# full domain water current velocity output
FORT64="--fort64freq 3600.0 --fort64netcdf"
# met station output
FORT7172="--fort7172freq 3600.0 --fort7172netcdf"
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

EMAILNOTIFY=yes # set to yes to have host platform email notifications
NOTIFY_SCRIPT=corps_cyclone_notify.sh
ACTIVATE_LIST="jason.fleming@seahorsecoastal.com"
NEW_ADVISORY_LIST="jason.fleming@seahorsecoastal.com"
POST_INIT_LIST="jason.fleming@seahorsecoastal.com"
POST_LIST="jason.fleming@seahorsecoastal.com"
JOB_FAILED_LIST="jason.fleming@seahorsecoastal.com"
NOTIFYUSER=jason.fleming@seahorsecoastal.com
ASGSADMIN=jason.fleming@seahorsecoastal.com

# Post processing and publication

INTENDEDAUDIENCE=developers-only
INITPOST=null_init_post.sh
#POSTPROCESS=corps_post_atrenci.sh
POSTPROCESS=ncfs_post.sh
TARGET=hatteras
POSTPROCESS2=null_post.sh
WEBHOST=alpha.he.net
WEBUSER=seahrse
WEBPATH=/home/seahrse/public_html/ASGS
OPENDAPHOST=br0.renci.org
OPENDAPUSER=ncfs
OPENDAPBASEDIR=/projects/ncfs/opendap/data
NUMCERASERVERS=2

# Archiving

ARCHIVE=ncfs_archive.sh
ARCHIVEBASE=/projects/ncfs/data
ARCHIVEDIR=archive

# Forecast ensemble members

RMAX=default
PERCENT=default
ENSEMBLESIZE=1 # number of storms in the ensemble
case $si in
-1)
      # do nothing ... this is not a forecast
   ;;
0)
   ENSTORM=nhcConsensus
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac
