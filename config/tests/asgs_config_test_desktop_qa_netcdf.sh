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
# Copyright(C) 2014 Jason Fleming
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
INSTANCENAME=qatestsnetcdf   # name of this ASGS process
COLDSTARTDATE=1970010100
HOTORCOLD=coldstart         # "hotstart" or "coldstart" 
LASTSUBDIR=null
HINDCASTLENGTH=2.0         # length of initial hindcast, from cold (days)
REINITIALIZESWAN=no         # used to bounce the wave solution

# Source file paths

ADCIRCDIR=~/adcirc/xdmf2_test/work # ADCIRC executables 
SCRIPTDIR=~/asgs/trunk  # ASGS scripts/executables  
INPUTDIR=${SCRIPTDIR}/input/meshes/qa # dir containing grid and other input files 
OUTPUTDIR=${SCRIPTDIR}/output # dir containing post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL    # dir with DateCale.pm perl module

# Physical forcing from external data sources

BACKGROUNDMET=off   # [de]activate NAM download/forcing 
TIDEFAC=off         # [de]activate tide factor recalc 
TROPICALCYCLONE=off # [de]activate tropical cyclone forcing (temp. broken)
WAVES=off           # [de]activate wave forcing 
VARFLUX=off         # [de]activate variable river flux forcing
VORTEXMODEL=null    # specify which vortex model to use when TROPICALCYCLONE=on

# Computational Resources

TIMESTEPSIZE=174.656
SWANDT=1200
HINDCASTWALLTIME="24:00:00"
ADCPREPWALLTIME="00:15:00"
NOWCASTWALLTIME="10:00:00"  # must have leading zero, e.g., 05:00:00
FORECASTWALLTIME="05:00:00" # must have leading zero, e.g., 05:00:00
NCPU=2
NCPUCAPACITY=480
CYCLETIMELIMIT="05:00:00"
# queue
QUEUENAME=null
SERQUEUE=null

# External data sources : none

# If phyical forcing has not been activated at all, the ASGS needs to 
# have some way of filling in the ADCIRC control (fort.15) file. This
# section is only used by ASGS if all the phyiscal forcings (i.e., external
# data sources) are deactivated. This is usually used in test situations.
NOWCASTDAYS=2.0
FORECASTDAYS=1.0

# External data sources : Tropical cyclones

# This section is only used if TROPICALCYCLONE=on above.
STORM=01  # storm number, e.g. 05=ernesto in 2006 
YEAR=2014 # year of the storm (useful for historical storms) 
TRIGGER=rssembedded    # either "ftp" or "rss"
#RSSSITE=www.nhc.noaa.gov
RSSSITE=filesystem 
#FTPSITE=ftp.nhc.noaa.gov  # real anon ftp site for hindcast/forecast files
FTPSITE=filesystem
#FDIR=/atcf/afst     # forecast dir on nhc ftp site 
FDIR=/home/ncfs/asgs/trunk/input/sample_advisories
#HDIR=/atcf/btk      # hindcast dir on nhc ftp site 
HDIR=/home/ncfs/asgs/trunk/input/sample_advisories

# External data sources : Background Meteorology

# This section is only used if BACKGROUNDMET=on above.
FORECASTCYCLE="00,12"
BACKSITE=ftp.ncep.noaa.gov          # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours of NAM forecast to run (max 84)
PTFILE=ptFile_oneEighth.txt         # the lat/lons for the OWI background met
ALTNAMDIR="/projects/ncfs/data/asgs16441"

# External data sources : River Flux

# This section is only used if VARFLUX=on above.
RIVERSITE=data.disaster.renci.org
RIVERDIR=/opt/ldm/storage/SCOOP/RHLRv9-OKU
RIVERUSER=ldm
RIVERDATAPROTOCOL=scp

# Input files and templates

GETINPUTSCRIPT=null
GRIDFILE=fort.14
GRIDNAME=fort14
CONTROLTEMPLATE=fort.15.template
ELEVSTATIONS=qa_stations.txt
VELSTATIONS=qa_stations.txt
METSTATIONS=null
NAFILE=null
SWANTEMPLATE=null
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

EMAILNOTIFY=no # set to yes to have host platform email notifications
NOTIFY_SCRIPT=ncfs_cyclone_notify.sh
ACTIVATE_LIST="jason.fleming@seahorsecoastal.com jason.g.fleming@gmail.com"
NEW_ADVISORY_LIST="jason.fleming@seahorsecoastal.com jason.g.fleming@gmail.com"
POST_INIT_LIST="jason.fleming@seahorsecoastal.com jason.g.fleming@gmail.com"
POST_LIST="jason.fleming@seahorsecoastal.com jason.g.fleming@gmail.com"
JOB_FAILED_LIST="jason.fleming@seahorsecoastal.com jason.g.fleming@gmail.com"
NOTIFYUSER=jason.fleming@seahorsecoastal.com
ASGSADMIN=jason.fleming@seahorsecoastal.com

# Post processing and publication

INTENDEDAUDIENCE=developers-only
INITPOST=null_init_post.sh
POSTPROCESS=test/qa_post.sh
TARGET=desktop
POSTPROCESS2=null_post.sh
WEBHOST=alpha.he.net
WEBUSER=seahrse
WEBPATH=/home/seahrse/public_html/ASGS
OPENDAPHOST=br0.renci.org
OPENDAPUSER=ncfs
OPENDAPBASEDIR=/projects/ncfs/opendap/data
NUMCERASERVERS=2

# Archiving

ARCHIVE=null_archive.sh
ARCHIVEBASE=/projects/ncfs/data
ARCHIVEDIR=archive

# Forecast ensemble members

RMAX=default
PERCENT=default
ENSEMBLESIZE=3 # number of storms in the ensemble
case $si in
-1)
   # do nothing ... this is not a forecast
   ;;
0)
   # ascii output files; ascii hotstart files; no writers
   ENSTORM=nowriters
   ;;
1)
   # ascii output files; ascii hostart files; 1 writer
   ENSTORM=onewriter
   NUMWRITERS=1
   ;;
2) 
   # ascii output files; ascii hotstart files; 10 writers
   ENSTORM=tenwriters
   NUMWRITERS=10
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac
