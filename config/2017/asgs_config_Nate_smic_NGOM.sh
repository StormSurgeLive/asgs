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
# Copyright(C) 2015 Jason Fleming
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

# User Specifis
# need this here so hstime can find its netcdf  shared libraries
#export LD_LIBRARY_PATH=/home/nate/install/lib:$LD_LIBRARY_PATH



# Fundamental

INSTANCENAME=nate     # "name" of this ASGS process
COLDSTARTDATE=2017092012 # calendar year month day hour YYYYMMDDHH24
HOTORCOLD=coldstart      # "hotstart" or "coldstart"
LASTSUBDIR=null          # path to previous execution (if HOTORCOLD=hotstart)
HINDCASTLENGTH=14.0      # length of initial hindcast, from cold (days)
REINITIALIZESWAN=no      # used to bounce the wave solution

# Source file paths

ADCIRCDIR=/home/mbilskie/src/adcirc/adcirc-cg-52.30.13/work # ADCIRC executables
SCRIPTDIR=/home/mbilskie/scratch/asgs/2014stable   # ASGS executables
INPUTDIR=/work/mbilskie/asgs/NGOM_4.0.19/mesh # grid and other input files
OUTPUTDIR=${SCRIPTDIR}/output # post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL    # DateCale.pm perl module
#SCRATCHDIR=/home/mbilskie/scratch/asgs/LA_v12g-WithUpperAtch/Gustav #overides setting in platforms.sh -where state file will be written
SCRATCHDIR=/work/mbilskie/asgs/NGOM_4.0.19/nate #overides setting in platforms.sh -where state file will be written

# Physical forcing

BACKGROUNDMET=off    # NAM download/forcing
TIDEFAC=on           # tide factor recalc
TROPICALCYCLONE=on   # tropical cyclone forcing
WAVES=off           # wave forcing
VARFLUX=off          # variable river flux forcing

# Computational Resources

TIMESTEPSIZE=1.0            # adcirc time step size (seconds)
SWANDT=1200                 # swan time step size (seconds)
HINDCASTWALLTIME="10:00:00" # hindcast wall clock time
ADCPREPWALLTIME="00:25:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="12:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="12:00:00" # forecast wall clock time
NCPU=1019                   # number of compute CPUs for all simulations
NCPUCAPACITY=2040
NUMWRITERS=1
CYCLETIMELIMIT="08:00:00"
QUEUENAME=workq
SERQUEUE=single
ACCOUNT=hpc_crc_smi_17

# External data sources : Tropical cyclones

STORM=16                         # storm number, e.g. 05=ernesto in 2006
YEAR=2017                        # year of the storm
TRIGGER=rssembedded              # either "ftp" or "rss"
#RSSSITE=filesystem
#FTPSITE=filesystem
#FDIR=/work/mbilskie/LA_v17a/asgs/Katrina/advisories
#HDIR=/work/mbilskie/LA_v17a/asgs/Katrina/advisories
RSSSITE=www.nhc.noaa.gov         # site information for retrieving advisories
FTPSITE=ftp.nhc.noaa.gov         # hindcast/nowcast ATCF formatted files
FDIR=/atcf/afst                  # forecast dir on nhc ftp site
HDIR=/atcf/btk                   # hindcast dir on nhc ftp site

# External data sources : Background Meteorology

FORECASTCYCLE="00,12"
BACKSITE=ftp.ncep.noaa.gov          # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours of NAM forecast to run (max 84)
PTFILE=ptFile_oneEighth.txt         # the lat/lons for the OWI background met
ALTNAMDIR="/projects/ncfs/data/asgs5463","/projects/ncfs/data/asgs14174"

# External data sources : River Flux

RIVERSITE=ftp.nssl.noaa.gov
RIVERDIR=/projects/ciflow/adcirc_info

# Input files and templates

GRIDFILE=NGOM_4.0.19_chk.grd # mesh (fort.14) file
GRIDNAME=NGOM_4.0.19_chk
CONTROLTEMPLATE=NGOM_4.0.19.15.template  # fort.15 template
ELEVSTATIONS=NGOM_stations.txt   # or substitute your own stations file
VELSTATIONS=NGOM_stations.txt
METSTATIONS=NGOM_stations.txt
NAFILE=NGOM_4.0.19.13
SWANTEMPLATE= # only used if WAVES=on
RIVERINIT=null                          # this mesh has no rivers ...
RIVERFLUX=null
HINDCASTRIVERFLUX=null
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz

# Output files

# water surface elevation station output
FORT61="--fort61freq 900.0 --fort61netcdf" 
# water current velocity station output
FORT62="--fort62freq 0"                    
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
MINMAX=continuous
#MINMAX=reset


#--------------------------------------------------------
#  Notificaiton Configuration
#--------------------------------------------------------
EMAILNOTIFY=yes # set to yes to have host platform email notifications
NOTIFY_SCRIPT=cera_notify.sh
#ACTIVATE_LIST="natedill@gmail.com cera1g-l@listserv.lsu.edu nathan.dill@ransomenv.com"
ACTIVATE_LIST="mbilsk3@lsu.edu"
NEW_ADVISORY_LIST="mbilsk3@lsu.edu"
POST_INIT_LIST="mbilsk3@lsu.edu stephen.medeiros@ucf.edu"
#POST_LIST="natedill@gmail.com nathan.dill@ransomenv.com asgs.cera.lsu@gmail.com"
POST_LIST="mbilsk3@lsu.edu"
#POST_LIST="natedill@gmail.com nathan.dill@ransomenv.com"
JOB_FAILED_LIST="mbilsk3@lsu.edu"
NOTIFYUSER="mbilsk3@lsu.edu"
ASGSADMIN="mbilsk3@lsu.edu"
#

# Post processing and publication

INTENDEDAUDIENCE=developers-only  # general or developers-only, gets written to run.properties
INITPOST=null_init_post.sh
#POSTPROCESS=cera_post.sh
POSTPROCESS=NGOM_post.sh
POSTPROCESS2=null_post.sh
TARGET=queenbee
#OPENDAPHOST=br0.renci.org
#OPENDAPUSER=ncfs
#OPENDAPBASEDIR=/projects/ncfs/opendap/data
NUMCERASERVERS=2
WEBHOST=webserver.hostingco.com
WEBUSER=remoteuser
WEBPATH=/home/remoteuser/public_html/ASGS/outputproducts

# Archiving

ARCHIVE=null_archive.sh
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
      # this (and subsequent ensemble members are the forecasts)
   ENSTORM=nhcOfficial
   ;;
1)
   ENSTORM=veerLeft50
   PERCENT=-50
   ;;
2)
   ENSTORM=veerRight50
   PERCENT=50
   ;;
3)
   ENSTORM=highermaxWindSpeed
   PERCENT=20
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac
