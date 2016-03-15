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
# Copyright(C) 2012--2014 Jason Fleming
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

INSTANCENAME=namcpra2017     # "name" of this ASGS process
COLDSTARTDATE=2015071800 # calendar year month day hour YYYYMMDDHH24
HOTORCOLD=coldstart      # "hotstart" or "coldstart"
LASTSUBDIR=null          # path to previous execution (if HOTORCOLD=hotstart)
HINDCASTLENGTH=30.0      # length of initial hindcast, from cold (days)
REINITIALIZESWAN=no      # used to bounce the wave solution

# Source file paths

ADCIRCDIR=~/adcirc/v51release/work # ADCIRC executables
SCRIPTDIR=~/asgs/2014stable        # ASGS executables
INPUTDIR=${SCRIPTDIR}/input/meshes/cpra2017   # grid and other input files
OUTPUTDIR=${SCRIPTDIR}/output # post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL    # DateCale.pm perl module

# Physical forcing

BACKGROUNDMET=on     # NAM download/forcing
TIDEFAC=on           # tide factor recalc
TROPICALCYCLONE=off  # tropical cyclone forcing
WAVES=off            # wave forcing
VARFLUX=off          # variable river flux forcing

# Computational Resources

TIMESTEPSIZE=1.0            # adcirc time step size (seconds)
SWANDT=600                  # swan time step size (seconds)
HINDCASTWALLTIME="23:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="08:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="08:00:00" # forecast wall clock time
NCPU=960                   # number of compute CPUs for all simulations
NCPUCAPACITY=2880
CYCLETIMELIMIT=99:00:00  
# queue
ACCOUNT=ERDCV00898N10
#ACCOUNT=ERDCV00898HSP
#QUEUENAME=high
#SERQUEUE=high
#QUEUENAME=R130625
#SERQUEUE=R130625
QUEUENAME=standard
SERQUEUE=standard

# External data sources : Tropical cyclones

STORM=04                         # storm number, e.g. 05=ernesto in 2006
YEAR=2015                        # year of the storm
TRIGGER=rssembedded              # either "ftp" or "rss"
#RSSSITE=filesystem
#FTPSITE=filesystem
#FDIR=~/asgs/input
#HDIR=~/asgs/input
RSSSITE=www.nhc.noaa.gov         # site information for retrieving advisories
FTPSITE=ftp.nhc.noaa.gov         # hindcast/nowcast ATCF formatted files
FDIR=/atcf/afst                  # forecast dir on nhc ftp site
HDIR=/atcf/btk                   # hindcast dir on nhc ftp site

# External data sources : Background Meteorology

FORECASTCYCLE="06"
BACKSITE=ftp.ncep.noaa.gov          # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours of NAM forecast to run (max 84)
PTFILE=ptFile_oneEighth.txt         # the lat/lons for the OWI background met
ALTNAMDIR="/work/jgflemin/asgs30658"

# External data sources : River Flux

RIVERSITE=ftp.nssl.noaa.gov
RIVERDIR=/projects/ciflow/adcirc_info
RIVERUSER=ldm
RIVERDATAPROTOCOL=scp

# Input files and templates

GRIDFILE=cpra_2017_v07a_chk.grd # mesh (fort.14) file
GRIDNAME=cpra_2017_v07a_chk
CONTROLTEMPLATE=cpra_2017_v07a_626900cfs.15.template  # fort.15 template
#ELEVSTATIONS=hsdrrs_2014_stations.txt  # or substitute your own stations file
#VELSTATIONS=hsdrrs_2014_stations.txt
#METSTATIONS=hsdrrs_2014_stations.txt
ELEVSTATIONS=cpra2017v07_stations.txt # or substitute your own stations file
VELSTATIONS=cpra2017v07_stations.txt
METSTATIONS=cpra2017v07_stations.txt
NAFILE=cpra_2017_v07a.13 
#NAFILE=cpra_2017_v07a_datum15cm.13
SWANTEMPLATE=cpra_2017_v07a.26.template   # only used if WAVES=on
RIVERINIT=null                # this mesh no variable flux rivers 
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
FORT64="--fort64freq 0" 
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

EMAILNOTIFY=yes         # yes to have host HPC platform email notifications
NOTIFY_SCRIPT=corps_nam_notify.sh
ACTIVATE_LIST="jason.g.fleming@gmail.com"
NEW_ADVISORY_LIST="jason.g.fleming@gmail.com"
POST_INIT_LIST="jason.g.fleming@gmail.com"
POST_LIST="jason.g.fleming@gmail.com"
JOB_FAILED_LIST="jason.g.fleming@gmail.com"
NOTIFYUSER=jason.g.fleming@gmail.com
ASGSADMIN=jason.g.fleming@gmail.com

# Post processing and publication

INITPOST=null_init_post.sh
POSTPROCESS=corps_post.sh
POSTPROCESS2=null_post.sh
TARGET=garnet
WEBHOST=alpha.he.net
WEBUSER=seahrse
WEBPATH=/home/seahrse/public_html/ASGS
OPENDAPHOST=br0.renci.org
OPENDAPUSER=ncfs
OPENDAPBASEDIR=/projects/ncfs/opendap/data
NUMCERASERVERS=3

# Archiving

ARCHIVE=null_archive.sh
ARCHIVEBASE=/projects/ncfs/data
ARCHIVEDIR=archive

# Common control properties

SLAM0=265.5
SFEA0=29.0

INTENDEDAUDIENCE=developers-only

# Forecast ensemble members

RMAX=default
PERCENT=default
ENSEMBLESIZE=1 # number of storms in the ensemble
case $si in
-1)
      # do nothing ... this is not a forecast
   ;;
0)
   ENSTORM=namforecast
   ;;
1)
   ENSTORM=veerRight50
   PERCENT=50
   ;;
2)
   ENSTORM=veerLeft50
   PERCENT=-50
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac
