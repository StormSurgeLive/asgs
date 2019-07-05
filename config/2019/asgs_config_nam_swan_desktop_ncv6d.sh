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
# Copyright(C) 2015--2019 Jason Fleming
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

INSTANCENAME=sarops      # "name" of this ASGS process
COLDSTARTDATE=2019060100 # calendar year month day hour YYYYMMDDHH24
HOTORCOLD=coldstart      # "hotstart" or "coldstart"
LASTSUBDIR=null          # path to previous execution (if HOTORCOLD=hotstart)
HINDCASTLENGTH=30.0      # length of initial hindcast, from cold (days)
REINITIALIZESWAN=no      # used to bounce the wave solution

# Source file paths

ADCIRCDIR=~/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
SWANDIR=~/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
SCRIPTDIR=~/asgs/jasonfleming/master               # ASGS executables
INPUTDIR=${SCRIPTDIR}/input/meshes/nc_v6b          # grid and other input files
OUTPUTDIR=${SCRIPTDIR}/output                      # post processing scripts

# Physical forcing

BACKGROUNDMET=on     # NAM download/forcing
TIDEFAC=on           # tide factor recalc
TROPICALCYCLONE=off  # tropical cyclone forcing
WAVES=on             # wave forcing
VARFLUX=on           # variable river flux forcing

# Computational Resources

TIMESTEPSIZE=0.5            # adcirc time step size (seconds)
SWANDT=1200                 # swan time step size (seconds)
HINDCASTWALLTIME="36:00:00" # hindcast wall clock time
ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="24:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="24:00:00" # forecast wall clock time
NCPU=2                      # number of compute CPUs for all simulations
NUMWRITERS=1
NCPUCAPACITY=3
CYCLETIMELIMIT="99:00:00"

# External data sources : Tropical cyclones

PSEUDOSTORM=y
STORM=18                         # storm number, e.g. 05=ernesto in 2006
YEAR=2012                        # year of the storm
TRIGGER=rssembedded              # either "ftp" or "rss"
RSSSITE=filesystem
FTPSITE=filesystem
FDIR=~/asgs/branches/nowcastarchive/input/sample_advisories/2012
HDIR=~/asgs/branches/nowcastarchive/input/sample_advisories/2012
#RSSSITE=www.nhc.noaa.gov         # site information for retrieving advisories
#FTPSITE=ftp.nhc.noaa.gov         # hindcast/nowcast ATCF formatted files
#FDIR=/atcf/afst                  # forecast dir on nhc ftp site
#HDIR=/atcf/btk                   # hindcast dir on nhc ftp site

# External data sources : Background Meteorology

FORECASTCYCLE="00"
BACKSITE=ftp.ncep.noaa.gov          # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours of NAM forecast to run (max 84)
PTFILE=ptFile_oneEighth.txt         # the lat/lons for the OWI background met
ALTNAMDIR="/srv/asgs/asgs6011","/projects/ncfs/data/asgs14174"

# External data sources : River Flux

RIVERSITE=ftp.nssl.noaa.gov
RIVERDIR=/projects/ciflow/adcirc_info

# Input files and templates

GRIDFILE=nc_inundation_v6d_rivers_msl.grd
GRIDNAME=ncv6d  # @jasonfleming 20170814: should be nc6d
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=v6brivers_explicit_rlevel51_fort.15_template
CONTROLPROPERTIES=v6brivers_fort.15.properties
ELEVSTATIONS=v6brivers_elev_stations.txt
VELSTATIONS=null
METSTATIONS=v6brivers_met_stations.txt
NAFILE=v6brivers_rlevel.13
NAPROPERTIES=${NAFILE}.properties
SWANTEMPLATE=fort.26.v6b.template
RIVERINIT=v6brivers.88
RIVERFLUX=v6brivers_fort.20_default
HINDCASTRIVERFLUX=v6brivers_fort.20_hc_default
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz

# Output files

# water surface elevation station output
FORT61="--fort61freq 300.0 --fort61netcdf" 
# water current velocity station output
FORT62="--fort62freq 300.0 --fort62netcdf"                    
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

EMAILNOTIFY=no         # yes to have host HPC platform email notifications
NOTIFY_SCRIPT=null_notify.sh
ACTIVATE_LIST="null"
NEW_ADVISORY_LIST="null"
POST_INIT_LIST="null"
POST_LIST="null"
JOB_FAILED_LIST=jason.fleming@seahorsecoastal.com
NOTIFYUSER=jason.fleming@seahorsecoastal.com
ASGSADMIN=jason.fleming@seahorsecoastal.com

# ASGS monitor

# ( use default values from platforms.sh and asgs_main.sh )

# Post processing and publication

INTENDEDAUDIENCE=uscg 
INITPOST=null_init_post.sh
POSTPROCESS=sarops_post.sh

# OPeNDAP 

TDS=( digitalocean_tds )
TARGET=jason-desktop     # used in post processing to pick up HPC platform config
OPENDAPUSER=jgflemin     # default value that works for RENCI opendap 
OPENDAPNOTIFY="jason.fleming@seahorsecoastal.com"

# Archiving

ARCHIVE=enstorm_pedir_removal.sh
ARCHIVEBASE=/srv/asgs
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
   ENSTORM=namforecast
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac
