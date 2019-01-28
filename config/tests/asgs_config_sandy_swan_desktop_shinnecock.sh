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

INSTANCENAME=shinsandy    # "name" of this ASGS process
COLDSTARTDATE=2012101712 # calendar year month day hour YYYYMMDDHH24
HOTORCOLD=coldstart      # "hotstart" or "coldstart"
LASTSUBDIR=null          # path to previous execution (if HOTORCOLD=hotstart)
HINDCASTLENGTH=5.0      # length of initial hindcast, from cold (days)
REINITIALIZESWAN=no      # used to bounce the wave solution

# Source file paths

ADCIRCDIR=~/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
SWANDIR=~/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
SCRIPTDIR=~/asgs/jasonfleming/master           # ASGS executables
INPUTDIR=${SCRIPTDIR}/input/meshes/shinnecock   # grid and other input files
OUTPUTDIR=${SCRIPTDIR}/output                   # post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL                      # DateCale.pm perl module

# Physical forcing

BACKGROUNDMET=off    # NAM download/forcing
TIDEFAC=on           # tide factor recalc
TROPICALCYCLONE=on   # tropical cyclone forcing
WAVES=on             # wave forcing
VARFLUX=off          # variable river flux forcing
VORTEXMODEL=GAHM     # default is GAHM (NWS20); ASYMMETRIC (NWS19) possible

# Computational Resources

TIMESTEPSIZE=6.0            # adcirc time step size (seconds)
SWANDT=1200                 # swan time step size (seconds)
HINDCASTWALLTIME="12:00:00" # hindcast wall clock time
ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="12:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="12:10:00" # forecast wall clock time
NCPU=2                      # number of compute CPUs for all simulations
NUMWRITERS=1
NCPUCAPACITY=4
CYCLETIMELIMIT="01:00:00"

# External data sources : Tropical cyclones

PSEUDOSTORM=y
STORM=18                         # storm number, e.g. 05=ernesto in 2006
YEAR=2012                        # year of the storm
TRIGGER=rssembedded              # either "ftp" or "rss"
RSSSITE=filesystem
FTPSITE=filesystem
FDIR=$SCRIPTDIR/input/sample_advisories/2012
HDIR=$SCRIPTDIR/input/sample_advisories/2012
#RSSSITE=www.nhc.noaa.gov         # site information for retrieving advisories
#FTPSITE=ftp.nhc.noaa.gov         # hindcast/nowcast ATCF formatted files
#FDIR=/atcf/afst                  # forecast dir on nhc ftp site
#HDIR=/atcf/btk                   # hindcast dir on nhc ftp site

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

GRIDFILE=shinnecock_inlet_coarse.grd     # mesh (fort.14) file
GRIDNAME=shinnecock_inlet_coarse
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=shinnecock_asgs.fort.15.template # fort.15 template
CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties # fort.15 template
ELEVSTATIONS=shinnecock_stations.txt     # or substitute your own stations file
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
NAFILE=null                              # no nodal attributes for shinnecock inlet
NAPROPERTIES=shinnecock_nodal_attributes.properties
SWANTEMPLATE=fort.26.shinnecock.template # only used if WAVES=on
RIVERINIT=null                           # this mesh has no rivers ...
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

EMAILNOTIFY=no         # yes to have host HPC platform email notifications
NOTIFY_SCRIPT=null_notify.sh
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
POSTPROCESS=null_post.sh

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
   ENSTORM=nhcConsensus
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac
