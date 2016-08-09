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
# Copyright(C) 2016 Jason Fleming
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

INSTANCENAME=testearl${USER}   # "name" of this ASGS process
COLDSTARTDATE=2016062000       # calendar year month day hour YYYYMMDDHH24
HOTORCOLD=hotstart        # "hotstart" or "coldstart"
LASTSUBDIR=/scratch/00976/jgflemin/asgs111754/2016080200  # path to previous execution (if HOTORCOLD=hotstart)
HINDCASTLENGTH=20.0       # length of initial hindcast, from cold (days)
REINITIALIZESWAN=no       # used to bounce the wave solution

# Source file paths

ADCIRCDIR=$WORK/ASGS2016/adcirc-cg-52release/work # ADCIRC executables
SCRIPTDIR=$WORK/ASGS2016/asgs                     # ASGS executables
INPUTDIR=${SCRIPTDIR}/input/meshes/texas2008_r35h # grid and other input files
OUTPUTDIR=${SCRIPTDIR}/output # post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL    # DateCale.pm perl module
QSCRIPT=lonestar.template.earl.slurm
PREPCONTROLSCRIPT=lonestar.template.serial.earl.slurm

# Physical forcing

BACKGROUNDMET=off     # NAM download/forcing
TIDEFAC=on            # tide factor recalc
TROPICALCYCLONE=on    # tropical cyclone forcing
WAVES=off             # wave forcing
VARFLUX=off           # variable river flux forcing
VORTEXMODEL=GAHM

# Computational Resources

TIMESTEPSIZE=1.0             # adcirc time step size (seconds)
SWANDT=1200                  # swan time step size (seconds)
HINDCASTWALLTIME="18:00:00"  # hindcast wall clock time
ADCPREPWALLTIME="00:30:00"   # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="01:00:00"   # longest nowcast wall clock time
FORECASTWALLTIME="05:00:00"  # forecast wall clock time
NCPU=2400                    # number of compute CPUs for all simulations
NUMWRITERS=24
NCPUCAPACITY=2424
CYCLETIMELIMIT="05:00:00"

# External data sources : Tropical cyclones

STORM=05                         # storm number, e.g. 05=ernesto in 2006
YEAR=2016                        # year of the storm
#
TRIGGER=rssembedded              # either "ftp", "rss", or rssembedded
#
# read advisories from file system (comment these out for real event)
#RSSSITE=filesystem
#FTPSITE=filesystem
#FDIR=${SCRIPTDIR}/input/sample_advisories
#HDIR=${SCRIPTDIR}/input/sample_advisories
#
# uncomment these for real event
RSSSITE=www.nhc.noaa.gov         # site information for retrieving advisories
FTPSITE=ftp.nhc.noaa.gov         # hindcast/nowcast ATCF formatted files
FDIR=/atcf/afst                  # forecast dir on nhc ftp site
HDIR=/atcf/btk                   # hindcast dir on nhc ftp site

# External data sources : Background Meteorology
#
# These are ignored if the forcing is from tropical cyclone
FORECASTCYCLE="06"
BACKSITE=ftp.ncep.noaa.gov          # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours of NAM forecast to run (max 84)
PTFILE=ptFile_oneEighth.txt         # the lat/lons for the OWI background met
ALTNAMDIR="/projects/ncfs/data/asgs5463","/projects/ncfs/data/asgs14174"

# External data sources : River Flux
#
# ignored if there is no river flux forcing
RIVERSITE=ftp.nssl.noaa.gov
RIVERDIR=/projects/ciflow/adcirc_info

# Input files and templates

GRIDFILE=tx2008_r35h.grd # mesh (fort.14) file
GRIDNAME=tx2008_r35h
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=tx2008r35h_template.15   # fort.15 template
CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
ELEVSTATIONS=texas2008r35h.cera_stations.20160705 # or substitute your own stations file
VELSTATIONS=texas2008r35h.cera_stations.20160705
METSTATIONS=texas2008r35h.cera_stations.20160705
NAFILE=tx2008_r35h.13
NAPROPERTIES=${NAFILE}.properties
SWANTEMPLATE=fort.26.ut.fric.template    # only used if WAVES=on
RIVERINIT=null                           # this mesh has no rivers ...
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
MINMAX=reset                               

# Notification
#
# if a parameter should have more than one email address, they should be 
# enclosed in quotes and separated by spaces 
EMAILNOTIFY=yes         # yes to have host HPC platform email notifications
NOTIFY_SCRIPT=ut-nhc-notify.sh
ACTIVATE_LIST=null
NEW_ADVISORY_LIST=null
POST_INIT_LIST=null
POST_LIST=null
JOB_FAILED_LIST=jason.g.fleming@gmail.com
NOTIFYUSER=jason.g.fleming@gmail.com
ASGSADMIN=jason.g.fleming@gmail.com	

# Post processing and publication
#
INTENDEDAUDIENCE=developers-only
INITPOST=null_init_post.sh
POSTPROCESS=ut-post2015.sh
POSTPROCESS2=null_post.sh

# opendap
#
TDS=(tacc_tds)
TARGET=lonestar  # used in post processing to pick up HPC platform config
# You must first have your ssh public key in ~/.ssh/authorized_keys2 file 
# on the opendap server machine in order to scp files there via
# opendap_post.sh. OPENDAPHOST is set to each value in the TDS array specified
# above and used by your post processing script to successively trigger 
# configuration via platforms.sh. The OPENDAPUSER parameter needs to be set
# here, rather than in platforms.sh or your post processing script,
# because multiple Operators may be posting to a particular opendap server
# using different usernames. 
OPENDAPUSER=jgflemin        # not sure this is needed on TACC TDS 
#if [[ $OPENDAPHOST = "fortytwo.cct.lsu.edu" ]]; then
#   OPENDAPUSER=jgflemin  # change this for other Operator running on queenbee
#fi
# OPENDAPNOTIFY is used by opendap_post.sh and could be regrouped with the 
# other notification parameters above. 
#OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com,zbyerly@cct.lsu.edu"
OPENDAPNOTIFY="jason.fleming@seahorsecoastal.com,asgs.cera.lsu@gmail.com,clint@ices.utexas.edu,jennifer@ices.utexas.edu,gwells.mte@gmail.com,howard@csr.utexas.edu,rick_luettich@unc.edu,bblanton@renci.org"
#
# The following are deprecated and ignored
NUMCERASERVERS=2
WEBHOST=webserver.hostingco.com
WEBUSER=remoteuser
WEBPATH=/home/remoteuser/public_html/ASGS/outputproducts

# Archiving

ARCHIVE=ut-archive.sh
ARCHIVEBASE=/corral-tacc/utexas/hurricane/ASGS/2016
ARCHIVEDIR="${INSTANCENAME}_${STORM}${YEAR}"

# Forecast ensemble members

RMAX=default
PERCENT=default
ENSEMBLESIZE=2 # number of storms in the ensemble
case $si in
-1)
      # do nothing ... this is not a forecast
   ;;
0)
   ENSTORM=nhcConsensus
   ;;
1)
   ENSTORM=veerRight100
   PERCENT=100
   ;;
2)
   ENSTORM=veerLeft100
   PERCENT=-100
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac
