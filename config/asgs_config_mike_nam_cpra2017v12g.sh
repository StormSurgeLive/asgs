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
export LD_LIBRARY_PATH=/home/nate/install/lib:$LD_LIBRARY_PATH



# Fundamental

INSTANCENAME=mike_nam  # "name" of this ASGS process
COLDSTARTDATE=2016071400 # calendar year month day hour YYYYMMDDHH24
HOTORCOLD=coldstart      # "hotstart" or "coldstart"
LASTSUBDIR=null      # path to previous execution (if HOTORCOLD=hotstart)
HINDCASTLENGTH=18.0      # length of initial hindcast, from cold (days)
REINITIALIZESWAN=no      # used to bounce the wave solution

# Source file paths

#ADCIRCDIR=/home/nate/cera/adcirc/adcirc_v51.15_mvapich2/trunk/work # ADCIRC executables
ADCIRCDIR=/home/nate/cera/adcirc/v52release/work # ADCIRC executables
SCRIPTDIR=/home/nate/cera/asgs   # ASGS executables
INPUTDIR=${SCRIPTDIR}/input/meshes/cpra2017 # grid and other input files
OUTPUTDIR=${SCRIPTDIR}/output # post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL    # DateCale.pm perl module
SCRATCHDIR=/work/cera/cpra2017/mike_nam #overides setting in platforms.sh -where state file will be written

# Physical forcing

BACKGROUNDMET=on    # NAM download/forcing
TIDEFAC=on           # tide factor recalc
TROPICALCYCLONE=off   # tropical cyclone forcing
WAVES=off           # wave forcing
VARFLUX=off          # variable river flux forcing

# Computational Resources

TIMESTEPSIZE=1.0           # adcirc time step size (seconds)
SWANDT=1200                 # swan time step size (seconds)
HINDCASTWALLTIME="10:00:00" # hindcast wall clock time
ADCPREPWALLTIME="00:25:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="05:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="05:00:00" # forecast wall clock time
NCPU=400                    # number of compute CPUs for all simulations
NCPUCAPACITY=400
CYCLETIMELIMIT="05:00:00"
QUEUENAME=checkpt
SERQUEUE=single
ACCOUNT=hpc_cera_2016

# External data sources : Tropical cyclones

STORM=04                         # storm number, e.g. 05=ernesto in 2006
YEAR=2015                        # year of the storm
TRIGGER=rssembedded              # either "ftp" or "rss"
#RSSSITE=filesystem
#FTPSITE=filesystem
#FDIR=/home/nate/cera/asgs/advisories
#HDIR=/home/nate/cera/asgs/advisories
RSSSITE=www.nhc.noaa.gov         # site information for retrieving advisories
FTPSITE=ftp.nhc.noaa.gov         # hindcast/nowcast ATCF formatted files
FDIR=/atcf/afst                  # forecast dir on nhc ftp site
HDIR=/atcf/btk                   # hindcast dir on nhc ftp site

# External data sources : Background Meteorology

FORECASTCYCLE="06,18"
BACKSITE=ftp.ncep.noaa.gov          # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours of NAM forecast to run (max 84)
PTFILE=ptFile_oneEighth.txt         # the lat/lons for the OWI background met
ALTNAMDIR="/projects/ncfs/data/asgs5463","/projects/ncfs/data/asgs14174"

# External data sources : River Flux

RIVERSITE=ftp.nssl.noaa.gov
RIVERDIR=/projects/ciflow/adcirc_info


# Fixed River Flux Info
MSRIVERBOUNDARYTYPE=52
MSRIVERBOUHDARYCONDITION=380000  #cfs



# Input files and templates

GRIDFILE=LA_v12g_chk.grd         # mesh (fort.14) file
GRIDNAME=LA_v12g
CONTROLTEMPLATE=LA_v12g.15.template  # fort.15 template
ELEVSTATIONS=LA_v12gAtch_stations.txt   # or substitute your own stations file
VELSTATIONS=${ELEVSTATIONS}
METSTATIONS=${ELEVSTATIONS}
NAFILE=LA_v12g.13
SWANTEMPLATE=LA_v12g.26.template # only used if WAVES=on
RIVERINIT=null                          # this mesh has no rivers ...
RIVERFLUX=null
HINDCASTRIVERFLUX=null
PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
HINDCASTARCHIVE=prepped_${GRIDNAME}_hc_${INSTANCENAME}_${NCPU}.tar.gz

# Output files

# water surface elevation station output
#FORT61="--fort61freq 900.0 --fort61netcdf" 
FORT61="--fort61freq 900.0" 
# water current velocity station output
FORT62="--fort62freq 0"                    
# full domain water surface elevation output
FORT63="--fort63freq 3600.0 --fort63netcdf" 
# full domain water current velocity output
FORT64="--fort64freq 3600.0 --fort64netcdf" 
# met station output
#FORT7172="--fort7172freq 3600.0 --fort7172netcdf"           
FORT7172="--fort7172freq 3600.0"           
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



#--------------------------------------------------------
#  Notificaiton Configuration
#--------------------------------------------------------
EMAILNOTIFY=yes # set to yes to have host platform email notifications
NOTIFY_SCRIPT=cera_notify.sh
#ACTIVATE_LIST="natedill@gmail.com cera1g-l@listserv.lsu.edu nathan.dill@ransomenv.com"
ACTIVATE_LIST="natedill@gmail.com nathan.dill@ransomenv.com"
NEW_ADVISORY_LIST="natedill@gmail.com nathan.dill@ransomenv.com"
POST_INIT_LIST="natedill@gmail.com "
POST_LIST="natedill@gmail.com nathan.dill@ransomenv.com asgs.cera.lsu@gmail.com"
#POST_LIST="natedill@gmail.com nathan.dill@ransomenv.com"
JOB_FAILED_LIST="natedill@gmail.com nathan.dill@ransomenv.com"
NOTIFYUSER=nathan.dill@ransomenv.com
ASGSADMIN=nathan.dill@ransomenv.com
#

# Post processing and publication

INTENDEDAUDIENCE=developers-only  # general or developers-only, gets written to run.properties
INITPOST=null_init_post.sh
POSTPROCESS=cera_post.sh
POSTPROCESS2=null_post.sh

# opendap
TARGET=mike  # used in post processing to pick up HPC platform config
# You must first have your ssh public key in ~/.ssh/authorized_keys2 file 
# on the opendap server machine in order to scp files there via
# opendap_post.sh. OPENDAPHOST is set to each value in the TDS array specified
# above and used by your post processing script to successively trigger 
# configuration via platforms.sh. The OPENDAPUSER parameter needs to be set
# here, rather than in platforms.sh or your post processing script,
# because multiple Operators may be posting to a particular opendap server
# using different usernames. 
OPENDAPUSER=nate         # default value that works for RENCI opendap 
if [[ $OPENDAPHOST = "fortytwo.cct.lsu.edu" ]]; then
   OPENDAPUSER=nate  # change this for other Operator running on queenbee
fi
# OPENDAPNOTIFY is used by opendap_post.sh and could be regrouped with the 
# other notification parameters above. 
OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,natedill@gmail.com,nathan.dill@ransomenv.com"
TDS=(lsu_tds)
TARGET=mike

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
   ENSTORM=nhcConsensus
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac
