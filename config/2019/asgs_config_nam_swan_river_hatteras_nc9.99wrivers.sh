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
# Copyright(C) 2006--2019 Jason Fleming
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
INSTANCENAME=hiresr-RENCI       # name of this ASGS process, to differentiate results
#COLDSTARTDATE=2016061300
#COLDSTARTDATE=2016111500
#COLDSTARTDATE=2017010100
#COLDSTARTDATE=2017012400
#COLDSTARTDATE=2017071000
#COLDSTARTDATE=2017121500
#COLDSTARTDATE=2018020900
#COLDSTARTDATE=2018022100
#COLDSTARTDATE=2018090100
#COLDSTARTDATE=2018091500
#COLDSTARTDATE=2018111000
#COLDSTARTDATE=2018120100
#COLDSTARTDATE=2018121700
COLDSTARTDATE=2019010500
HOTORCOLD=coldstart       # "hotstart" or "coldstart" 
LASTSUBDIR=null
HINDCASTLENGTH=30.0       # length of initial hindcast, from cold (days)
REINITIALIZESWAN=yes       # used to bounce the wave solution

# Source file paths

ADCIRCDIR=~/adcirc-cg/jasonfleming/v53release_openmpi/work # ADCIRC executables
SWANDIR=~/adcirc-cg/jasonfleming/v53release_openmpi/swan   # SWAN executables 
SCRIPTDIR=~/asgs/jasonfleming/2014stable     # ASGS scripts/executables  
INPUTDIR=${SCRIPTDIR}/input/meshes/nc_v9.99_w_rivers # dir containing grid and other input files 
OUTPUTDIR=${SCRIPTDIR}/output # dir containing post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL    # dir with DateCale.pm perl module

# Physical forcing

BACKGROUNDMET=on     # [de]activate NAM download/forcing 
TIDEFAC=on           # [de]activate tide factor recalc 
TROPICALCYCLONE=off  # [de]activate tropical cyclone forcing (temp. broken)
WAVES=on             # [de]activate wave forcing 
VARFLUX=on           # [de]activate variable river flux forcing

# Computational Resources

TIMESTEPSIZE=0.5
SWANDT=1200
HINDCASTWALLTIME="12:00:00"
ADCPREPWALLTIME="01:15:00"
NOWCASTWALLTIME="10:00:00"  # must have leading zero, e.g., 05:00:00
FORECASTWALLTIME="05:00:00" # must have leading zero, e.g., 05:00:00
NCPU=160
NCPUCAPACITY=640
NUMWRITERS=0
CYCLETIMELIMIT="05:00:00"
# queue
QUEUENAME=null
SERQUEUE=null
SCRATCHDIR=/projects/ncfs/data # for the NCFS on blueridge
#ACCOUNT=batch # or "ncfs" on hatteras to use pre-empt capability
PARTITION=ncfs
RESERVATION=null
#CONSTRAINT='sandybridge&hatteras'
CONSTRAINT=null
QSCRIPT=hatteras-openmpi.template.slurm

# External data sources : Tropical cyclones

STORM=12  # storm number, e.g. 05=ernesto in 2006 
YEAR=2013 # year of the storm (useful for historical storms) 
TRIGGER=rssembedded    # either "ftp" or "rss"
RSSSITE=www.nhc.noaa.gov 
FTPSITE=ftp.nhc.noaa.gov  # real anon ftp site for hindcast/forecast files
FDIR=/atcf/afst     # forecast dir on nhc ftp site 
HDIR=/atcf/btk      # hindcast dir on nhc ftp site 

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

GRIDFILE=nc_inundation_v9.99a_w_rivers.grd
GRIDNAME=nc_inundation_v9.99_w_rivers
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=nc_9.99wrivers_vortex_fort.15.template
CONTROLPROPERTIES=fort.15.properties
ELEVSTATIONS=ncv999_stations_20180907.txt
VELSTATIONS=${ELEVSTATIONS}
METSTATIONS=${ELEVSTATIONS}
NAFILE=nc_inundation_v9.99_rivers.13
NAPROPERTIES=${NAFILE}.properties
SWANTEMPLATE=fort.26.template
RIVERINIT=v6brivers.88
RIVERFLUX=v6brivers_fort.20_default
HINDCASTRIVERFLUX=v6brivers_fort.20_hc_default
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

EMAILNOTIFY=yes # set to yes to have host platform email notifications
NOTIFY_SCRIPT=ncfs_nam_notify.sh
ACTIVATE_LIST=null
NEW_ADVISORY_LIST=null
POST_INIT_LIST=null
POST_LIST=null
JOB_FAILED_LIST=jason.g.fleming@gmail.com
NOTIFYUSER=jason.g.fleming@gmail.com
ASGSADMIN=jason.g.fleming@gmail.com
# RMQ Messaging
RMQMessaging_Enable="on"      #  enables message generation ("on" | "off")
RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
RMQMessaging_Script="${SCRIPTDIR}/asgs-msgr.py"
RMQMessaging_NcoHome="/home/bblanton/"
RMQMessaging_Python="/projects/storm_surge/anaconda/bin/python"
RMQMessaging_LocationName="RENCI"
RMQMessaging_ClusterName="Hatteras"

# Post processing and publication

INTENDEDAUDIENCE=general
INITPOST=null_init_post.sh
POSTPROCESS=ncfs_post_min.sh
POSTPROCESS2=null_post.sh

TDS=(renci_tds)
TARGET=hatteras  # used in post processing to pick up HPC platform config
OPENDAPUSER=ncfs         # default value that works for RENCI opendap 
if [[ $OPENDAPHOST = "fortytwo.cct.lsu.edu" ]]; then
   OPENDAPUSER=jgflemin  # change this for other Operator running on queenbee
fi
# OPENDAPNOTIFY is used by opendap_post.sh and could be regrouped with the 
# other notification parameters above. 
OPENDAPNOTIFY="asgs.cera.lsu@gmail.com,jason.g.fleming@gmail.com"

# Archiving

ARCHIVE=enstorm_pedir_removal.sh
ARCHIVEBASE=/projects/ncfs/data
ARCHIVEDIR=archive

# Forecast ensemble members

RMAX=default
PERCENT=default
ENSEMBLESIZE=2 # number of storms in the ensemble
case $si in
-1)
      # do nothing ... this is not a forecast
   ;;
0)
   ENSTORM=namforecast
   PARTITION=ncfs
   RESERVATION=null
   #CONSTRAINT='sandybridge&hatteras'
   ;;
1)
   ENSTORM=namforecastWind10m
   PARTITION=ncfs
   RESERVATION=null
   #CONSTRAINT='sandybridge&hatteras'
   ADCPREPWALLTIME="00:20:00"  # adcprep wall clock time, including partmesh
   FORECASTWALLTIME="00:20:00" # forecast wall clock time
   CONTROLTEMPLATE=nc_9.99wrivers.nowindreduction.fort.15.template
   CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
   TIMESTEPSIZE=300.0   # 5 minute time steps
   NCPU=15               # so total cpus match with other ensemble members
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
   # prevent collisions in prepped archives
   PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
   #POSTPROCESS=wind10m_post.sh
   ;;
*)
   echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
   ;;
esac

