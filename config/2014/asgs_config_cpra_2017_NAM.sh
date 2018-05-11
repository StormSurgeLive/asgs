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
# Copyright(C) 2006, 2007, 2008, 2009, 2010, 2011 Jason Fleming
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
# need this here so hstime can find its netcdf  shared libraries
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
#-------------------------------------------------------------------
# Fundamental Configuration 
#-------------------------------------------------------------------
INSTANCENAME=lsudaily # the name for this instance of the ASGS
COLDSTARTDATE=2014061000     # date corresponding to simulation coldstart 
HOTORCOLD=coldstart            # "hotstart" or "coldstart" 
LASTSUBDIR=null  #  advisory dir where latest hotstart file is located (fort.67 could be in LASTSUBDIR/nowcast/PE0000 or LASTSUBDIR/hindcast/PE0000 dir) use null if coldstart
HINDCASTLENGTH=30.0            # length of initial hindcast, from cold (days)  COLDSTARTDATE + HINDCASTLENGTH must found in btk file. only used to set RNDAY for coldstart 
REINITIALIZESWAN=no  # used to bounce the wave solution (i.e. yes if SWAN solution is having problems)


#-------------------------------------------------------------------
# Source File Paths
#-------------------------------------------------------------------
ADCIRCDIR=/home/acd/adcirc/adc51_15/work # dir containing the ADCIRC executables 
SCRIPTDIR=/home/acd/asgs/trunk        # dir where ASGS executables located 
INPUTDIR=${SCRIPTDIR}/input/meshes/cpra2017  # dir containing grid and other input files 
OUTPUTDIR=${SCRIPTDIR}/output # dir containing post processing scripts
PERL5LIB=${SCRIPTDIR}/PERL    # dir with DateCale.pm perl module

#-------------------------------------------------------------------
# Physical Forcing
#--------------------------------------------------------------------
BACKGROUNDMET=on     # [de]activate NAM download/forcing 
TIDEFAC=on           # [de]activate tide factor recalc 
TROPICALCYCLONE=off  # [de]activate tropical cyclone forcing (temp. broken)
WAVES=off            # [de]activate wave forcing 
VARFLUX=off          # [de]activate variable river flux forcing

#-----------------------------------------------------------------
# Computational Resources
#-----------------------------------------------------------------
TIMESTEPSIZE=1.0     # timestep for adcirc simulation (seconds)
SWANDT=1200          # timestep for swan (seconds 
NCPU=400             # number of procs to use for simulation
NUMWRITERS=0         # number of writer CPUs 
#PPN=4               # not used on tezpur (this is set in the environment dispach sedtion of asgs_main.sh)
HINDCASTWALLTIME="36:00:00"
ADCPREPWALLTIME="04:00:00"
NOWCASTWALLTIME="04:00:00"
FORECASTWALLTIME="09:00:00"
WALLTIME="04:00:00"
QUEUENAME=null           # queue for tezpur non-emergency
SERQUEUE=null            # don't use "single" queue b/c for real events the wait time can be too long.  #PBS -l nodes=1:ppn=4 is set in tezpur.adcprep.template.pbs
#QUEUENAME=priority
#SERQUEUE=priority 
CYCLETIMELIMIT="06:30:00"
NCPUCAPACITY=512


#------------------------------------------------------------------
# External data sources configuration
#------------------------------------------------------------------
#
# Tropical Cyclone configuration
#
#
STORM=01  # storm number, e.g. 05=ernesto in 2006 
YEAR=2014 # year of the storm (useful for historical storms) 
#TRIGGER=rss         # either "ftp" or "rss" or "rssembedded"
TRIGGER=rssembedded         # either "ftp" or "rss" or "rssembedded"
RSSSITE=www.nhc.noaa.gov    # site for retriving advisories
#RSSSITE=www.woodsholegroup.com    # site for retriving advisories
#FTPSITE=filesystem    # site for getting hindcast/nowcast atcf files
#FTPSITE=140.186.63.117   # site for getting hindcast/nowcast atcf files
FTPSITE=ftp.nhc.noaa.gov  # real anon ftp site for hindcast/forecast files
FDIR=/atcf/afst     # forecast dir on nhc ftp site 
HDIR=/atcf/btk      # hindcast dir on nhc ftp site 
#FDIR=null             # forecast dir on ftp site (only used with RSSSITE=filesystem)
#HDIR=/atcf/btk             # hindcast dir on ftp site
#
#
#
#
# Background Meterological forcing configuraiton  (NAM)
#FORECASTCYCLE="00,06,12,18"     # NAM cycles to run forecasts for, "none" if you don't want forecasts
FORECASTCYCLE="06"     # NAM cycles to run forecasts for, "none" if you don't want forecasts
BACKSITE=ftp.ncep.noaa.gov       # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours od NAM forecast to run (max 84)
PTFILE=ptFile.txt   # the file that provides the lat/lons for the OWI background met 
ALTNAMDIR=/work/nate/asgs/input    # alternate locations to search for NAM input data on local file system


# VARIABLE RIVER FLUX
#
RIVERSITE=ftp.nssl.noaa.gov  # ftp Location to download fort.20 files for variable river flux data. 
RIVERDIR=/projects/ciflow/adcirc_info # path on ftp server to get fort.20 files


#------------------------------------------------------------------
# Input FIles and Templates
#-----------------------------------------------------------------
# 
#GRIDFILE=ocpr_v19a_DesAllemands4CERA.14
GRIDFILE=cpra_2017_v07a_chk.grd
#GRIDFILE=cpra_2011_v03a_chk.grd
#GRIDFILE=sl15_2010_HSDRRS_2012_v9.grd
#GRIDNAME=ocpr_v19a_DesAllemands4CERA
GRIDNAME=cpra_2017
#GRIDNAME=sl15_HSDRRS
#CONTROLTEMPLATE=ocpr_v19a_DesAllemands4CERA.15.template
#CONTROLTEMPLATE=powell_cpra_v03a.15.template
CONTROLTEMPLATE=cpra_2017_v07a.15.template
#CONTROLTEMPLATE=hsdrrs_fort.15.template
ELEVSTATIONS=cpra2017v07_stations.txt
#ELEVSTATIONS=corps_elev_stations.txt
VELSTATIONS=null
METSTATIONS=cpra2017v07_stations.txt
#METSTATIONS=corps_elev_stations.txt
#NAFILE=ocpr_v19a_DesAllemands4CERA.13
#NAFILE=  # leave blank if there is none 
NAFILE=cpra_2017_v07a.13 
#NAFILE=sl15_2010_HSDRRS_2012.13
PREPPEDARCHIVE=cpra_2017.tar.gz
HINDCASTARCHIVE=cpra_2017.hc.tar.gz

# SWAN template control file (for creating fort.26 files)
#SWANTEMPLATE=null
SWANTEMPLATE=cpra_2017_v07a.26
RIVERINIT=null
RIVERFLUX=null
HINDCASTRIVERFLUX=null


#-----------------------------------------------------------------
# Storm Ensemble Configuration
#-----------------------------------------------------------------
ENSEMBLESIZE=1 # number of storms in the ensemble
PERCENT=default
RMAX=default
case $si in    #$si is set in asgs_main.sh
-1)
     # do nothing ... this is not a forecast
  ;;
0)
  # ENSTORM=nhcConsensus
   ENSTORM=namforecast
  ;;
1)
  ENSTORM=veerLeft50
  PERCENT=-50
 ;;
esac




# array of storm names that are to be run ... array length is 
#STORMLIST[0]=1 # nhcConsensus  
#STORMLIST[0]=8 # namforecast  
#
#NAME[0]="nowcast"
#NAME[1]="nhcConsensus"
#NAME[2]="higherMaxWindSpeed"
#NAME[3]="slowerOverlandSpeed"
#NAME[4]="veerRight"
#NAME[5]="veerLeft"
#NAME[6]="largerRmax"
#NAME[7]="smallerRmax"
#NAME[8]="namforecast"
#
#PERCENT[0]=0
#PERCENT[1]=0
#RMAX[0]=default
#
# The end time is used to cut the simulation off after a certain point, e.g.,
# if it has gone too far inland to be of interest
#ENDTIME=YYYYMMDDHH

#--------------------------------------------------------
# Output Files Configuration
#--------------------------------------------------------
#FORT61="--fort61freq 600.0 --fort61netcdf " # water surface elevation station output 
FORT61="--fort61freq 900.0" # water surface elevation station output 
FORT62="--fort62freq 0" # water current velocity station output       
FORT63="--fort63freq 1800.0  --fort63netcdf --netcdf4" # full domain water surface elevation output
#FORT63="--fort63freq 3600.0" # full domain water surface elevation output
FORT64="--fort64freq 00.0" # full domain water current velocity output 
FORT7172="--fort7172freq 900.0" # met station output
FORT7374="--fort7374freq 1800.0  --fort7374netcdf --netcdf4" # full domain meteorological output
#FORT7374="--fort7374freq 3600.0" # full domain meteorological output
SPARSE="--sparse-output"
OUTPUTOPTIONS="${SPARSE} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
HOTSTARTCOMP=fulldomain  # "subdomain to write subdimain hotstart files, "fulldomai" to write full domain hotstart files
HOTSTARTFORMAT=netcdf  # netcdf or binary
MINMAX="reset"  # "continuous" to maintain maxele.63 from coldstart, "reset" to just get forecast maxele.63

#--------------------------------------------------------
#  Notificaiton Configuration
#--------------------------------------------------------

EMAILNOTIFY=yes # set to yes to have host platform email notifications                                                                                                                                            
NOTIFY_SCRIPT=ncfs_nam_notify.sh
ACTIVATE_LIST="zbyerly@cct.lsu.edu jason.g.fleming@gmail.com"
NEW_ADVISORY_LIST="zbyerly@cct.lsu.edu jason.g.fleming@gmail.com"
POST_INIT_LIST="zbyerly@cct.lsu.edu jason.g.fleming@gmail.com"
POST_LIST="zbyerly@cct.lsu.edu jason.g.fleming@gmail.com"
JOB_FAILED_LIST="zbyerly@cct.lsu.edu jason.g.fleming@gmail.com"
NOTIFYUSER=zbyerly@cct.lsu.edu
ASGSADMIN=zbyerly@cct.lsu.edu
#

#--------------------------------------------------------
#  Post Processing and Publicaiton Configuration
#--------------------------------------------------------
INITPOST=null_init_post.sh  # executable to run at start for forecast (e.g. to create directories for results on external website)
POSTPROCESS=arete_post.sh    # executable to do in-situ post-processing
TARGET=arete                # gor the RenciGETools package
POSTPROCESS2=null_post.sh
WEBHOST=www.somewebserver.com
WEBUSER=remoteuser
WEBPATH=/path/to/nowhere/for/now
OPENDAPBASEDIR=/scratch/acd/opendap/

AUDIENCE=developers-only  # developers-only or general ,  gets written to run.properties
NUMCERASERVERS=1          # number of cera servers available for post processing.  used to balance post processing load


#--------------------------------------------------------
# Archiving Configuration
#--------------------------------------------------------
ARCHIVE=null_archive.sh
ARCHIVEBASE=/work/cera/hurricane
ARCHIVEDIR=asgs_ec95d



