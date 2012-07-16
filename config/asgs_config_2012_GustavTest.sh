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
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/packages/hdf5/1.8.2/intel-11.1-mvapich-1.1/lib
#
#-------------------------------------------------------------------
# Fundamentsl Configuration 
#-------------------------------------------------------------------
INSTANCENAME=0 # the name for this instance of the ASGS
COLDSTARTDATE=2008082612     # date corresponding to simulation coldstart 
HOTORCOLD=coldstart            # "hotstart" or "coldstart" 
LASTSUBDIR=null  #  advisory dir where latest hotstart file is located (fort.67 could be in LASTSUBDIR/nowcast/PE0000 or LASTSUBDIR/hindcast/PE0000 dir) use null if coldstart
HINDCASTLENGTH=5.0            # length of initial hindcast, from cold (days)  COLDSTARTDATE + HINDCASTLENGTH must found in btk file. only used to set RNDAY for coldstart 
REINITIALIZESWAN=no  # used to bounce the wave solution (i.e. yes if SWAN solution is having problems)


#-------------------------------------------------------------------
# Source File Paths
#-------------------------------------------------------------------
ADCIRCDIR=/home/nate/CERA_2012/adcirc/trunk/work # dir containing the ADCIRC executables 
INPUTDIR=/home/nate/CERA_2012/asgs/trunk/input  # dir containing grid and other input files 
OUTPUTDIR=/home/nate/CERA_2012/asgs/trunk/output # dir containing post processing scripts
PERL5LIB=/home/nate/CERA_2012/asgs/trunk/PERL    # dir with DateCale.pm perl module
SCRIPTDIR=/home/nate/CERA_2012/asgs/trunk        # dir where ASGS executables located 

#-------------------------------------------------------------------
# Physical Forcing
#--------------------------------------------------------------------
BACKGROUNDMET=off     # [de]activate NAM download/forcing 
TIDEFAC=on           # [de]activate tide factor recalc 
TROPICALCYCLONE=on  # [de]activate tropical cyclone forcing (temp. broken)
WAVES=off             # [de]activate wave forcing 
VARFLUX=off          # [de]activate variable river flux forcing

#-----------------------------------------------------------------
# Computational Resources
#-----------------------------------------------------------------
TIMESTEPSIZE=1.0     # timestep for adcirc simulation (seconds)
SWANDE=1200          # timestep for swan (seconds 
NCPU=512             # number of procs to use for simulation
NUMWRITERS=0         # number of writer CPUs 
#PPN=4               # not used on tezpur (this is set in the environment dispach sedtion of asgs_main.sh)
HINDCASTWALLTIME="12:00:00"
ADCPREPWALLTIME="12:00:00"
NOWCASTWALLTIME="12:00:00"
FORECASTWALLTIME="12:00:00"
WALLTIME="12:00:00"
#QUEUENAME=checkpt           # queue for tezpur non-emergency
#SERQUEUE=checkpt            # don't use "single" queue b/c for real events the wait time can be too long.  #PBS -l nodes=1:ppn=4 is set in tezpur.adcprep.template.pbs
QUEUENAME=priority
SERQUEUE=priority 



STARTADVISORYNUM=0   #check to see if this is still used, its not in the operators guide         # starting advisory number, set to zero if
		      # downloading forecast advisories via rss

#------------------------------------------------------------------
# External data sources configuration
#------------------------------------------------------------------
#
# Tropical Cyclone configuration
#
#
STORM=07  # storm number, e.g. 05=ernesto in 2006 
YEAR=2008 # year of the storm (useful for historical storms) 
TRIGGER=rss         # either "ftp" or "rss" or "rssembedded"
#TRIGGER=rssembedded         # either "ftp" or "rss" or "rssembedded"
#RSSSITE=www.nhc.noaa.gov    # site for retriving advisories
RSSSITE=www.woodsholegroup.com    # site for retriving advisories
#FTPSITE=filesystem    # site for getting hindcast/nowcast atcf files
FTPSITE=140.186.63.117   # site for getting hindcast/nowcast atcf files
FDIR=null             # forecast dir on ftp site (only used with RSSSITE=filesystem)
HDIR=/atcf/btk             # hindcast dir on ftp site
#
#
#
#
# Background Meterological forcing configuraiton  (NAM)
FORECASTCYCLE="00,06,12,18"     # NAM cycles to run forecasts for, "none" if you don't want forecasts
BACKSITE=ftp.ncep.noaa.gov       # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
FORECASTLENGTH=84                   # hours od NAM forecast to run (max 84)
PTFILE=ptFile.txt   # the file that provides the lat/lons for the OWI background met 
ALTNAMDIR=/work/nate/asgs    # alternate locations to search for NAM input data on local file system


# VARIABLE RIVER FLUX
#
RIVERSITE=ftp.nssl.noaa.gov  # ftp Location to download fort.20 files for variable river flux data. 
RIVERDIR=/projects/ciflow/adcirc_info # path on ftp server to get fort.20 files


#------------------------------------------------------------------
# Input FIles and Templates
#-----------------------------------------------------------------
# 
GRIDFILE=ocpr_v19a_DesAllemands4CERA.14
GRIDNAME=ocpr_v19a_DesAllemands4CERA
CONTROLTEMPLATE=ocpr_v19a_DesAllemands4CERA.15.template
ELEVSTATIONS=cera_stations_ocpr.txt
VELSTATIONS=null
METSTATIONS=cera_stations_ocpr.txt
NAFILE=ocpr_v19a_DesAllemands4CERA.13

PREPPEDARCHIVE=cera_ocpr_512proc.tar.gz
HINDCASTARCHIVE=cera_ocpr_512proc.hc.tar.gz

# SWAN template control file (for creating fort.26 files)
SWANTEMPLATE=fort.26.v6b.template
RIVERINIT=v6brivers.88
RIVERFLUX=v6brivers_fort.20_default
HINDCASTRIVERFLUX=v6brivers_fort.20_hc_default


#-----------------------------------------------------------------
# Storm Ensemble Configuration
#-----------------------------------------------------------------
ENSEMBLESIZE=1 # number of storms in the ensemble

# array of storm names that are to be run ... array length is 
STORMLIST[0]=1 # nhcConsensus  
#STORMLIST[0]=8 # namforecast  
#
NAME[0]="nowcast"
NAME[1]="nhcConsensus"
NAME[2]="higherMaxWindSpeed"
NAME[3]="slowerOverlandSpeed"
NAME[4]="veerRight"
NAME[5]="veerLeft"
NAME[6]="largerRmax"
NAME[7]="smallerRmax"
NAME[8]="namforecast"
#
PERCENT[0]=0
PERCENT[1]=0
RMAX[0]=default
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
#FORT63="--fort63freq 1800.0 --fort63netcdf" # full domain water surface elevation output
FORT63="--fort63freq 3600.0" # full domain water surface elevation output
FORT64="--fort64freq 00.0" # full domain water current velocity output 
FORT7172="--fort7172freq 900.0" # met station output
#FORT7374="--fort7374freq 1800.0 --fort7374netcdf" # full domain meteorological output
FORT7374="--fort7374freq 3600.0" # full domain meteorological output
SPARSE=
OUTPUTOPTIONS="${SPARSE} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
HOTSTARTCOMP=fulldomain  # "subdomain to write subdimain hotstart files, "fulldomai" to write full domain hotstart files
HOTSTARTFORMAT=binary  # netcdf or binary


#--------------------------------------------------------
#  Notificaiton Configuration
#--------------------------------------------------------
EMAILNOTIFY=yes # set to yes to have host platform email notifications
NOTIFY_SCRIPT=cera_notify.sh
ACTIVATE_LIST="natedill@gmail.com cera1g-l@listserv.lsu.edu ndill@whgrp.com"
NEW_ADVISORY_LIST="natedill@gmail.com ndill@whgrp.com"
POST_INIT_LIST="natedill@gmail.com "
POST_LIST="natedill@gmail.com ndill@whgrp.com ckaiser@cct.lsu.edu"
JOB_FAILED_LIST="natedill@gmail.com ndill@whgrp.com"
NOTIFYUSER=ndill@whgrp.com
ASGSADMIN=ndill@whgrp.com
#

#--------------------------------------------------------
#  Post Processing and Publicaiton Configuration
#--------------------------------------------------------
INITPOST=null_init_post.sh  # executable to run at start for forecast (e.g. to create directories for results on external website)
POSTPROCESS=cera_post.sh    # executable to do in-situ post-processing
TARGET=null                 # gor the RenciGETools package
POSTPROCESS2=null_post.sh
WEBHOST=www.somewebserver.com
WEBUSER=remoteuser
WEBPATH=/path/to/nowhere/for/now



#--------------------------------------------------------
# Archiving Configuration
#--------------------------------------------------------
ARCHIVE=null_archive.sh
ARCHIVEBASE=/work/jgflemin/hurricane
ARCHIVEDIR=asgs_ec95d


# nld June 25, 2012.  the configuration above was re-organized from a previous 
# asgs configure script to be conistent with the order the configuration is described
# in the asgs operators manual (not that the order really matters for the execution).  What 
# is below has all been commented out, but kept for reference
  







#-------------------------------------------------------------------
# Platform-related configuration
#-------------------------------------------------------------------
#QUEUENAME=R11329  # example dedicated queue name on ERDC machines
#SERQUEUE=R11299   # example dedicated serial queue name on ERDC machines
#SERQUEUE=debug     # for running test problems on ERDC machines
#QUEUENAME=standard # for ERDC machines, non-emergency queue 
#QUEUENAME=nbatch   # for ERDC machines, this is emergency queue name 
#QUEUENAME=debug   # for ERDC machines, this is small (test) queue name 
#QUEUENAME=512cpu  # for UNC (topsail); can also use 128cpu or 32cpu 
#QUEUENAME=priority 
#QUEUENAME=workq   # for queenbee.loni.org or tezpur.hpc.lsu.edu; non-emergency 
#QUEUENAME=checkpt
#QUEUENAME=priority #for queenbee.loni.org or tezpur.hpc.lsu.edu; emergency 
#SERQUEUE=priority
#SERQUEUE=workq
#QUEUENAME=request   # for TACC (ranger); 
#QUEUENAME=development   # for TACC (ranger) development;
#QUEUENAME=desktop  # for workstations with no queue
#ACCOUNT=erdcvenq
#QUEUENAME=desktop
#QUEUENAME=kittyhawk # doesn't seem to be a divided queue structure at RENCI
#QUEUENAME=batch # default queue name at RENCI
#QUEUENAME=armycore # also a possibility on blueridge
#SERQUEUE=armycore  # name of queue to be used by adcprep on blueridge
#if [[ $QUEUENAME = batch ]]; then PPN=8 ; else PPN=12 ; fi # for blueridge



#SCRATCHDIR=/work/cera # check to see if this is still used, its not in the operators guide   # for the NCFS on blueridge


#
#-------------------------------------------------------------------
# Input configuration
#-------------------------------------------------------------------
#
# TROPICAL CYCLONE
#
# Once activated, the ASGS may be set to trigger a new nowcast/forecast cycle
# in one of  ways: (1) a changed forecast file on the NHC ftp site, or a new
# advisory announced on the NHC RSS feed 
#TRIGGER=rss    # either "ftp" or "rss"
#
# site information for retrieving advisories
#RSSSITE=www.nhc.noaa.gov 
#RSSSITE=www.seahorsecoastal.com # used for testing 
#RSSSITE=www.woodsholegroup.com # used for testing 
#
#FTPSITE=ftp.nhc.noaa.gov  # real anon ftp site for hindcast/forecast files
#FDIR=/atcf/afst     # forecast dir on nhc ftp site 
#HDIR=/atcf/btk      # hindcast dir on nhc ftp site 
#  the following are used at UNC for test storms
#FTPSITE=ftp.unc.edu
#FDIR=/pub/ims/rick/ADCIRC/NHC_Advisories/fst # forecast dir on test site
#HDIR=/pub/ims/rick/ADCIRC/NHC_Advisories/btk # hindcast dir on test site
#
# NAM
#
# Location to download the background meteorological data from the directory
# must contain the nam.yyyymmdd files
#BACKSITE=ftp.ncep.noaa.gov       # NAM forecast data from NCEP
#BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
#FORECASTLENGTH=84                   # hours od NAM forecast to run (max 84)
# the file that provides the lat/lons for the OWI background met 
#PTFILE=ptFile.txt
#
# These local directories are alternate locations for searching for NAM input
# data --- in case old data cannot be found on the NCEP ftp site. When looking
# for nowcast files, the ASGS will look for grib2 files in the directories 
# $ALTNAMDIR/*/nowcast/erl.YYMMDD/" (2 digit year, 2 digit month,
# 2 digit day). When looking for forecast data, the ASGS will look for grib2
# files in the following directories: $ALTNAMDIR/YYYYMMDDHH24/namforecast/
# (4 digit year, 2 digit month, 2 digit day, 2 digit hour of 24 hour clock).
# The directories must be separated by commas with no spaces. No need for a
# trailing forward slash.
#ALTNAMDIR="/corral/hurricane/asgs_output,/corral/hurricane/asgs_ec95d"
#ALTNAMDIR=/work/nate/asgs__
#
# VARIABLE RIVER FLUX
#
# Location to download fort.20 files for variable river flux data. 
#RIVERSITE=ftp.nssl.noaa.gov
#RIVERDIR=/projects/ciflow/adcirc_info
#
# FORT FILES
# It is assumed that the following files are in the INPUTDIR defined above
#
# file that contains the mesh (fort.14)
#GRIDFILE=v6brivers.14
#GRIDFILE=ec_95d.grd
#GRIDFILE=ocpr_v19a_DesAllemands4CERA.14
#GRIDNAME=nc6b
#GRIDNAME=ocpr_v19a_DesAllemands4CERA
#GRIDFILE=sl
#GRIDNAME=sl15_2007_IHNC_r03q_levchk
#GRIDFILE=sl15_2007_IHNC_r03q_levchk.grd
#GRIDFILE=texas_2.85Mnode.grd 
#GRIDFILE=sl15v3_2007_r10.grd
# file that acts as a template for the control file (fort.15)
#CONTROLTEMPLATE=v6brivers_fort.15_template
#CONTROLTEMPLATE=ec_95_nc_stations_M2_fort.15_template
#CONTROLTEMPLATE=ec_95_nc_stations_fort.15_template
#CONTROLTEMPLATE=ec_95_fort.15_template
#CONTROLTEMPLATE=ocpr_v19a_DesAllemands4CERA.15.template
#CONTROLTEMPLATE=fort.15.sl15v7.tides.corps.template
#CONTROLTEMPLATE=fort.15.sl15v7.notides.corps.template
#CONTROLTEMPLATE=cera_sl15v7_fort.15.template
#CONTROLTEMPLATE=texas_2.85Mnode.fort.15_template
#CONTROLTEMPLATE=fort.15.sl15.corps.template
# lists of stations to include in the fort.15 file ... if there are no
# stations of a particular type, set the corresponding variable to "null"
#ELEVSTATIONS=cera_stations.txt
#VELSTATIONS=null
#METSTATIONS=cera_stations.txt
# nodal attributes file (fort.13)
#NAFILE=ocpr_v19a_DesAllemands4CERA.13
#NAFILE=v6brivers.13 
#NAFILE=sl15_2007_IHNC_r03q_EVIS2.13
#NAFILE=texas_2.85Mnode.fort.13
#NAFILE=sl15v3_2007_r09f.13
#
# SWAN template control file (for creating fort.26 files)
#SWANTEMPLATE=fort.26.v6b.template
#
#RIVERINIT=v6brivers.88
#RIVERFLUX=v6brivers_fort.20_default
#HINDCASTRIVERFLUX=v6brivers_fort.20_hc_default
#
# archive of the fort.14 and fort.13 that have already been preprocessed for a
# certain number of CPUs (include the num of CPUs in the file name)
# ... if the number of stations is different between the hindcast and operation
# (e.g., there is met forcing and there are met stations in the actual
# operation but these are absent in the hindcast) then there must be a 
# separately adcprepped archive for each 
#PREPPEDARCHIVE=cera_ocpr_512proc.tar.gz
#HINDCASTARCHIVE=cera_ocpr_512proc.hc.tar.gz
#PREPPEDARCHIVE=cera_sl15v7_512.tar.gz
#HINDCASTARCHIVE=cera_sl15v7_512.hc.tar.gz
#PREPPEDARCHIVE=prepped_ec_95d_16proc_nc_stations_M2.tar.gz
#HINDCASTARCHIVE=prepped_ec_95d_hc_16proc_nc_stations_M2.tar.gz
#PREPPEDARCHIVE=prepped_ec_95d_16proc_nc_stations.tar.gz
#HINDCASTARCHIVE=prepped_ec_95d_hc_16proc_nc_stations.tar.gz
#PREPPEDARCHIVE=prepped_ec_95d_4proc.tar.gz
#PREPPEDARCHIVE=prepped_ec_95d_16proc_corps_stations.tar.gz
#PREPPEDARCHIVE=prepped_sl15v7_corps_1000.tar.gz
#PREPPEDARCHIVE=prepped_texas_2.85Mnode_256proc.tar.gz
#PREPPEDARCHIVE=prepped_sl15v3_corps_1000.tar.gz
# 
# size of the time step to use
#TIMESTEPSIZE=1.0
#
# estimated wall clock time for queueing systems that require an estimate
# this varies widely among platforms, different grids, different timestep
# sizes, etc. Leave a cushion on the expected time so that the run does
# not get dumped out of the queue b/c it ran out of time ... also give nowcasts
# a little extra time, since some nowcasts are longer than others, depending
# on the circumstances.
#HINDCASTWALLTIME="24:00:00"
#ADCPREPWALLTIME="02:15:00"
#NOWCASTWALLTIME="08:00:00"
#FORECASTWALLTIME="08:00:00"
#WALLTIME="08:30:00"
#HINDCASTWALLTIME="12:00:00"
#ADCPREPWALLTIME="12:00:00"
#NOWCASTWALLTIME="12:00:00"
#FORECASTWALLTIME="12:00:00"
#WALLTIME="12:00:00"
#
#-------------------------------------------------------------------
# Storm ensemble configuration 
#-------------------------------------------------------------------
# 
#ENSEMBLESIZE=1 # number of storms in the ensemble
#
# array of storm names that are to be run ... array length is ENSEMBLESIZE
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
#PERCENT=
#
# The end time is used to cut the simulation off after a certain point, e.g.,
# if it has gone too far inland to be of interest
#ENDTIME=2008090200
#
# NAM CYCLE SELECTION
#
# The ASGS will always nowcast each cycle. It will also always download
# forecast files. This section is used to configure which cycles will
# actually run the forecast (by default, the system will run all
# forecasts). 
#FORECASTCYCLE="00,06,12,18"
#FORECASTCYCLE="00,06,12,18"
#
#-------------------------------------------------------------------
# Notification configuration 
#-------------------------------------------------------------------
#EMAILNOTIFY=yes # set to yes to have host platform email notifications
# this script is expected to be found in the output directory; it contains all
# the email notification subroutines 
#NOTIFY_SCRIPT=corps_nam_notify.sh
#NOTIFY_SCRIPT=cera_notify.sh
# the following list of addresses will receive an announcement when the ASGS is
# activated for a particular storm (some end users consider this to be junk
# mail)
#ACTIVATE_LIST="ndill@whgrp.com natedill@gmail.com ckaiser@cct.lsu.edu"
#ACTIVATE_LIST="ndill@whgrp.com ckaiser@cct.lsu.edu jason.fleming@seahorsecoastal.com rick_luettich@unc.edu rtwilley@louisiana.edu natedill@gmail.com khu@lsu.edu"
#ACTIVATE_LIST="ndill@whgrp.com ckaiser@cct.lsu.edu hampton@lsu.edu honggao@cct.lsu.edu janelle.fleming@seahorsecoastal.com jason.fleming@seahorsecoastal.com jgflemin@email.unc.edu krobbins@srcc.lsu.edu kenneth.graham@noaa.gov jonathan.brazzell@noaa.gov dschlotzhauer@ohsep.louisiana.gov rick_luettich@unc.edu rtwilley@louisiana.edu rtwilley@lsu.edu mwolcott@agctr.lsu.edu qchen@lsu.edu meselhe@louisiana.edu natedill@gmail.com"
# addresses to receive notification that a new advisory is now running
#NEW_ADVISORY_LIST="ndill@whgrp.com natedill@gmail.com ckaiser@cct.lsu.edu"
#NEW_ADVISORY_LIST="ndill@whgrp.com ckaiser@cct.lsu.edu jason.fleming@seahorsecoastal.com rick_luettich@unc.edu rtwilley@louisiana.edu natedill@gmail.com khu@lsu.edu"
#NEW_ADVISORY_LIST="ndill@whgrp.com ckaiser@cct.lsu.edu hampton@lsu.edu honggao@cct.lsu.edu janelle.fleming@seahorsecoastal.com jason.fleming@seahorsecoastal.com jgflemin@email.unc.edu krobbins@srcc.lsu.edu kenneth.graham@noaa.gov jonathan.brazzell@noaa.gov dschlotzhauer@ohsep.louisiana.gov rick_luettich@unc.edu rtwilley@louisiana.edu rtwilley@lsu.edu mwolcott@agctr.lsu.edu qchen@lsu.edu meselhe@louisiana.edu natedill@gmail.com"
# addresses to receive notification that postprocessing has been initialized 
#POST_INIT_LIST="ndill@whgrp.com ckaiser@cct.lsu.edu jason.fleming@seahorsecoastal.com rick_luettich@unc.edu rtwilley@louisiana.edu natedill@gmail.com khu@lsu.edu"
#POST_INIT_LIST="ndill@whgrp.com natedill@gmail.com ckaiser@cct.lsu.edu"
#POST_INIT_LIST="ndill@whgrp.com ckaiser@cct.lsu.edu hampton@lsu.edu honggao@cct.lsu.edu janelle.fleming@seahorsecoastal.com jason.fleming@seahorsecoastal.com jgflemin@email.unc.edu krobbins@srcc.lsu.edu kenneth.graham@noaa.gov jonathan.brazzell@noaa.gov dschlotzhauer@ohsep.louisiana.gov rick_luettich@unc.edu rtwilley@louisiana.edu rtwilley@lsu.edu mwolcott@agctr.lsu.edu qchen@lsu.edu meselhe@louisiana.edu natedill@gmail.com"
# addresses to receive notification that new results are now available
#POST_LIST="ndill@whgrp.com natedill@gmail.com ckaiser@cct.lsu.edu"
#POST_LIST="ndill@whgrp.com ckaiser@cct.lsu.edu jason.fleming@seahorsecoastal.com rick_luettich@unc.edu rtwilley@louisiana.edu natedill@gmail.com khu@lsu.edu"
#POST_LIST="ndill@whgrp.com ckaiser@cct.lsu.edu hampton@lsu.edu honggao@cct.lsu.edu janelle.fleming@seahorsecoastal.com jason.fleming@seahorsecoastal.com jgflemin@email.unc.edu krobbins@srcc.lsu.edu kenneth.graham@noaa.gov jonathan.brazzell@noaa.gov dschlotzhauer@ohsep.louisiana.gov rick_luettich@unc.edu rtwilley@louisiana.edu rtwilley@lsu.edu mwolcott@agctr.lsu.edu qchen@lsu.edu meselhe@louisiana.edu natedill@gmail.com"
# this is the email address in the PBS job script
#NOTIFYUSER=ndill@whgrp.com
#ASGSADMIN=ndill@whgrp.com
#
#-------------------------------------------------------------------
# ADCIRC Output configuration 
#-------------------------------------------------------------------
# by default, all output is turned off; output of each type can be activated by
# providing the corresponding command line argument for the output frequency;
# if activated then  a new output file will be created for each run (rather
# than appending the file from the previous run), although continuous output
# files may be selected with the corresponding "append" string, which will
# cause the data file will reflect the whole time series from cold start ...
# maxele and maxwvel files are always generated and always reflect the whole
# time span since cold start ... the other max.min*.63 files are ignored by the
# asgs
#
# output frequency in this section is in SECONDS, not timesteps
#
#FORT61="--fort61freq 900.0 --fort61netcdf" # water surface elevation station output 
#FORT61="--fort61freq 600.0" # water surface elevation station output 
#FORT62="--fort62freq 0" # water current velocity station output       
#FORT63="--fort63freq 1800.0" # full domain water surface elevation output
#FORT64="--fort64freq 00.0" # full domain water current velocity output 
#FORT7172="--fort7172freq 600.0" # met station output
#FORT7374="--fort7374freq 1800.0" # full domain meteorological output
#SPARSE="--sparse-output"
#SPARSE="--fort63netcdf 1 --fort7374netcdf 1"
#
#OUTPUTOPTIONS="${SPARSE} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
#
# The composition of hotstart files can be either "subdomain" to produce
# local hotstart files, or "fulldomain" to produce global hotstart files.
#HOTSTARTCOMP=fulldomain
#
#HOTSTARTFORMAT=binary
#
#
#-------------------------------------------------------------------
# Post Processing and Publication configuration 
#-------------------------------------------------------------------
# It is assumed that these postprocessing scripts are located in the
# ASGS directory in the "output" subdir
#
# this is the script that sets up post processing for each advisory, 
# before the run happens
#INITPOST=null_init_post.sh
# this is the name of the post processing script
#POSTPROCESS=cera_post.sh
#POSTPROCESS=corps_post.sh
# the platform that the post processing script will be running on ...
# used in setting up the paths to various graphics libraries and executables
# for the generation of KMZ and JPG files
# (see config_simple_gmt_pp.sh for choices)
#TARGET=blueridge
#
#POSTPROCESS2=null_post.sh
#
# This will post results to the password protected www.seahorsecoastal.com site
#WEBHOST=alpha.he.net
#WEBUSER=seahrse
#WEBPATH=/home/seahrse/public_html/ASGS
#
#-------------------------------------------------------------------
# Archive configuration 
#-------------------------------------------------------------------
# This section is used to configure the process of archiving the output
# data (if any). 
#
# It is assumed that this script is present in the output subdirectory
#ARCHIVE=null_archive.sh
# path to the archive directory (should already exist when the 
# ASGS kicks off)
#ARCHIVEBASE=/work/jgflemin/hurricane
# path to the subdirectory under $ARCHIVE where ASGS run data should be
# copied (will be created if it does not already exist)
#ARCHIVEDIR=asgs_ec95d
