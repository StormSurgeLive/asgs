#!/bin/sh
#-------------------------------------------------------------------
# config.sh: 
# This file is read at the beginning of the execution of the ASGS to set up the
# runs  that follow. It is reread at the beginning of every cycle, every time
# it polls the datasource for a new advisory. This gives the user the
# opportunity to edit this file mid-storm to change config parameters (e.g.,
# the name of the queue to submit to, the addresses on the mailing list, etc)
#-------------------------------------------------------------------
#
# Copyright(C) 2006, 2007 Brett Estrade
# Copyright(C) 2006, 2007, 2008, 2009, 2010 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------
#
#-------------------------------------------------------------------
# General configuration 
#-------------------------------------------------------------------
STORM=09             # storm number, e.g. 05=ernesto in 2006
YEAR=2008            # year of the storm (useful for historical storms)
COLDSTARTDATE=       # date corresponding to simulation coldstart
HOTORCOLD=coldstart  # "hotstart" or "coldstart"
LASTSUBDIR=          # if hotstarting, the subdir to get the hs file from
ADCIRCDIR=~/adcirc/v48release/work  # dir containing the ADCIRC executables
INPUTDIR=~/asgs/trunk/input   # dir containing grid and other input files 
OUTPUTDIR=~/asgs/trunk/output # dir containing post processing scripts
PERL5LIB=~/asgs/trunk/PERL    # dir with DateCale.pm perl module
SCRIPTDIR=~/asgs/trunk        # dir where ASGS executables located
NCPU=16                       # number of CPUs to use for all simulations
STARTADVISORYNUM=41           # starting advisory number, set to zero if
                              # downloading forecast advisories via rss
BACKGROUNDMET=on     # [de]activate NAM download/forcing
TIDES=off            # [de]activate tidal forcing
TROPICALCYCLONE=off  # [de]activate tropical cyclone forcing
PARTICLETRACK=off    # [de]activate particle tracking 
#
#-------------------------------------------------------------------
# Platform-related configuration
#-------------------------------------------------------------------
#QUEUENAME=R11329  # example dedicated queue name on ERDC machines
#SERQUEUE=R11299   # example dedicated serial queue name on ERDC machines
#SERQUEUE=debug     # for running test problems on ERDC machines
#QUEUENAME=standard # for ERDC machines, non-emergency queue
#QUEUENAME=nbatch  # for ERDC machines, this is emergency queue name
#QUEUENAME=debug   # for ERDC machines, this is small (test) queue name
#QUEUENAME=512cpu  # for UNC (topsail); can also use 128cpu or 32cpu
#QUEUENAME=workq   # for queenbee.loni.org or tezpur.hpc.lsu.edu; non-emergency
#QUEUENAME=priority #for queenbee.loni.org or tezpur.hpc.lsu.edu; emergency
#QUEUENAME=normal   # for TACC (ranger);
#QUEUENAME=development   # for TACC (ranger) development;
QUEUENAME=desktop  # for workstations with no queue
#
#-------------------------------------------------------------------
# Input configuration
#-------------------------------------------------------------------
#
# Once activated, the ASGS may be set to trigger a new nowcast/forecast cycle
# in one of  ways: (1) a changed forecast file on the NHC ftp site, or a new
# advisory announced on the NHC RSS feed 
TRIGGER=ftp    # either "ftp" or "rss"
#
# site information for retrieving advisories
RSSSITE=www.nhc.noaa.gov
#RSSSITE=www.seahorsecoastal.com # used for testing 
#
#FTPSITE=ftp.nhc.noaa.gov  # real anon ftp site for hindcast/forecast files 
#FDIR=/atcf/afst     # forecast dir on nhc ftp site
#HDIR=/atcf/btk      # hindcast dir on nhc ftp site
#  the following are used at UNC for test storms
FTPSITE=ftp.unc.edu
FDIR=/pub/ims/weaver/NHC_Advisories/fst # forecast dir on test site
HDIR=/pub/ims/weaver/NHC_Advisories/btk # hindcast dir on test site
#
# Location to download the background meteorological data from 
# the directory must contain the nam.yyyymmdd files
BACKSITE=ftpprd.ncep.noaa.gov       # NAM forecast data from NCEP
BACKDIR=/pub/data/nccf/com/nam/prod # contains the nam.yyyymmdd files
#
# It is assumed that the following files are in the INPUTDIR defined above
#
# file that contains the mesh (fort.14)
GRIDFILE=ec_95d.grd
#GRIDFILE=sl15_2007_IHNC_r03q_levchk.grd
#GRIDFILE=texas_2.85Mnode.grd
#GRIDFILE=sl15v3_2007_r10.grd
# file that acts as a template for the control file (fort.15)
#CONTROLTEMPLATE=ec_95_tides_fort.15_template
CONTROLTEMPLATE=ec_95_notides_fort.15_template
#CONTROLTEMPLATE=fort.15.sl15v7.tides.corps.template
#CONTROLTEMPLATE=fort.15.sl15v7.notides.corps.template
#CONTROLTEMPLATE=texas_2.85Mnode.fort.15_template
#CONTROLTEMPLATE=fort.15.sl15.corps.template
# nodal attributes file (fort.13)
NAFILE=
#NAFILE=sl15_2007_IHNC_r03q_EVIS2.13
#NAFILE=texas_2.85Mnode.fort.13
#NAFILE=sl15v3_2007_r09f.13
# archive of the fort.14 and fort.13 that have already been preprocessed
# for a certain number of CPUs (include the num of CPUs in the file name)
PREPPEDARCHIVE=prepped_ec_95d_4proc.tar.gz
#PREPPEDARCHIVE=prepped_ec_95d_16proc.tar.gz
#PREPPEDARCHIVE=prepped_sl15v7_corps_1000.tar.gz
#PREPPEDARCHIVE=prepped_texas_2.85Mnode_256proc.tar.gz
#PREPPEDARCHIVE=prepped_sl15v3_corps_1000.tar.gz
# 
# size of the time step to use
TIMESTEPSIZE=30.0
#
# estimated wall clock time for queueing systems that require an
# estimate; must be set small when sending test problems to the debug queue;
# for production, must be set large enough for any run, including initial
# hindcast run, which may be longer than 5 days in some cases
WALLTIME="2:00:00"
#WALLTIME="10:00:00"
#
#-------------------------------------------------------------------
# Storm ensemble configuration 
#-------------------------------------------------------------------
# 
ENSEMBLESIZE=1 # number of storms in the ensemble
#
# array of storm names that are to be run ... array length is ENSEMBLESIZE
STORMLIST[0]=1 # NHC consensus forecast  
#
NAME[0]="nowcast"
NAME[1]="nhcConsensus"
NAME[2]="higherMaxWindSpeed"
NAME[3]="slowerOverlandSpeed"
NAME[4]="veerRight"
NAME[5]="veerLeft"
NAME[6]="largerRmax"
NAME[7]="smallerRmax"
NAME[8]="" # fill in the blank for custom variation
#
PERCENT=
#
# The end time is used to cut the simulation off after a certain point, 
# e.g., if it has gone too far inland to be of interest
#ENDTIME=2008090200
#
#-------------------------------------------------------------------
# Notification configuration 
#-------------------------------------------------------------------
EMAILNOTIFY=yes # set to yes to have host platform email notifications
# this script is expected to be found in the output directory; it contains
# all the email notification subroutines 
NOTIFY_SCRIPT=notify.sh
# the following list of addresses will receive an announcement when
# the ASGS is activated for a particular storm (some end users consider
# this to be junk mail)
ACTIVATE_LIST="weav999@yahoo.com rjweaver@email.unc.edu"
# addresses to receive notification that a new advisory is now running
NEW_ADVISORY_LIST="weav999@yahoo.com rjweaver@email.unc.edu"
# addresses to receive notification that postprocessing has been initialized 
POST_INIT_LIST="weav999@yahoo.com rjweaver@email.unc.edu"
# addresses to receive notification that new results are now available
POST_LIST="weav999@yahoo.com rjweaver@email.unc.edu"
# this is the email address in the PBS job script
NOTIFYUSER=rjweaver@email.unc.edu
ASGSADMIN=jason.fleming@seahorsecoastal.com
#
#-------------------------------------------------------------------
# Output configuration 
#-------------------------------------------------------------------
#
# It is assumed that these files are located in the asgs2009 directory
# in the "output" subdir
#
# this is the script that sets up post processing for each advisory, 
# before the run happens
INITPOST=post_init.sh
# this is the name of the post processing script
POSTPROCESS=corps_post.sh
