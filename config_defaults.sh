#!/bin/bash
#----------------------------------------------------------------
#
# config_defaults.sh: This script provides the default 
# configuration parameters for the ASGS.
#
#----------------------------------------------------------------
# Copyright(C) 2014 Jason Fleming
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
#----------------------------------------------------------------
#
INSTANCENAME=1
BACKGROUNDMET=on
TIDEFAC=off
TROPICALCYCLONE=off
WAVES=off
VARFLUX=off
MINMAX=continuous
REINITIALIZESWAN=no
USERIVERFILEONLY=no
STORMNAME=stormname
RIVERSITE=ftp.nssl.noaa.gov
RIVERDIR=/projects/ciflow/adcirc_info
RIVERUSER=null
RIVERDATAPROTOCOL=null
ELEVSTATIONS=null
VELSTATIONS=null
METSTATIONS=null
GETINPUTSCRIPT=null
GRIDFILE=fort.14
GRIDNAME=fort14
OUTPUTOPTIONS=
ARCHIVEBASE=/dev/null
ARCHIVEDIR=null
FORECASTCYCLE="00,06,12,18"
TRIGGER="rss"
LASTADVISORYNUM=0
ADVISORY=0
FORECASTLENGTH=84
ALTNAMDIR=null
HOTSTARTCOMP=fulldomain
HINDCASTWALLTIME="10:00:00"
ADCPREPWALLTIME="00:30:00"
NOWCASTWALLTIME="02:00:00"
FORECASTWALLTIME="05:00:00"
SWANDT=600
UMASK=002
GROUP=""
DRY=1
DEMO=null
STORM=0
YEAR=null
CSDATE=null
HOTORCOLD=coldstart
NOWCASTLENGTH=null
FORECASTLENGTH=null
LASTSUBDIR=null
FTPSITE=null
FTPFCSTDIR=null
FTPHCSTDIR=null
ADCIRCDIR=null
SCRATCHDIR=null
MAILINGLIST=null
ENV=null
QUEUESYS=null
QUEUENAME=null
SERQUEUE=null
QCHECKCMD=null
NCPU=null
JOBTYPE=null
NUMWRITERS=0
ACCOUNT=desktop
SUBMITSTRING=null
INTERSTRING=null
RESULTSHOST=null
RESULTSPATH=null
RESULTSUSERNAME=null
RESULTSPROMPT=null
RESULTSPASSWORD=null
NOTIFYUSER=null
RUNDIR=null
INPUTDIR=null
PERL5LIB=
HOTSTARTFORMAT=null
DEFAULTSFILE=null
STORMDIR=stormdir
SSHKEY=null
PPN=1
VELOCITYMULTIPLIER=1.0
HOTSWAN=off
ONESHOT=no      # yes if ASGS is launched by cron
NCPUCAPACITY=2  # total number of CPUs available to run jobs
STATEFILE=null
ENSTORM=hindcast
CYCLETIMELIMIT="05:00:00"
IMAGEMAGICKBINPATH=null
SERQSCRIPT=null
SERQSCRIPTGEN=null
