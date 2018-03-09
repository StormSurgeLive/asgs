#!/bin/bash
#----------------------------------------------------------------
# pedir_removal_driver.sh: Finds ASGS instance directories older
# than a certain age and removes the PE* subdirectories via
# pedir_removal.sh.
#----------------------------------------------------------------
# Copyright(C) 20178, 2018 Jason Fleming
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
# This script expects that when it starts, PWD points to an 
# asgs run directory with subdirectories representing various
# advisories
#----------------------------------------------------------------
# Sample invokation:
# ~/asgs/2014stable/archive/pedir_removal_driver.sh -h ~/adcirc/forks/adcirc/master/work/swan -e hatteras -a ~/asgs/2014stable/archive -c 
#----------------------------------------------------------------
logFile="pe_removal_driver.log"
COMPRESSION=""
#
while getopts ":h:e:a:c" optname; do
  case $optname in
    h) HOTTIFYPATH=${OPTARG} 
       ;;
    e) MACHINE=${OPTARG}     # e.g., -e queenbee 
       ;;
    a) ARCHIVEPATH=${OPTARG}     
       ;;
    c) COMPRESSION="-c"
       ;;
    \?) echo "$THIS: The command line option $optname ${OPTARG} was not recognized." >> $instancedir/$logFile
       ;;
  esac
done
#
ageThresholdSec=2592000 # number of seconds in 30 days
rundir=$PWD # start in the directory containing all the asgsxxxxx directories
nowdate=`date`
nowsec=`date +%s`
echo "INFO: Current date is ${nowdate}." > $logFile
echo "INFO: Current time is $nowsec seconds." >> $logFile
for instancedir in `ls -dtr asgs*`; do
   echo "INFO: Instancedir is $instancedir" >> $logFile
   # operate on instance directories
   if [[ -d $instancedir ]]; then
      echo "INFO: Instance directory is $instancedir" >> $logFile
      # check last modification time on the directory
      lastModTimeSec=`date +%s -r $instancedir`
      echo "INFO: Last modification time is $lastModTimeSec" >> $logFile
      # determine how long ago the directory was last modified
      ageSec=`expr $nowsec - $lastModTimeSec`
      echo "INFO: Directory age in seconds is $ageSec" >> $logFile
      # if longer than the threshold, then clean out the subdomain directories
      if [[ $ageSec -gt $ageThresholdSec ]]; then
         cd $instancedir
         echo "INFO: Must clean out the directory $instancedir" >> ../$logFile
         ${ARCHIVEPATH}/pedir_removal.sh -h $HOTTIFYPATH -e $MACHINE -a $ARCHIVEPATH $COMPRESSION
         cd ..
      else
         echo "INFO: The directory $instancedir is too recent." >> $logFile
      fi
   fi
done
