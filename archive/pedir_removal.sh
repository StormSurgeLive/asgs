#!/bin/bash
#----------------------------------------------------------------
# pedir_removal.sh: Removes the PE* subdirectories that were
# created by adcprep for use in a parallel adcirc simulation.  
#----------------------------------------------------------------
# Copyright(C) 2017 Jason Fleming
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
logFile=pedir_removal.log
instancedir=$PWD
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
for advisdir in `ls`; do
   # operate on advisory directories
   if [[ -d $advisdir ]]; then
      echo "advisdir is $advisdir" >> $instancedir/$logFile
      cd $advisdir 2>> $instancedir/$logFile
      # operate on different runs within the advisory directory
      for stormdir in `ls`; do
         if [[ -d $stormdir ]]; then
            echo "stormdir is $stormdir" >> $instancedir/$logFile 
            cd $stormdir 2>> $instancedir/$logFile
            ${ARCHIVEPATH}/enstorm_pedir_removal.sh -h $HOTTIFYPATH -e $MACHINE $COMPRESSION >> $instancedir/$logFile 2>&1
            # switch back to advisory directory
            cd ..
         fi
      done
      # switch back to instance directory
      cd ..
      echo "next advisory" >> $instancedir/$logFile
   fi
done
