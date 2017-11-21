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
HOTTIFYPATH=~/adcirc/swan
MACHINE=jason-desktop
REMOVALCMD="rm"
while getopts "h:e" optname; do   
  case $optname in
    h) HOTTIFYPATH=${OPTARG} # -h /path/to/HottifySWAN.x (or unhcat.exe)
       # set name of SWAN executable that knits together the subdomain
       # SWAN hotstart files into a fulldomain SWAN hotstart file
       hSWANExe=null
       if [[ -e ${HOTTIFYPATH}/HottifySWAN.x ]]; then
          hSWANExe=HottifySWAN.x
       fi
       if [[ -e ${HOTTIFYPATH}/unhcat.exe ]]; then
          hSWANExe=unhcat.exe
       fi
       if [[ $hSWANExe = null ]]; then
          echo "ERROR: Could not find HottifySWAN.x or unhcat.exe in the directory ${HOTTIFYPATH}."
          exit
       fi
       ;;
    e) MACHINE=${OPTARG}     # e.g., -e queenbee 
       ;;
  esac
done
case $MACHINE in
  queenbee) REMOVALCMD="rmpurge"
       ;;
  *) REMOVALCMD="rm"
       ;;
esac
logFile="pedir_removal.log"
instancedir=$PWD
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
            # preserve the swan log file so we can see 
            if [[ -e ./PE0000/asgs_swan.prt ]]; then
               cp ./PE0000/asgs_swan.prt . 2>> $instancedir/$logFile
            fi
            # if it is a nowcast, archive the swan hotstart file if any
            if [[ $stormdir = nowcast ]]; then
               # construct fulldomain swan hotstart file and compress
               if [[ -e ./PE0000/swan.67 ]]; then
                  ${HOTTIFYPATH}/$hSWANExe <<EOF >> $instancedir/$logFile 2>&1 
1
swan.67
F
EOF
                  bzip2 swan.67 >> $instancedir/$logFile 2>&1
               fi
               if [[ -e ./PE0000/swan.68 ]]; then
                  ${HOTTIFYPATH}/$hSWANExe <<EOF >> $instancedir/$logFile 2>&1 
1
swan.68
F
EOF
                  bzip2 swan.68 >> $instancedir/$logFile 2>&1
               fi
            fi
            # now delete the subdomain directories
            rm -rf PE* 2>> $instancedir/$logFile
            # switch back to advisory directory
            cd ..
         fi
      done
      # switch back to instance directory
      cd ..
      echo "next advisory" >> $instancedir/$logFile
   fi
done
