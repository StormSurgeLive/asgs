#!/bin/bash
#----------------------------------------------------------------
# esnstorm_pedir_removal.sh: Removes the PE* subdirectories that were
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
# This script expects that when it starts, PWD points to a 
# directory contining subdomain (PE*) directories. 
#----------------------------------------------------------------
HOTTIFYPATH=/home/bblanton/adcirc-cg-52release/swan
MACHINE=pod-login
COMPRESSION=no
#
logFile="enstorm_pedir_removal.log"
stormdir=$PWD
REMOVALCMD="rm"
THIS=enstorm_pedir_removal.sh
ENSTORM=`basename $PWD`
echo "$ENSTORM: $THIS: Command line options are $@" >> $stormdir/$logFile
#
while getopts ":h:e:c" optname; do   
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
          echo "ERROR: $ENSTORM: $THIS: Could not find HottifySWAN.x or unhcat.exe in the directory ${HOTTIFYPATH}." >> $stormdir/$logFile
          exit
       else
          echo "$ENSTORM: $THIS: The SWAN hotstart composition executable is ${HOTTIFYPATH}/${hSWANExe}." >> $stormdir/$logFile
       fi
       ;;
    e) MACHINE=${OPTARG}     # e.g., -e queenbee 
       ;;
    c) COMPRESSION=yes 
       ;;
    \?) echo "$ENSTORM: $THIS: The command line option $optname ${OPTARG} was not recognized." >> $stormdir/$logFile
       ;;
  esac
done
case $MACHINE in
  queenbee) REMOVALCMD="rmpurge"
       ;;
  *) REMOVALCMD="rm -rf"
       ;;
esac
echo "$ENSTORM: $THIS: stormdir is $stormdir" >> $stormdir/$logFile
# preserve the swan log file so we can see it later
if [[ -e ./PE0000/asgs_swan.prt ]]; then
   cp ./PE0000/asgs_swan.prt . 2>> $stormdir/$logFile
fi
# preserve the swan Errfile if any
if [[ -e ./PE0000/Errfile ]]; then
   cp ./PE0000/Errfile . 2>> $stormdir/$logFile
fi
# construct fulldomain swan hotstart file and compress
if [[ -e ./PE0000/swan.67 ]]; then
   ${HOTTIFYPATH}/$hSWANExe <<EOF >> $stormdir/$logFile 2>&1 
1
swan.67
F
EOF
   if [[ $COMPRESSION = yes ]]; then
      bzip2 swan.67 >> $stormdir/$logFile 2>&1
   fi
fi
if [[ -e ./PE0000/swan.68 ]]; then
   ${HOTTIFYPATH}/$hSWANExe <<EOF >> $stormdir/$logFile 2>&1 
1
swan.68
F
EOF
   if [[ $COMPRESSION = yes ]]; then
      bzip2 swan.68 >> $stormdir/$logFile 2>&1
   fi
fi
# now delete the subdomain directories
for dir in `ls -d PE*`; do 
   $REMOVALCMD $dir 2>> $stormdir/$logFile
done
