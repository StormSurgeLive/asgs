#!/bin/bash
#----------------------------------------------------------------
# esnstorm_pedir_removal.sh: Removes the PE* subdirectories that were
# created by adcprep for use in a parallel adcirc simulation.  
#----------------------------------------------------------------
# Copyright(C) 2017--2018 Jason Fleming
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
# This script gets all the information it needs from the 
# run.properties file.
#----------------------------------------------------------------
THIS=enstorm_pedir_removal.sh
logFile="enstorm_pedir_removal.log"
stormdir=$PWD
COMPRESSION=yes
#
ENSTORM=`sed -n 's/[ ^]*$//;s/asgs.config.enstorm\s*:\s*//p' run.properties`
echo "$ENSTORM: $THIS: stormdir is $stormdir" >> $stormdir/$logFile

WAVES=`sed -n 's/[ ^]*$//;s/asgs.config.waves\s*:\s*//p' run.properties`
if [[ $WAVES = on ]]; then
   ADCIRCDIR=`sed -n 's/[ ^]*$//;s/asgs.config.adcircdir\s*:\s*//p' run.properties`
   # FIXME: path to swan executables is a hardcoded path relative to ADCIRCDIR
   HOTTIFYPATH=$ADCIRCDIR/../swan
   #
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
   #
   # preserve the swan log file and swan Errfile (if any) so we can see it later
   # FIXME: the name of the asgs_swan.prt file is provided in swaninit but it
   # is hardcoded here as asgs_swan.prt
   for file in ./PE0000/asgs_swan.prt ./PE0000/Errfile ; do 
      if [[ -e $file ]]; then
         cp $file . 2>> $stormdir/$logFile
      fi
   done
   for file in swan.67 swan.68 ; do
      # tar up the swan subdomain hotstart files to create a fast
      # start for hotstarted jobs using the same number of processors;
      # then construct fulldomain swan hotstart file(s) and compress
      if [[ -e ./PE0000/$file ]]; then
         tar cvjf ${file}.tar.bz2 ./PE*/$file 2>> $stormdir/$logFile 2>&1 &
         (
            ${HOTTIFYPATH}/$hSWANExe <<EndInput >> $stormdir/$logFile 2>&1 
1
$file
F
EndInput
            if [[ $COMPRESSION = yes ]]; then
               bzip2 $file >> $stormdir/$logFile 2>&1 
            fi
         ) &
      fi
   done
fi
#
# allow all backgrounded processes to finish before going on to 
# delete subdomain directories
wait 
#
#
# pull in platform-specific value for the command used to remove directories
# (some platforms have a special command that is nicer for their filesystem)
REMOVALCMD="rm -rf"
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/asgs.config.scriptdir\s*:\s*//p' run.properties`
. ${SCRIPTDIR}/platforms.sh # pick up platform specific config
HPCENVSHORT=`sed -n 's/[ ^]*$//;s/hostname\s*:\s*//p' run.properties`
env_dispatch ${HPCENVSHORT}
# now delete the subdomain directories
for dir in `ls -d PE*`; do 
   $REMOVALCMD $dir 2>> $stormdir/$logFile
done
