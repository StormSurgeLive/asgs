#!/bin/bash
#----------------------------------------------------------------
# esnstorm_pedir_removal.sh: Removes the PE* subdirectories that were
# created by adcprep for use in a parallel adcirc simulation.  
#----------------------------------------------------------------
# Copyright(C) 2017--2019 Jason Fleming
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
# run.properties file instead of using command line arguments. 
#----------------------------------------------------------------
HOTTIFYPATH=/home/ncfs-dev/ADCIRC/v53release/swan
MACHINE=hatteras
COMPRESSION=no
#
logFile="enstorm_pedir_removal.log"
stormdir=$PWD
REMOVALCMD="rm"
localLogMessage()
{
  LEVEL=$1
  MSG=$2
  DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  echo "[${DATETIME}] ${ENSTORM}: ${THIS}: ${LEVEL}: ${MSG}" >> $LOGFILE 
}
# static initialization
THIS=enstorm_pedir_removal.sh
LOGFILE="archive.log"
# run configuration
STORMDIR=`sed -n 's/[ ^]*$//;s/asgs.path.stormdir\s*:\s*//p' run.properties`
ENSTORM=`sed -n 's/[ ^]*$//;s/asgs.enstorm\s*:\s*//p' run.properties`
SWANHSCOMPRESSION=`sed -n 's/[ ^]*$//;s/config.coupling.waves.swan.swanhscompression\s*:\s*//p' run.properties`
# pull in logging functions 
SYSLOG=`sed -n 's/[ ^]*$//;s/asgs.file.syslog\s*:\s*//p' run.properties`
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/config.path.scriptdir\s*:\s*//p' run.properties`
. ${SCRIPTDIR}/logging.sh
#
logMessage "Starting cleanup of subdomain (PE*) subdirectories."
localLogMessage "INFO" "Starting cleanup of subdomain (PE*) subdirectories."
#
WAVES=`sed -n 's/[ ^]*$//;s/config.coupling.waves\s*:\s*//p' run.properties`
if [[ $WAVES = on ]]; then
   localLogMessage "INFO" "Wave coupling with SWAN is active."
   SWANDIR=`sed -n 's/[ ^]*$//;s/config.path.swandir\s*:\s*//p' run.properties`
   localLogMessage "INFO" "The path to SWAN executables is ${SWANDIR}."
   #
   # set name of SWAN executable that knits together the subdomain
   # SWAN hotstart files into a fulldomain SWAN hotstart file
   hSWANExe=null
   if [[ -e ${SWANDIR}/HottifySWAN.x ]]; then
      hSWANExe=HottifySWAN.x
   fi
   if [[ -e ${SWANDIR}/unhcat.exe ]]; then
      hSWANExe=unhcat.exe
   fi
   if [[ $hSWANExe = null ]]; then
      localLogMessage "ERROR" "Could not find HottifySWAN.x or unhcat.exe in the directory ${SWANDIR}."
      exit
   else
      localLogMessage "INFO" "The SWAN hotstart composition executable is ${SWANDIR}/${hSWANExe}." 
   fi
   #
   # preserve the swan log file and swan Errfile (if any) so we can see it later
   # FIXME: the name of the asgs_swan.prt file is provided in swaninit but it
   # is hardcoded here as asgs_swan.prt
   for file in ./PE0000/asgs_swan.prt ./PE0000/Errfile ; do 
      if [[ -e $file ]]; then
         localLogMessage "INFO" "Preserving SWAN $file file."
         cp $file . 2>> $LOGFILE
      fi
   done
   for file in swan.67 swan.68 ; do
      if [[ -e $file ]]; then
         localLogMessage "INFO" "Renaming existing fulldomain $file file to ${file}.start."
         mv $file ${file}.start 2>> $LOGFILE
      fi
      # tar up the swan subdomain hotstart files to create a fast
      # start for hotstarted jobs using the same number of processors;
      # then construct fulldomain swan hotstart file(s) and compress
      if [[ -e ./PE0000/$file ]]; then
         (
            localLogMessage "INFO" "Creating tar archive of subdomain $file files."
            tar cf ${file}.tar ./PE*/$file 2>> $LOGFILE 2>&1
            if [[ $SWANHSCOMPRESSION = yes ]]; then 
               bzip2 ${file}.tar 2>> $LOGFILE 2>&1
            fi
            localLogMessage "INFO" "Finished creating tar archive of subdomain $file files."
         ) &
         (
            localLogMessage "INFO" "Creating fulldomain SWAN hotstart file from subdomain $file files."         
            ${SWANDIR}/$hSWANExe <<EndInput >> $LOGFILE 2>&1 
1
$file
F
EndInput
            if [[ $SWANHSCOMPRESSION = yes ]]; then
               bzip2 $file >> $LOGFILE 2>&1 
            fi
            localLogMessage "INFO" "Finished creating fulldomain SWAN hotstart file from subdomain $file files."
         ) &
      fi
   done
fi
#
# allow all backgrounded processes to finish before going on to 
# delete subdomain directories
wait 
#
# archive the subdomain fort.16 log files
tar cjf fort.16.tar.bz2 ./PE*/fort.16 2>> $LOGFILE 2>&1
#
#
# pull in platform-specific value for the command used to remove directories
# (some platforms have a special command that is nicer for their filesystem)
REMOVALCMD="rm -rf"
. ${SCRIPTDIR}/platforms.sh # pick up platform specific config
HPCENVSHORT=`sed -n 's/[ ^]*$//;s/hpc.hpcenvshort\s*:\s*//p' run.properties`
env_dispatch ${HPCENVSHORT}
# now delete the subdomain directories
for dir in `ls -d PE*`; do 
   $REMOVALCMD $dir 2>> $LOGFILE
done
#$REMOVALCMD metis_graph.txt partmesh.txt fort.80
$REMOVALCMD partmesh.txt
logMessage "Finished cleanup of subdomain (PE*) subdirectories."
localLogMessage "INFO " "Finished cleanup of subdomain (PE*) subdirectories."
