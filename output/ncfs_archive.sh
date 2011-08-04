#!/bin/bash
#--------------------------------------------------------------------------
# ncfs_archive.sh 
#--------------------------------------------------------------------------
# Compresses files that should be saved and deletes everything else.
#--------------------------------------------------------------------------
# 
# Copyright(C) 2010, 2011 Jason Fleming
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
#
#--------------------------------------------------------------------------
ADVISDIR=$1
OUTPUTDIR=$2
STORM=$3
YEAR=$4
ADVISORY=$5
HOSTNAME=$6
ENSTORM=$7
ARCHIVEBASE=$8
ARCHIVEDIR=$9
#
startDir=`pwd`
cd $ADVISDIR 2>&1
for dir in `ls -d */`; do 
   cd $dir
   echo "ncfs_archive.sh: Deleting *.nc.gz files in ${ADVISDIR}/${dir}."
   # get rid of any compressed netcdf files ... those have already been
   # copied to an alternate location
   rm -rf *.nc.gz 2>&1
   #
   # tar up the swan output logs
   if [[ -e PE0000/asgs_swan.prt ]]; then
      echo "ncfs_archive.sh: Archiving asgs_swan.prt files in ${ADVISDIR}/${dir}."
      tar cvzf asgs_swan.prt.tar.gz PE*/asgs_swan.prt > asgs_swan.prt.log 2>&1
   fi
   #
   # compress the fort.63, swan files, and all the min/max files
   for file in `ls *.63`; do
      echo "ncfs_archive.sh: Compressing $file file in ${ADVISDIR}/${dir}."
      gzip $file 2>&1
   done
   #
   # compress the remaining fulldomain output files
   for file in fort.64 fort.73 fort.74; do 
      echo "ncfs_archive.sh: Compressing the $file file in ${ADVISDIR}/${dir}."
      gzip $file 2>&1
   done
   #
   # globalize and compress the swan hotstart file that was used to 
   # hotstart this nowcast run
   if [[ $dir = "nowcast/" ]]; then
      echo "ncfs_archive.sh: Globalizing the swan.68 file in ${ADVISDIR}/${dir}."
      ${OUTPUTDIR}/HottifySWAN.x -g -u 68 2>&1
      echo "ncfs_archive.sh: Compressing the swan.68 file in ${ADVISDIR}/${dir}."
      gzip swan.68 2>&1
   fi
   #
   # now get rid of the subdomain directories
   #if [[ $dir != "nowcast/" ]]; then
   #  rm -rf PE* 2>&1
   #fi
   cd ..
done
cd $startDir
