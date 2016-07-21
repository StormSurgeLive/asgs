#!/bin/bash
#--------------------------------------------------------------------------
# ncfs_archive.sh 
#--------------------------------------------------------------------------
# Compresses files that should be saved and deletes everything else.
#--------------------------------------------------------------------------
# 
# Copyright(C) 2010-2016 Jason Fleming
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
   #
   # globalize and compress the swan hotstart file that was used to 
   # hotstart this nowcast run
   if [[ $dir = "nowcast/" ]]; then
      # 
      # check to see if SWAN was used, and if so, re-compose a fulldomain
      # SWAN hotstart file from subdomain SWAN hotstart files
      if [[ -e PE0000/swan.68 ]]; then
         echo "ncfs_archive.sh: Globalizing the swan.68 file in ${ADVISDIR}/${dir}."
         ${OUTPUTDIR}/HottifySWAN.x -g -u 68 2>&1
      fi
   fi
   cd ..
done
cd $startDir
