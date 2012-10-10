#!/bin/bash
#--------------------------------------------------------------------------
# ranger_archive.sh 
#--------------------------------------------------------------------------
# Copies all files to corral.
#--------------------------------------------------------------------------
# 
# Copyright(C) 2010 Jason Fleming
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
if [[ -e $ARCHIVEBASE ]]; then
   if [[ ! -e ${ARCHIVEBASE}/${ARCHIVEDIR} ]]; then
      mkdir ${ARCHIVEBASE}/${ARCHIVEDIR} 
   fi
   if [[ ! -e ${ARCHIVEBASE}/${ARCHIVEDIR}/${ADVISORY} ]]; then
      mkdir ${ARCHIVEBASE}/${ARCHIVEDIR}/${ADVISORY}
   fi
   chgrp -R G-803086 ${ADVISDIR} 
   chmod -R 750 ${ADVISDIR}
   cp -a $ADVISDIR/*/*.zip ${ARCHIVEBASE}/${ARCHIVEDIR}/${ADVISORY}
   cp -a $ADVISDIR/* ${ARCHIVEBASE}/${ARCHIVEDIR}/${ADVISORY}
else
   echo "Archival process failed, archive base directory '$ARCHIVEBASE' does not exist." 1>&2
fi
