#!/bin/bash
#-----------------------------------------------------------------------
# gomex_renci_post.sh : Post processing for the Gulf of Mexico 
# implementation of the ASGS running at RENCI.  
#-----------------------------------------------------------------------
# Copyright(C) 2012 Jason Fleming
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
#-----------------------------------------------------------------------
#
CONFIG=$1
ADVISDIR=$2
STORM=$3
YEAR=$4
ADVISORY=$5
HOSTNAME=$6
ENSTORM=$7
CSDATE=$8
HSTIME=$9
GRIDFILE=${10}
OUTPUTDIR=${11}
SYSLOG=${12}
SSHKEY=${13}
#
# jgf20120626: Currently, this script makes symbolic links from the results
# locations to directory where the RENCI opendap server can find them. 
# TODO: Add in-situ post processing and graphics generation if needed, and
# post link those products to RENCI opendap for distribution as well. 
#
OPENDAPBASEPATH=/projects/ncfs/opendap/data
cd ${ADVISDIR}/${ENSTORM}
. ${CONFIG} # grab all static config info. 
#
# write the target area to the run.properties file for the CERA
# web app
echo "asgs : gomex" >> run.properties
#
# create opendap path
FORCING=WNAMAW12-NCP
if [[ TROPICALCYCLONE = on ]]; then
   FORCING="vortex-"
   if [[ WAVES = on ]]; then 
      FORCING=${FORCING}nws319
   else
      FORCING=${FORCING}nws19
   fi 
fi
MODEL=PADCIRC
if [[ WAVES = on ]]; then
   MODEL=PADCSWAN
fi
OPENDAPPATH=${OPENDAPBASEPATH}/${HOSTNAME}:2/${MODEL}/${GRIDNAME}/${FORCING}/${YEAR}/${MONTH}/${DAY}/${HOUR}
mkdir -p $OPENDAPPATH
cd $OPENDAPPATH
ln -s ${ADVISDIR}/${ENSTORM}/*.nc .
ln -s ${ADVISDIR}/${ENSTORM}/fort.13 .
ln -s ${ADVISDIR}/${ENSTORM}/fort.14 .
ln -s ${ADVISDIR}/${ENSTORM}/fort.15 .
ln -s ${ADVISDIR}/${ENSTORM}/fort.22 .
ln -s ${ADVISDIR}/${ENSTORM}/run.properties .
cd ${ADVISDIR}/${ENSTORM}
