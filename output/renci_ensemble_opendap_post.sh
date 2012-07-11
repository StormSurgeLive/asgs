#!/bin/bash
#
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
cd ${ADVISDIR}/${ENSTORM}
. ${CONFIG} # grab all static config info. 
#
# generate XDMF xml files 
for file in `ls *.nc`; do 
   ${OUTPUTDIR}/generateXDMF.x --datafile $file 2>> $SYSLOG
done
#
# make the directory where the results will be posted
if [[ $TROPICALCYCLONE = on ]]; then
   STORMCLASSNAME=`cat nhcClassName`
   # find the space between the storm class (TD, TS, HU, etc) and the NHC name
   ind=`expr index "$STORMCLASSNAME" ' '`
   # just use the storm's name 
   STORMNAME=${STORMCLASSNAME:$ind}
fi
STORMNAMEPATH=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
#
# make opendap directory
OPENDAPDIR=/projects/ncfs/opendap/data/tc/$STORMNAMEPATH/$ADVISORY/$GRIDNAME/$HOSTNAME/asgs$INSTANCENAME/$ENSTORM
mkdir -p $OPENDAPDIR
cd $OPENDAPDIR
ln -s ${ADVISDIR}/${ENSTORM}/fort.14 .
ln -s ${ADVISDIR}/${ENSTORM}/fort.15 .
ln -s ${ADVISDIR}/${ENSTORM}/fort.22 .
ln -s ${ADVISDIR}/${ENSTORM}/fort.6?.nc .
ln -s ${ADVISDIR}/${ENSTORM}/fort.7?.nc .
ln -s ${ADVISDIR}/${ENSTORM}/max*.nc .
ln -s ${ADVISDIR}/${ENSTORM}/min*.nc .
ln -s ${ADVISDIR}/${ENSTORM}/run.properties .
ln -s ${ADVISDIR}/${ENSTORM}/*.xmf .
