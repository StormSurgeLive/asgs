#!/bin/bash
#
# Copyright(C) 2011 Jason Fleming
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
# convert files to netcdf and copy to directory where they can be 
# published via opendap
perl ${OUTPUTDIR}/asgsConvertToNETCDF.pl --ppdir ${SCRIPTDIR}/output 
#
# create Google Earth images of water surface elevation and significant
# wave height
${OUTPUTDIR}/asgsCreateKMZs.sh -c ${OUTPUTDIR}/setupPostProcessGraphics.sh > graphics.log 2>&1
#
# copy the Google Earth images to a directory where they can be 
# published via opendap
OPENDAPDIR=`cat opendappath.log`;
NCFS_CURRENT_DIR=/projects/ncfs/opendap/data/NCFS_CURRENT
cp graphics/*.kmz ${OPENDAPDIR} >> graphics.log 2>&1 
rm ${NCFS_CURRENT_DIR}/*.kmz >> graphics.log 2>&1
cp graphics/*.kmz ${NCFS_CURRENT_DIR} >> graphics.log 2>&1
#
# Copy the run.properties file to a consistent location (an external web 
# server in this case) with a path set according to the name of the ASGS
# instance that created the results.
cp run.properties /projects/ncfs/opendap/data/NCFS_CURRENT/run.properties.${HOSTNAME}.${INSTANCENAME} 2>> ${SYSLOG}
#
# jgf20110729: There seem to be issues ith ssh key permissions on blueridge so 
# I commented this out.
ssh ${WEBHOST} -l ${WEBUSER} "mkdir -p ${WEBPATH}/${INSTANCENAME}"
scp run.properties ${WEBUSER}@${WEBHOST}:${WEBPATH}/${INSTANCENAME}/run.properties.${HOSTNAME}.${INSTANCENAME}
ssh ${WEBHOST} -l ${WEBUSER} "chmod -R 755 ${WEBPATH}/${INSTANCENAME}"
