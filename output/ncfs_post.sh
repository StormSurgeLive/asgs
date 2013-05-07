#!/bin/bash
#
# Copyright(C) 2011, 2012 Jason Fleming
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
# grab all config info
si=-1
. ${CONFIG}
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
cd ${ADVISDIR}/${ENSTORM}
#
# write the target area to the run.properties file for the CERA
# web app
echo "asgs : nc" >> run.properties
#
# write the intended audience to the run.properties file for CERA
echo "intendedAudience : general" >> run.properties
#
# generate XDMF xml files 
#for file in `ls *.nc`; do
#   ${OUTPUTDIR}/generateXDMF.x --datafile $file 2>> $SYSLOG
#done
#
# create Google Earth images of water surface elevation and significant
# wave height
#${OUTPUTDIR}/asgsCreateKMZs.sh -c ${OUTPUTDIR}/setupPostProcessGraphics.sh > graphics.log 2>&1
#
# copy the Google Earth images to a directory where they can be 
# published via opendap
#OPENDAPDIR=`cat opendappath.log`;
#NCFS_CURRENT_DIR=/projects/ncfs/opendap/data/NCFS_CURRENT
#cp graphics/*.kmz ${OPENDAPDIR} >> graphics.log 2>&1 
#rm ${NCFS_CURRENT_DIR}/*.kmz >> graphics.log 2>&1
#cp graphics/*.kmz ${NCFS_CURRENT_DIR} >> graphics.log 2>&1
#
# construct the opendap directory path where the results will be posted
#
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
STORMNAMEPATH=null
DOWNLOADPREFIX="http://opendap.renci.org:1935/thredds/fileServer"
CATALOGPREFIX="http://opendap.renci.org:1935/thredds/catalog"
if [[ $BACKGROUNDMET = on ]]; then
   # for NAM, the "advisory number" is actually the cycle time 
   STORMNAMEPATH=tc/nam
fi
if [[ $TROPICALCYCLONE = on ]]; then
   STORMNAME=`grep "stormname" ${STORMDIR}/run.properties | sed 's/stormname.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
   STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
   STORMNAMEPATH=tc/$STORMNAMELC
fi
OPENDAPSUFFIX=$ADVISORY/$GRIDNAME/$HOSTNAME/$INSTANCENAME/$ENSTORM
# put the opendap download url in the run.properties file for CERA to find
downloadURL=$DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX
echo "downloadurl : $downloadURL" >> run.properties
# now actually make the directory (OPENDAPBASEDIR is specified in CONFIG)
OPENDAPDIR=$OPENDAPBASEDIR/$STORMNAMEPATH/$OPENDAPSUFFIX
mkdir -p $OPENDAPDIR 2>> ${SYSLOG}
# make symbolic links from the opendap dir to the important files for the run
cd $OPENDAPDIR 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.14 .  2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.15 . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.*.nc . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/swan*.nc . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/max*.nc . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/min*.nc . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/run.properties . 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/*.xmf . 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/*.kmz . 2>> ${SYSLOG}
for file in fort.13 fort.22 fort.26 fort.221 fort.222 ; do 
   if [ -e ${ADVISDIR}/${ENSTORM}/$file ]; then
      ln -s ${ADVISDIR}/${ENSTORM}/$file . 2>> ${SYSLOG}
   fi
done
for file in al*.fst bal*.dat ; do 
   if [ -e ${ADVISDIR}/$file ]; then
      ln -s ${ADVISDIR}/$file . 2>> ${SYSLOG}
   fi
done
#
# Copy the latest run.properties file to a consistent location in opendap
cp run.properties /projects/ncfs/opendap/data/NCFS_CURRENT/run.properties.${HOSTNAME}.${INSTANCENAME} 2>> ${SYSLOG}
#
# send an email to CERA web application to notify it that results are ready
COMMA_SEP_LIST="jason.fleming@seahorsecoastal.com,nc.cera.renci2@gmail.com"
runStartTime=`grep RunStartTime run.properties | sed 's/RunStartTime.*://' | sed 's/\s//g'`
subject="ADCIRC NCFS POSTED for $runStartTime"
if [[ $TROPICALCYCLONE = on ]]; then
   subject=${subject}" (TROPICAL CYCLONE)"
fi
cat <<END > ${STORMDIR}/cera_results_notify.txt 

The ADCIRC NCFS solutions for $ADVISORY have been posted to $CATALOGPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX

The run.properties file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
   
or wget the file with the following command

wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
END
#
echo "INFO: ncfs_post.sh: Sending 'results available' email to the following addresses: $COMMA_SEP_LIST." >> $SYSLOG
cat ${STORMDIR}/cera_results_notify.txt | mail -s "$subject" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
