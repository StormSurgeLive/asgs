#!/bin/bash
#-----------------------------------------------------------------------
# best_post.sh : Post processing for best track forecast/advisories.
#-----------------------------------------------------------------------
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
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR} 2>> ${SYSLOG}
# get the forecast ensemble member number 
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
# grab all config info
si=$ENMEMNUM
. ${CONFIG}
# Bring in logging functions
. ${SCRIPTDIR}/monitoring/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#--------------------------------------------------------------------------
#          O P E N D A P   P U B L I C A T I O N
#--------------------------------------------------------------------------
#
# construct the opendap directory path where the results will be posted
#
STORMNAMEPATH=null
DOWNLOADPREFIX="http://opendap.renci.org:1935/thredds/fileServer"
CATALOGPREFIX="http://opendap.renci.org:1935/thredds/catalog"
if [[ $BACKGROUNDMET = on ]]; then
   # for NAM, the "advisory number" is actually the cycle time 
   STORMNAMEPATH=tc/nam
fi
currentDir=NCFS_CURRENT_DAILY
if [[ $TROPICALCYCLONE = on ]]; then
   STORMNAME=`grep "stormname" ${STORMDIR}/run.properties | sed 's/stormname.*://' | sed 's/^\s//g' | tail -n 1` 2>> ${SYSLOG}
   STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
   STORMNAMEPATH=tc/$STORMNAMELC
   currentDir=NCFS_CURRENT_TROPICAL
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
for file in `ls ${STORMDIR}/*.nc ${STORMDIR}/run.properties`; do  
   if [ -e $file ]; then
      ln -s $file . 2>> ${SYSLOG}
   fi
done
#
# Link to input files to document how the run was performed.
ln -s ${ADVISDIR}/${ENSTORM}/fort.14 .  2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.15 . 2>> ${SYSLOG}
for file in fort.13 fort.22 fort.26 fort.221 fort.222 ; do 
   if [ -e ${ADVISDIR}/${ENSTORM}/$file ]; then
      ln -s ${ADVISDIR}/${ENSTORM}/$file . 2>> ${SYSLOG}
   fi
done
#
# Link to the tropical cyclone forecast/advisory and tc best track
# file, if available. 
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
COMMA_SEP_LIST="jason.g.fleming@gmail.com,nc.cera.renci2@gmail.com"
#COMMA_SEP_LIST="jason.fleming@seahorsecoastal.com"
runStartTime=`grep RunStartTime run.properties | sed 's/RunStartTime.*://' | sed 's/\s//g'`
subject="ADCIRC NCFS $runStartTime $HOSTNAME.$INSTANCENAME $ENMEMNUM"
if [[ $TROPICALCYCLONE = on ]]; then
   subject=${subject}" (TC)"
fi
#subject="${subject} $CERASERVER"
echo "INFO: ncfs_post.sh: The cera_results_notify.txt email subject line is '$subject'." >> ${SYSLOG}
cat <<END > ${STORMDIR}/cera_results_notify.txt 

The ADCIRC NCFS solutions for $ADVISORY have been posted to $CATALOGPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX

The run.properties file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
   
or wget the file with the following command

wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
END
#
echo "INFO: ncfs_post.sh: Sending 'results available' email to the following addresses: $COMMA_SEP_LIST." >> $SYSLOG
cat ${STORMDIR}/cera_results_notify.txt | asgs-sendmail --subject "$subject" --to "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
#
# now hack symbolic links to the next advisory
nextAdvisory=`expr $ADVISORY + 1`
nextAdvisoryStr=`printf '%02d' $nextAdvisory`
cd ~/asgs/2014stable/input/sample_advisories/2016
rm nowcast_offset.dat ; ln -s $nextAdvisoryStr.nowcast.surface.dat nowcast_offset.dat
rm forecast_offset.dat ; ln -s $nextAdvisoryStr.end.surface.dat forecast_offset.dat
rm bal142016.dat ; ln -s $nextAdvisoryStr.bal142016.dat bal142016.dat
rm al142016.fst ; ln -s $nextAdvisoryStr.al142016.fst al142016.fst
