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
echo "asgs : nc" >> run.properties
#
# generate XDMF xml files 
for file in `ls *.nc`; do 
   ${OUTPUTDIR}/generateXDMF.x --datafile $file 2>> $SYSLOG
done
#
# make the directory where the results will be posted
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
STORMNAME=null
if [[ $BACKGROUNDMET = on ]]; then
   # the NAM cycle time is the last two digits of the "advisory"
   namcyclehour=${ADVISORY:8:2}
   STORMNAME="NAM${namcyclehour}Z"
fi
if [[ $TROPICALCYCLONE = on ]]; then
   STORMNAME=`grep "storm name" ${STORMDIR}/run.properties | sed 's/storm name.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
fi
STORMNAMEPATH=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
#
# make opendap directory
OPENDAPDIR=/projects/ncfs/opendap/data/tc/$STORMNAMEPATH/$ADVISORY/$GRIDNAME/$HOSTNAME/$INSTANCENAME/$ENSTORM
mkdir -p $OPENDAPDIR 2>> ${SYSLOG}
cd $OPENDAPDIR 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.14 .  2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.15 . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.22 . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.*.nc . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/max*.nc . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/min*.nc . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/run.properties . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/*.xmf . 2>> ${SYSLOG}
#
# OpenDAP publication for CERA
#
# make symbolic links to directory structure that CERA web app uses
#$openDAPDirectory = "$OPENDAPBASEDIR/blueridge.renci.org:2/$model/$ADCIRCgrid/$windtag/$year/$mon/$mday/$cycle"
# RunStartTime : 2012080812
cd $STORMDIR 2>> ${SYSLOG}
OPENDAPBASEDIR=/projects/ncfs/opendap/data
if [[ $ENSTORM = nhcConsensus || $ENSTORM = namforecast ]]; then
   runStartTime=`grep RunStartTime run.properties | sed 's/RunStartTime.*://' | sed 's/\s//g'`
   year=${runStartTime:0:4}
   month=${runStartTime:4:2}
   day=${runStartTime:6:2}
   currentCycle=`grep currentcycle run.properties | sed 's/currentcycle.*://' | sed 's/\s//g'`
   model=`grep ^Model run.properties | sed 's/Model.*://' | sed 's/\s//g'`
   mesh=`grep mesh run.properties | sed 's/mesh.*://' | sed 's/\s//g'`
   windtag=`grep WindModel run.properties | sed 's/WindModel.*://' | sed 's/\s//g'`
   # CERA web app is hard coded to look for blueridge.renci.org:2
   openDapDirectory=$OPENDAPBASEDIR/blueridge.renci.org:2/$model/$mesh/$windtag/$year/$month/$day/$currentCycle
   mkdir -p $openDapDirectory 2>> $SYSLOG
   cd $openDapDirectory 2>> ${SYSLOG}
   ln -s ${ADVISDIR}/${ENSTORM}/fort.14 . 2>> ${SYSLOG}
   ln -s ${ADVISDIR}/${ENSTORM}/fort.15 . 2>> ${SYSLOG}
   ln -s ${ADVISDIR}/${ENSTORM}/fort.22 . 2>> ${SYSLOG}
   ln -s ${ADVISDIR}/${ENSTORM}/*.nc . 2>> ${SYSLOG}  
   ln -s ${ADVISDIR}/al*.fst . 2>> ${SYSLOG}  
   ln -s ${ADVISDIR}/bal*.dat . 2>> ${SYSLOG}
   ln -s ${ADVISDIR}/${ENSTORM}/run.properties . 2>> ${SYSLOG}
   #
   COMMA_SEP_LIST="jason.fleming@seahorsecoastal.com,nc.cera.renci2@gmail.com"
   subject="ADCIRC NCFS POSTED for $runStartTime"
   if [[ $TROPICALCYCLONE = on ]]; then
      subject=${subject}" (TROPICAL CYCLONE)"
   fi
   openDAPPrefix="http://opendap.renci.org:1935/thredds/catalog/blueridge.renci.org:2/$model/$mesh"
   httpPathName="http://opendap.renci.org:1935/thredds/fileServer/blueridge.renci.org:2/$model/$mesh"
   path_suffix="$windtag/$year/$month/$day/$currentCycle"
   cat <<END > ${STORMDIR}/cera_results_notify.txt 

The ADCIRC NCFS solutions for $runStartDate have been posted to $openDAPPrefix/$path_suffix

The run.properties file is : $httpPathName/$path_suffix/run.properties
   
or wget the file with the following command

wget  $httpPathName/$path_suffix/run.properties
END
#
   echo "INFO: renci_ensemble_opendap_post.sh: Sending 'results available' email to the following addresses: $COMMA_SEP_LIST."
   cat ${STORMDIR}/cera_results_notify.txt | mail -s "$subject" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
fi
