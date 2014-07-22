#!/bin/bash
#------------------------------------------------------------------------
# Copyright(C) 2008--2013 Jason Fleming
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
#------------------------------------------------------------------------
#
#~/asgs/trunk/output/arete_post.sh ~/asgs/config/asgs_config_nam_arete_cprav03.sh /scratch/acd/asgs13334/2014043018 99 2014 2014043018 arete.cct.lsu.edu namforecast 2014032800 2916000.0000000000 cpra_2011_v03a_chk.14 ~/asgs/trunk/output ~/asgs/log/asgs-2014-Apr-29-T16:41:42.13334.log ~/.ssh/id_rsa.pub
#                e.g.:
CONFIG=$1      # ~/asgs/config/(name)
ADVISDIR=$2    # /scratch/acd/asgs###/date/namforecast
STORM=$3       # 99
YEAR=$4        # 2014
ADVISORY=$5    # date
HOSTNAME=$6    # arete.cct.lsu.edu
ENSTORM=$7     # namforecast
CSDATE=$8      # cold start date from CONFIG
HSTIME=$9      # see log file for hstime output e.g., 2592000.00000
GRIDFILE=${10} # cpra_v03something (from CONFIG)
OUTPUTDIR=${11} # ~/asgs/trunk/output
SYSLOG=${12}    # ~/asgs/log/asgs-log-******
SSHKEY=${13}    # ~/.ssh/id_rsa.pub
#
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR}
echo "STORMDIR is $STORMDIR"
# get the forecast ensemble member number for use in CERA load balancing
# as well as picking up any bespoke configuration for this ensemble
# member in the configuration files
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
si=$ENMEMNUM
#
# grab all config info
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
export PERL5LIB=${PERL5LIB}:${SCRIPTDIR}/PERL
export PATH=$PATH:$IMAGEMAGICKBINPATH # if ImageMagick is in nonstd location
#
# we expect the ASGS config file to tell us how many cera servers there
# are with CERASERVERNUM and assume they are consecutively named 
# cera1, cera2, etc. We alternate the forecast ensemble members evenly 
# among them
#CERASERVERNUM=`expr $ENMEMNUM % $NUMCERASERVERS + 1`
#CERASERVER=cera$CERASERVERNUM
#echo "ceraServer : $CERASERVER" >> run.properties
#
# write the intended audience to the run.properties file for CERA
#echo "intendedAudience : $INTENDEDAUDIENCE" >> run.properties
#
# write the target area to the run.properties file for the CERA
# web app
echo "asgs : ng" >> run.properties 2>> $SYSLOG
echo "enstorm : $ENSTORM" >> run.properties 2>> $SYSLOG
#
#  O P E N  D A P    P U B L I C A T I O N 
#
STORMNAMEPATH=null
DOWNLOADPREFIX="http://arete.cct.lsu.edu:8080/thredds/fileServer"
CATALOGPREFIX="http://arete.cct.lsu.edu:8080/thredds/catalog"
if [[ $BACKGROUNDMET = on ]]; then
   # for NAM, the "advisory number" is actually the cycle time 
   STORMNAMEPATH=daily/nam
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
#
#logMessage "Transferring files to $OPENDAPDIR on $OPENDAPHOST as user $OPENDAPUSER with the ssh key in $SSHKEY."
#ssh $OPENDAPHOST -l $OPENDAPUSER -i $SSHKEY "mkdir -p $OPENDAPDIR" 2>> $SYSLOG
#for file in `ls *.nc *.xmf ${ADVISDIR}/al*.fst ${ADVISDIR}/bal*.dat fort.15 fort.22 run.properties`; do 
#   chmod +r $file 2>> $SYSLOG
#   logMessage "Transferring $file."
#   scp -i $SSHKEY $file ${OPENDAPUSER}@${OPENDAPHOST}:${OPENDAPDIR} 2>> $SYSLOG
#   ssh $OPENDAPHOST -l $OPENDAPUSER -i $SSHKEY "chmod +r $OPENDAPDIR/$file"
#done
#

BACKUPDIR=/netapp/acd/$STORMNAMEPATH/$OPENDAPSUFFIX
mkdir -p $BACKUPDIR 2>> ${SYSLOG}
cd $BACKUPDIR 2>> ${SYSLOG}
cp ${ADVISDIR}/${ENSTORM}/fort.14 .  2>> ${SYSLOG}
cp ${ADVISDIR}/${ENSTORM}/fort.15 . 2>> ${SYSLOG}
cp ${ADVISDIR}/${ENSTORM}/fort.*.nc . 2>> ${SYSLOG}
#cp ${ADVISDIR}/${ENSTORM}/swan*.nc . 2>> ${SYSLOG}
cp ${ADVISDIR}/${ENSTORM}/max*.nc . 2>> ${SYSLOG}
cp ${ADVISDIR}/${ENSTORM}/min*.nc . 2>> ${SYSLOG}
cp ${ADVISDIR}/${ENSTORM}/run.properties . 2>> ${SYSLOG}
#cp ${ADVISDIR}/${ENSTORM}/*.xmf . 2>> ${SYSLOG}
#cp ${ADVISDIR}/${ENSTORM}/*.kmz . 2>> ${SYSLOG}
for file in fort.13 fort.22 fort.26 fort.221 fort.222 fort.61; do
   if [ -e ${ADVISDIR}/${ENSTORM}/$file ]; then
      cp ${ADVISDIR}/${ENSTORM}/$file . 2>> ${SYSLOG}
   fi
done
for file in al*.fst bal*.dat ; do
   if [ -e ${ADVISDIR}/$file ]; then
      cp ${ADVISDIR}/$file . 2>> ${SYSLOG}
   fi
done



mkdir -p $OPENDAPDIR 2>> ${SYSLOG}
# make symbolic links from the opendap dir to the important files for the run
cd $OPENDAPDIR 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.14 .  2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.15 . 2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.*.nc . 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/swan*.nc . 2>> ${SYSLOG}
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


COMMA_SEP_LIST="jason.fleming@seahorsecoastal.com,asgs.cera.lsu@gmail.com,zbyerly@gmail.com"
runStartTime=`grep RunStartTime ${STORMDIR}/run.properties | sed 's/RunStartTime.*://' | sed 's/\s//g'`
subject="ADCIRC NCFS POSTED for $runStartTime"
if [[ $TROPICALCYCLONE = on ]]; then
   subject=${subject}" (TROPICAL CYCLONE)"
fi
subject="${subject} $CERASERVER"
subject="${subject} $HOSTNAME.$INSTANCENAME $ENMEMNUM"
cat <<END > ${STORMDIR}/cera_results_notify.txt 

The ADCIRC NCFS solutions for $ADVISORY have been posted to $CATALOGPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX

The run.properties file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
   
or wget the file with the following command

wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
END
echo "INFO: corps_post.sh: Sending 'results available' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/cera_results_notify.txt | mail -s "$subject" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
