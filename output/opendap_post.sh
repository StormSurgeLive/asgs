#!/bin/bash
#------------------------------------------------------------------------
# opendap_post.sh : Makes results available to thredds data server.
#------------------------------------------------------------------------
# Copyright(C) 2015 Jason Fleming
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
CONFIG=$1
ADVISDIR=$2
ADVISORY=$3
HOSTNAME=$4
ENSTORM=$5
HSTIME=$6
SYSLOG=$7
TDS=$8
FILES=($9) # array of files to post to opendap
OPENDAPNOTIFY=${10}
#
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR}
# get the forecast ensemble member number for use in 
# picking up any bespoke configuration for this ensemble
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
#  O P E N  D A P    P U B L I C A T I O N 
#
STORMNAMEPATH=null
#
# pick up config of the thredds data server where the files are to be posted
logMessage "Setting opendap server parameters with env_dispatch ${TDS}."
env_dispatch $TDS   # from platforms.sh
#
# form path to results on tds based on type of forcing or name of storm
if [[ $BACKGROUNDMET != off ]]; then
   # for NAM, the "advisory number" is actually the cycle time 
   STORMNAMEPATH=tc/nam
fi
if [[ $TROPICALCYCLONE = on ]]; then
   STORMNAME=`grep -m 1 "stormname" ${STORMDIR}/run.properties | sed 's/stormname.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
   STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
   STORMNAMEPATH=tc/$STORMNAMELC
fi
OPENDAPSUFFIX=$ADVISORY/$GRIDNAME/$HOSTNAME/$INSTANCENAME/$ENSTORM
#
# Create full path to results for server file sytem. 
# OPENDAPBASEDIR is specified in platforms.sh.
OPENDAPDIR=$OPENDAPBASEDIR/$STORMNAMEPATH/$OPENDAPSUFFIX
# create the opendap download url for the run.properties file 
downloadURL=$DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX
#
# Determine whether to create symbolic links or use scp by looking at the
# list of HPC machines that share a common filesystem with this TDS. 
OPENDAPPOSTMETHOD=scp
for hpc in ${LINKABLEHOSTS[*]}; do 
   if [[ $hpc = $TARGET ]]; then
      OPENDAPPOSTMETHOD=link
   fi
done
#
# If the HPC and TDS do not share a common filesystem, use scp with 
# public key authentication to copy the files to the server hosting THREDDS.
if [[ $OPENDAPPOSTMETHOD = scp ]]; then
   logMessage "Transferring files to $OPENDAPDIR on $OPENDAPHOST as user $OPENDAPUSER."
   ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "mkdir -p $OPENDAPDIR" 2>> $SYSLOG
   for file in ${FILES[*]}; do 
      chmod +r $file 2>> $SYSLOG
      logMessage "Transferring $file."
      scp -P $SSHPORT $file ${OPENDAPUSER}@${OPENDAPHOST}:${OPENDAPDIR} 2>> $SYSLOG
      ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "chmod +r $OPENDAPDIR/$file"
      # We must add this new property to the run.properties after copying it
      # to the remote server so we don't contaminate the original
      # run.properties with this downloadurl property.
      ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "echo downloadurl : $downloadURL >> $OPENDAPDIR/run.properties"
   done
else
   #
   # if the HPC and TDS do share a common filesystem, create symbolic links
   # to the actual results files in a place where TDS can find them
   cd $OPENDAPDIR 2>> ${SYSLOG}
   for file in ${FILES[*]}; do 
      chmod +r $file 2>> $SYSLOG
      # We must copy the run.properties so we don't contaminate the
      # original run.properties with this downloadurl property.
      if [[ $file = run.properties ]]; then
         logMessage "Copying $file."
         cp ${ADVISDIR}/${ENSTORM}/$file . 2>> ${SYSLOG}
         echo downloadurl : $downloadurl >> $file 2>> ${SYSLOG}
      else
         logMessage "Symbolically linking to $file."
         ln -s ${ADVISDIR}/${ENSTORM}/$file . 2>> ${SYSLOG}
      fi
   done
fi
#
runStartTime=`grep RunStartTime run.properties | sed 's/RunStartTime.*://' | sed 's/\s//g'`
subject="ADCIRC POSTED for $runStartTime"
if [[ $TROPICALCYCLONE = on ]]; then
   subject=${subject}" (TC)"
fi
subject="${subject} $HOSTNAME.$INSTANCENAME $ENMEMNUM"
cat <<END > ${STORMDIR}/opendap_results_notify.txt 

The results for cycle $ADVISORY have been posted to $CATALOGPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX

The run.properties file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
   
or wget the file with the following command

wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
END
echo "INFO: queenbee_daily_post.sh: Sending 'results available' email to the following addresses: $OPENDAPNOTIFY."
cat ${STORMDIR}/opendap_results_notify.txt | mail -s "$subject" $OPENDAPNOTIFY 2>> ${SYSLOG} 2>&1
