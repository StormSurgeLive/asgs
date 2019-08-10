#!/bin/bash
#------------------------------------------------------------------------
# opendap_post.sh : Makes results available to thredds data server.
#------------------------------------------------------------------------
# Copyright(C) 2015--2019 Jason Fleming
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
HPCENV=$4
ENSTORM=$5
HSTIME=$6
SYSLOG=$7
SERVER=$8
FILES=("$9") # array of files to post to opendap
#
declare -A properties
# get loadProperties function   
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' run.properties 2>>$SYSLOG`   
source $SCRIPTDIR/properties.sh

#
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR}
# load run.properties file into associative array
loadProperties $STORMDIR/run.properties  
# get the forecast ensemble member number for use in 
# picking up any bespoke configuration for this ensemble
# member in the configuration files
ENMEMNUM=${properties["forecastEnsembleMemberNumber"]}
si=$ENMEMNUM
# load asgs operator email address
ASGSADMIN=${properties["notification.email.asgsadmin"]}
#
## grab all config info
. ${CONFIG} 
# Bring in logging functions
. ${SCRIPTDIR}/monitoring/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# pick up config of the thredds data server where the files are to be posted
logMessage "Setting opendap server parameters with env_dispatch ${SERVER}."
env_dispatch $SERVER   # from platforms.sh
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
THIS="opendap_post.sh-->$SERVER"
#--------------------------------------------------------------------
#  O P E N  D A P    P A T H   F O R M A T I O N
#--------------------------------------------------------------------
STORMNAMEPATH=null
#
# form path to results on tds based on type of forcing or name of storm
if [[ $BACKGROUNDMET != off ]]; then
   # for NAM, the "advisory number" is actually the cycle time 
   YEAR=${properties["forcing.nwp.year"]}
   NWPMODEL=${properties["forcing.nwp.model"]}
   STORMNAMEPATH=$YEAR/$NWPMODEL
fi
if [[ $TROPICALCYCLONE = on ]]; then
   YEAR=${properties["forcing.tropicalcyclone.year"]}
   if [[ $YEAR = "" ]]; then 
      # 2014stable version of properties
      YEAR=${properties["config.forcing.tropicalcyclone.year"]}
   fi
   STORMNAME=${properties["forcing.tropicalcyclone.stormname"]}
   if [[ $STORMNAME = "" ]]; then
      # 2014stable version of properties
      STORMNAME=${properties["config.forcing.tropicalcyclone.stormname"]}
   fi
   STORMNUMBER=${properties["forcing.tropicalcyclone.stormnumber"]}
   if [[ $STORMNUMBER = "" ]]; then
      STORMNUMBER=${properties["config.forcing.tropicalcyclone.stormnumber"]}
   fi
   STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
   basin="al" # FIXME: write/read a property instead of hardcoding the atlantic basin
   STORMNAMEPATH=$YEAR/$basin$STORMNUMBER
   ALTSTORMNAMEPATH=$YEAR/$STORMNAMELC  # symbolic link with name
fi
OPENDAPSUFFIX=$ADVISORY/$GRIDNAME/$HPCENV/$INSTANCENAME/$ENSTORM
echo $OPENDAPSUFFIX

#
# Create full path to results for server file sytem. 
# OPENDAPBASEDIR is specified in platforms.sh.
OPENDAPDIR=$OPENDAPBASEDIR/$STORMNAMEPATH/$OPENDAPSUFFIX
# create the opendap download url for the run.properties file 
downloadURL=$DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX
# add downloadurl or downloadurl_backup property to run.properties file
if [[ ! `grep downloadurl run.properties` =~ downloadurl ]]; then
   echo "downloadurl : $downloadURL" >> run.properties 2>> ${SYSLOG}
else
   backupNum=`grep downloadurl run.properties | wc -l`
   echo "downloadurl_backup$backupNum : $downloadURL" >> run.properties 2>> ${SYSLOG}
fi      
#-----------------------------------------------------------------------
#           D E T E R M I N E   M E T H O D
#-----------------------------------------------------------------------
# Establish the default method of posting results for service via opendap
# The actual method will depend on the configuration in platforms.sh.
OPENDAPPOSTMETHOD=scp
#
# mvb20190620: Testing rsync with the LSU CCR thredds server
if [[ $SERVER = "lsu_ccr_tds" ]]; then
    OPENDAPPOSTMETHOD=rsync
fi
#
# Determine whether to copy files instead of using scp by looking at the
# list of HPC machines that share a common filesystem with this TDS. 
for hpc in ${COPYABLEHOSTS[*]}; do 
   if [[ $hpc = $TARGET ]]; then
      OPENDAPPOSTMETHOD=copy
   fi
done
#
# Determine whether to create symbolic links by looking at the
# list of HPC machines that share a common filesystem with this TDS. 
# This comes last, so it takes precedence above the others if multiple
# file delivery mechanisms are possible. 
for hpc in ${LINKABLEHOSTS[*]}; do 
   if [[ $hpc = $TARGET ]]; then
      OPENDAPPOSTMETHOD=link
   fi
done
#
# If the HPC and TDS do not share a common filesystem, use scp with 
# public key authentication to copy the files to the server hosting THREDDS.
#
# jgf20160317: Added status check for posting to opendap so that if 
# there is a failure, the Operator is notified rather than downstream
# data consumers.
threddsPostStatus=ok
#
#-------------------------------------------------------------------
#     C R E A T E    N O T I F I C A T I O N   E M A I L
#-------------------------------------------------------------------
# @jasonfleming: Hack in the ability to send the notification email 
# before all the files have been posted. 
opendapEmailSent=no
#
runStartTime=${properties["RunStartTime"]}
subject="ADCIRC POSTED for $runStartTime"
if [[ $TROPICALCYCLONE = on ]]; then
   subject=${subject}" (TC)"
fi
#Click on the link: 
#
#$CATALOGPREFIX/$STORMNAMEPATH/${OPENDAPSUFFIX}/catalog.html
subject="${subject} $ENMEMNUM $HPCENV.$INSTANCENAME $ASGSADMIN"
cat <<END > ${STORMDIR}/opendap_results_notify_${SERVER}.txt 

The results for cycle $ADVISORY have been posted to $CATALOGPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/catalog.html

The run.properties file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
   
or wget the file with the following command

wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
END

#
#-------------------------------------------------------------------
#                P O S T   V I A   S C P
#-------------------------------------------------------------------
# jgf20160803: Changed if/then to case-switch to accommodate new "copy" method.
logMessage "$ENSTORM: $THIS: Posting to $OPENDAPHOST using the '$OPENDAPPOSTMETHOD' method."
case $OPENDAPPOSTMETHOD in
"scp")
   serverAliveInterval=10
   timeoutRetryLimit=3
   sshOptions="$OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT -o ServerAliveInterval=$serverAliveInterval -o StrictHostKeyChecking=no -o ConnectTimeout=60"
   scpOptions="-P $SSHPORT -o ServerAliveInterval=$serverAliveInterval -o StrictHostKeyChecking=no -o ConnectTimeout=60"
   logMessage "$ENSTORM: $THIS: Transferring files to $OPENDAPDIR on $OPENDAPHOST as user $OPENDAPUSER."
   retry=0
   while [[ $retry -lt $timeoutRetryLimit ]]; do 
      ssh $sshOptions "mkdir -p $OPENDAPDIR" 2>> $SYSLOG
      if [[ $? != 0 ]]; then
         warn "$ENSTORM: $THIS: Failed to create the directory $OPENDAPDIR on the remote machine ${OPENDAPHOST}."
         threddsPostStatus=fail
      else
         logMessage "$ENSTORM: $THIS: Successfully created the directory $OPENDAPDIR on the remote machine ${OPENDAPHOST}."
         break
      fi
      retry=`expr $retry + 1`
      if [[ $retry -lt $timeoutRetryLimit ]]; then
         logMessage "$ENSTORM: $THIS: Trying again."
      else
         logMessage "$ENSTORM: $THIS: Maximum number of retries has been reached. Moving on to the next operation."
      fi
   done
   # add code to create write permissions on directories so that other 
   # Operators can post results to the same directories
   #OPENDAPSUFFIX=$ADVISORY/$GRIDNAME/$HPCENV/$INSTANCENAME/$ENSTORM
   partialPath=$OPENDAPDIR
   while [[ $partialPath != $OPENDAPBASEDIR  ]]; do 
      retry=0
      while [[ $retry -lt $timeoutRetryLimit ]]; do 
         ssh $sshOptions "chmod a+wx $partialPath" 2>> $SYSLOG
         if [[ $? != 0 ]]; then
            warn "$ENSTORM: $THIS: Failed to change permissions on the directory $partialPath on the remote machine ${OPENDAPHOST}."
            threddsPostStatus=fail
         else
            logMessage "$ENSTORM: $THIS: Successfully changed permissions."
            break
         fi
         retry=`expr $retry + 1`
         if [[ $retry -lt $timeoutRetryLimit ]]; then
            logMessage "$ENSTORM: $THIS: Trying again."
         else
            logMessage "$ENSTORM: $THIS: Maximum number of retries has been reached. Moving on to the next operation."
         fi
      done
      # cut off the end of the partial path and keep going until we get down
      # to OPENDAPBASEDIR
      partialPath=`dirname $partialPath` 
   done
   #
   # add a symbolic link for the storm name if this is tropicalcyclone forcing
   if [[ $TOPICALCYCLONE != off ]]; then 
      retry=0
      while [[ $retry -lt $timeoutRetryLimit ]]; do 
         ssh $sshOptions "ln -s $OPENDAPBASEDIR/$STORMNAMEPATH $OPENDAPBASEDIR/$ALTSTORMNAMEPATH" 2>> $SYSLOG
         if [[ $? != 0 ]]; then
            warn "$ENSTORM: $THIS: Failed to create symbolic link for the storm name."
            threddsPostStatus=fail
         else
            logMessage "$ENSTORM: $THIS: Successfully created symbolic link to storm name."
            break
         fi
         retry=`expr $retry + 1`
         if [[ $retry -lt $timeoutRetryLimit ]]; then
            logMessage "$ENSTORM: $THIS: Trying again."
         else
            logMessage "$ENSTORM: $THIS: Maximum number of retries has been reached. Moving on to the next operation."
         fi
      done
   fi
   for file in ${FILES[*]}; do 
      # send opendap posting notification email early if directed
      if [[ $file = "sendNotification" ]]; then
         logMessage "$ENSTORM: $THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
         # use asgs sendmail if Operator has set it up 
         if [[ -e $HOME/asgs-global.conf ]]; then
            $SCRIPTDIR/asgs-sendmail.pl --subject "$subject" --to "$OPENDAPNOTIFY" < ${STORMDIR}/opendap_results_notify_${SERVER}.txt 2>> ${SYSLOG} 2>&1
         else
            cat ${STORMDIR}/opendap_results_notify_${SERVER}.txt | mail  -S "replyto=$ASGSADMIN" -s "$subject" $OPENDAPNOTIFY 2>> ${SYSLOG} 2>&1
         fi
         opendapEmailSent=yes
         continue        
      fi
      chmod +r $file 2>> $SYSLOG
      logMessage "$ENSTORM: $THIS: Transferring $file to ${OPENDAPHOST}."
      retry=0
      while [[ $retry -lt $timeoutRetryLimit ]]; do 
         scp $scpOptions $file ${OPENDAPUSER}@${OPENDAPHOST}:${OPENDAPDIR} 2>> $SYSLOG  2>&1   
         if [[ $? != 0 ]]; then
            threddsPostStatus=fail
            warn "$ENSTORM: $THIS: Failed to transfer the file $file to ${OPENDAPHOST}:${OPENDAPDIR}."
         else
            logMessage "$ENSTORM: $THIS: Successfully transferred the file."
            break
         fi
         retry=`expr $retry + 1`
         if [[ $retry -lt $timeoutRetryLimit ]]; then
            logMessage "$ENSTORM: $THIS: Trying again."
         else
            logMessage "$ENSTORM: $THIS: Maximum number of retries has been reached. Moving on to the next operation."
         fi
      done
      # give the file read permissions
      fname=`basename $file` 
      retry=0
      while [[ $retry -lt $timeoutRetryLimit ]]; do 
         ssh $sshOptions "chmod +r $OPENDAPDIR/$fname"
         if [[ $? != 0 ]]; then
            threddsPostStatus=fail
            warn "$ENSTORM: $THIS: Failed to give the file $fname read permissions in ${OPENDAPHOST}:${OPENDAPDIR}."
         else
            logMessage "$ENSTORM: $THIS: Successfully changed permissions."
            break
         fi
         retry=`expr $retry + 1`
         if [[ $retry -lt $timeoutRetryLimit ]]; then
            logMessage "$ENSTORM: $THIS: Trying again."
         else
            logMessage "$ENSTORM: $THIS: Maximum number of retries has been reached. Moving on to the next operation."
         fi
      done
   done
   ;;
#-------------------------------------------------------------------
#                P O S T   V I A   R S Y N C
#-------------------------------------------------------------------
# mvb20190618: Added to support time-out issues with general scp transfers
"rsync")
   rsyncSSHOptions=(--rsh="ssh -p $SSHPORT")
   rsyncOptions="-z --copy-links"
   sshOptions="$OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT"
   logMessage "$ENSTORM: $THIS: Transferring files to $OPENDAPDIR on $OPENDAPHOST as user $OPENDAPUSER."

   ssh $sshOptions "mkdir -p $OPENDAPDIR" 2>> $SYSLOG
   if [[ $? != 0 ]]; then
      warn "$ENSTORM: $THIS: Failed to create the directory $OPENDAPDIR on the remote machine ${OPENDAPHOST}."
      threddsPostStatus=fail
   fi
   # add code to create write permissions on directories so that other 
   # Operators can post results to the same directories
   partialPath=$OPENDAPDIR
   while [[ $partialPath != $OPENDAPBASEDIR  ]]; do 
      retry=0
      while [[ $retry -lt $timeoutRetryLimit ]]; do 
         ssh $sshOptions "chmod a+wx $partialPath" 2>> $SYSLOG
         if [[ $? != 0 ]]; then
            warn "$ENSTORM: $THIS: Failed to change permissions on the directory $partialPath on the remote machine ${OPENDAPHOST}."
            threddsPostStatus=fail
         else
            logMessage "$ENSTORM: $THIS: Successfully changed permissions."
            break
         fi
         retry=`expr $retry + 1`
         if [[ $retry -lt $timeoutRetryLimit ]]; then
            logMessage "$ENSTORM: $THIS: Trying again."
         else
            logMessage "$ENSTORM: $THIS: Maximum number of retries has been reached. Moving on to the next operation."
         fi
      done
      # cut off the end of the partial path and keep going until we get down
      # to OPENDAPBASEDIR
      partialPath=`dirname $partialPath` 
   done
   for file in ${FILES[*]}; do 
      # send opendap posting notification email early if directed
      if [[ $file = "sendNotification" ]]; then
         logMessage "$ENSTORM: $THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
         # use asgs sendmail if Operator has set it up 
         if [[ -e $HOME/asgs-global.conf ]]; then
            $SCRIPTDIR/asgs-sendmail.pl --subject "$subject" --to "$OPENDAPNOTIFY" < ${STORMDIR}/opendap_results_notify_${SERVER}.txt 2>> ${SYSLOG} 2>&1
         else
            cat ${STORMDIR}/opendap_results_notify_${SERVER}.txt | mail  -S "replyto=$ASGSADMIN" -s "$subject" $OPENDAPNOTIFY 2>> ${SYSLOG} 2>&1
         fi
         opendapEmailSent=yes
         continue        
      fi
      chmod +r $file 2>> $SYSLOG
      logMessage "$ENSTORM: $THIS: Transferring $file to ${OPENDAPHOST}."
      rsync "${rsyncSSHOptions}" ${rsyncOptions}  ${file} ${OPENDAPUSER}@${OPENDAPHOST}:${OPENDAPDIR} 2>> $SYSLOG 2>&1
      if [[ $? != 0 ]]; then
         threddsPostStatus=fail
         warn "$ENSTORM: $THIS: Failed to transfer the file $file to ${OPENDAPHOST}:${OPENDAPDIR}."
      fi
   done
   ;;
#-------------------------------------------------------------------
#        P O S T   V I A   S Y M B O L I C   L I N K 
#           O R   F I L E S Y S T E M   C O P Y
#-------------------------------------------------------------------
"link"|"copy")
   #
   # jgf20160803: link and copy are almost the same procedure and only differ
   # in the command used to post the data files
   postCMD='ln -s'
   postDesc='Symbolically linking'
   if [[ $OPENDAPPOSTMETHOD = "copy" ]]; then
      postCMD='cp'
      postDesc='Copying'
   fi
   #
   # if the HPC and TDS do share a common filesystem, create symbolic links
   # to the actual results files in a place where TDS can find them
   mkdir -p $OPENDAPDIR 2>> $SYSLOG
   # add code to create write permissions on directories so that other 
   # Operators can post results to the same directories
   partialPath=$OPENDAPDIR
   while [[ $partialPath != $OPENDAPBASEDIR  ]]; do 
      retry=0
      while [[ $retry -lt $timeoutRetryLimit ]]; do 
         chmod a+wx $partialPath 2>> $SYSLOG
         if [[ $? != 0 ]]; then
            warn "$ENSTORM: $THIS: Failed to change permissions on the directory ${partialPath}."
            threddsPostStatus=fail
         else
            logMessage "$ENSTORM: $THIS: Successfully changed permissions."
            break
         fi
         retry=`expr $retry + 1`
         if [[ $retry -lt $timeoutRetryLimit ]]; then
            logMessage "$ENSTORM: $THIS: Trying again."
         else
            logMessage "$ENSTORM: $THIS: Maximum number of retries has been reached. Moving on to the next operation."
         fi
      done
      # cut off the end of the partial path and keep going until we get down
      # to OPENDAPBASEDIR
      partialPath=`dirname $partialPath` 
   done
   #
   # create link with storm name instead of storm number
   if [[ $TROPICALCYCLONE != off ]]; then
      ln -s $OPENDAPBASEDIR/$STORMNAMEPATH $OPENDAPBASEDIR/$ALTSTORMNAMEPATH 2>> $SYSLOG
   fi
   for file in ${FILES[*]}; do 
      # send opendap posting notification email early if directed
      if [[ $file = "sendNotification" ]]; then
         logMessage "$ENSTORM: $THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
         # use asgs sendmail if Operator has set it up 
         if [[ -e $HOME/asgs-global.conf ]]; then
            $SCRIPTDIR/asgs-sendmail.pl --subject "$subject" --to "$OPENDAPNOTIFY" < ${STORMDIR}/opendap_results_notify_${SERVER}.txt 2>> ${SYSLOG} 2>&1
         else
            cat ${STORMDIR}/opendap_results_notify_${SERVER}.txt | mail  -S "replyto=$ASGSADMIN" -s "$subject" $OPENDAPNOTIFY 2>> ${SYSLOG} 2>&1
         fi
         opendapEmailSent=yes
         continue        
      fi
      chmod +r $file 2>> $SYSLOG
      logMessage "$ENSTORM: $THIS: $postDesc $file."
      $postCMD $file $OPENDAPDIR 2>> ${SYSLOG}
      if [[ $? != 0 ]]; then
         threddsPostStatus=fail
         warn "$ENSTORM: $THIS: $postDesc $file to ${OPENDAPDIR} failed."
      fi
   done
   ;;
*)
   threddsPostStatus=fail
   warn "$ENSTORM: $THIS: The opendap post method $OPENDAPPOSTMETHOD was not recognized."
   ;;
esac
#
#-------------------------------------------------------------------
#      S E N D   N O T I F I C A T I O N   E M A I L
#-------------------------------------------------------------------
# jgf20160322: FIXME: post to opendap even if there was an error so we can 
# see what the error is
#
#if [[ threddsPostStatus != ok ]]; then
#   error "opendap_post.sh: A failure occurred when the ASGS instance $INSTANCENAME attempted to post data to the THREDDS Data Server ${SERVER}. Downstream data consumers will not receive an email for these results. However, the opendap results notification will be sent to ${ASGSADMIN}."
#   cat ${STORMDIR}/opendap_results_notify.txt | mail  -S "replyto=$ASGSADMIN" -s "$subject" $ASGSADMIN 2>> ${SYSLOG} 2>&1
#else
if [[ $opendapEmailSent = "no" ]]; then 
   logMessage "$ENSTORM: $THIS: Sending 'results available' email to the following addresses: $OPENDAPNOTIFY."
   # use asgs sendmail if Operator has set it up 
   if [[ -e $HOME/asgs-global.conf ]]; then
      $SCRIPTDIR/asgs-sendmail.pl --subject "$subject" --to "$OPENDAPNOTIFY" < ${STORMDIR}/opendap_results_notify_${SERVER}.txt 2>> ${SYSLOG} 2>&1
   else
      cat ${STORMDIR}/opendap_results_notify_${SERVER}.txt | mail  -S "replyto=$ASGSADMIN" -s "$subject" $OPENDAPNOTIFY 2>> ${SYSLOG} 2>&1
   fi
fi
