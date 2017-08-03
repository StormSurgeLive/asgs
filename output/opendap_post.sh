#!/bin/bash
#------------------------------------------------------------------------
# opendap_post.sh : Makes results available to thredds data server.
#------------------------------------------------------------------------
# Copyright(C) 2015--2016 Jason Fleming
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
SERVER=$8
FILES=($9) # array of files to post to opendap
#
echo "SERVER is $SERVER" >> ${SYSLOG}
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR}
# get the forecast ensemble member number for use in 
# picking up any bespoke configuration for this ensemble
# member in the configuration files
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
si=$ENMEMNUM
#
## grab all config info
. ${CONFIG} 
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# pick up config of the thredds data server where the files are to be posted
logMessage "Setting opendap server parameters with env_dispatch ${SERVER}."
env_dispatch $SERVER   # from platforms.sh
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#--------------------------------------------------------------------
#  O P E N  D A P    P A T H   F O R M A T I O N
#--------------------------------------------------------------------
STORMNAMEPATH=null
#
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
# write the downloadurl to a file for later reference as well as 
# for retrieving and writing backup urls 
echo $downloadURL >> $STORMDIR/downloadurl.log 2>> ${SYSLOG}
# write the opendap dir to a file for later reference as well as
# for retrieving and writing backup urls
echo $OPENDAPDIR >> $STORMDIR/opendapdir.log 2>> ${SYSLOG}
#-----------------------------------------------------------------------
#           D E T E R M I N E   M E T H O D
#-----------------------------------------------------------------------
# Establish the default method of posting results for service via opendap
OPENDAPPOSTMETHOD=scp
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
#
#-------------------------------------------------------------------
#                P O S T   V I A   S C P
#-------------------------------------------------------------------
# jgf20160803: Changed if/then to case-switch to accommodate new "copy" method.
case $OPENDAPPOSTMETHOD in
"scp")
   logMessage "Transferring files to $OPENDAPDIR on $OPENDAPHOST as user $OPENDAPUSER."
   ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "mkdir -p $OPENDAPDIR" 2>> $SYSLOG
   if [[ $? != 0 ]]; then
      warn "Failed to create the directory $OPENDAPDIR on the remote machine ${OPENDAPHOST}."
      threddsPostStatus=fail
   fi
   # add code to create write permissions on directories so that other 
   # Operators can post results to the same directories
   ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "chmod a+w $OPENDAPBASEDIR" 2>> $SYSLOG
   ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "chmod a+w $OPENDAPBASEDIR/$STORMNAMEPATH" 2>> $SYSLOG
   ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "chmod -R a+w $OPENDAPBASEDIR/$STORMNAMEPATH/$ADVISORY" 2>> $SYSLOG
   if [[ $? != 0 ]]; then
      warn "Failed to change permissions on the directory $OPENDAPBASEDIR/$STORMNAMEPATH on the remote machine ${OPENDAPHOST}."
      threddsPostStatus=fail
   fi
   for file in ${FILES[*]}; do 
      # send opendap posting notification email early if directed
      if [[ $file = "sendNotification" ]]; then
         logMessage "$ENSTORM: $THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
         cat ${STORMDIR}/opendap_results_notify.txt | mail -s "$subject" $OPENDAPNOTIFY 2>> ${SYSLOG} 2>&1
         opendapEmailSent=yes
         continue        
      fi
      chmod +r $file 2>> $SYSLOG
      logMessage "Transferring $file."
      scp -P $SSHPORT $file ${OPENDAPUSER}@${OPENDAPHOST}:${OPENDAPDIR} 2>> $SYSLOG
      if [[ $? != 0 ]]; then
         threddsPostStatus=fail
         warn "Failed to transfer the file $file to ${OPENDAPHOST}:${OPENDAPDIR}."
      fi
      # give the file read permissions
      fname=`basename $file` 
      ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "chmod +r $OPENDAPDIR/$fname"
      if [[ $? != 0 ]]; then
         threddsPostStatus=fail
         warn "Failed to give the file $file read permissions in ${OPENDAPHOST}:${OPENDAPDIR}."
      fi      
      # We must add this new property to the run.properties after copying it
      # to the remote server so we don't contaminate the original
      # run.properties with this downloadurl property.
      ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "echo downloadurl : $downloadURL >> $OPENDAPDIR/run.properties"
      if [[ $? != 0 ]]; then
         threddsPostStatus=fail
         warn "Failed to add the downloadurl property to run.properties in ${OPENDAPHOST}:${OPENDAPDIR}."
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
   chmod a+w $OPENDAPBASEDIR 2>> $SYSLOG
   chmod a+w $OPENDAPBASEDIR/$STORMNAMEPATH 2>> $SYSLOG
   chmod -R a+w $OPENDAPBASEDIR/$STORMNAMEPATH/$ADVISORY 2>> $SYSLOG
   cd $OPENDAPDIR 2>> ${SYSLOG}
   for file in ${FILES[*]}; do 
      # send opendap posting notification email early if directed
      if [[ $file = "sendNotification" ]]; then
         logMessage "$ENSTORM: $THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
         cat ${STORMDIR}/opendap_results_notify.txt | mail -s "$subject" $OPENDAPNOTIFY 2>> ${SYSLOG} 2>&1
         opendapEmailSent=yes
         continue        
      fi
      chmod +r ${ADVISDIR}/${ENSTORM}/$file 2>> $SYSLOG
      # We must copy the run.properties so we don't contaminate the
      # original run.properties with this downloadurl property.
      if [[ $file = 'run.properties' ]]; then
         logMessage "Copying $file."
         cp ${ADVISDIR}/${ENSTORM}/$file . 2>> ${SYSLOG}
         if [[ $? != 0 ]]; then
            threddsPostStatus=fail
            warn "Failed to copy the run.properties file to ${OPENDAPDIR}."
         fi         
         echo downloadurl : $downloadURL >> $file 2>> ${SYSLOG}
      else
         logMessage "$postDesc $file."
         $postCMD ${ADVISDIR}/${ENSTORM}/$file . 2>> ${SYSLOG}
         if [[ $? != 0 ]]; then
           threddsPostStatus=fail
           warn "$postDesc $file to ${OPENDAPDIR} failed."
         fi
      fi
   done
   ;;
*)
   threddsPostStatus=fail
   warn "The opendap post method $OPENDAPPOSTMETHOD was not recognized."
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
#   cat ${STORMDIR}/opendap_results_notify.txt | mail -s "$subject" $ASGSADMIN 2>> ${SYSLOG} 2>&1
#else
if [[ $opendapEmailSent = "no" ]]; then 
   logMessage "opendap_post.sh: Sending 'results available' email to the following addresses: $OPENDAPNOTIFY."
   cat ${STORMDIR}/opendap_results_notify.txt | mail -s "$subject" $OPENDAPNOTIFY 2>> ${SYSLOG} 2>&1
fi
