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
THIS=$(basename -- $0)
#
EXIT_SUCCESS=0
EXIT_ERROR=1
declare -A properties
SCENARIODIR=$PWD
RUNPROPERTIES=$SCENARIODIR/run.properties
if [[ $# -eq 1 ]]; then
   RUNPROPERTIES=$1
   SCENARIODIR=`dirname $RUNPROPERTIES`
   MANUAL=1
fi
# this script can be called with just one command line option: the
# full path to the run.properties file
echo "Loading properties."
# get loadProperties function
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' $RUNPROPERTIES`
source $SCRIPTDIR/properties.sh
# load run.properties file into associative array
loadProperties $RUNPROPERTIES
echo "Finished loading properties."
if [ -z "$OPENDAPNOTIFY" ]; then
  echo "Loading OPENDAPNOTIFY address from run.properties..."
  OPENDAPNOTIFY=${properties['notification.opendap.email.opendapnotify']}
fi
echo "Email list via 'OPENDAPNOTIFY' is '$OPENDAPNOTIFY'"
CONFIG=${properties['config.file']}
COLDSTARTDATE=${properties["adcirc.time.coldstartdate"]} # used for the hindcast path
CYCLEDIR=${properties['path.advisdir']}
CYCLE=${properties['advisory']}
# if this is an initialization, there is no advisory number
# to use in the opendap path
if [[ ${properties['advisory']} = "0" ]]; then
   CYCLE=$COLDSTARTDATE
else
   # this does not actually seem to be used in this script
   HSTIME=${properties['InitialHotStartTime']}
fi
HPCENV=${properties['hpc.hpcenv']}
SCENARIO=${properties['scenario']}
SYSLOG=${properties['monitoring.logging.file.syslog']}
CYCLELOG=${properties['monitoring.logging.file.cyclelog']}
SCENARIOLOG=${properties['monitoring.logging.file.scenariolog']}
source $SCRIPTDIR/monitoring/logging.sh
source $SCRIPTDIR/platforms.sh
declare -a SERVERS
serverList=${properties['post.opendap.tds']}
IFS=' ' read -r -a SERVERS <<< "$serverList"  # FIXME: contains "(" and ")" (don't use eval)
if [[ ${#SERVERS[@]} -eq 0 ]]; then
   MSG="cycle $CYCLE: $SCENARIO: $THIS: No opendap servers in $RUNPROPERTIES."
   if [ "$MANUAL" == 1 ]; then
     echo "$MSG"
   else
     warn "$MSG"
   fi
   exit
fi
declare -a FILES
fileList=${properties["post.opendap.files"]} # array of files to post to opendap
IFS=' ' read -r -a FILES <<< "$fileList"  # FIXME: contains "(" and ")" (don't use eval)
if [[ ${#FILES[@]} -eq 0 ]]; then
   MSG="cycle $CYCLE: $SCENARIO: $THIS: No files to post to opendap servers in $RUNPROPERTIES."
   if [ "$MANUAL" == 1 ]; then
     echo "$MSG"
   else
     warn "$MSG"
   fi
   exit
fi
if [[ $SCENARIO == "asgs.instance.status" ]]; then
    statusDir=${properties['path.statusdir']}
    cd ${statusDir} > errmsg 2>&1 || warn "$SCENARIO: $THIS: Failed to change directory to '$statusDir': `cat errmsg`."
else
    SCENARIODIR=${CYCLEDIR}/${SCENARIO}       # shorthand
    cd ${SCENARIODIR} > errmsg 2>&1 || warn "cycle $CYCLE: $SCENARIO: $THIS: Failed to change directory to '$SCENARIODIR': `cat errmsg`."
fi
# load asgs operator email address
ASGSADMIN=${properties["notification.email.asgsadmin"]}
GRIDNAME=${properties["adcirc.gridname"]}
INSTANCENAME=${properties["instancename"]}
HPCENVSHORT=${properties["hpc.hpcenvshort"]}
TROPICALCYCLONE=${properties["forcing.tropicalcyclone"]}
BACKGROUNDMET=${properties["forcing.backgroundmet"]}
enableStatusNotify=${properties["notification.opendap.email.enable"]}
if [[ -z $enableStatusNotify || $enableStatusNotify = "" ]]; then
   enableStatusNotify="no"
fi
# get the scenario number from Operator config
case $SCENARIO in
"hindcast")
   SCENARIONUMBER=-2
   ;;
"nowcast")
   SCENARIONUMBER=-1
   ;;
*)
   SCENARIONUMBER=${properties["forecast.scenario.number"]} # this is used in the subject line of the email
   ;;
esac
env_dispatch $HPCENVSHORT # set up JOBENV with perlbrew for asgs-sendmail etc
OPENDAPMAILSERVER=${properties["notification.opendap.email.opendapmailserver"]}
declare -a LINKABLEHOSTS
declare -a COPYABLEHOSTS
timeoutRetryLimit=${timeoutRetryLimit:-5} # FIXME: hardcoded to 5; make this more granular
serverAliveInterval=${serverAliveInterval:-10}

for server in ${SERVERS[*]}; do
   if [[ $server = "(" || $server = ")" ]]; then
      continue
   fi
   allMessage "cycle $CYCLE: $SCENARIO: $THIS: Posting to opendap server ${server}."
   # pick up config of the thredds data server where the files are to be posted
   allMessage "Setting opendap server parameters with writeTDSProperties ${server}."
   # write platform-dependent properties related to posting to thredds server for
   # opendap service  (from platforms.sh)
   writeTDSProperties $server $RUNPROPERTIES  # this writes to a local run.properties file
   if [[ $SCENARIO == "asgs.instance.status" ]]; then
      cat run.properties >> $RUNPROPERTIES
      rm run.properties # so we don't keep appending to it
      $SCRIPTDIR/metadata.pl --redact --jsonify --metadatafile $RUNPROPERTIES --converted-file-name asgs.instance.status.json 2>> $SYSLOG
      sed --in-place "s/$USER/\$USER/g" asgs.instance.status.json 2>> $SYSLOG 
   fi
   # FIXME: enable Operator to override TDS parameter settings from platforms.sh
   _THIS="output/opendap_post.sh-->$server"
   loadProperties $RUNPROPERTIES # reload to pick up properties written by writeTDSProperties
   LINKABLEHOSTS=${properties["post.opendap.${server}.linkablehosts"]}
   COPYABLEHOSTS=${properties["post.opendap.${server}.copyablehosts"]}
   OPENDAPHOST=${properties["post.opendap.${server}.opendaphost"]}
   DOWNLOADPREFIX=${properties["post.opendap.${server}.downloadprefix"]}
   CATALOGPREFIX=${properties["post.opendap.${server}.catalogprefix"]}
   OPENDAPBASEDIR=${properties["post.opendap.${server}.opendapbasedir"]}
   #
   #--------------------------------------------------------------------
   #  O P E N  D A P    P A T H   F O R M A T I O N
   #--------------------------------------------------------------------
   STORMNAMEPATH=null
   #
   if [[ $BACKGROUNDMET != off ]]; then
      # for NAM, the "advisory number" is actually the cycle time
      YEAR=${properties["forcing.nwp.year"]}
      NWPMODEL=${properties["forcing.nwp.model"]}
      STORMNAMEPATH=$YEAR/$NWPMODEL
   fi
   if [[ $TROPICALCYCLONE = on ]]; then
      YEAR=${properties["forcing.tropicalcyclone.year"]}
      STORMNAME=${properties["forcing.tropicalcyclone.stormname"]}
      STORMNUMBER=${properties["forcing.tropicalcyclone.stormnumber"]}
      STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
      basin="al" # FIXME: write/read a property instead of hardcoding the atlantic basin
      STORMNAMEPATH=$YEAR/$basin$STORMNUMBER
      ALTSTORMNAMEPATH=$YEAR/$STORMNAMELC  # symbolic link with name
   fi
   if [[ $SCENARIO = "hindcast" ]]; then
      YEAR=${COLDSTARTDATE:0:4}
      STORMNAMEPATH=$YEAR/initialize
   fi
   # form path to results on tds based on type of forcing or name of storm
   if [[ $SCENARIO == "asgs.instance.status" ]]; then
      YEAR=${COLDSTARTDATE:0:4}
      STORMNAMEPATH=$YEAR/status
      OPENDAPSUFFIX=$HPCENV/$INSTANCENAME
      # update the url properties in the status json files before posting them
      # and save the url for keeping track of the previous url
      hookStatusURL=$DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/hook.status.json
      asgsInstanceStatusURL=$DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/asgs.instance.status.json
      cp -f hook.status.json tmp.hook.status.json
      awk -f $SCRIPTDIR/monitoring/replaceURL.awk -v u=\"$hookStatusURL\" -v i=\"$asgsInstanceStatusURL\" tmp.hook.status.json > hook.status.json
      rm tmp.hook.status.json
   else
      OPENDAPSUFFIX=$CYCLE/$GRIDNAME/$HPCENV/$INSTANCENAME/$SCENARIO
   fi
   echo "post.opendap.${server}.opendapsuffix : $OPENDAPSUFFIX" >> $RUNPROPERTIES 2>> $SYSLOG
   #
   # Create full path to results for server file sytem.
   # OPENDAPBASEDIR is specified in platforms.sh.
   OPENDAPDIR=$OPENDAPBASEDIR/$STORMNAMEPATH/$OPENDAPSUFFIX
   echo "post.opendap.${server}.opendapdir : $OPENDAPDIR" >> $RUNPROPERTIES 2>> $SYSLOG
   # create the opendap download url for the run.properties file
   downloadURL=$DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX
   # add downloadurl or downloadurl_backup property to run.properties file
   if [[ ! `grep downloadurl $RUNPROPERTIES` =~ downloadurl ]]; then
      echo "downloadurl : $downloadURL" >> $RUNPROPERTIES 2>> ${SYSLOG}
   else
      backupNum=`grep downloadurl $RUNPROPERTIES | wc -l`
      echo "downloadurl_backup$backupNum : $downloadURL" >> $RUNPROPERTIES 2>> ${SYSLOG}
   fi
   #-----------------------------------------------------------------------
   #           D E T E R M I N E   M E T H O D
   #-----------------------------------------------------------------------
   # Establish the default method of posting results for service via opendap
   # The actual method will depend on the configuration in platforms.sh.
   OPENDAPPOSTMETHOD=scp
   #
   # mvb20190620: Testing rsync with the LSU CCR thredds server
   if [[ $server = "lsu_ccr_tds" ]]; then
       OPENDAPPOSTMETHOD=rsync
   fi
   #
   # Determine whether to copy files instead of using scp by looking at the
   # list of HPC machines that share a common filesystem with this TDS.
   for hpc in ${COPYABLEHOSTS[*]}; do
      if [[ $hpc = $HPCENVSHORT ]]; then
         OPENDAPPOSTMETHOD=copy
      fi
   done
   #
   # Determine whether to create symbolic links by looking at the
   # list of HPC machines that share a common filesystem with this TDS.
   # This comes last, so it takes precedence above the others if multiple
   # file delivery mechanisms are possible.
   for hpc in ${LINKABLEHOSTS[*]}; do
      if [[ $hpc = $HPCENVSHORT ]]; then
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
   echo "post.opendap.${server}.opendappostmethod : $OPENDAPPOSTMETHOD" >> $RUNPROPERTIES 2>> $SYSLOG
   #
   #-------------------------------------------------------------------
   #     C R E A T E    N O T I F I C A T I O N   E M A I L
   #-------------------------------------------------------------------
   # @jasonfleming: Hack in the ability to send the notification email
   # before all the files have been posted.
   opendapEmailSent=no
   if [[ $enableStatusNotify == "no" ]]; then
       opendapEmailSent=yes # hack to prevent this script from sending the notification email
   fi
   #
   runStartTime=${properties["RunStartTime"]}

   # dev NOTE: we should not get crazy here with all of the options of
   # passing metadata via subject; the following does little but gives
   # an indication of the chaos that can ensue if we perpetuate this
   # method - if email subject modification is requested again, be sure
   # bring it up for discussion during team deliberations

   # default primary subject
   subject="ADCIRC POSTED for $runStartTime"

   # modify primary subject
   if [[ "$SCENARIO" == "nowcast" ]]; then
     subject="ADCIRC NOWCAST POSTED for $runStartTime"
   elif [[ "$SCENARIO" == "hindcast" ]]; then
     subject="ADCIRC HINDCAST POSTED for $runStartTime"
   elif [[ "$SCENARIO" == "asgs.instance.status" ]]; then
     subject="ADCIRC STATUS POSTED for $runStartTime"
   fi

   # decorate subject (append or prepend) - so works with any value of primary
   #  "$subject" as it's determined above
   if [[ $TROPICALCYCLONE == "on" ]]; then
      subject="${subject} (TC)"
   fi
   subject="${subject} $SCENARIONUMBER $HPCENV.$INSTANCENAME $ASGSADMIN"
   echo "post.opendap.${server}.subject : $subject" >> $RUNPROPERTIES 2>> $SYSLOG
   if [[ "$SCENARIO" == "asgs.instance.status" ]]; then
      logfile=`basename $SYSLOG`
      subject="ADCIRC POSTED status of $HPCENV.$INSTANCENAME"
      echo "post.opendap.${server}.subject : $subject" >> $RUNPROPERTIES 2>> $SYSLOG
cat <<END > ${SCENARIODIR}/opendap_results_notify_${server}.txt

The status of $HPCENV.$INSTANCENAME has been posted to $CATALOGPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/catalog.html

The instance status file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/asgs.instance.status.json
The hook status file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/hook.status.json
The log file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/$logfile

or wget the file with the following commands

wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/asgs.instance.status.json
wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/hook.status.json
wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/$logfile

END
      $SCRIPTDIR/metadata.pl --jsonify --redact --metadatafile $RUNPROPERTIES --converted-file-name asgs.instance.status.json 2>> $SYSLOG
      sed --in-place "s/$USER/\$USER/g" asgs.instance.status.json 2>> $SYSLOG 
   else
cat <<END > ${SCENARIODIR}/opendap_results_notify_${server}.txt

The results for cycle $CYCLE have been posted to $CATALOGPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/catalog.html

The run.properties file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties

or wget the file with the following command

wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
END

   fi
   #
   #-------------------------------------------------------------------
   #                P O S T   V I A   S C P
   #-------------------------------------------------------------------
   # jgf20160803: Changed if/then to case-switch to accommodate new "copy" method.
   allMessage "$SCENARIO: $_THIS: Posting to $OPENDAPHOST using the '$OPENDAPPOSTMETHOD' method."
   case $OPENDAPPOSTMETHOD in
   "scp")
      allMessage "$SCENARIO: $_THIS: Transferring files to $OPENDAPDIR on $OPENDAPHOST."
      retry=0
      mkdirRetryLimit=10 # FIXME: hardcoded for now
      while [[ $retry -lt $mkdirRetryLimit ]]; do
         MSG="(WAL: remote cmd) ssh $OPENDAPHOST \"mkdir -p $OPENDAPDIR\""
         if [ "$MANUAL" == 1 ]; then
           echo "$MSG"
         else
           allMessage "$MSG" >> "$SYSLOG"
         fi
         unset MSG
         ssh $OPENDAPHOST <<SSHCMD >> $SCENARIOLOG 2>&1
# this block will be executed on the remote server,
# variables are interpolated locally unless escaped
# with a backslash, '\'
mkdir -p "$OPENDAPDIR"
SSHCMD
         if [[ $? != 0 ]]; then
            MSG="$SCENARIO: $_THIS: Failed to create the directory $OPENDAPDIR on the remote machine ${OPENDAPHOST}."
            if [ "$MANUAL" == 1 ]; then
              echo "$MSG"
            else
              warn "$MSG"
            fi
            unset MSG
            threddsPostStatus=fail
         else
            MSG="$SCENARIO: $_THIS: Successfully created the directory $OPENDAPDIR on the remote machine ${OPENDAPHOST}."
            if [ "$MANUAL" == 1 ]; then
             echo "$MSG"
            else
             allMessage "$MSG" >> "$SYSLOG"
            fi
            unset MSG
            break
         fi
         retry=`expr $retry + 1`
         if [[ $retry -lt $mkdirRetryLimit ]]; then
            allMessage "$SCENARIO: $_THIS: Trying again."
         else
            MSG="$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
            if [ "$MANUAL" == 1 ]; then
             echo "$MSG"
            else
             allMessage "$MSG" >> "$SYSLOG"
            fi
            unset MSG
         fi
      done
      # add code to create write permissions on directories so that other
      # Operators can post results to the same directories
      #OPENDAPSUFFIX=$CYCLE/$GRIDNAME/$HPCENV/$INSTANCENAME/$SCENARIO
      partialPath=$OPENDAPDIR
      while [[ $partialPath != $OPENDAPBASEDIR  ]]; do
         retry=0
         while [[ $retry -lt $timeoutRetryLimit ]]; do
            # this operation is expected to fail on the top level directory
            # (the year) on most (but not all) thredds servers, so only need
            # to try this once on the top level directory
            # to avoid filling log files with unhelpful and somewhat
            # alarming error messages
            if [[ `basename $partialPath` == $YEAR || `basename $partialPath` == "nam" ]]; then
                # avoid retrying and associated log messages
                retry=$timeoutRetryLimit
            fi
            MSG="(WAL: remote cmd) ssh $OPENDAPHOST \"chmod a+wx $partialPath\""
            if [ "$MANUAL" == 1 ]; then
             echo "$MSG"
            else
             allMessage "$MSG" >> "$SYSLOG"
            fi
            unset MSG
            ssh $OPENDAPHOST bash <<SSHCMD >> $SYSLOG 2>&1
# this block will be executed on the remote server,
# variables are interpolated locally unless escaped
# with a backslash, '\'
chmod a+wx "$partialPath"
SSHCMD
            if [[ $? != 0 ]]; then
               MSG="$SCENARIO: $_THIS: Failed to change permissions on the directory $partialPath on the remote machine ${OPENDAPHOST}."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 allMessage "$MSG" >> "$SYSLOG"
               fi
               unset MSG
               threddsPostStatus=fail
            else
               MSG="$SCENARIO: $_THIS: Successfully changed permissions to a+wx on '$partialPath'."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 allMessage "$MSG" >> "$SYSLOG"
               fi
               unset MSG
               break
            fi
            retry=`expr $retry + 1`
            if [[ $retry -lt $timeoutRetryLimit ]]; then
               allMessage "$SCENARIO: $_THIS: Trying again."
            else
               MSG="$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 allMessage "$MSG" >> "$SYSLOG"
               fi
               unset MSG
            fi
         done
         # cut off the end of the partial path and keep going until we get down
         # to OPENDAPBASEDIR
         partialPath=`dirname $partialPath`
      done
      #
      # add a symbolic link for the storm name if this is tropicalcyclone forcing
      if [[ $TROPICALCYCLONE != off ]]; then
         retry=0
         while [[ $retry -lt $timeoutRetryLimit ]]; do
            MSG="(WAL: remote cmd) ssh $OPENDAPHOST \"ln -s $OPENDAPBASEDIR/$STORMNAMEPATH $OPENDAPBASEDIR/$ALTSTORMNAMEPATH\""
            if [ "$MANUAL" == 1 ]; then
              echo "$MSG"
            else
              allMessage "$MSG" >> "$SYSLOG"
            fi
            unset MSG
            ssh $OPENDAPHOST bash <<SSHCMD >> $SYSLOG 2>&1
# this block will be executed on the remote server,
# variables are interpolated locally unless escaped
# with a backslash, '\'
ln -s "$OPENDAPBASEDIR/$STORMNAMEPATH" "$OPENDAPBASEDIR/$ALTSTORMNAMEPATH"
SSHCMD
            if [[ $? != 0 ]]; then
               MSG="$SCENARIO: $_THIS: Failed to create symbolic link for the storm name."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 warn "$MSG"
               fi
               unset MSG
               threddsPostStatus=fail
            else
               allMessage "$SCENARIO: $_THIS: Successfully created symbolic link to storm name."
               break
            fi
            retry=`expr $retry + 1`
            if [[ $retry -lt $timeoutRetryLimit ]]; then
               allMessage "$SCENARIO: $_THIS: Trying again."
            else
               MSG="$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                allMessage "$MSG" >> "$SYSLOG"
               fi
               unset MSG
            fi
         done
      fi
      fileIndex=1 # skip the opening "("
      while [[ $fileIndex -lt `expr ${#FILES[@]} - 1` ]] ; do  # skip the closing "("
         file="${FILES[$fileIndex]}"
         if [[ $file = '(' || $file = ')' ]]; then
            fileIndex=`expr $fileIndex + 1` 2>> $SCENARIOLOG
            continue
         fi
         echo "Processing $file"
         # send opendap posting notification email early if directed
         if [[ $file == "sendNotification" && $OPENDAPNOTIFY != "null" && $OPENDAPNOTIFY != "" ]]; then
            MSG="$SCENARIO: $_THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
            if [ "$MANUAL" == 1 ]; then
              echo "$MSG"
            else
              allMessage "$MSG" >> "$SYSLOG"
            fi
            unset MSG
            # use asgs sendmail if Operator has set it up
            MSG="asgs-sendmail --subject '$subject' --to '$OPENDAPNOTIFY' < ${SCENARIODIR}/opendap_results_notify_${server}.txt 2>> ${SYSLOG} 2>&1"
            if [ "$MANUAL" == 1 ]; then
              echo "$MSG"
            else
              allMessage "$MSG" >> "$SYSLOG"
            fi
            unset MSG

            asgs-sendmail --subject "$subject" --to "$OPENDAPNOTIFY" < ${SCENARIODIR}/opendap_results_notify_${server}.txt >> ${SCENARIOLOG} 2>&1
            ERR=$?
            if [[ $ERR != $EXIT_SUCCESS ]]; then
              MSGS="$THIS: Failed to send email to '$OPENDAPNOTIFY'"
              if [ "$MANUAL" == 1 ]; then
                echo "$MSG"
              else
                warn "$MSG"
              fi
              unset MSG
            else
              opendapEmailSent=yes
            fi 
            fileIndex=`expr $fileIndex + 1` 2>> $SCENARIOLOG
            continue
         fi
         chmod +r $file 2>> $SCENARIOLOG
         MSG="$SCENARIO: $_THIS: Transferring $file to ${OPENDAPHOST}:${OPENDAPDIR}."
         if [ "$MANUAL" == 1 ]; then
           echo "$MSG"
         else
           allMessage "$MSG" >> "$SYSLOG"
         fi
         unset MSG
         retry=0
         while [[ $retry -lt $timeoutRetryLimit ]]; do
            MSG="(WAL: remote file xfer) scp ./$file ${OPENDAPHOST}:${OPENDAPDIR}"
            if [ "$MANUAL" == 1 ]; then
              echo "$MSG"
            else
              allMessage "$MSG" >> "$SYSLOG"
            fi
            unset MSG
            scp ./$file ${OPENDAPHOST}:${OPENDAPDIR} >> $SCENARIOLOG 2>&1
            if [[ $? != 0 ]]; then
               threddsPostStatus=fail
               MSG="$SCENARIO: $_THIS: Failed to transfer the file $file to ${OPENDAPHOST}:${OPENDAPDIR}."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 warn "$MSG"
               fi
               unset MSG
            else
               MSG="$SCENARIO: $_THIS: Successfully transferred the file."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 allMessage "$MSG" >> "$SYSLOG"
               fi
               unset MSG
               break
            fi
            retry=`expr $retry + 1`
            if [[ $retry -lt $timeoutRetryLimit ]]; then
               allMessage "$SCENARIO: $_THIS: Trying again."
            else
               MSG="$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 allMessage "$MSG" >> "$SYSLOG"
               fi
               unset MSG
            fi
         done
         # give the file read permissions on the remote filesystem
         fname=`basename $file`
         retry=0
         while [[ $retry -lt $timeoutRetryLimit ]]; do
            MSG="(WAL: remote cmd) ssh $OPENDAPHOST \"chmod +r $OPENDAPDIR/$fname\""
            if [ "$MANUAL" == 1 ]; then
              echo "$MSG"
            else
              allMessage "$MSG" >> "$SYSLOG"
            fi
            unset MSG
            ssh $OPENDAPHOST bash <<SSHCMD 2>> $SYSLOG
# this block will be executed on the remote server,
# variables are interpolated locally unless escaped
# with a backslash, '\'
chmod +r "$OPENDAPDIR/$fname"
SSHCMD
            if [[ $? != 0 ]]; then
               threddsPostStatus=fail
               MSG="$SCENARIO: $_THIS: Failed to give the file $fname read permissions in ${OPENDAPHOST}:${OPENDAPDIR}."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 warn "$MSG"
               fi
               unset MSG
            else
               allMessage "$SCENARIO: $_THIS: Successfully changed permissions to +r on $OPENDAPDIR/$fname."
               break
            fi
            retry=`expr $retry + 1`
            if [[ $retry -lt $timeoutRetryLimit ]]; then
               allMessage "$SCENARIO: $_THIS: Trying again."
            else
               MSG="$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 allMessage "$MSG" >> "$SYSLOG"
               fi
               unset MSG
            fi
         done
         fileIndex=`expr $fileIndex + 1` 2>> $SCENARIOLOG
      done
      ;;
   #-------------------------------------------------------------------
   #                P O S T   V I A   R S Y N C
   #-------------------------------------------------------------------
   # mvb20190618: Added to support time-out issues with general scp transfers
   "rsync")
      echo "post.opendap.${server}.rsyncsshoptions : $rsyncSSHOptions" >> $RUNPROPERTIES 2>> $SYSLOG
      rsyncOptions="-z --copy-links"
      echo "post.opendap.${server}.rsyncoptions : $rsyncOptions" >> $RUNPROPERTIES 2>> $SYSLOG
      allMessage "$SCENARIO: $_THIS: Transferring files to $OPENDAPDIR on $OPENDAPHOST."
      allMessage "(WAL: remote cmd) ssh $OPENDAPHOST \"mkdir -p $OPENDAPDIR\"" >> $SYSLOG
      ssh $OPENDAPHOST bash <<SSHCMD >> $SCENARIOLOG 2>&1
# this block will be executed on the remote server,
# variables are interpolated locally unless escaped
# with a backslash, '\'
mkdir -p "$OPENDAPDIR"
SSHCMD
      if [[ $? != 0 ]]; then
         MSG="$SCENARIO: $_THIS: Failed to create the directory $OPENDAPDIR on the remote machine ${OPENDAPHOST}."
         if [ "$MANUAL" == 1 ]; then
           echo "$MSG"
         else
           warn "$MSG"
         fi
         unset MSG
         threddsPostStatus=fail
      fi
      # add code to create write permissions on directories so that other
      # Operators can post results to the same directories
      partialPath=$OPENDAPDIR
      while [[ $partialPath != $OPENDAPBASEDIR  ]]; do
         retry=0
         while [[ $retry -lt $timeoutRetryLimit ]]; do
            allMessage "(WAL: remote cmd) ssh $OPENDAPHOST \"chmod a+wx $partialPath\"" >> $SYSLOG
            ssh $OPENDAPHOST bash >> $SYSLOG 2>&1
# this block will be executed on the remote server,
# variables are interpolated locally unless escaped
# with a backslash, '\'
if [ -d "$partialPath" ]; then
  chmod a+wx $partialPath
fi
SSHCMD
            if [[ $? != 0 ]]; then
               MSGS="$SCENARIO: $_THIS: Failed to change permissions on the directory $partialPath on the remote machine ${OPENDAPHOST}."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 warn "$MSG"
               fi
               unset MSG
               threddsPostStatus=fail
            else
               allMessage "$SCENARIO: $_THIS: Successfully changed permissions."
               break
            fi
            retry=`expr $retry + 1`
            if [[ $retry -lt $timeoutRetryLimit ]]; then
                allMessage "$SCENARIO: $_THIS: Trying again."
            else
               allMessage "$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
            fi
         done
         # cut off the end of the partial path and keep going until we get down
         # to OPENDAPBASEDIR
         partialPath=`dirname $partialPath`
      done
      for file in ${FILES[*]}; do
         if [[ "$file" = "(" || $file = ")" ]]; then
            continue
         fi
         # send opendap posting notification email early if directed
         if [[ $file = "sendNotification"  && $OPENDAPNOTIFY != "null" && $OPENDAPNOTIFY != "" ]]; then
            allMessage "$SCENARIO: $_THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
            echo "$SCENARIO: $_THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
            cat ${SCENARIODIR}/opendap_results_notify_${server}.txt | asgs-sendmail  --subject "$subject" --to "$OPENDAPNOTIFY" 2>> ${SYSLOG} 2>&1
            ERR=$?
            if [[ $ERR != $EXIT_SUCCESS ]]; then
              MSGS="$THIS: Failed to send email to '$OPENDAPNOTIFY'"
              if [ "$MANUAL" == 1 ]; then
                echo "$MSG"
              else
                warn "$MSG"
              fi
              unset MSG
            else
              opendapEmailSent=yes
            fi 
            continue
         fi
         chmod +r "$file" 2>> $SYSLOG
         allMessage "$SCENARIO: $_THIS: Transferring $file to ${OPENDAPHOST}:${OPENDAPDIR}."
         rsync ${rsyncOptions} ./${file} ${OPENDAPHOST}:${OPENDAPDIR} >> $SCENARIOLOG 2>&1
         if [[ $? != 0 ]]; then
            threddsPostStatus=fail
            MSGS="$SCENARIO: $_THIS: Failed to transfer the file $file to ${OPENDAPHOST}:${OPENDAPDIR}."
            if [ "$MANUAL" == 1 ]; then
              echo "$MSG"
            else
              warn "$MSG"
            fi
            unset MSG
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
               MSG="$SCENARIO: $_THIS: Failed to change permissions on the directory ${partialPath}."
               if [ "$MANUAL" == 1 ]; then
                 echo "$MSG"
               else
                 warn "$MSG"
               fi
               unset MSG
               threddsPostStatus=fail
            else
               allMessage "$SCENARIO: $_THIS: Successfully changed permissions."
               break
            fi
            retry=`expr $retry + 1`
            if [[ $retry -lt $timeoutRetryLimit ]]; then
               allMessage "$SCENARIO: $_THIS: Trying again."
            else
               allMessage "$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
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
         if [[ "$file" = "(" || $file = ")" ]]; then
            continue
         fi
         # send opendap posting notification email early if directed
         if [[ $file = "sendNotification" && $OPENDAPNOTIFY != "null" && $OPENDAPNOTIFY != "" ]]; then
            allMessage "$SCENARIO: $_THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
            echo "$SCENARIO: $_THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
            # use asgs sendmail if Operator has set it up
            cat ${SCENARIODIR}/opendap_results_notify_${server}.txt | asgs-sendmail --subject "$subject" --to "$OPENDAPNOTIFY" 2>> ${SYSLOG} 2>&1
            ERR=$?
            if [[ $ERR != $EXIT_SUCCESS ]]; then
              MSGS="$_THIS: Failed to send email to '$OPENDAPNOTIFY'"
              if [ "$MANUAL" == 1 ]; then
                echo "$MSG"
              else
                warn "$MSG"
              fi
              unset MSG
            else
              opendapEmailSent=yes
            fi 
            continue
         fi
         chmod +r $file 2>> $SYSLOG
         logMessage "$SCENARIO: $_THIS: $postDesc $file."
         $postCMD $file $OPENDAPDIR 2>> ${SYSLOG}
         if [[ $? != 0 ]]; then
            threddsPostStatus=fail
            MSG="$SCENARIO: $_THIS: $postDesc $file to ${OPENDAPDIR} failed."
            if [ "$MANUAL" == 1 ]; then
              echo "$MSG"
            else
              warn "$MSG"
            fi
            unset MSG
         fi
      done
      ;;
   *)
      threddsPostStatus=fail
      MSGS="$SCENARIO: $_THIS: The opendap post method $OPENDAPPOSTMETHOD was not recognized."
      if [ "$MANUAL" == 1 ]; then
        echo "$MSG"
      else
        warn "$MSG"
      fi
      unset MSG
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
   #   error "opendap_post.sh: A failure occurred when the ASGS instance $INSTANCENAME attempted to post data to the THREDDS Data Server ${server}. Downstream data consumers will not receive an email for these results. However, the opendap results notification will be sent to ${ASGSADMIN}."
   #   cat ${SCENARIODIR}/opendap_results_notify.txt | asgs-sendmail --subject "$subject" --to "$ASGSADMIN" 2>> ${SYSLOG} 2>&1
   #else
   if [[ $opendapEmailSent = "no" && $OPENDAPNOTIFY != "null" && $OPENDAPNOTIFY != "" ]]; then
      MSG="$SCENARIO: $_THIS: Sending 'results available' email to the following addresses: $OPENDAPNOTIFY."
      if [ "$MANUAL" == 1 ]; then
        echo "$MSG"
      else
        allMessage "$MSG" >> "$SYSLOG"
      fi
      unset MSG
      # use asgs sendmail if Operator has set it up
      cat ${SCENARIODIR}/opendap_results_notify_${server}.txt | asgs-sendmail --subject "$subject" "$OPENDAPNOTIFY" 2>> ${SYSLOG} 2>&1
      ERR=$?
      if [[ $ERR != $EXIT_SUCCESS ]]; then
        MSGS="$_THIS: Failed to send email to '$OPENDAPNOTIFY'"
        if [ "$MANUAL" == 1 ]; then
          echo "$MSG"
        else
          warn "$MSG"
        fi
        unset MSG
      fi 
   fi
done # end loop over opendap servers
