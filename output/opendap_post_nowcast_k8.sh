#!/bin/bash
#------------------------------------------------------------------------
# opendap_post.sh : Makes nowcast results available to thredds data server.
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

# check to see if we are in a nowcast directory. If not, check for 
# ../nowcast.  If not, exit
if [ `basename "$PWD"` != "nowcast" ] ; then
    echo "not in nowcast"
    if [ ! -d $PWD"/../nowcast" ] ; then
        echo "nowcast dir not adjacent to $PWD.  Returning without doing anything."
        exit
    else
        cd $PWD"/../nowcast"
    fi
fi
#echo "Working in $PWD."

declare -A properties
SCENARIODIR=$PWD
RUNPROPERTIES=$SCENARIODIR/run.properties
if [[ $# -eq 1 ]]; then
   RUNPROPERTIES=$1
   SCENARIODIR=`dirname $RUNPROPERTIES`
fi

# this script can be called with just one command line option: the
# full path to the run.properties file
echo "Loading properties."
# get loadProperties function
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' $RUNPROPERTIES`
source $SCRIPTDIR/properties.sh
SFTP_SCRIPT='sftp.xfer.coms'

# load run.properties file into associative array
loadProperties $RUNPROPERTIES
CONFIG=${properties['config.file']}
CYCLEDIR=${properties['path.advisdir']}
CYCLE=${properties['advisory']}
HPCENV=${properties['hpc.hpcenv']}
SCENARIO=${properties['scenario']}
HSTIME=${properties['InitialHotStartTime']}
SYSLOG=${properties['monitoring.logging.file.syslog']}
CYCLELOG=${properties['monitoring.logging.file.cyclelog']}
SCENARIOLOG=${properties['monitoring.logging.file.scenariolog']}
source $SCRIPTDIR/monitoring/logging.sh
source $SCRIPTDIR/platforms.sh

declare -a SERVERS
serverList=${properties['post.opendap.tds']}
IFS=' ' read -r -a SERVERS <<< "$serverList"  # FIXME: contains "(" and ")" (don't use eval)
if [[ ${#SERVERS[@]} -eq 0 ]]; then
   warn "cycle $CYCLE: $SCENARIO: $THIS: No opendap servers in run.properties."
fi
declare -a FILES
#fileList=${properties["post.opendap.files"]} # array of files to post to opendap
FILES=( `ls $CONFIG run.properties endrisinginun.63.nc everdried.63.nc fort.61.nc fort.15 fort.63.nc fort.64.nc fort.68.nc fort.71.nc fort.72.nc fort.73.nc initiallydry.63.nc inundationtime.63.nc maxinundepth.63.nc maxele.63.nc maxrs.63.nc maxvel.63.nc minpr.63.nc rads.64.nc swan_DIR.63.nc swan_DIR_max.63.nc swan_TMM10.63.nc swan_TMM10_max.63.nc 2>> $SCENARIOLOG` )

if [[ ${#FILES[@]} -eq 0 ]]; then
   warn "cycle $CYCLE: $SCENARIO: $THIS: No files to post to opendap servers in run.properties."
   exit
fi

OPENDAPNOTIFY="${properties['notification.opendap.email.opendapnotify']}"

#
SCENARIODIR=${CYCLEDIR}/${SCENARIO}       # shorthand
cd ${SCENARIODIR} > errmsg 2>&1 || warn "cycle $CYCLE: $SCENARIO: $THIS: Failed to change directory to '$SCENARIODIR': `cat errmsg`."
# load asgs operator email address
ASGSADMIN=${properties["notification.email.asgsadmin"]}
GRIDNAME=${properties["adcirc.gridname"]}
INSTANCENAME=${properties["instancename"]}
HPCENVSHORT=${properties["hpc.hpcenvshort"]}
TROPICALCYCLONE=${properties["forcing.tropicalcyclone"]}
BACKGROUNDMET=${properties["forcing.backgroundmet"]}
# get the scenario number from Operator config
SCENARIONUMBER=${properties["forecast.scenario.number"]}
env_dispatch $HPCENVSHORT # set up JOBENV with perlbrew for asgs-sendmail.pl etc

OPENDAPMAILSERVER=${properties["notification.opendap.email.opendapmailserver"]}
declare -a LINKABLEHOSTS
declare -a COPYABLEHOSTS
timeoutRetryLimit=${timeoutRetryLimit:-5} # FIXME: hardcoded to 5; make this more granular
serverAliveInterval=${serverAliveInterval:-10}
#

mk_recursive_dir(){
    dirr=$1
    OPENDAPHOST=$2
    IFS='/'
    read -a strarr <<< "$dirr"
    IFS=""
    rdir=""
    #lf="temp.com"
    lf=`mktemp`

    for val in "${strarr[@]}";
    do
        rdir=`printf "%s%s/" $rdir $val `
        echo  "mkdir $rdir" > $lf
        echo sftp -b $lf $OPENDAPHOST # > log  2>&1 
        sftp -b $lf $OPENDAPHOST  > log  2>&1 
        if [[ $? != 0 ]]; then
          echo "Dir $rdir already exists on $OPENDAPHOST."
        else
          echo "mkdir-d $rdir on $OPENDAPHOST."
        fi
        rm $lf
    done
}

for server in ${SERVERS[*]}; do
   if [[ $server = "(" || $server = ")" ]]; then 
      continue
   fi
   # skip all servers except renci k8
   if [[ $server != "renci_tds-k8" ]]; then
      continue
   fi

   allMessage "cycle $CYCLE: $SCENARIO: $THIS: Posting to opendap server ${server}."
   # pick up config of the thredds data server where the files are to be posted
   scenarioMessage "Setting opendap server parameters with writeTDSProperties ${server}."
   # write platform-dependent properties related to posting to thredds server for 
   # opendap service  (from platforms.sh)
   writeTDSProperties $server
   # FIXME: enable Operator to override TDS parameter settings from platforms.sh
   _THIS="$THIS-->$server"
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
   # form path to results on tds based on type of forcing or name of storm
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
   OPENDAPSUFFIX=$CYCLE/$GRIDNAME/$HPCENV/$INSTANCENAME/$SCENARIO
   echo "post.opendap.${server}.opendapsuffix : $OPENDAPSUFFIX" >> run.properties 2>> $SYSLOG
   #
   # Create full path to results for server file sytem. 
   # OPENDAPBASEDIR is specified in platforms.sh.
   OPENDAPDIR=$OPENDAPBASEDIR/$STORMNAMEPATH/$OPENDAPSUFFIX
   echo "post.opendap.${server}.opendapdir : $OPENDAPDIR" >> run.properties 2>> $SYSLOG
   # create the opendap download url for the run.properties file 
   downloadURL=$DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX
   # add downloadurl or downloadurl_backup property to run.properties file
   if [[ ! `grep downloadurl run.properties` =~ downloadurl ]]; then
      echo "downloadurl : $downloadURL" >> run.properties 2>> ${SYSLOG}
   else
      backupNum=`grep downloadurl run.properties | wc -l`
      echo "downloadurl_backup$backupNum : $downloadURL" >> run.properties 2>> ${SYSLOG}
   fi      

   echo "$_THIS -->> downloadurl : $downloadURL" 
   
   OPENDAPPOSTMETHOD=sftp
   threddsPostStatus=ok
   echo "post.opendap.${server}.opendappostmethod : $OPENDAPPOSTMETHOD" >> run.properties 2>> $SYSLOG
   #
   #-------------------------------------------------------------------
   #     C R E A T E    N O T I F I C A T I O N   E M A I L
   #-------------------------------------------------------------------
   # @jasonfleming: Hack in the ability to send the notification email 
   # before all the files have been posted. 
   opendapEmailSent=no
   #
   runStartTime=${properties["RunStartTime"]}
   subject="ADCIRC NOWCAST POSTED for $runStartTime"
   if [[ $TROPICALCYCLONE = on ]]; then
      subject=${subject}" (TC)"
   fi
   #Click on the link: 
   #
   #$CATALOGPREFIX/$STORMNAMEPATH/${OPENDAPSUFFIX}/catalog.html
   subject="${subject} $SCENARIONUMBER $HPCENV.$INSTANCENAME $ASGSADMIN"
   echo "post.opendap.${server}.subject : $subject" >> run.properties 2>> $SYSLOG
cat <<END > ${SCENARIODIR}/opendap_results_notify_${server}.txt 

The results for cycle $CYCLE have been posted to $CATALOGPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/catalog.html

The run.properties file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
   
or wget the file with the following command

wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
END

   #
   #-------------------------------------------------------------------
   #                P O S T   V I A   S F T P
   #-------------------------------------------------------------------

   # scenarioMessage "$SCENARIO: $_THIS: Transferring files to $OPENDAPDIR on $OPENDAPHOST."
   echo "$SCENARIO: $_THIS: Transferring files to $OPENDAPDIR on $OPENDAPHOST."

   retry=0
   mkdirRetryLimit=10 # FIXME: hardcoded for now
#   mk_recursive_dir_list $STORMNAMEPATH/$OPENDAPSUFFIX > $SFTP_SCRIPT
#   sftp -b $SFTP_SCRIPT $OPENDAPHOST
#   if [[ $? == 0 ]]; then
#      echo "Dir $STORMNAMEPATH/$OPENDAPSUFFIX generated on  $OPENDAPHOST."
#   else
#      echo "Failed to make $STORMNAMEPATH/$OPENDAPSUFFIX on $OPENDAPHOST. Might already exist."
#   fi 
#   scp run.properties ${OPENDAPHOST}:$STORMNAMEPATH/$OPENDAPSUFFIX

   while [[ $retry -lt $mkdirRetryLimit ]]; do
      # ssh $OPENDAPHOST "mkdir -p $OPENDAPDIR" # >> $SCENARIOLOG 2>&1
      # sh ~/messaging/test_for_dir.sh "$STORMNAMEPATH/$OPENDAPSUFFIX" $OPENDAPHOST
      mk_recursive_dir "$STORMNAMEPATH/$OPENDAPSUFFIX" $OPENDAPHOST

      if [[ $? != 0 ]]; then
         #warn "$SCENARIO: $_THIS: Failed to create the directory $OPENDAPDIR on the remote machine ${OPENDAPHOST}."
         echo "$SCENARIO: $_THIS: Failed to create the directory $OPENDAPDIR on the remote machine ${OPENDAPHOST}."
         threddsPostStatus=fail
      else
         scenarioMessage "$SCENARIO: $_THIS: Successfully created the directory $OPENDAPDIR on the remote machine ${OPENDAPHOST}."
         break
      fi

      retry=`expr $retry + 1`
      if [[ $retry -lt $mkdirRetryLimit ]]; then
         scenarioMessage "$SCENARIO: $_THIS: Trying again."
      else
         scenarioMessage "$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
      fi
      fileIndex=`expr $fileIndex + 1` # 2>> $SCENARIOLOG
   done
   
   # add code to create write permissions on directories so that other
   # Operators can post results to the same directories
   #OPENDAPSUFFIX=$CYCLE/$GRIDNAME/$HPCENV/$INSTANCENAME/$SCENARIO
#   partialPath=$OPENDAPDIR
#   while [[ $partialPath != $OPENDAPBASEDIR  ]]; do
#      retry=0
#      while [[ $retry -lt $timeoutRetryLimit ]]; do
#         ssh $OPENDAPHOST "chmod a+wx $partialPath" # 2>> $SYSLOG
#         if [[ $? != 0 ]]; then
#            warn "$SCENARIO: $_THIS: Failed to change permissions on the directory $partialPath on the remote machine ${OPENDAPHOST}."
#            threddsPostStatus=fail
#         else
#            scenarioMessage "$SCENARIO: $_THIS: Successfully changed permissions to a+wx on '$partialPath'."
#            break
#         fi
#         retry=`expr $retry + 1`
#         if [[ $retry -lt $timeoutRetryLimit ]]; then
#            scenarioMessage "$SCENARIO: $_THIS: Trying again."
#         else
#            scenarioMessage "$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
#         fi
#      done
#      # cut off the end of the partial path and keep going until we get down
#      # to OPENDAPBASEDIR
#      partialPath=`dirname $partialPath`
#   done
   #
   # add a symbolic link for the storm name if this is tropicalcyclone forcing
#   if [[ $TROPICALCYCLONE != off ]]; then
#      retry=0
#      while [[ $retry -lt $timeoutRetryLimit ]]; do
#         ssh $OPENDAPHOST "ln -s $OPENDAPBASEDIR/$STORMNAMEPATH $OPENDAPBASEDIR/$ALTSTORMNAMEPATH" # 2>> $SYSLOG
#         if [[ $? != 0 ]]; then
#            warn "$SCENARIO: $_THIS: Failed to create symbolic link for the storm name."
#            threddsPostStatus=fail
#         else
#            scenarioMessage "$SCENARIO: $_THIS: Successfully created symbolic link to storm name."
#            break
#         fi
#         retry=`expr $retry + 1`
#         if [[ $retry -lt $timeoutRetryLimit ]]; then
#            scenarioMessage "$SCENARIO: $_THIS: Trying again."
#         else
#            scenarioMessage "$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
#         fi
#      done
#   fi
   fileIndex=1 # skip the opening "("
   while [[ $fileIndex -lt `expr ${#FILES[@]} - 1` ]] ; do  # skip the closing "("
      file=${FILES[$fileIndex]}
      if [[ $file = '(' || $file = ')' ]]; then
         fileIndex=`expr $fileIndex + 1` # 2>> $SCENARIOLOG
         continue
      fi
      # send opendap posting notification email early if directed
      #if [[ $file == "sendNotification" && $OPENDAPNOTIFY != "null" && $OPENDAPNOTIFY != "" ]]; then
      #   scenarioMessage "$SCENARIO: $_THIS: Sending 'results available' email to the following addresses before the full set of results has been posted: $OPENDAPNOTIFY."
      #   # use asgs sendmail if Operator has set it up
      #   if [[ $OPENDAPMAILSERVER = "aws" ]]; then
      #      scenarioMessage "perl $SCRIPTDIR/asgs-sendmail.pl --config ${HOME}/asgs-global.conf --subject '$subject' --to $OPENDAPNOTIFY < ${SCENARIODIR}/opendap_results_notify_${server}.txt 2>> ${SYSLOG} 2>&1"
      #      perl $SCRIPTDIR/asgs-sendmail.pl --config ${HOME}/asgs-global.conf --subject "$subject" --to $OPENDAPNOTIFY < ${SCENARIODIR}/opendap_results_notify_${server}.txt >> ${SCENARIOLOG} 2>&1
      #   else
      #      cat ${SCENARIODIR}/opendap_results_notify_${server}.txt | mail  -S "replyto=$ASGSADMIN" -s "$subject" $OPENDAPNOTIFY 2>> ${SYSLOG} 2>&1
      #   fi
      #   opendapEmailSent=yes
      #   fileIndex=`expr $fileIndex + 1` 2>> $SCENARIOLOG
      #   continue
      #fi
      #chmod +r $file # 2>> $SCENARIOLOG
      #scenarioMessage "$SCENARIO: $_THIS: Transferring $file to ${OPENDAPHOST}:${OPENDAPDIR}."
      echo "$SCENARIO: $_THIS: Transferring $file to ${OPENDAPHOST}:$STORMNAMEPATH/$OPENDAPSUFFIX" 
      retry=0
      while [[ $retry -lt $timeoutRetryLimit ]]; do
         echo scp $file ${OPENDAPHOST}:$STORMNAMEPATH/$OPENDAPSUFFIX # ${OPENDAPDIR} # >> $SCENARIOLOG 2>&1
         scp $file ${OPENDAPHOST}:$STORMNAMEPATH/$OPENDAPSUFFIX # ${OPENDAPDIR} # >> $SCENARIOLOG 2>&1
         if [[ $? != 0 ]]; then
            threddsPostStatus=fail
            #warn "$SCENARIO: $_THIS: Failed to transfer the file $file to ${OPENDAPHOST}:${OPENDAPDIR}."
            echo "$SCENARIO: $_THIS: Failed to transfer the file $file to ${OPENDAPHOST}:${OPENDAPDIR}."
         else
            # scenarioMessage "$SCENARIO: $_THIS: Successfully transferred the file."
            echo "$SCENARIO: $_THIS: Successfully transferred the file."
            break
         fi
         retry=`expr $retry + 1`
         if [[ $retry -lt $timeoutRetryLimit ]]; then
            # scenarioMessage "$SCENARIO: $_THIS: Trying again."
            echo "$SCENARIO: $_THIS: Trying again."
         else
            # scenarioMessage "$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
            echo "$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
         fi
      done
      # give the file read permissions on the remote filesystem
#      fname=`basename $file`
#      retry=0
#      while [[ $retry -lt $timeoutRetryLimit ]]; do
#         ssh $OPENDAPHOST "chmod +r $OPENDAPDIR/$fname"
#         if [[ $? != 0 ]]; then
#            threddsPostStatus=fail
#            warn "$SCENARIO: $_THIS: Failed to give the file $fname read permissions in ${OPENDAPHOST}:${OPENDAPDIR}."
#         else
#            scenarioMessage "$SCENARIO: $_THIS: Successfully changed permissions to +r on $OPENDAPDIR/$fname."
#            break
#         fi
#         retry=`expr $retry + 1`
#         if [[ $retry -lt $timeoutRetryLimit ]]; then
#            scenarioMessage "$SCENARIO: $_THIS: Trying again."
#         else
#            scenarioMessage "$SCENARIO: $_THIS: Maximum number of retries has been reached. Moving on to the next operation."
#         fi
#      done
      fileIndex=`expr $fileIndex + 1` # 2>> $SCENARIOLOG
   done
   #
   #-------------------------------------------------------------------
   #      S E N D   N O T I F I C A T I O N   E M A I L
   #-------------------------------------------------------------------
   # jgf20160322: FIXME: post to opendap even if there was an error so we can 
   # see what the error is
   #
   #if [[ threddsPostStatus != ok ]]; then
   #   error "opendap_post.sh: A failure occurred when the ASGS instance $INSTANCENAME attempted to post data to the THREDDS Data Server ${server}. Downstream data consumers will not receive an email for these results. However, the opendap results notification will be sent to ${ASGSADMIN}."
   #   cat ${SCENARIODIR}/opendap_results_notify.txt | mail  -S "replyto=$ASGSADMIN" -s "$subject" $ASGSADMIN 2>> ${SYSLOG} 2>&1
   #else
#   if [[ $opendapEmailSent = "no" ]]; then 
#      scenarioMessage "$SCENARIO: $THIS: Sending 'results available' email to the following addresses: $OPENDAPNOTIFY."
#      # use asgs sendmail if Operator has set it up 
#      if [[ $OPENDAPMAILSERVER = "aws" ]]; then
#         $SCRIPTDIR/asgs-sendmail.pl --subject "$subject" --to "$OPENDAPNOTIFY" < ${SCENARIODIR}/opendap_results_notify_${server}.txt 2>> ${SYSLOG} 2>&1
#      else
#         cat ${SCENARIODIR}/opendap_results_notify_${server}.txt | mail  -S "replyto=$ASGSADMIN" -s "$subject" $OPENDAPNOTIFY 2>> ${SYSLOG} 2>&1
#      fi
#   fi
done # end loop over opendap servers

