#!/bin/bash
#----------------------------------------------------------------
#
# logging.sh: This file contains functions required for logging.
# It is sourced by asgs_main.sh and any other shell script that
# requires logging capabilities.
#
#----------------------------------------------------------------
# Copyright(C) 2012--2024 Jason Fleming
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
#----------------------------------------------------------------
#
# set the name of the asgs log file
setSyslogFileName()
{
   if [[ ! -d $WORK/log ]]; then
      mkdir -p $WORK/log
      consoleMessage "$I Created log directory '$WORK/log'"
   else
      consoleMessage "$I Found log directory '$WORK/log'"
   fi
   SYSLOG=${SYSLOG:-$WORK/log/${INSTANCENAME}.asgs-${STARTDATETIME}.$$.log}
}
#
# write an INFO-level message to the main asgs log file
logMessage()
{
  MSG="[$(date +'%Y-%h-%d-T%H:%M:%S%z')] INFO: $1"
  for syslogfile in $SYSLOG $2 ; do
    if [[ -f $syslogfile ]]; then
      echo ${MSG} >> $syslogfile
    fi
  done
}
#
# write an INFO-level message to the cycle (or advisory log file)
cycleMessage()
{
  MSG="[$(date +'%Y-%h-%d-T%H:%M:%S%z')] INFO: $1"
  for cyclelogfile in $CYCLELOG $2 ; do
    if [[ -e $cyclelogfile ]]; then
      echo ${MSG} >> $cyclelogfile
    fi
  done
}
#
# write an INFO-level message to the cycle (or advisory log file)
scenarioMessage()
{
  LOGMESSAGE=$1
  EXTRALOGFILE=$2
  MSG="[$(date +'%Y-%h-%d-T%H:%M:%S%z')] INFO: $LOGMESSAGE"
  for scenariologfile in $SCENARIOLOG $EXTRALOGFILE ; do
     if [[ -e $scenariologfile ]]; then
        echo "${MSG}" >> $scenariologfile
     fi
  done
}
#
# write a message to log file associated with a particular script or executable
# (typically debug messages that would normally just clutter up other log files
# but come in very handy for occasional troubleshooting) ... the
# suggested name of the log file is the script or executable name followed by .log
appMessage()
{
  LOGMESSAGE=$1
  APPLOGFILE=$2
  MSG="[$(date +'%Y-%h-%d-T%H:%M:%S%z')] DEBUG: $LOGMESSAGE"
  if [[ -e $RUNDIR ]]; then
     echo ${MSG} >> $APPLOGFILE
  fi
}
#
# send a message to the console (i.e., window where the script was started)
# (these should be strategic to enable real time progress monitoring,
# not forensic troubleshooting or focused debugging)
consoleMessage()
{
  MSG="[$(date +'%Y%m%d-%H%M%S')] $1"
  echo ${MSG}
  if [[ -e $2 ]]; then
     echo ${MSG} >> $2
  fi
}
#
# send INFO message to main asgs log file, cycle (advisory) log file, as well
# as scenario log file
allMessage()
{
#   consoleMessage $@
   logMessage "$1" "$2"
   cycleMessage "$1" "$2"
   scenarioMessage "$1" "$2"
}
#
# log a warning message, execution continues
warn()
{
  MSG="[$(date +'%Y-%h-%d-T%H:%M:%S%z')] WARNING: $1"
  for warnlogfile in $SYSLOG $2 ; do
    if [[ -e $warnlogfile ]]; then
      local frame=0
      while caller $frame; do
         caller $frame >> $warnlogfile
         ((frame++));
      done
      echo ${MSG} >> $warnlogfile
    fi
  done
  #echo ${MSG}  # send to console
}
#
# log an error message, notify Operator
error()
{
  MSG="[$(date +'%Y-%h-%d-T%H:%M:%S%z')] ERROR: $1"
  echo ${MSG}  # send to console
  # added ability for Operator to supply a "local" log file (e.g., postprocess.log)
  for errorlogfile in $SYSLOG $CYCLELOG $SCENARIOLOG $2; do
    if [[ -e $errorlogfile ]]; then
      local frame=0
      while caller $frame; do
         caller $frame >> $errorlogfile
         ((frame++));
      done
      echo ${MSG} >> $errorlogfile
    fi
  done
  # email the operator
  if [[ $EMAILNOTIFY = yes || $EMAILNOTIFY = YES ]]; then
     echo $MSG | asgs-sendmail --subject "[ASGS] Attn: Error for $INSTANCENAME" --to "${ASGSADMIN}"
  fi
}
#
# log an error message, execution halts
fatal()
{
  MSG="[$(date +'%Y-%h-%d-T%H:%M:%S%z')] FATAL ERROR: $1"
  for fatallogfile in $SYSLOG $CYCLELOG $SCENARIOLOG $2; do
    if [[ -e $fatallogfile ]]; then
      local frame=0
      while caller $frame; do
         caller $frame >> $fatallogfile
         ((frame++));
      done
      echo ${MSG} >> $fatallogfile
    fi
  done
  if [[ $EMAILNOTIFY = yes || $EMAILNOTIFY = YES ]]; then
     echo $MSG | asgs-sendmail --subject "[ASGS] Fatal Error for PROCID ($$)" --to "${ASGSADMIN}"
  fi
  echo ${MSG} # send to console
  exit ${EXIT_NOT_OK}
}
#
# log a debug message
debugMessage()
{
  MSG="[$(date +'%Y-%h-%d-T%H:%M:%S%z')] DEBUG: $1"
  for debuglogfile in $SCENARIOLOG $2; do
     if [[ -e $debuglogfile ]]; then
        echo ${MSG} >> $debuglogfile
     fi
  done
}

# includes asgs configuration that is not expected to vary
# between scenarios (mesh, machine, operator, config file, etc)
writeASGSInstanceStatus()
{
    local THIS="asgs_main->monitoring/logging.sh->writeASGSInstanceStatus()"
    local statfile="$statusDir/asgs.instance.status.properties"
    local jsonfile="$statusDir/asgs.instance.status.json"
    local logfile=$(basename -- $SYSLOG)
    local textlog="${logfile%.*}.txt" # directly viewable in web browser
    local STATUSLOG=$RUNDIR/status.log
    echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Writing status associated with ASGS configuration and situation to $statfile." >> $STATUSLOG
    #
    # update time stamp
    echo "time.status.lastupdated : $(date +'%Y-%h-%d-T%H:%M:%S%z')" > $statfile  # <--<< OVERWRITE
    # basic asgs configuration
    echo "config.file : $CONFIG" >> $statfile
    echo "instancename : $INSTANCENAME" >> $statfile
    echo "adcirc.time.coldstartdate : $CSDATE" >> $statfile
    echo "path.adcircdir : $ADCIRCDIR" >> $statfile
    echo "path.scriptdir : $SCRIPTDIR" >> $statfile
    echo "path.inputdir : $INPUTDIR" >> $statfile
    echo "path.outputdir : $OUTPUTDIR" >> $statfile
    echo "path.scratchdir : $SCRATCHDIR" >> $statfile
    echo "forcing.schedule.cycletimelimit : $CYCLETIMELIMIT" >> $statfile
    echo "forcing.tropicalcyclone : $TROPICALCYCLONE" >> $statfile
    echo "forcing.backgroundmet : $BACKGROUNDMET" >> $statfile
    echo "forcing.schedule.forecast.forecastcycle : \"${FORECASTCYCLE}\"" >> $statfile
    echo "coupling.waves : $WAVES" >> $statfile
    # static hpc environment properties
    echo "hpc.hpcenv : $HPCENV" >> $statfile
    echo "hpc.hpcenvshort : $HPCENVSHORT" >> $statfile
    echo "hpc.jobs.ncpucapacity : $NCPUCAPACITY" >> $statfile
    echo "hpc.job.default.account : $ACCOUNT" >> $statfile
    echo "hpc.job.default.queuename : $QUEUENAME" >> $statfile
    echo "hpc.job.default.serqueue : $SERQUEUE" >> $statfile
    # static input files, templates, and property files
    echo "adcirc.file.input.gridfile : $GRIDFILE" >> $statfile
    echo "adcirc.gridname : $GRIDNAME" >> $statfile
    echo "adcirc.file.elevstations : $ELEVSTATIONS" >> $statfile
    echo "adcirc.file.velstations : $VELSTATIONS" >> $statfile
    echo "adcirc.file.metstations : $METSTATIONS" >> $statfile
    # other adcirc specific
    echo "adcirc.hotstartformat : $HOTSTARTFORMAT" >> $statfile
    echo "adcirc.hotstartcomp : $HOTSTARTCOMP" >> $statfile
    # notification
    echo "notification.emailnotify : $EMAILNOTIFY" >> $statfile
    echo "intendedAudience : $INTENDEDAUDIENCE" >> $statfile
    # archiving
    echo "archive.executable.archive : $ARCHIVE" >> $statfile
    echo "archive.path.archivebase : $ARCHIVEBASE" >> $statfile
    echo "archive.path.archivedir : $ARCHIVEDIR" >> $statfile
    # runtime
    echo "path.rundir : $RUNDIR" >> $statfile
    echo "path.statusdir : $statusDir" >> $statfile
    echo "path.lastsubdir : $LASTSUBDIR" >> $statfile
    echo "asgs.instance.status.url : $asgsInstanceStatusURL" >> $statfile
    echo "hook.status.url : $hookStatusURL" >> $statfile
    echo "hook.status.url.previous : $previousHookStatusURL" >> $statfile
    echo "post.opendap.tds : ( ${TDS[@]} )" >> $statfile
    echo "notification.opendap.email.opendapmailserver : $OPENDAPMAILSERVER" >> $statfile
    echo "notification.opendap.email.enable : $notifyNow" >> $statfile
    # the syslog file can get pretty big, I don't think we want to
    # send it every 60 seconds, need to figure out how we can just send
    # the extra increment each time and append on the remote server
    statusFiles="$statusDir/asgs.instance.status.json $statusDir/hook.status.json"
    #statusFiles="asgs.instance.status.json hook.status.json $textlog"
    if [[ $previousHookStatusFile != "null" ]]; then
        statusFiles+=" $statusDir/$previousHookStatusFile"
    fi
    statusFiles+=" sendNotification"
    echo "notification.opendap.email.opendapnotify : $statusNotify" >> $statfile
    echo "post.opendap.files : ( $statusFiles )" >> $statfile
    echo "status.file.previous : $previousStatusFile" >> $statfile
    echo "status.hook.latest : $latestHook" >> $statfile
    echo "monitoring.logging.file.syslog : $SYSLOG" >> $statfile   # for use in opendap_post.sh
    echo "monitoring.logging.file.cyclelog : null" >> $statfile    # for use in opendap_post.sh
    echo "monitoring.logging.file.scenariolog : $statusDir/asgs.instance.status.log" >> $statfile # for use in opendap_post.sh
    echo "scenario : asgs.instance.status" >> $statfile  # for use in opendap_post.sh
    echo "path.advisdir : $RUNDIR/$ADVISORY" >> $statfile             # for use in opendap_post.sh
    echo "advisory : $ADVISORY" >> $statfile             # for use in opendap_post.sh
    echo "InitialHotStartTime : $HSTIME" >> $statfile    # for use in opendap_post.sh
    # forecast scenario package
    echo "forecast.scenariopackagesize : $SCENARIOPACKAGESIZE" >> $statfile
    local myscenarios=$(str="( " ; si=0 ; while [[ $si -lt $SCENARIOPACKAGESIZE ]]; do source $ASGS_CONFIG > /dev/null 2>&1 ; str+="$ENSTORM " ; si=$(($si + 1)) ; done ; str+=")" ; echo $str )
    echo "forecast.scenarios : $myscenarios" >> $statfile
    #
    ADCIRCVERSION=`${ADCIRCDIR}/adcirc -v`
    echo "adcirc.version : $ADCIRCVERSION" >> $statfile
    # convert to asgs.instance.status.json
    $SCRIPTDIR/metadata.pl --redact \
       < $statfile                  \
       > $jsonfile                  \
     2>> $SYSLOG
}
#
# post the asgs instance status and hook status files to opendap
postStatus() {
    local THIS="asgs_main->monitoring/logging.sh->postStatus()"
    local statfile="$statusDir/asgs.instance.status.properties"
    local STATUSLOG=$RUNDIR/status.log
    if [[ -s $statfile ]]; then
        echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Posting status associated with ASGS configuration and hooks in '$statfile' to opendap." >> $STATUSLOG
        # redact username from log file and rename with .txt extension so
        # we can review it directly in a web browser
        local logfile=$(basename -- $SYSLOG)
        textlog="${logfile%.*}.txt"
        sed "s/$USER/\$USER/g" $SYSLOG > $statusDir/$textlog
        # NB: OPENDAPPOST is defined in platforms.sh; also full path to
        # $OPENDAPPOST is needed because of the way it's currently used in
        # the list of $POSTPROCESS scripts defined in the ASGS config being used
        $SCRIPTDIR/output/$OPENDAPPOST $statfile
    else
        logMessage "$THIS: Status file '$statfile' does not exist." $STATUSLOG
    fi
}

initFileStatusMonitoring() {
   #
   #  F I L E   S T A T U S
   #
   declare -a -g fileStatusList
   # water surface elevation and water velocity
   fileStatusList+=( fort.61.nc fort.62.nc fort.63.nc fort.64.nc )
   # barometric pressure and wind velocity
   fileStatusList+=( fort.71.nc fort.72.nc fort.73.nc fort.74.nc )
   # inundation
   fileStatusList+=( initiallydry.63.nc inundationtime.63.nc endrisinginun.63.nc everdried.63.nc maxinundepth.63.nc )
   # adcirc min/max files
   fileStatusList+=( maxele.63.nc maxvel.63.nc maxwvel.63.nc minpr.63.nc )
   # swan files
   fileStatusList+=( swan_TPS.63.nc swan_TMM10.63.nc swan_HS.63.nc swan_DIR.63.nc rads.64.nc )
   # swan max files
   fileStatusList+=( maxrs.63.nc swan_TPS_max.63.nc swan_TMM10_max.63.nc swan_HS_max.63.nc swan_DIR_max.63.nc )
   # files without datasets
   declare -g -a fileStatusCheckList=( scenario.log partmesh.txt )
   # files first updated
   declare -g -A filesFirstTimeUpdated
}

# Executed at the start of a scenario
nullifyFilesFirstTimeUpdated()
{
    local THIS="asgs_main->monitoring/logging.sh->nullifyFilesFirstTimeUpdated()"
    local STATUSLOG=$RUNDIR/status.log
    echo "$THIS: Nullifying the first updated times associated with each output file." >> $STATUSLOG
    for k in ${fileStatusList[@]} ${fileStatusCheckList[@]} ; do
        filesFirstTimeUpdated[$k]="null"
    done
}

# includes asgs configuration that is expected to vary
# between scenarios (file status, job status, etc)
writeScenarioFilesStatus()
{
   local THIS="asgs_main->monitoring/logging.sh->writeScenarioFilesStatus()"
   local fileStatusPath="."
   if [[ $# -gt 0 ]]; then
      fileStatusPath=$1
   fi
   local jsonfile=$fileStatusPath/files.status.json
   local cycleJSON=$(expr $ADVISORY + 0) # strip leading zero (if any) before writing to json format
   #
   # update time stamp
   # write the status associated with each file
   echo "{" > $jsonfile # <-<< OVERWRITE
   echo \""hpc.hpcenv\" : \"$HPCENV\"," >> $jsonfile
   echo \""instancename\" : \"$INSTANCENAME\"," >> $jsonfile
   echo \""cycle\" : $cycleJSON," >> $jsonfile
   echo \""scenario\" : \"$SCENARIO\"," >> $jsonfile
   echo \""time.files.status.lastupdated\" : \"$(date +'%Y-%h-%d-T%H:%M:%S%z')\"," >> $jsonfile
   echo \""files.status\" : {" >> $jsonfile
   # these are all netcdf files
   for f in ${fileStatusList[@]} ; do
      local e="false"
      local found=0       # number of datasets actually found in the file
      local first="null"  # first modification time
      local last="null"   # most recent modification time
      local ne=0          # number of expected datasets (when run is complete)
      if [[ -e $fileStatusPath/$f ]]; then
         e="true"
         # determine number of datasets expected in this file at the end of the run
         if [[ -e $fileStatusPath/run.properties ]]; then
             ne=$(awk -v prop=adcirc.file.output.${f}.numdatasets 'BEGIN { FS=":" } $1~prop { print $2 }' ${fileStatusPath}/run.properties)
             if [[ $ne == "" ]]; then
                # echo "run.properties returned no data"
                ne=0
             fi
         else
             ne=0
         fi
         # number of datasets actually in the file
         found=$(ncdump -h $fileStatusPath/$f | grep currently | grep -Eo [0-9]+)
         if [[ $found == "" ]]; then
            found=0
         fi
         if [[ ${filesFirstTimeUpdated[$f]} == "null" || ${filesFirstTimeUpdated[$f]} == "" || -z ${filesFirstTimeUpdated[$f]} ]]; then
            filesFirstTimeUpdated[$f]=$(date -r $fileStatusPath/$f +'%Y-%h-%d-T%H:%M:%S%z')
         fi
         first=\"${filesFirstTimeUpdated[$f]}\"
         last=\"$(date -r $fileStatusPath/$f +'%Y-%h-%d-T%H:%M:%S%z')\"
      fi
      echo \""$f\" : { \"exists\" : $e, \"numdatasets\" : { \"expected\" : $ne, \"found\" : $found }, \"time.updated\" : { \"first\" : $first, \"last\" : $last } },"  >> $jsonfile
   done
   for f in ${fileStatusCheckList[@]} ; do
      e="false"
      first="null"  # first modification time
      last="null"   # most recent modification time
      local fileSize=0
      if [[ -e $fileStatusPath/$f ]]; then
         e="true"
         if [[ ${filesFirstTimeUpdated[$f]} == "null" || ${filesFirstTimeUpdated[$f]} == "" || -z ${filesFirstTimeUpdated[$f]} ]]; then
            filesFirstTimeUpdated[$f]=$(date -r $fileStatusPath/$f +'%Y-%h-%d-T%H:%M:%S%z')
         fi
         first=\"${filesFirstTimeUpdated[$f]}\"
         last=\"$(date -r $fileStatusPath/$f +'%Y-%h-%d-T%H:%M:%S%z')\"
         fileSize=$(stat --printf='%s' $fileStatusPath/$f)
      fi
      echo -n \""$f\" : { \"exists\" : $e, \"size.bytes\" : $fileSize, \"time.updated\" : { \"first\" : $first, \"last\" : $last } }" >> $jsonfile
      if [[ $f != ${fileStatusCheckList[-1]} ]]; then
         echo "," >> $jsonfile # comma then newline
      else
         echo " " >> $jsonfile # just new line
      fi
   done
   echo "}" >> $jsonfile # end of files.status.json file
   # collect the scenario files and scenario jobs into one json file
   if [[ -e $fileStatusPath/jobs.status ]]; then
      awk -f $SCRIPTDIR/monitoring/scenarioStatus.awk $jsonfile $fileStatusPath/jobs.status > $fileStatusPath/scenario.status.json 2>>$SYSLOG
   else
      cp $jsonfile $fileStatusPath/scenario.status.json
   fi
}
#
# post the asgs instance status and hook status files to opendap
postScenarioStatus() {
    local THIS="asgs_main->monitoring/logging.sh->postScenarioStatus()"
    local STATUSLOG=$RUNDIR/status.log
    scenarioStatusDir=$SCENARIODIR/status
    echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Posting status associated with scenario $SCENARIO in $scenarioStatusDir for opendap service." >> $STATUSLOG
    if [[ ! -d $scenarioStatusDir ]]; then
       mkdir -p $scenarioStatusDir 2>> $SYSLOG
    fi
    cp $SCENARIODIR/scenario.status.json $scenarioStatusDir 2>> $SYSLOG
    cp $SCENARIODIR/run.properties $scenarioStatusDir 2>> $SYSLOG
    # FIXME: need to have a better way of separating the list of
    # files to be posted to opendap on any particular execution of
    # output/opendap_post.sh from the run.properties file
    echo "post.opendap.files : ( $SCENARIODIR/scenario.status.json sendNotification )" >> $scenarioStatusDir/run.properties 2>> $SYSLOG
    # this would seem to count on the behavior that the last value
    # of a property in the file will take precedence; this would fail
    # if some other utility reads properties and writes them back
    # out in random order (issue #1329)
    echo "notification.opendap.email.opendapnotify : null" >> $scenarioStatusDir/run.properties 2>> $SYSLOG
    $SCRIPTDIR/output/$OPENDAPPOST $scenarioStatusDir/run.properties
}
#
#  send message when shutting down on INT and clear all processes
sigint() {
   allMessage "Received Ctrl-C from console.  Shutting ASGS instance $INSTANCENAME down."
   trap - SIGTERM && kill -- -$$ # "untrap" SIGTERM and send SIGTERM to all processes in this process group
   exit 0
}
#
#  send message when shutting down on TERM and clear all processes
sigterm() {
   allMessage "Received SIGTERM. Shutting ASGS instance $INSTANCENAME down."
   trap - SIGTERM && kill -- -$$ # "untrap" SIGTERM and send SIGTERM to all processes in this process group
   exit 0
}

#
# send message when shutting down on EXIT and clear all processes
sigexit() {
   allMessage "Received SIGEXIT.  Shutting ASGS instance $INSTANCENAME down."
   trap - SIGTERM && kill -- -$$ # "untrap" SIGTERM and send SIGTERM to all processes in this process group
   exit 0
}
