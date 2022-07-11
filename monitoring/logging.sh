#!/bin/bash
#----------------------------------------------------------------
#
# logging.sh: This file contains functions required for logging.
# It is sourced by asgs_main.sh and any other shell script that
# requires logging capabilities.
#
#----------------------------------------------------------------
# Copyright(C) 2012--2021 Jason Fleming
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
# Log file will be in the directory where the asgs was executed
#
#
# set up logging so that output from various processes within a scenario
# is also sent to scenario.log file for centralized logging
initCentralizedScenarioLogging() {
   unset logFiles
   unset subshellPIDs
   if [[ "$JOBTYPE" =~ prep || $JOBTYPE = partmesh ]]; then
      logFiles=( fort.6 fort.16 ${JOBTYPE}.out )
   fi
   if [[ $JOBTYPE = padcirc ]]; then
      logFiles=( fort.6 fort.16 adcirc.log ${JOBTYPE}.out )
   fi
   if [[ $JOBTYPE = padcswan ]]; then
      logFiles=( fort.6 fort.16 adcirc.log PE0000/asgs_swan.prt PE0000/Errfile  ${JOBTYPE}.out )
   fi
   #
   # initialize log files if they do not exist so tail doesn't exit immediately
   for file in ${logFiles[*]} ; do
      echo "Initializing $file file." | awk -v level=INFO -v this="logging.sh" -f $SCRIPTDIR/monitoring/timestamp.awk >> scenario.log 2>&1
      if [[ -e $file ]]; then
         rm $file
      fi
      # make a zero length file
      touch $file
      # execute logs monitoring in the background
      (
         tail -f $file >> scenario.log 2>&1
      ) &
      # add this process ID to the list of background subshell jobs
      subshellPIDs+=($!)
   done
   # write the logging PIDs to the run.properties file so they can be
   # cleaned up later
   SUBSHELLPIDSTRING="("
   for string in ${subshellPIDs[*]}; do
      SUBSHELLPIDSTRING="$SUBSHELLPIDSTRING $string"
   done
   SUBSHELLPIDSTRING="$SUBSHELLPIDSTRING )"
   echo "hpc.job.${JOBTYPE}.subshellpids : $SUBSHELLPIDSTRING" >> $STORMDIR/run.properties
}

# terminate centralized logging subshell processes
finalizeCentralizedScenarioLogging() {
   unset subshellPIDs
   # grab list of associated subshell PIDs from run.properties file
   declare -a subshellPIDs=`grep hpc.job.$JOBTYPE.subshellpids run.properties | cut -d':' -f 2- | sed -n 's/^\s*//p'`
   # loop over subshell processes
   for pid in ${subshellPIDs[*]}; do
      # terminate each one
      echo "Terminating previously spawned subshell process ID ${pid}." | awk -v level=INFO -v this=logging.sh -f $SCRIPTDIR/monitoring/timestamp.awk >> scenario.log 2>&1
      kill -TERM $pid 2>&1 | awk -v this=logging.sh -v level=INFO -f $SCRIPTDIR/monitoring/timestamp.awk >> scenario.log 2>&1
   done
   unset subshellPIDs
}

# send SIGTERM to tail processes owned by this Operator that are children
# of init (i.e., process)
# NOTE: this `ps` command is required because it outputs the parent process id, which is
# "1" if an orphan - which is what we are looking to kill here UNLESS we're in a docker
# environment. This is filtered with the "grep -v '/dev/null'" and is needed due to the fact
# that the "official" docker container used for ASGS stays alive via a 'tail -f /dev/null'
# call that can be seen in the /docker-entrypoint.sh script.

findAndClearOrphans() {
   for pid in $(ps -eo pid,ppid,user,command | grep [t]ail | grep -v '/dev/null' | awk -v user=$USER '$3==user && $2==1 { print $1 } '); do
      logMessage "Found orphan 'tail -f' process ID $pid and now clearing it."
      kill $pid
   done
}

function IncrementNCEPCycle()
{
	local DATE='date --utc'
	local inc=21600  # cycle increment in secs

	if [  $# -eq 0 ] ; then
		d=`$DATE +%Y%m%d%H`
		cy=${d:8:2}
		cy=`echo "6*($cy/6)" | bc`
	else
		d=$1
	fi

	if [ ${#d} -lt 10 ] ; then
		echo input date must of YYYYMMDDHH
		exit 1
	fi

	#cy=${d:8:2}
	#cy=`echo "6*($cy/6)" | bc`
	#echo $d, $cy
	#echo Current NCEP Cycle = $cy

	# input YYMMDDHH in epoch seconds
	d1=`$DATE -d "${d:0:4}-${d:4:2}-${d:6:2} ${d:8:2}:00:00" +%s`
	d2=$[$d1+$inc]
	d2=`$DATE -d "1970-01-01 UTC $d2 seconds" +%Y%m%d%H`
	cy=${d2:8:2}
	cy=`echo "6*(${d2:8:2}/6)" | bc`
	d2=`printf "${d2:0:8}%02d" $cy`
	echo $d2
	#echo Next NCEP Cycle = $cy
}

RMQMessageStartup()
{
  _CONFIG=$1
  if [[ ${RMQMessaging_Enable} == "on" && -e "${RMQMessaging_StartupScript}" && -e "${_CONFIG}" ]];
  then
    local DATETIME=`date --utc +'%Y-%h-%d-T%H:%M:%S'`
    CONFIG_DUMP=$(cat "$_CONFIG" | sed '/^#/d' | sed '/^$/d')
    ${RMQMessaging_StartupScript}                      \
         --Uid "$$"                                    \
         --LocationName "${RMQMessaging_LocationName}" \
         --ClusterName "${RMQMessaging_ClusterName}"   \
         --Message "$CONFIG_DUMP"                      \
         --InstanceName "$INSTANCENAME"                \
         --Transmit "${RMQMessaging_Transmit}" >> $SYSLOG 2>&1
  fi
}

# MTYPE EVENT PROCESS STATE MSG PCTCOM
RMQMessage()
{
  # short circuit if not enabled or start up script is not defined or doesn't exist
  if [[ ${RMQMessaging_Enable} == "off" || ! -e "${RMQMessaging_StartupScript}" ]]
  then
    return
  fi

  MTYPE=$1
  EVENT=$2
  PROCESS=$3
  STATE=$4
  MSG=$5
  local DATETIME=`date --utc +'%Y-%h-%d-T%H:%M:%S'`
  #MSG="RMQ-$MTYPE : $EVENT : $STATE : ${DATETIME} : $MSG"
  PCTCOM=0

  if [ "$#" -eq 6 ]
  then
    PCTCOM=$6
  fi

  # adding log file specific to RMQMessaging to augment and eventually maybe
  # replace echoing messages to the console


  APPLOGFILE=$RUNDIR/RMQMessaging.log

  if [[ 10#$RMQADVISORY -lt 0 ]]
  then
	echo "warn: RMQA ($RMQADVISORY) < 0.  Not sending message ..."
	appMessage "warn: RMQA ($RMQADVISORY) < 0.  Not sending message ..." $APPLOGFILE
	return
  fi

  re='^[0-9]+([.][0-9]+)?$'
  if ! [[ $PCTCOM =~ $re ]]
  then
      echo "warn: PCTCOM ($PCTCOM) not a number in RMQMessage.  Not sending message ..."
      appMessage "warn: PCTCOM ($PCTCOM) not a number in RMQMessage.  Not sending message ..." $APPLOGFILE
  else
     printf "RMQ : %s : %10s : %4s : %4s : %21s : %4s : %5.1f : %s : %s\n" ${INSTANCENAME} ${RMQADVISORY} ${MTYPE} ${EVENT} ${DATETIME} ${STATE} ${PCTCOM} ${PROCESS} "${MSG}"

     # Send message to RabbitMQ queue.  The queue parameters are in the asgs_msgr.py code
     ${RMQMessaging_Script} \
         --Uid "$$"                                    \
         --LocationName "${RMQMessaging_LocationName}" \
         --ClusterName "${RMQMessaging_ClusterName}"   \
         --StormNumber "$STORM"                        \
         --StormName "$STORMNAME"                      \
         --AdvisoryNumber "$RMQADVISORY"               \
         --Message "$MSG"                              \
         --EventType "$EVENT"                          \
         --Process "$PROCESS"                          \
         --PctComplete 0                               \
         --SubPctComplete "$PCTCOM"                    \
         --State "$STATE"                              \
         --RunParams "$RMQRunParams"                   \
         --InstanceName "$INSTANCENAME"                \
         --Transmit "${RMQMessaging_Transmit}" >> ${SYSLOG} 2>&1
   fi
   unset re
}

# send run.properties as a message to the asgs monitor queue
RMQMessageRunProp()
{
  if [[ ${RMQMessaging_Enable} == "off" || ! -e "${RMQMessaging_Script_RP}" ]]
  then
    estr="warn: RMQMessageRunProp did not send rp message because either RMQMessaging_Enable is off or RMQMessaging_Script_RP not found. "
	echo $estr
	allMessage $estr
    return 1
  fi

  RPDIR=$1
  ASGS_PID=$2
  local DATETIME=`date --utc +'%Y-%h-%d-T%H:%M:%S'`

  # adding log file specific to RMQMessaging to augment and eventually maybe
  # replace echoing messages to the console

  APPLOGFILE=$RUNDIR/RMQMessaging.log

  ${RMQMessaging_Script_RP} \
     --Uid $ASGS_PID \
     --LocationName "${RMQMessaging_LocationName}" \
     --InstanceName "$INSTANCENAME"                \
     --Transmit "${RMQMessaging_Transmit}"         \
     --input_filename "$RPDIR/run.properties"      \
     --output_filename "$RPDIR/run.properties.json" >> ${SYSLOG} 2>&1

  if [ $? == 0 ] ; then
    allMessage "${RMQMessaging_Script_RP} returned a 0 status."
  else
    allMessage "${RMQMessaging_Script_RP} returned a non-0 status, with these parameters: $ASGS_PID, $INSTANCENAME, $RPDIR, ${RMQMessaging_Transmit}"
  fi

}

# set the name of the asgs log file
setSyslogFileName()
{
   local DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
   if [[ ! -d $WORK/log ]]; then
      mkdir -p $WORK/log
      echo "[$DATETIME] INFO: Created subdirectory for ASGS log files : '$WORK/log'."
   else
      echo "[$DATETIME] INFO: Found subdirectory for ASGS log files : '$WORK/log'."
   fi
   SYSLOG=${SYSLOG:-$WORK/log/${INSTANCENAME}.asgs-${STARTDATETIME}.$$.log}
   consoleMessage "Set ASGS log file parameter SYSLOG to '$SYSLOG'." $SYSLOG
}
#
# write an INFO-level message to the main asgs log file
logMessage()
{ local DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] INFO: $1"
  for syslogfile in $SYSLOG $2 ; do
    if [[ -f $syslogfile ]]; then
      echo ${MSG} >> $syslogfile
    fi
  done
}
#
# write an INFO-level message to the cycle (or advisory log file)
cycleMessage()
{ local DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] INFO: $1"
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
  local DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] INFO: $LOGMESSAGE"
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
  local DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] DEBUG: $LOGMESSAGE"
  if [[ -e $RUNDIR ]]; then
     echo ${MSG} >> $APPLOGFILE
  fi
}
#
# send a message to the console (i.e., window where the script was started)
# (these should be rare)
consoleMessage()
{ local DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] ATTN: $1"
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
   logMessage $1 $2
   cycleMessage $1 $2
   scenarioMessage $1 $2
}
#
# log a warning message, execution continues
warn()
{ local DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] WARNING: $1"
  for warnlogfile in $SYSLOG $CYCLELOG $SCENARIOLOG $2 ; do
    if [[ -e $warnlogfile ]]; then
      caller 1 >> $warnlogfile
      echo ${MSG} >> $warnlogfile
    fi
  done
  #echo ${MSG}  # send to console
}
#
# log an error message, notify Operator
error()
{ local DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] ERROR: $1"
  echo ${MSG}  # send to console
  # added ability for Operator to supply a "local" log file (e.g., postprocess.log)
  for errorlogfile in $SYSLOG $CYCLELOG $SCENARIOLOG $2; do
    if [[ -e $errorlogfile ]]; then
      caller 1 >> $errorlogfile
      echo ${MSG} >> $errorlogfile
    fi
  done
  # email the operator
  if [[ $EMAILNOTIFY = yes || $EMAILNOTIFY = YES ]]; then
     echo $MSG | mail -s "[ASGS] Attn: Error for $INSTANCENAME" "${ASGSADMIN}"
  fi
}
#
# log an error message, execution halts
fatal()
{ local DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] FATAL ERROR: $1"
  for fatallogfile in $SYSLOG $CYCLELOG $SCENARIOLOG $2; do
    if [[ -e $fatallogfile ]]; then
      echo ${MSG} >> $fatallogfile
    fi
  done
  if [[ $EMAILNOTIFY = yes || $EMAILNOTIFY = YES ]]; then
     echo $MSG | mail -s "[ASGS] Fatal Error for PROCID ($$)" "${ASGSADMIN}"
  fi
  echo ${MSG} # send to console
  exit ${EXIT_NOT_OK}
}
#
# log a debug message
debugMessage()
{ local DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] DEBUG: $1"
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
    local jsonfile="asgs.instance.status.json"
    logMessage "$THIS: Writing status associated with ASGS configuration and situation to $statfile."
    local logfile=$(basename -- $SYSLOG)
    local textlog="${logfile%.*}.txt" # directly viewable in web browser
    #
    # update time stamp
    dateTime=`date +'%Y-%h-%d-T%H:%M:%S%z'`
    echo "time.status.lastupdated : $dateTime" > $statfile  # <--<< OVERWRITE
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
    # monitoring (includes logging)
    echo "monitoring.rmqmessaging.enable : $RMQMessaging_Enable " >> $statfile
    echo "monitoring.rmqmessaging.transmit : $RMQMessaging_Transmit" >> $statfile
    # archiving
    echo "archive.executable.archive : $ARCHIVE" >> $statfile
    echo "archive.path.archivebase : $ARCHIVEBASE" >> $statfile
    echo "archive.path.archivedir : $ARCHIVEDIR" >> $statfile
    # runtime
    echo "path.rundir : $RUNDIR" >> $statfile
    echo "path.statusdir : $RUNDIR/status" >> $statfile
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
    statusFiles="asgs.instance.status.json hook.status.json"
    #statusFiles="asgs.instance.status.json hook.status.json $textlog"
    if [[ $previousHookStatusFile != "null" ]]; then
        statusFiles+=" $previousHookStatusFile"
    fi
    echo "notification.opendap.email.opendapnotify : $statusNotify" >> $statfile
    echo "post.opendap.files : ( $statusFiles )" >> $statfile
    echo "status.file.previous : $previousStatusFile" >> $statfile
    echo "status.hook.latest : $latestHook" >> $statfile
    echo "monitoring.logging.file.syslog : $SYSLOG" >> $statfile   # for use in opendap_post.sh
    echo "monitoring.logging.file.cyclelog : null" >> $statfile    # for use in opendap_post.sh
    echo "monitoring.logging.file.scenariolog : asgs.instance.status.log" >> $statfile # for use in opendap_post.sh
    echo "scenario : asgs.instance.status" >> $statfile  # for use in opendap_post.sh
    echo "path.advisdir : $RUNDIR/$ADVISORY" >> $statfile             # for use in opendap_post.sh
    echo "advisory : $ADVISORY" >> $statfile             # for use in opendap_post.sh
    echo "InitialHotStartTime : $HSTIME" >> $statfile    # for use in opendap_post.sh
    # forecast scenario package
    echo "forecast.scenariopackagesize : $SCENARIOPACKAGESIZE" >> $statfile
    local myscenarios=$(str="( " ; si=0 ; while [[ $si -lt $SCENARIOPACKAGESIZE ]]; do source $ASGS_CONFIG ; str+="$ENSTORM " ; si=$(($si + 1)) ; done ; str+=")" ; echo $str )
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
    statfile="$statusDir/asgs.instance.status.properties"
    if [[ -s $statfile ]]; then
        logMessage "$THIS: Posting status associated with ASGS configuration and hooks in '$statfile' to opendap."
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
        logMessage "$THIS: Status file '$statfile' does not exist."
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
    logMessage "$THIS: Nullifying the first updated times associated with each output file."
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
   dateTime=`date +'%Y-%h-%d-T%H:%M:%S%z'`
   # write the status associated with each file
   echo "{" > $jsonfile # <-<< OVERWRITE
   echo \""hpc.hpcenv\" : \"$HPCENV\"," >> $jsonfile
   echo \""instancename\" : \"$INSTANCENAME\"," >> $jsonfile
   echo \""cycle\" : $cycleJSON," >> $jsonfile
   echo \""scenario\" : \"$SCENARIO\"," >> $jsonfile
   echo \""time.files.status.lastupdated\" : \"$dateTime\"," >> $jsonfile
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
    scenarioStatusDir=$SCENARIODIR/status
    logMessage "$THIS: Posting status associated with scenario $SCENARIO in $scenarioStatusDir for opendap service."
    if [[ ! -d $scenarioStatusDir ]]; then
       mkdir -p $scenarioStatusDir 2>> $SYSLOG
    fi
    cp $SCENARIODIR/scenario.status.json $scenarioStatusDir 2>> $SYSLOG
    cp $SCENARIODIR/run.properties $scenarioStatusDir 2>> $SYSLOG
    # FIXME: need to have a better way of separating the list of
    # files to be posted to opendap on any particular execution of
    # output/opendap_post.sh from the run.properties file
    echo "post.opendap.files : ( scenario.status.json )" >> $scenarioStatusDir/run.properties 2>> $SYSLOG
    echo "notification.opendap.email.enable : no" >> $scenarioStatusDir/run.properties 2>> $SYSLOG
    $SCRIPTDIR/output/$OPENDAPPOST $scenarioStatusDir/run.properties
}
#
#  send message when shutting down on INT and clear all processes
sigint() {
   RMQMessage "EXIT" "EXIT" "asgs_main.sh>sigint()" "EXIT" "Received Ctrl-C from console.  Shutting ASGS down ..."
   allMessage "Received Ctrl-C from console.  Shutting ASGS instance $INSTANCENAME down."
   trap - SIGTERM && kill -- -$$ # "untrap" SIGTERM and send SIGTERM to all processes in this process group
   exit 0
}
#
#  send message when shutting down on TERM and clear all processes
sigterm() {
   RMQMessage "EXIT" "EXIT" "asgs_main.sh>sigterm()" "EXIT" "Received SIGTERM.  Shutting ASGS down ..."
   allMessage "Received SIGTERM. Shutting ASGS instance $INSTANCENAME down."
   trap - SIGTERM && kill -- -$$ # "untrap" SIGTERM and send SIGTERM to all processes in this process group
   exit 0
}

#
# send message when shutting down on EXIT and clear all processes
sigexit() {
   RMQMessage "EXIT" "EXIT" "asgs_main.sh>sigexit()" "EXIT" "Received SIGEXIT.  Shutting ASGS down ..."
   allMessage "Received SIGEXIT.  Shutting ASGS instance $INSTANCENAME down."
   trap - SIGTERM && kill -- -$$ # "untrap" SIGTERM and send SIGTERM to all processes in this process group
   exit 0
}
