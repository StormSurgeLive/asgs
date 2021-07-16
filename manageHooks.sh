#!/bin/bash
#----------------------------------------------------------------
# writeStatus.sh: Writes configuration, ASGS instance status,
# and resource status info to metadata file.
#----------------------------------------------------------------
# Copyright(C) 2021 Jason Fleming
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
# Executed upon startup initialization
nullifyHooksTimes()
{
    local THIS="asgs_main->manageHooks->nullifyHooksTimes()"
    logMessage "$THIS: Nullifying the time values associated with each hook."
    for k in "${startFinishHooks[@]}" "${spinupHooks[@]}" "${nowcastHooks[@]}" "${forecastHooks[@]}"  ; do
        hooksTimes["$k"]="null"
    done
}
# nullify just the nowcast and forecast hook times;
# executed when a new nowcast/forecast cycle starts
nullifyNowcastForecastHooksTimes()
{
    local THIS="asgs_main->manageHooks->nullifyNowcastForecastHooksTimes()"
    logMessage "$THIS: Nullifying the time values associated with each nowcast and forecast hook."
    for k in "${nowcastHooks[@]}" "${forecastHooks[@]}" ; do
        hooksTimes["$k"]="null"
    done
}
#
# set (or append) timestamp to hooksTimes entry
timestampHook()
{
    hook=$1
    local THIS="asgs_main->manageHooks->timestampHook()"
    logMessage "$THIS: Updating timestamp for the $hook hook."
    dateTime=`date +'%Y-%h-%d-T%H:%M:%S%z'`
    if [[ ${hooksTimes[$hook]} = "null" ]]; then
        hooksTimes[$hook]=$dateTime  # nuke out the null entry
    else
        hooksTimes[$hook]+=" $dateTime"
    fi
    latestHook=$hook  # to be written into the status file

}
# includes asgs configuration that is not expected to vary
# between scenarios (mesh, machine, operator, config file, etc)
writeASGSInstanceStatus()
{
    local THIS="asgs_main->monitorinbg/writeStatus.sh->writeASGSInstanceStatus()"
    statfile="$statusDir/asgs.instance.status.properties"
    logMessage "$THIS: Writing status associated with ASGS configuration and situation to $statfile."
    #
    # update time stamp
    dateTime=`date +'%Y-%h-%d-T%H:%M:%S%z'`
    echo "time.status.last.updated : $dateTime" > $statfile  # <--<< OVERWRITE
    echo "status.file.previous : $previousStatusFile" >> $statfile
    echo "status.hook.latest : $latestHook" >> $statfile
    # basic asgs configuration
    echo "config.file : $CONFIG" >> $statfile
    echo "instancename : $INSTANCENAME" >> $statfile
    echo "operator : $operator" >> $statfile
    echo "adcirc.time.coldstartdate : $CSDATE" >> $statfile
    echo "path.adcircdir : $ADCIRCDIR" >> $statfile
    echo "path.scriptdir : $SCRIPTDIR" >> $statfile
    echo "path.inputdir : $INPUTDIR" >> $statfile
    echo "path.outputdir : $OUTPUTDIR" >> $statfile
    echo "path.scratchdir : $SCRATCHDIR" >> $statfile
    echo "forcing.schedule.cycletimelimit : $CYCLETIMELIMIT" >> $statfile
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
    echo "adcirc.file.input.nafile : $NAFILE" >> $statfile
    echo "adcirc.file.template.controltemplate : $CONTROLTEMPLATE" >> $statfile
    echo "adcirc.file.elevstations : $ELEVSTATIONS" >> $statfile
    echo "adcirc.file.velstations : $VELSTATIONS" >> $statfile
    echo "adcirc.file.metstations : $METSTATIONS" >> $statfile
    # other adcirc specific
    echo "adcirc.hotstartformat : $HOTSTARTFORMAT" >> $statfile
    echo "adcirc.timestepsize : $TIMESTEPSIZE" >> $statfile
    echo "adcirc.hotstartcomp : $HOTSTARTCOMP" >> $statfile
    # notification
    echo "notification.emailnotify : $EMAILNOTIFY" >> $statfile
    echo "notification.email.asgsadmin : $ASGSADMIN" >> $statfile
    # monitoring (includes logging)
    echo "monitoring.rmqmessaging.enable : $RMQMessaging_Enable " >> $statfile
    echo "monitoring.rmqmessaging.transmit : $RMQMessaging_Transmit" >> $statfile
    # archiving
    echo "archive.executable.archive : $ARCHIVE" >> $statfile
    echo "archive.path.archivebase : $ARCHIVEBASE" >> $statfile
    echo "archive.path.archivedir : $ARCHIVEDIR" >> $statfile
    # runtime
    echo "path.rundir : $RUNDIR" >> $statfile
    # forecast scenario package size
    echo "forecast.scenariopackagesize : $SCENARIOPACKAGESIZE" >> $STORMDIR/run.properties
    #
    ADCIRCVERSION=`${ADCIRCDIR}/adcirc -v`
    echo "adcirc.version : $ADCIRCVERSION" >> $statfile
    #
    # write the time value(s) associated with each hook; will be null
    # if that hook has not been reached for this cycle
    for k in "${!hooksTimes[@]}" ; do
        echo "time.monitoring.hook.$k : ( $hooksTimes[$k] )" >> $statfile
    done
    # convert to scenario.json
    $SCRIPTDIR/metadata.pl --jsonify --metadatafile $statfile
}
#
# execute hook scripts
executeHookScripts()
{
    hook=$1
    local THIS="asgs_main->manageHooks->executeHookScripts()"
    timestampHook $hook
    if [[ $hook != "START_INIT" ]]; then
        logMessage "$THIS: Executing scripts for the $hook hook."
        # write status immediately, in case one of the hook scripts
        # wants to post the status file somewhere
        # (can't write status file immediately for the START_INIT script
        # b/c RUNDIR not established yet)
        writeASGSInstanceStatus
    fi
    for hs in ${hooksScripts[$hook]} ; do
        logMessage "$THIS: Executing $hook hook script $SCRIPTDIR/$hs."
        $SCRIPTDIR/$hs >> ${SYSLOG} 2>&1
    done
}