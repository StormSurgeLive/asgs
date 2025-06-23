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
nullifyHooks()
{
    local THIS="asgs_main->manageHooks->nullifyHooks()"
    local STATUSLOG=$RUNDIR/status.log
    if [[ $RUNDIR == "null" ]]; then
        STATUSLOG=$WORK/log/status.log
    fi
    echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Nullifying the time values associated with each hook." >> $STATUSLOG
    for k in ${allHooks[@]} ; do
        hooksTimes[$k]="null"
        echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Setting hooksTimes['$k'] to '${hooksTimes[$k]}'." >> $STATUSLOG
    done
}
#
# nullify just the nowcast and forecast hook times;
# executed when a new nowcast/forecast cycle starts
nullifyNowcastForecastHooks()
{
    local THIS="asgs_main->manageHooks->nullifyNowcastForecastHooks()"
    local STATUSLOG=$RUNDIR/status.log
    if [[ $RUNDIR == "null" ]]; then
        STATUSLOG=$WORK/log/status.log
    fi
    echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Nullifying the time values associated with each nowcast and forecast hook." >> $STATUSLOG
    for k in "${nowcastHooks[@]}" "${forecastHooks[@]}" ; do
        hooksTimes["$k"]="null"
        echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Setting hooksTimes['$k'] to '${hooksTimes[$k]}'." >> $STATUSLOG
    done
}
#
# set (or append) timestamp to hooksTimes entry
timestampHook()
{
    hook=$1
    local THIS="asgs_main->manageHooks->timestampHook()"
    local STATUSLOG=$RUNDIR/status.log
    if [[ $RUNDIR == "null" ]]; then
        STATUSLOG=$WORK/log/status.log
    fi
    echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Updating timestamp for the '$hook' hook." >> $STATUSLOG
    local status="null"
    local statusURL="null"
    case $hook in
    "START_INIT")
        mypath="null"
        ;;
    "FINISH_INIT")
        mypath="status"
        ;;
    "START_SPINUP_STAGE"|"HOT_SPINUP"|"FINISH_SPINUP_STAGE")
        mypath="initialize"
        ;;
    "BUILD_SPINUP"|"SUBMIT_SPINUP"|"FINISH_SPINUP_SCENARIO")
        mypath="initialize/hindcast"
        ;;
    "START_NOWCAST_STAGE"|"NOWCAST_POLLING")
        mypath="status"
        ;;
    "NOWCAST_TRIGGERED"|"BUILD_NOWCAST_SCENARIO"|"SUBMIT_NOWCAST_SCENARIO"|"FINISH_NOWCAST_SCENARIO")
        mypath="$ADVISORY/nowcast"
        ;;
    "FINISH_NOWCAST_STAGE"|"START_FORECAST_STAGE")
        mypath="$ADVISORY"
        ;;
    "INITIALIZE_FORECAST_SCENARIO"|"CAPACITY_WAIT"|"BUILD_FORECAST_SCENARIO"|"SUBMIT_FORECAST_SCENARIO")
        mypath="$ADVISORY/$SCENARIO"
        ;;
    "FINISH_FORECAST_STAGE")
        mypath=$ADVISORY
        ;;
    "EXIT_STAGE")
        mypath="status"
        ;;
    *)
        warn "$THIS: Unrecognized hook '${hooksTimes[$hook]}'."
        ;;
    esac
    # TODO: go to that directory and get the statusURL from the status.json file
    # and put the statusURL into this status.json file
    for file in cycle.status.json scenario.status.json ; do
        if [[ $mypath != "null" && -e $RUNDIR/$mypath/$file ]]; then
           status=\"${mypath}/${file}\"
           # the URL should be in the $SCENARIODIR/status/run.properties file
           # as the downloadurl property
           if [[ $file == "scenario.status.json" && -e $RUNDIR/$mypath/status/run.properties ]]; then
               local downloadurl=$(grep downloadurl $RUNDIR/$mypath/status/run.properties)
               if [[ ! -z $downloadurl && $downloadurl != "" ]]; then
                   statusURL=\"${downloadurl##downloadurl : }/$file\"
               fi
           fi
        fi
    done
    if [[ $mypath != "null" ]]; then
        mypath=\"$mypath\"
    fi
    json="{ \"time\" : \"$(date +'%Y-%h-%d-T%H:%M:%S%z')\", \"path\" : $mypath, \"statusfile\" : $status, \"statusURL\" : $statusURL }"
    # determine number of spaces required to get the status objects to line up
    longestHookKey=0
    for h in ${allHooks[@]} ; do
        if [[ ${#h} -gt $longestHookKey ]]; then longestHookKey=${#h} ; fi
    done
    len=$[ 11 + $longestHookKey ]
    if [[ ${hooksTimes[$hook]} == "null" ]]; then
        firstlen=$[ $longestHookKey - ${#hook} ]
        printf -v spaces "%*s%s" $firstlen " "
        if [[ ${#hook} -eq $longestHookKey ]]; then spaces="" ; fi
        hooksTimes[$hook]="$spaces$json"   # nuke out the null entry
    else
        printf -v spaces "%*s%s" $len " "
        hooksTimes[$hook]+=",\n$spaces$json"  # add it to the list
    fi
    latestHook=$hook  # to be written into the status file
}
#
writeHookStatus()
{
    local THIS="asgs_main->manageHooks.sh->writeHookStatus()"
    local STATUSLOG=$RUNDIR/status.log
    local jsonfile="$statusDir/hook.status.json"
    echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Writing status associated with ASGS hooks to '$jsonfile'." >> $STATUSLOG
    #
    # write the time value(s) associated with each hook; will be null
    # if that hook has not been reached for this cycle
    echo "{" > $jsonfile # <-<< OVERWRITE
    echo \""hpc.hpcenv\" : \"$HPCENV\"," >> $jsonfile
    echo \""instancename\" : \"$INSTANCENAME\"," >> $jsonfile
    echo \""path.rundir\" : \"$RUNDIR\"," >> $jsonfile
    echo \""path.scriptdir\" : \"$SCRIPTDIR\"," >> $jsonfile
    if [[ $LASTSUBDIR != "null" ]]; then
       echo \""path.lastsubdir\" : \"$LASTSUBDIR\"," >> $jsonfile
    else
       echo \""path.lastsubdir\" : null," >> $jsonfile
    fi
    echo \""monitoring.logging.file.syslog\" : \"$SYSLOG\"," >> $jsonfile
    echo \""config.file\" : \"$CONFIG\"," >> $jsonfile
    echo "\"monitoring.hook\" : {" >> $jsonfile
    for k in ${allHooks[@]} ; do
        comma="," ; if [[ $k == "EXIT_STAGE" ]]; then comma="" ; fi
        if [[ ${hooksTimes[$k]} != "null" ]]; then
            echo -n "    \"$k\" : [ "           >> $jsonfile
            echo -e "${hooksTimes[$k]} ]$comma" >> $jsonfile
        else
            json="    \"$k\" : [ { \"time\" : null,  \"path\" : null, \"statusfile\" : null, \"statusURL\" : null } ]$comma"
            echo "$json" >> $jsonfile
        fi
    done
    echo "}," >> $jsonfile
    # update time stamp
    echo "\"asgs.instance.status.file\" : \"asgs.instance.status.json\"," >> $jsonfile
    echo "\"asgs.instance.status.url\" : \"$asgsInstanceStatusURL\","   >> $jsonfile
    echo "\"time.hook.status.lastupdated\" : \"$(date +'%Y-%h-%d-T%H:%M:%S%z')\"," >> $jsonfile
    echo "\"hook.status.url\" : \"$hookStatusURL\","                    >> $jsonfile
    echo "\"hook.status.file.previous\" : \"$previousHookStatusFile\"," >> $jsonfile
    echo "\"hook.status.url.previous\" : \"$previousHookStatusURL\","   >> $jsonfile
    echo "\"hook.status.latest\" : \"$latestHook\""                     >> $jsonfile
    echo "}"                                                            >> $jsonfile
    # redact the username for security
    sed --in-place "s/$USER/\$USER/g" $jsonfile
}
#
# execute hook scripts
executeHookScripts()
{
    hook=$1
    local THIS="asgs_main->manageHooks->executeHookScripts()"
    local STATUSLOG=$RUNDIR/status.log
    timestampHook $hook
    if [[ $hook != "START_INIT" ]]; then
        echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Executing scripts for the $hook hook." >> $STATUSLOG
        consoleMessage "$I ${hook}"
        # write status immediately, in case one of the hook scripts
        # wants to post the status file somewhere
        # (can't write status file immediately for the START_INIT script
        # b/c RUNDIR not established yet)
        if [[ $hook == "FINISH_INIT" ]]; then
            notifyNow=$enableStatusNotify
        else
            notifyNow="no"
        fi
        writeASGSInstanceStatus
        writeHookStatus
        if [[ $enablePostStatus == "yes" ]]; then
            postStatus
        fi
    fi
    for hs in ${hooksScripts[$hook]} ; do
        if [[ $hook != "START_INIT" ]]; then
           echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Executing $hook hook script $SCRIPTDIR/$hs." >> $STATUSLOG
        fi
        $SCRIPTDIR/$hs >> ${SYSLOG} 2>&1
    done
    if [[ $hook == "FINISH_FORECAST_STAGE" ]]; then
        previousHookStatusFile=${ADVISORY}.hook.status.json
        previousHookStatusURL=$hookStatusURL
        mv $RUNDIR/status/hook.status.json $RUNDIR/status/$previousHookStatusFile 2>> $SYSLOG
        nullifyNowcastForecastHooks # clears out the timestamps and statuses of these hooks
    fi
}
#
# generic routine to add a script to a hook
#
# sample usage:
#
#  addHookScript FINISH_NOWCAST_SCENARIO output/includeWind10m.sh
#
addHookScript()
{
    local hook="$1"
    local hookScript="$2"
    local THIS="asgs_main->manageHooks->addHookScript()"
    if [[ ! -e "$hookScript" ]]; then
        consoleMessage "$W ${THIS}: The '$hook' hook script '$hookScript' was not found."
    else
        logMessage "$I ${THIS}: Adding the '$hook' hook script '$hookScript'."
        hooksScripts["$hook"]+=" $hookScript"
    fi
}
#
# generic routine to remove a script from a hook
#
# sample usage:
#
#  removeHookScript FINISH_NOWCAST_SCENARIO output/includeWind10m.sh
#
removeHookScript()
{
    local hook="$1"
    local hookScript="$2"
    local s=""          # list of scripts for this hook
    local THIS="asgs_main->manageHooks->removeHookScript()"
    for k in ${hookScripts["$hook"]}; do
        if [[ $k != "$hookScript" ]]; then
            s+=" $k"
        fi
    done
    hookScripts["$hook"]="$s"
}
#
# hook-specific routine to add a hook script
#
# sample usage:
#
#    addScriptTo_FINISH_NOWCAST_SCENARIO output/includeWind10m.sh
#
addScriptTo_FINISH_NOWCAST_SCENARIO()
{
    hookScript="$1"
    local THIS="asgs_main->manageHooks->addScriptTo_FINISH_NOWCAST_SCENARIO()"
    if [[ ! -e "$hookScript" ]]; then
        consoleMessage "$W ${THIS}: The FINISH_NOWCAST_SCENARIO hook script '$hookScript' was not found."
    else
        logMessage "$I ${THIS}: Adding the FINISH_NOWCAST_SCENARIO hook script '$hookScript'."
        hooksScripts["FINISH_NOWCAST_SCENARIO"]+=" $hookScript"
    fi
}
#
# hook-specific routine to remove a hook script
#
# sample usage:
#
#    removeScriptFrom_FINISH_NOWCAST_SCENARIO output/includeWind10m.sh
#
removeScriptFrom_FINISH_NOWCAST_SCENARIO()
{
    hookScript="$1"
    local s=""          # list of scripts for this hook
    local THIS="asgs_main->manageHooks->removeScriptFrom_FINISH_NOWCAST_SCENARIO()"
    for k in ${hookScripts["FINISH_NOWCAST_SCENARIO"]}; do
        if [[ $k != "$hookScript" ]]; then
            s+=" $k"
        fi
    done
    hookScripts["FINISH_NOWCAST_SCENARIO"]="$s"
}

#
# list of hook-specific routines
#

addScriptTo_FINISH_INIT()
{}
removeScriptFrom_FINISH_INIT()
{}

addScriptTo_START_SPINUP_STAGE()
{}
removeScriptFrom_START_SPINUP_STAGE()
{}

addScriptTo_HOT_SPINUP()
{}
removeScriptFrom_HOT_SPINUP()
{}

addScriptTo_FINISH_SPINUP_STAGE()
{}
removeScriptFrom_FINISH_SPINUP_STAGE()
{}

addScriptTo_BUILD_SPINUP()
{}
removeScriptFrom_BUILD_SPINUP()
{}

addScriptTo_SUBMIT_SPINUP()
{}
removeScriptFrom_SUBMIT_SPINUP()
{}

addScriptTo_FINISH_SPINUP_SCENARIO()
{}
removeScriptFrom_FINISH_SPINUP_SCENARIO()
{}

addScriptTo_START_NOWCAST_STAGE()
{}
removeScriptFrom_START_NOWCAST_STAGE()
{}

addScriptTo_NOWCAST_POLLING()
{}
removeScriptFrom_NOWCAST_POLLING()
{}

addScriptTo_NOWCAST_TRIGGERED()
{}
removeScriptFrom_NOWCAST_TRIGGERED()
{}

addScriptTo_BUILD_NOWCAST_SCENARIO()
{}
removeScriptFrom_BUILD_NOWCAST_SCENARIO()
{}

addScriptTo_SUBMIT_NOWCAST_SCENARIO()
{}
removeScriptFrom_SUBMIT_NOWCAST_SCENARIO()
{}

addScriptTo_FINISH_NOWCAST_SCENARIO()
{}
removeScriptFrom_FINISH_NOWCAST_SCENARIO()
{}

addScriptTo_FINISH_NOWCAST_STAGE()
{}
removeScriptFrom_FINISH_NOWCAST_STAGE()
{}

addScriptTo_START_FORECAST_STAGE()
{}
removeScriptFrom_START_FORECAST_STAGE()
{}

addScriptTo_INITIALIZE_FORECAST_SCENARIO()
{}
removeScriptFrom_INITIALIZE_FORECAST_SCENARIO()
{}

addScriptTo_CAPACITY_WAIT()
{}
removeScriptFrom_CAPACITY_WAIT()
{}

addScriptTo_BUILD_FORECAST_SCENARIO()
{}
removeScriptFrom_BUILD_FORECAST_SCENARIO()
{}

addScriptTo_SUBMIT_FORECAST_SCENARIO()
{}
removeScriptFrom_SUBMIT_FORECAST_SCENARIO()
{}

addScriptTo_FINISH_FORECAST_STAGE()
{}
removeScriptFrom_FINISH_FORECAST_STAGE()
{}

addScriptTo_EXIT_STAGE()
{}
removeScriptFrom_EXIT_STAGE()
{}