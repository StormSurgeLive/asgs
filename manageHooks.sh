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
    for k in ${initHooks[@]} ${spinupHooks[@]} ${nowcastHooks[@]} ${forecastHooks[@]} EXIT_STAGE ; do
        hooksTimes[$k]="null"
        logMessage "$THIS: Setting hooksTimes[$k] to ${hooksTimes[$k]}"
    done
    logMessage "There are ${#hooksTimes[@]} elements in hooksTimes."
}
#
# Executed upon startup initialization
nullifyHooksScenarios()
{
    local THIS="asgs_main->manageHooks->nullifyHooksScenarios()"
    logMessage "$THIS: Nullifying the time values associated with each hook."
    for k in ${spinupHooks[@]} ${nowcastHooks[@]} ${forecastHooks[@]}  ; do
        hooksScenarios[$k]="null"
        logMessage "$THIS: Setting hooksScenarios[$k] to ${hooksScenarios[$k]}"
    done
    logMessage "There are ${#hooksScenarios[@]} elements in hooksScenarios."
}
#
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
    if [[ ${hooksTimes[$hook]} == "null" ]]; then
        hooksTimes[$hook]=$dateTime  # nuke out the null entry
    else
        hooksTimes[$hook]+=", $dateTime"  # add it to the list
    fi
    latestHook=$hook  # to be written into the status file
    if [[ ${hooksScenarios[$hook]} == "null" ]]; then
        hooksScenarios[$hook]=$SCENARIO  # nuke out the null entry
    else
        hooksScenarios[$hook]+=", $SCENARIO" # add it to the list
    fi
}
#
writeHookStatus()
{
    local THIS="asgs_main->manageHooks.sh->writeHookStatus()"
    jsonfile="$statusDir/hook.status.json"
    logMessage "$THIS: Writing status associated with ASGS hooks to $jsonfile."
    #
    # write the time value(s) associated with each hook; will be null
    # if that hook has not been reached for this cycle
    echo "{" > $jsonfile # <-<< OVERWRITE
    for k in ${allHooks[@]} ; do
        echo "\"monitoring.hook.$k\" : ["              >> $jsonfile
        echo "    \"time\" : ["                        >> $jsonfile
        read -a timesarr <<< "${hooksTimes[$k]}"
        for t in ${timesarr[@]} ; do
        echo "        \"$t\""                          >> $jsonfile
        done
        echo "     ],"                                 >> $jsonfile
        echo "    \"scenario\" : ["                    >> $jsonfile
        read -a scenarioarr <<< "${hooksScenarios[$k]}"
        for s in ${scenarioarr[@]} ; do
        echo "        \"$s\""                          >> $jsonfile
        done
        echo "     ],"                                 >> $jsonfile
        echo "],"                                      >> $jsonfile
    done
    # update time stamp
    dateTime=`date +'%Y-%h-%d-T%H:%M:%S%z'`
    echo \""time.hook.status.lastupdated\" : \"$dateTime\","      >> $jsonfile
    echo \""hook.status.file.previous\" : \"$previousStatusFile\"," >> $jsonfile
    echo \""status.hook.latest\" : \"$latestHook\"" >> $jsonfile
    echo "}" >> $jsonfile
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
        writeHookStatus
    fi
    for hs in ${hooksScripts[$hook]} ; do
        if [[ $hook != "START_INIT" ]]; then
           logMessage "$THIS: Executing $hook hook script $SCRIPTDIR/$hs."
	    fi
        $SCRIPTDIR/$hs >> ${SYSLOG} 2>&1
    done
}