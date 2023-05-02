#!/bin/bash

#trap read debug
#----------------------------------------------------------------
# downloadBackgroundMet.sh: subroutine that polls an external
# ftp site for background meteorology data and writes metadata
# to document the current state. Sourced by asgs_main.sh.
#----------------------------------------------------------------
# Copyright(C) 2006--2022 Jason Fleming
# Copyright(C) 2006--2007, 2019--2021 Brett Estrade
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
downloadBackgroundMet()
{
    SCENARIODIR=$1
    RUNDIR=$2
    SCRIPTDIR=$3
    BACKSITE=$4
    BACKDIR=$5
    ENSTORM=$6
    CSDATE=$7
    HSTIME=$8
    FORECASTLENGTH=$9
    ALTNAMDIR=${10}
    FORECASTCYCLE=${11}
    ARCHIVEBASE=${12}
    ARCHIVEDIR=${13}
    STATEFILE=${14}
    #
    local THIS="asgs_main.sh>downloadBackgroundMet.sh"
    CURRENT_STATE="WAIT"
    logMessage "$ENSTORM: $THIS: Downloading meteorological data."
    cd $RUNDIR 2>> ${SYSLOG}
    #
    # if there isn't an archive directory for NAM data inside
    # the instance directory, make one
    instanceNamDir=$RUNDIR/nam
    if [[ ! -d $instanceNamDir ]]; then
       mkdir -p $instanceNamDir 2>> $SYSLOG
    fi
    # if there isn't an archive directory for NAM data in the
    # ASGS WORK directory, make one
    platformNamDir=$WORK/nam
    if [[ ! -d $platformNamDir ]]; then
       mkdir -p $platformNamDir 2>> $SYSLOG
    fi
    # create the json file to act as input to the
    # status checker and nam downloader
    namTemplateName="get_nam_template.json"
    # escape forward slashes to prevent sed from getting confused
    escBACKDIR=${BACKDIR////'\/'}
    escRUNDIR=${RUNDIR////'\/'}
    escSCRIPTDIR=${SCRIPTDIR////'\/'}
    escInstanceNamDir=${instanceNamDir////'\/'}
    # JSON can't store numbers with leading zeroes, so we use
    # quoted strings - they are usually used as strings, anyway
    arrFORECASTCYCLE=${FORECASTCYCLE//,/\",\"}
    boolApplyRamp=false
    if [[ $SPATIALEXTRAPOLATIONRAMP == "yes" ]]; then
        boolApplyRamp=true
    fi
    ptFilePath=${SCRIPTDIR}/input/$PTFILE
    escPtFilePath=${ptFilePath////'\/'}
    filledNamTemplateName="asgs_main.sh_get_nam_status.json"
    #
    # N O W C A S T
    if [[ $stage == "NOWCAST" ]]; then
        if [[ $BACKSITE == "filesystem" ]]; then
           logMessage "$ENSTORM: $THIS: NAM nowcast data will be loaded from the $BACKDIR directory on the local filesystem."
        fi
        # determine the cycle time corresponding to the current state of the simulation
        csEpochSeconds=$(TZ=UTC date -d "${CSDATE:0:4}-${CSDATE:4:2}-${CSDATE:6:2} ${CSDATE:8:2}:00:00" "+%s")
        hsEpochSeconds=$((csEpochSeconds + ${HSTIME%.*}))
        lastCycle=$(TZ=UTC date -d "1970-01-01 UTC $hsEpochSeconds seconds" +"%Y%m%d%H")
        DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
        sed \
            -e "s/%NULLGETNAMTEMPLATEFILE%/$namTemplateName/" \
            -e "s/%NULLGETNAMTEMPLATEFILLEDFILE%/$filledNamTemplateName/" \
            -e "s/%NULLBACKSITE%/$BACKSITE/" \
            -e "s/%NULLBACKDIR%/$escBACKDIR/" \
            -e "s/%NULLCYCLE%/$lastCycle/" \
            -e "s/%NULLFORECASTCYCLE%/$arrFORECASTCYCLE/" \
            -e "s/%NULLSTAGE%/$stage/"                   \
            -e "s/%NULLSCRIPTDIR%/$escSCRIPTDIR/"        \
            -e "s/%NULLNAMDATAPATH%/$escInstanceNamDir/" \
            -e "s/\"%NULLNAMNOWCASTDOWNLOADED%\"/null/" \
            -e "s/\"%NULLNAMNOWCASTFOUND%\"/null/" \
            -e "s/\"%NULLNAMFORECASTDOWNLOADED%\"/null/" \
            -e "s/\"%NULLNAMFORECASTFOUND%\"/null/" \
            -e "s/\"%NULLNAMSTATUSFILE%\"/null/" \
            -e "s/\"%NULLNAMSELECTFILE%\"/null/" \
            -e "s/\"%NULLGETNAMFILE%\"/null/" \
            -e "s/%NULLLASTUPDATER%/$THIS/" \
            -e "s/%NULLLASTUPDATETIME%/$DATETIME/" \
             < $SCRIPTDIR/$namTemplateName \
             > $filledNamTemplateName \
           2>> $SYSLOG
        if [[ $? != 0 ]]; then
            warn "$THIS: Failed to fill in NAM data request template with sed."
        fi
        # determine the status of the latest NAM cycle posted by NCEP,
        # along with the range of cycles posted since the adcirc hotstart time
        latestCycle=0
        TRIES=0
        while [[ $latestCycle -le $lastCycle ]]; do
            if [[ $TRIES -ne 0 ]]; then
                sleep 60
            fi
            ((TRIES++))
            get_nam_status.pl < $filledNamTemplateName > get_nam_status.pl.json 2>> $SYSLOG
            if [[ $? != 0 ]]; then
                warn "$THIS: Failed to get status of NAM cycles with get_nam_status.pl."
                sleep 60
                continue
            fi
            latest.pl < get_nam_status.pl.json > latestCycle 2>> $SYSLOG
            if [[ $? != 0 ]]; then
                warn "$THIS: Failed to extract the latest NAM cycle from get_nam_status.pl.json with latest.pl"
                sleep 60
                continue
            fi
            latestCycle=$(<"latestCycle")
        done
        # refine the list of NAM cycles to end the nowcast on the correct cycle
        select_nam_nowcast.pl < get_nam_status.pl.json > select_nam_nowcast.pl.json 2>>$SYSLOG
        if [[ $? != 0 ]]; then
            warn "$THIS: Failed to select the proper NAM cycle to end the nowcast with select_nam_nowcast.pl."
        fi
        # then download the actual nowcast data for the time range of interest
        while [[ 1 ]]; do
            get_nam_data.pl < select_nam_nowcast.pl.json > get_nam_data.pl.json 2>> $SYSLOG
            if [[ $? != 0 ]]; then
                warn "$THIS: Failed to download NAM nowcast data with get_nam_data.pl.json."
                sleep 60
                continue
            else
                break
            fi
        done
        # record the new advisory number to the statefile
        latest.pl < select_nam_nowcast.pl.json > thisCycle 2>> $SYSLOG
        if [[ $? != 0 ]]; then
            warn "$THIS: Failed to extract the selected NAM nowcast cycle from select_nam_nowcast.pl.json with latest.pl"
        fi
        thisCycle=$(<"thisCycle")
        if [[ $BACKGROUNDMET == "on" || $BACKGROUNDMET == "NAM" ]]; then # don't need to do this for "namBlend"
            debugMessage "$THIS: $ENSTORM: The new NAM cycle is $thisCycle."
            cp -f $STATEFILE ${STATEFILE}.old 2>> ${SYSLOG} 2>&1
            sed 's/ADVISORY=.*/ADVISORY='$thisCycle'/' $STATEFILE > ${STATEFILE}.new
            debugMessage "Updating statefile $STATEFILE with new cycle number ${thisCycle}."
            cp -f ${STATEFILE}.new $STATEFILE 2>> ${SYSLOG} 2>&1
        fi
    else
        # F O R E C A S T
        # download forecast data
        sed "s/NOWCAST/FORECAST/" < get_nam_data.pl.json > get_nam_forecast.json 2>> $SYSLOG
        if [[ $? != 0 ]]; then
            warn "$THIS: Failed to replace NOWCAST with FORECAST in get_nam_data.pl.json with sed."
        fi
        # download the forecast data
        while [[ 1 ]]; do
            get_nam_data.pl < get_nam_forecast.json > get_nam_data.pl.json 2>> $SYSLOG
            if [[ $? != 0 ]]; then
                warn "$THIS: Failed to download NAM forecast data with get_nam_data.pl."
                sleep 60
                continue
            else
                break
            fi
        done
        # write the start and end dates of the forecast to the run.properties file
        # this is deprecated with the intent to get this info
        # from get_nam_data.pl.json instead
        if [[ -e $RUNDIR/forecast.properties ]]; then
            cat $RUNDIR/forecast.properties >> ${SCENARIODIR}/run.properties
            mv $RUNDIR/forecast.properties ${SCENARIODIR} 2>> ${SYSLOG}
        fi
   fi
}
