#!/bin/bash
#-----------------------------------------------------------------------
# stationRetrospective.sh : Create a retrospective analysis at a station.
#-----------------------------------------------------------------------
# Copyright(C) 2017--2022 Jason Fleming
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
#-----------------------------------------------------------------------
# This script assumes it is executed within an ASGS shell process
# and has access to all the normal environmental variables
# ($SCRIPTDIR, $PATH, etc). It receives a list of fort.61.nc
# files with fullpath in reverse chronological order on
# standard input.
#-----------------------------------------------------------------------
# path="/mnt/nas-storage/Operations/fortytwo.cct.lsu.edu/2022/nam/202204????/HSOFS/qbc.loni.org/HSOFS_nam_akheir/nowcast"
# fileList=$(printf "\"%s\",\n" $(ls -d $path | sort))
# printf "{ \"fort.61.nc\" : [ ${fileList::-1} ] }" | json_pp > fileList.json
#-----------------------------------------------------------------------
#
THIS=$(basename -- $0)
timePeriods = ( 1 2 4 7 14 21 30 ) # days back from the last available date
timePeriodCount=0
dataSetCount=0
targetEpochSeconds=0
declare -a timePeriodsSeconds
for p in ${timePeriods[@]}; do
    timePeriodsSeconds+=( $(( $p * 86400 )) )
done
while read fname
do
    echo "$fname"
    fileList+=( $fname )
    base_date=$(ncdump -h $fname | grep base_date | cut -d = -f 2 | tr -d '";')
    csEpochSeconds=$(TZ=UTC date -u -d "$base_date" "+%s" 2>>$SYSLOG)
    dataSetSecondsList=( $(ncks --json -v 'time' fort.63.nc | grep data | tr -d '"data:[,]') )
    numDataSets=$(ncdump -h $fname | grep currently | grep -Eo [0-9]+)
    dataSetCount=$(( $dataSetCount + $numDataSets ))
    declare -a dataSetEpochSecondsList
    for d in ${dataSetSecondsList[@]}; do
        dataSetEpochSecondsList+=( $(( $csEpochSeconds + $d )) )
    done
    if [[ ${#fileListfileCount[@]} -eq 1 ]]; then
        targetEpochSeconds=${dataSetEpochSeconds[-1]}
    fi
    for i in $(seq 1 $numDataSets); do
        period=$(( $targetEpochSeconds - ${dataSetEpochSecondsList["-$i"]} ))
        if [[ $period -ge ${timePeriodsSeconds[$timePeriodCount]} ]]; then
            ncrcat -O -d time,$(( $dataSetCount - $i )),$numDataSets ${$fileList[@]} $(printf "%02d_day.fort.61.nc" ${timePeriods[$timePeriodCount]})
            if [[ $timePeriodCount -lt $(( ${#timePeriods[@]} - 1 )) ]]; then
                timePeriodCount=$(( $timePeriodCount + 1 ))
            else
                echo "Finished concatenating data for all time periods."
                exit 0
            fi
        fi
    done
    unset dataSetEpochSecondsList
done < "${1:-/dev/stdin}"


