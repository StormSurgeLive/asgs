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
# april2022=( $(find /mnt/nas-storage/Operations/fortytwo.cct.lsu.edu/2022/nam -name LAv20a -print | grep 202204) )
# for p in ${april2022[@]}; do echo $p ; nowcastDirs+=( $(find $p -name nowcast -print) ) ; done
# rm -f tmp ; for d in ${nowcastDirs[@]} ; do echo $d/fort.61.nc >> tmp ; done ; cat tmp | sort -r > filelist.txt
# stationRetrospective.sh < filelist.txt
# -----------------------------------------------------------------------
# cat ../../../../Operations/autodownloader_config_april_nowcast.inventory | grep hsofs-nam-bob-2021 | grep fort.61.nc | sort -r > shortpath_filelist.txt
# awk '{ print "/mnt/nas-storage/Operations/"$0 }' shortpath_filelist.txt > filelist.txt
# stationRetrospective.sh < filelist.txt
# -----------------------------------------------------------------------
# cat ../../../Operations/autodownloader_config_april_nowcast.inventory | grep NCSC | grep fort.61.nc | sort -r > shortpath_filelist.txt
# awk '{ print "/mnt/nas-storage/Operations/"$0 }' shortpath_filelist.txt > filelist.txt
# stationRetrospective.sh < filelist.txt
# -----------------------------------------------------------------------
THIS=$(basename -- $0)
timePeriods=( 1 2 4 7 14 21 30 ) # days back from the last available date
timePeriodCount=0
dataSetCount=0
targetEpochSeconds=0
declare -a catList  # must ncrcat in chronological order
declare -a timePeriodsSeconds
for p in ${timePeriods[@]}; do
    timePeriodsSeconds+=( $(( $p * 86400 )) )
done
while read fname
do
    #echo "$fname"
    fileList+=( $fname )
    catList=( $fname ${catList[@]} )
    base_date=$(ncdump -h $fname | grep base_date | cut -d = -f 2 | tr -d '";')
    csEpochSeconds=$(TZ=UTC date -u -d "$base_date" "+%s")
    dataSetSecondsList=( $(ncks --json -v 'time' $fname | grep data | tr -d '"data:[,]') )
    numDataSets=$(ncdump -h $fname | grep currently | grep -Eo [0-9]+)
    dataSetCount=$(( $dataSetCount + $numDataSets ))
    declare -a dataSetEpochSecondsList
    for d in ${dataSetSecondsList[@]}; do
        dataSetEpochSecondsList+=( $(( $csEpochSeconds + $d )) )
    done
    #echo ${dataSetEpochSecondsList[-1]}
    if [[ ${#fileList[@]} -eq 1 ]]; then
        targetEpochSeconds=${dataSetEpochSecondsList[-1]}
        echo "targetEpochSeconds $targetEpochSeconds"
    fi
    for i in $(seq 1 $numDataSets); do
        period=$(( $targetEpochSeconds - ${dataSetEpochSecondsList[-$i]} ))
        #echo $period
        if [[ $period -ge ${timePeriodsSeconds[$timePeriodCount]} ]]; then
            retroFileName=$(printf "%02d_day.fort.61.nc" ${timePeriods[$timePeriodCount]})
            echo
            echo "ncrcat -O -d time,$(( $numDataSets - $i )),$dataSetCount ${catList[@]} $retroFileName"
            ncrcat -O -d time,$(( $dataSetCount - $i )),$numDataSets ${catList[@]} $retroFileName
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


