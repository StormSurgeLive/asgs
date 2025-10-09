#!/bin/bash

#trap read debug
#----------------------------------------------------------------
# downloadRRFS.sh: subroutine that polls an external https
# site to determine status of GFS data and then subsets
# and downloads the data via curl.
#----------------------------------------------------------------
# Copyright(C) 2025 Jason Fleming
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

performQualityChecksRRFS()
{
    if [[ ! -s $1 ]]; then
        appMessage "Attempt to download RRFS file '$1' failed to produce a local file (or a file with zero length) when using the following command: '$curlCommand'." $downloadRrfsLog
        return 1
    fi
    declare -A quality
    case $2 in
    "indexFile")
        quality['size']=80000
        quality['lines']=1500
        ;;
    "grib2File")
        quality['size']=80000
        quality['lines']=1500
        ;;
    *)
        warn "THIS: Quality check attempted on unrecognized file type '$2'."
        return 1
        ;;
    esac
    #
    # curl exited with success code; perform quality checks on index files
    localFileType="$(file $1 2>> $SYSLOG)"
    localFileSize=$(stat -c "%s" $1 2>> $SYSLOG)
    localFileLines=$(cat $1 2>> $SYSLOG | wc -l)
    # index file is plain ascii but error message is XML
    if [[ "$localFileType" =~ "XML" ]]; then
        appMessage "The requested RRFS index file '$1' was not found." $downloadRrfsLog
        mv $1 ${1}.errxml 2>> $SYSLOG
        return 1
    fi
    # make sure the downloaded file is at least 80kB (these files
    # seem to be about 100kB)
    if [[ $localFileSize -lt ${quality['size']} ]]; then
        appMessage "The file '$1' seems to be incomplete because it is only '$localFileSize' bytes." $downloadRrfsLog
        mv $1 ${1}.toosmall 2>> $SYSLOG
        return 1
    fi
    # I found 1691 lines in the one that I looked at
    if [[ $localFileLines -lt ${quality['lines']} ]]; then
        appMessage "The file '$1' only has '$localFileLines' line which does not seem to be enough." $downloadRrfsLog
        mv $1 ${1}.tooshort 2>> $SYSLOG
        return 1
    fi
    return 0
}


downloadRRFS()
{
    #
    local THIS="asgs_main.sh>downloadRRFS.sh"
    logMessage "$SCENARIO: $THIS: Downloading RRFS meteorological data."
    cd $RUNDIR 2>> ${SYSLOG}
    # if there isn't an archive directory for RRFS data in the
    # ASGS WORK directory, make one
    #
    # if there isn't an archive directory for GFS data inside
    # the instance directory, make one
    instanceRrfsDir=$RUNDIR/rrfs
    if [[ ! -d $instanceRrfsDir ]]; then
       mkdir -p $instanceRrfsDir 2>> $SYSLOG
    fi
    # create log file for this script
    downloadRrfsLog=$instanceRrfsDir/downloadRRFS.sh.log
    # epoch seconds associated with cold start and hotstart times
    csEpochSeconds=$(TZ=UTC date -u -d "${CSDATE:0:4}-${CSDATE:4:2}-${CSDATE:6:2} ${CSDATE:8:2}:00:00" "+%s" 2>>$SYSLOG)
    hsEpochSeconds=$((csEpochSeconds + ${HSTIME%.*}))
    # example data path
    # aws s3 ls --no-sign-request s3://noaa-rrfs-pds/rrfs_a/rrfs.20251004/18/
    # -> 2025-10-04 15:12:46     102829 rrfs.t18z.natlev.3km.f000.na.grib2.idx
    #
    #   N O W C A S T
    #
    if [[ $stage == "NOWCAST" ]]; then
        # determine the cycle time corresponding to the current state of the simulation
        lastCycle=$(TZ=UTC date -u -d "1970-01-01 UTC $hsEpochSeconds seconds" +"%Y%m%d%H" 2>>$SYSLOG)
        DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
        #
        #   F I L L   J S O N   T E M P L A T E
        #
        forecastCycleArray=$(printf "\"%s\"," ${FORECASTCYCLE//,/' '})
        sed \
            -e "s?%NULLGETRRFSTEMPLATEFILE%?${rrfs['TemplateName']}?" \
            -e "s?%NULLGETRRFSTEMPLATEFILLEDFILE%?${rrfs['FilledTemplateName']}?" \
            -e "s?%NULLBACKSITE%?${rrfs['BaseURL']}?" \
            -e "s?%NULLCYCLE%?$lastCycle?" \
            -e "s?%NULLSTAGE%?$stage?"                   \
            -e "s?\"%NULLRRFSNOWCASTDOWNLOADED%\"?null?" \
            -e "s?\"%NULLRRFSNOWCASTFOUND%\"?null?" \
            -e "s?\"%NULLRRFSFORECASTDOWNLOADED%\"?null?" \
            -e "s?\"%NULLRRFSFORECASTFOUND%\"?null?" \
            -e "s?\"%NULLRRFSSTATUSFILE%\"?null?" \
            -e "s?\"%NULLRRFSSELECTFILE%\"?null?" \
            -e "s?\"%NULLGETRRFSFILE%\"?null?" \
            -e "s?%NULLLASTUPDATER%?$THIS?" \
            -e "s?%NULLLASTUPDATETIME%?$DATETIME?" \
            -e "s?%NULLSCRIPTDIR%?$SCRIPTDIR?" \
            -e "s?%LOCALDATADIR%?$instanceRrfsDir?" \
            -e "s?\"%NULLFORECASTCYCLES%\"?$(echo ${forecastCycleArray%?})?" \
            < $SCRIPTDIR/${rrfs['TemplateName']} \
            > "${rrfs['FilledTemplateName']}" \
            2>> $SYSLOG
        if [[ $? != 0 ]]; then
            echo "$THIS: Failed to fill in RRFS data request template with sed."
        fi
        # stop here if we are only testing the filling of the JSON template
        if [[ $unitTest == "rrfs.template" ]]; then
            echo "$THIS: Finished unit test '$unitTest'."
            exit
        fi
        #
        #   C A T A L O G   A V A I L A B L E   D A T A
        #
        # determine the status of the latest RRFS cycle posted by NCEP,
        # along with the range of cycles posted since the adcirc hotstart time
        sleepSeconds=$(( ${rrfs['PollingInterval']} * 60 ))
        latestCycle=0
        TRIES=0
        printf "." # progress bar
        while [[ $latestCycle -le $lastCycle ]]; do
            echo latestCycle=$latestCycle #jgfdebug
            echo lastCycle=$lastCycle #jgfdebug
            if [[ $TRIES -ne 0 ]]; then
                spinner $sleepSeconds
                printf "\b.."
            fi
            ((TRIES++))
            #
            #   D O W N L O A D   C Y C L E   I N D E X   F I L E S
            #
            # getting rrfs status : start with a list of dates beginning on the hotstart
            # date and progressing for a number of days equal to rrfsLookAhead, check for
            # the existence of a grib2 index file for each cycle
            declare -a cycleList
            for d in $(seq 0 ${rrfs['LookAhead']}); do
                echo d=$d # jgfdebug
                cycleDate=$( TZ=UTC date --date="${lastCycle:0:8} +$d day" +"%Y%m%d" )
                for h in 00 06 12 18; do
                    echo h=$h          # jgfdebug
                    indexFileName=rrfs.t${h}z.natlev.3km.f000.na.grib2.idx
                    echo indexFileName=$indexFileName #jgfdebug
                    if [[ -e $instanceRrfsDir/$cycleDate/$h/$indexFileName ]]; then
                        #jgfdebug appMessage "Found the index file $instanceRrfsDir/$cycleDate/$h/$indexFileName in the local cache; no need to download it again." $downloadRrfsLog
                        cycleList+=( $cycleDate$h )
                    else
                        # check to see if it is available from NCEP
                        echo "curl --silent -O ${rrfs['BaseURL']}/rrfs.$cycleDate/$h/$indexFileName"
                        curlCommand="curl --silent -O ${rrfs['BaseURL']}/rrfs.$cycleDate/$h/$indexFileName"
                        $curlCommand
                        exitCode=$?
                        if [[ $exitCode != 0 ]]; then
                            logMessage "The curl command '$curlCommand' to download the RRFS index file has failed with exit code '$exitCode'."
                            break 2
                        else
                            performQualityChecksRRFS $indexFileName indexFile
                            if [[ $? -ne 0 ]]; then
                                break 2
                            fi
                        fi
                        # if the index file was downloaded successfully and passed quality checks,
                        # place it in the local cache
                        echo found  #jgfdebug
                        if [[ $cycleDate$h -ge $lastCycle ]]; then
                            cycleList+=( $cycleDate$h )
                            if [[ ! -e $instanceRrfsDir/$cycleDate/$h/$indexFileName ]]; then
                                if [[ ! -d $instanceRrfsDir/$cycleDate/$h ]]; then
                                    mkdir -p $instanceRrfsDir/$cycleDate/$h 2>> $SYSLOG
                                    mv $indexFileName $instanceRrfsDir/$cycleDate/$h 2>> $SYSLOG
                                fi
                            fi
                        fi
                    fi
                done
            done
            if [[ ${#cycleList[@]} -ne 0 ]]; then
                latestCycle=${cycleList[-1]}
                # update the cyclelist in json
                cycleListStr=$( (IFS=","; echo "${cycleList[*]}") )
                sed -e "s?%NULLCYCLELIST%?$cycleListStr?" < ${rrfs['FilledTemplateName']} > get_rrfs_status.json 2>> $SYSLOG
            else
                latestCycle=0
            fi
        done
        # stop here if we are only testing through the catalogging of the remote site
        if [[ $unitTest == "rrfs.template.catalog" ]]; then
            exit
        fi
        #
        #     N E W   N O W C A S T   C Y C L E
        #        H A S   B E E N   P O S T E D
        #
        # get the latest cycle that we want to nowcast to, if specific
        # forecast cycles have been specified; remove any forecast
        # cycles after the most recent one that we want to nowcast to
        forecastCycles=( $(echo ${FORECASTCYCLE//,/' '}) )
        if [[ ${#forecastCycles[@]} -gt 0 ]]; then
            forecastFound=0
            extraCycles=0
            for (( c=${#cycleList[@]}-1; c>=0; c-- )) ; do
                echo extraCycles=$extraCycles
                echo c=$c                #jgfdebug
                cycleday=${cycleList[$c]:0:8}
                echo cycleday=$cycleday #jgfdebug
                cyclehour=${cycleList[$c]:8:2}
                echo cyclehour=$cyclehour #jgfdebug
                # loop over the forecast cycles to see if this
                # cycle has been specified
                for f in ${forecastCycles[@]}; do
                    echo f=$f
                    if [[ ${forecastCycles[$f]} == $cyclehour ]]; then
                        forecastFound=1
                        break
                    fi
                done
                if [[ $forecastFound -eq 0 ]]; then
                    ((extraCycles++))
                else
                    break
                fi
            done
            # now remove the excess cycles that are beyond the end of the
            # required nowcast period, if any
            cycleList=( ${cycleList[@]:0:(( ${#cycleList[@]} - $extraCycles ))} )
            cycleListStr=$( (IFS=","; echo "${cycleList[*]}") )
        fi
        sed -e "s?%NULLCYCLELIST%?$cycleListStr?" < ${rrfs['FilledTemplateName']} > select_rrfs_nowcast.json 2>> $SYSLOG
        # stop here if we are only testing through the removal of extra cycles
        if [[ $unitTest == "rrfs.template.catalog.select" ]]; then
            exit
        fi
        thisCycle=${cycleList[-1]}
        #
        #   G E T    H O U R L Y   N O W C A S T
        #     G R I B 2   I N D E X   F I L E S
        #
        logMessage "$THIS: Downloading hourly RRFS nowcast grib2 files for cycle '$thisCycle'."
        consoleMessage "$I Downloading hourly RRFS nowcast grib2 files for cycle '$thisCycle'"
        for c in ${cycleList[@]} ; do
            cycleDate=${c:0:8}
            cycleHour=${c:8:2}
            # need the index files for the nowcast or "analysis" at each hour
            # between the cycle times
            for h in $(seq 0 5); do
                hh=$(printf "%02d" $(( $cycleHour + $h )) )
                indexFileName=rrfs.t${hh}z.natlev.3km.f000.na.grib2.idx
                if [[ ! -e $instanceRrfsDir/$cycleDate/$hh/$indexFileName ]]; then
                    # check to see if it is available from NCEP
                    echo "curl --silent -O ${rrfs['BaseURL']}/rrfs.$cycleDate/$hh/$indexFileName"
                    curlCommand="curl --silent -O ${rrfs['BaseURL']}/rrfs.$cycleDate/$hh/$indexFileName"
                    $curlCommand
                    exitCode=$?
                    if [[ $exitCode != 0 ]]; then
                        logMessage "The curl command '$curlCommand' to download the RRFS index file has failed with exit code '$exitCode'."
                        break 2
                    else
                        performQualityChecksRRFS $indexFileName indexFile
                        if [[ $? -ne 0 ]]; then
                            break 2
                        fi
                        # if the index file was downloaded successfully and passed quality checks
                        # place the index file with computed ranges in the local cache
                        echo found  #jgfdebug
                        if [[ ! -d $instanceRrfsDir/$cycleDate/$hh ]]; then
                            mkdir -p $instanceRrfsDir/$cycleDate/$hh 2>> $SYSLOG
                            mv $indexFileName $instanceRrfsDir/$cycleDate/$hh 2>> $SYSLOG
                        fi
                    fi
                fi
            done
        done
        # stop here if we are only testing through the download of hourly
        # nowcast index files
        if [[ $unitTest == "rrfs.template.catalog.select.hourly" ]]; then
            exit
        fi
        #
        #    C O M P U T E   B Y T E   R A N G E S
        #   A N D   D O W N L O A D   S U B S E T S
        #         B Y   V A R I A B L E
        #
        declare -A rrfsVar
        rrfsVar['UGRD']='UGRD:10 m above ground'
        rrfsVar['VGRD']='VGRD:10 m above ground'
        rrfsVar['PRES']='PRES:surface'
        for c in ${cycleList[@]} ; do
            cycleDate=${c:0:8}
            cycleHour=${c:8:2}
            # need the index files for the nowcast or "analysis" at each hour
            # between the cycle times
            for h in $(seq 0 5); do
                hh=$(printf "%02d" $(( $cycleHour + $h )) )
                indexFileDir=$instanceRrfsDir/$cycleDate/$hh
                indexFileName=rrfs.t${hh}z.natlev.3km.f000.na.grib2.idx
                if [[ ! -e $indexFileDir/$indexFileName.range ]]; then
                    awk 'BEGIN { FS=":" ; startRange=0 } NR==1 { startRange=$2 } NR>1 { print "range="startRange"-"($2-1) ; startRange=$2 }' $indexFileDir/$indexFileName > ranges 2>> $SYSLOG
                    paste -d "" $indexFileDir/$indexFileName ranges > $indexFileDir/$indexFileName.range 2>> $SYSLOG
                fi
                # attempt to download specific byte ranges via curl
                for v in UGRD VGRD PRES; do
                    byteRange=$(grep "${rrfsVar[$v]}" $indexFileDir/$indexFileName.range | grep -Eo '[0-9]*-[0-9]*')
                    grib2FileName=${indexFileName%.idx}
                    if [[ ! -e $indexFileDir/$v.$grib2FileName ]]; then
                        curlCommand="curl --range $byteRange --silent -o $v.$grib2FileName ${rrfs['BaseURL']}/rrfs.$cycleDate/$hh/$grib2FileName"
                        echo curlCommand="$curlCommand"
                        $curlCommand
                        performQualityChecksRRFS $v.$grib2FileName grib2File
                        if [[ $? -ne 0 ]]; then
                            rm $v.$grib2FileName 2>> $SYSLOG
                            break 3
                        fi
                        # the subset downloaded successfully and passed quality checks,
                        # copy it to the local grib2 file cache
                        mv $v.$grib2FileName $indexFileDir 2>> $SYSLOG
                    fi
                done
            done
        done
        # stop here if we are only testing through the download of hourly
        # nowcast index files
        if [[ $unitTest == "rrfs.template.catalog.select.hourly.download" ]]; then
            exit
        fi
        #
        #         R E G R I D   T O   G E O G R A P H I C
        #  P R O J E C T I O N   O N   S P E C I F I E D   D O M A I N
        #
        logMessage "$THIS: Regridding RRFS grib2 files to latlon."
        consoleMessage "$I Regridding RRFS grib2 files to latlon."
        for c in ${cycleList[@]} ; do
            cycleDate=${c:0:8}
            cycleHour=${c:8:2}
            for h in $(seq 0 5); do
                hh=$(printf "%02d" $(( $cycleHour + $h )) )
                for v in UGRD VGRD PRES; do
                    origFile=$instanceRrfsDir/$cycleDate/$hh/$v.rrfs.t${hh}z.natlev.3km.f000.na.grib2
                    latLonFile=${origFile}.latlon
                    lonSpec="${rrfsLatLonGrid['lon0']}:${rrfsLatLonGrid['nlon']}:${rrfsLatLonGrid['dlon']}"
                    latSpec="${rrfsLatLonGrid['lat0']}:${rrfsLatLonGrid['nlat']}:${rrfsLatLonGrid['dlat']}"
                    if [[ ! -s $latLonFile ]]; then
                        regridCommand="wgrib2 $origFile -inv /dev/null -set_grib_type same -new_grid_winds earth -new_grid_vectors none -new_grid latlon $lonSpec $latSpec $latLonFile"
                        appMessage "Regridding RRFS using '$regridCommand'." $downloadRrfsLog
                        $regridCommand
                    fi
                done
            done
        done
        # stop here if we are only testing through the regridding of hourly
        # nowcast index files
        if [[ $unitTest == "rrfs.template.catalog.select.hourly.download.regrid" ]]; then
            exit
        fi
        #
        #    W R I T E   A S C I I   O W I   W I N / P R E
        #        F I L E S   A N D   M E T A D A T A
        #
        # form win/pre file header line
        SWLat=$(printf "%3.4f" ${rrfsLatLonGrid['lat0']})
        SWLon=$(printf "%3.4f" ${rrfsLatLonGrid['lon0']})
        # form the file names of the first, second, and last grib2 files
        # to extract timing information
        firstFile=$instanceRrfsDir/${cycleList[0]:0:8}/${cycleList[0]:8:2}/UGRD.rrfs.t${cycleList[0]:8:2}z.natlev.3km.f000.na.grib2
        secondFile=$instanceRrfsDir/${cycleList[1]:0:8}/${cycleList[1]:8:2}/UGRD.rrfs.t${cycleList[1]:8:2}z.natlev.3km.f000.na.grib2
        lastFile=$instanceRrfsDir/${cycleList[-1]:0:8}/${cycleList[-1]:8:2}/UGRD.rrfs.t${cycleList[-1]:8:2}z.natlev.3km.f000.na.grib2
        #
        # grab the start time (YYYYMMDDHH) of the files from the
        # inventory in the first file
        d=$(wgrib2 $firstFile -match "UGRD" 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
        owiWinPre["startDateTime"]=$d
        owiStartEpochSeconds=$(TZ=UTC date -u -d "${d:0:4}-${d:4:2}-${d:6:2} ${d:8:2}:00:00" "+%s" 2>>$SYSLOG)
        sdate=$(date --date="${d:0:4}-${d:4:2}-${d:6:2} ${d:8:10}:00:00" '+%s' 2>>$SYSLOG)
        #
        # determine the WTIMINC (time increment between data sets in seconds,
        # needed for the ADCIRC fort.15 file)
        d=$(wgrib2 $secondFile -match "UGRD" 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
        ndate=$(date --date="${d:0:4}-${d:4:2}-${d:6:2} ${d:8:10}:00:00" '+%s' 2>>$SYSLOG)
        WTIMINC=$(( (ndate - sdate) )) # seconds
        #
        # determine the NWBS (number of blank snaps between the
        # hotstart time and the start of the OWI WIN/PRE data)
        # under typical circumstances, will be equal to 0
        owiWinPre["NWBS"]=$(echo "scale=0; ($owiStartEpochSeconds - $hsEpochSeconds)/$WTIMINC" | bc)
        # write the fort.22 file
        fort22="fort.22"
        if [[ $BACKGROUNDMET == "rrfsBlend" ]]; then
            fort22="owi_fort.22"
        fi
        echo ${owiWinPre["NWSET"]} > $fort22
        echo ${owiWinPre["NWBS"]} >> $fort22
        echo ${owiWinPre["DWM"]}  >> $fort22
        #
        # find the end time for use in the main file header
        owiWinPre["endDateTime"]=$(wgrib2 $lastFile -match "UGRD" 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
        #
        # write the headers to the win/pre files
        preFileName=RRFS_${stage^^}_${owiWinPre["startDateTime"]}_${owiWinPre["endDateTime"]}.221
        winFileName=RRFS_${stage^^}_${owiWinPre["startDateTime"]}_${owiWinPre["endDateTime"]}.222
        headerLineTemplate="$(printf "%s%38s%15s\n" "Oceanweather WIN/PRE Format" ${owiWinPre["startDateTime"]} ${owiWinPre["endDateTime"]})"
        headerLine="${headerLineTemplate:0:30}#${rrfsDomain['coverage']}${headerLineTemplate:$(expr 31 + ${#rrfsDomain['coverage']}):${#headerLineTemplate}}"
        echo "$headerLine" > $preFileName # fort.221
        echo "$headerLine" > $winFileName # fort.222
        #
        # extract the data from the grib2 files as ascii, reformat
        # into eight columns, and append the dataset to the corresponding file
        logMessage "$THIS: Writing RRFS ASCII WIN/PRE files."
        consoleMessage "$I Writing RRFS ASCII WIN/PRE files."
        unset winPreTimes
        for c in ${cycleList[@]} ; do
            cycleDate=${c:0:8}
            cycleHour=${c:8:2}
            for h in $(seq 0 5); do
                hh=$(printf "%02d" $(( $cycleHour + $h )) )
                f=$instanceRrfsDir/$cycleDate/$hh/PRES.rrfs.t${hh}z.natlev.3km.f000.na.grib2.latlon
                d=$(wgrib2 $f -match 'PRES' 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
                headerLine="$(printf "iLat=%4diLong=%4dDX=%6.3fDY=%6.3fSWLat=%8.3fSWLon=%8.3fDT=%8d00" ${rrfsLatLonGrid['nlat']} ${rrfsLatLonGrid['nlon']} ${rrfsLatLonGrid['dlon']} ${rrfsLatLonGrid['dlat']} $SWLat $SWLon $d)"
                winPreTimes+=( $d"00" )
                echo "$headerLine" >> $preFileName
                echo "$headerLine" >> $winFileName
                # replace missing surface pressure values with 101300 Pa (ADCIRC default)
                # convert barometric pressure from Pa to millibar in the process
                wgrib2 $f -match 'PRES' -inv /dev/null -text - 2>>$SYSLOG \
                    | awk '$1<=9999000000.0 { print $0 } $1>9999000000.0 { print "101300.0" }' \
                    | awk 'NR!=1 { printf("%10s",(sprintf("%4.4f",$1/100.0))); if ((NR-1)%8 == 0) print ""; }' \
                    >> $preFileName 2>> $SYSLOG
                for v in UGRD VGRD ; do
                    f=$instanceRrfsDir/$cycleDate/$hh/$v.rrfs.t${hh}z.natlev.3km.f000.na.grib2.latlon
                    # replace missing velocity values with 0.0 m/s
                    wgrib2 $f -match $v -inv /dev/null -text - 2>> $SYSLOG \
                        | awk '$1<=9999000000.0 { print $0 } $1>9999000000.0 { print "0.0" }' \
                        | awk 'NR!=1 { printf("%10f",$1); if ((NR-1)%8 == 0) print ""; }' \
                        >> $winFileName 2>> $SYSLOG
                done
            done
        done
        # stop here if we are only testing through the writing of owi win/pre
        # nowcast files
        if [[ $unitTest == "rrfs.template.catalog.select.hourly.download.regrid.owiwinpre" ]]; then
            exit
        fi
        #
        # write metadata to JSON
        bashJSON.pl \
            --mapscalar get="$THIS" \
            --mapscalar winPreHeader="$(printf "%s%38s%15s" "Oceanweather WIN/PRE Format" ${owiWinPre["startDateTime"]} ${owiWinPre["endDateTime"]})" \
            --mapscalar winPreVelocityFile="$winFileName" \
            --mapscalar winPrePressureFile="$preFileName" \
            --mapscalar winPreRecordLength=$(( ${gfsLatLonGrid['nlon']} * ${gfsLatLonGrid['nlat']} )) \
            --mapscalar gfsForecastValidStart="${owiWinPre["startDateTime"]}0000" \
            --mapscalar winPreWtimincSeconds="$WTIMINC" \
            --mapscalar winPreNumRecords="${#winPreTimes[*]}" \
            --maparray winPreDataTimes="$(echo ${winPreTimes[@]})" \
            --maparray filesDownloaded="$(echo ${downloaded[@]})" \
            --maparray filesFromCache="$(echo ${have[@]})" \
            < select_gfs_nowcast.pl.json \
            > ${THIS}.json
            2>> $SYSLOG
        if [[ $BACKGROUNDMET == "GFS" ]]; then # don't need to do this for "gfsBlend"
            ADVISDIR=$RUNDIR/$thisCycle
            CYCLEDIR=$ADVISDIR
            CYCLELOG=$CYCLEDIR/cycle.log
            NOWCASTDIR=$ADVISDIR/$ENSTORM
            SCENARIODIR=$CYCLEDIR/$SCENARIO
            SCENARIOLOG=$SCENARIODIR/scenario.log
            mkdir -p $SCENARIODIR 2>> $SYSLOG
            #
            # record the new advisory number to the statefile
            debugMessage "$THIS: $ENSTORM: The new GFS cycle is $thisCycle."
            cp -f $STATEFILE ${STATEFILE}.old 2>> ${SYSLOG} 2>&1
            sed 's/ADVISORY=.*/ADVISORY='$thisCycle'/' $STATEFILE > ${STATEFILE}.new
            debugMessage "Updating statefile $STATEFILE with new cycle number ${thisCycle}."
            cp -f ${STATEFILE}.new $STATEFILE 2>> ${SYSLOG} 2>&1
        fi
        #
        # put files in scenario directory
        mv $winFileName $preFileName $fort22 run.properties $SCENARIODIR 2>> $SYSLOG
        cp ${THIS}.json "${winFileName%.*}.json" 2>> $SYSLOG
        cp get_gfs_status.pl.* ${THIS}.json "${winFileName%.*}.json" $SCENARIODIR 2>> $SYSLOG

        cd $SCENARIODIR 2>> $SYSLOG
        # create links to the OWI WIN/PRE files with names that  ADCIRC expects
        ln -s $(basename $preFileName) fort.221 2>> $SYSLOG
        ln -s $(basename $winFileName) fort.222 2>> $SYSLOG
        cd $RUNDIR
    else
        #
        # F O R E C A S T
        #
        # download forecast data
        logMessage "$THIS: INFO: Downloading files for scenario '$SCENARIO'."
        consoleMessage "$I Downloading GFS files for scenario '$SCENARIO'."
        if [[ ! -d $SCENARIODIR ]]; then
            mkdir -p $SCENARIODIR
        fi
        #
        # grab the cycle that should be forecast
        gfsForecastCycle=$(bashJSON.pl --key cyclelist --last < select_gfs_nowcast.pl.json)
        # write the forecast.properties file
        echo "forecastValidStart : ${gfsForecastCycle}0000" > forecast.properties
        #
        # form the list of files to download
        declare -a gfsForecastFiles
        unset downloaded have
        cyc=${gfsForecastCycle:8:2}
        yyyymmdd=${gfsForecastCycle:0:8}
        logMessage "$THIS: Subsetting and downloading GFS grib2 forecast files."
        consoleMessage "$I Subsetting and downloading GFS grib2 forecast files."
        maxRetries=10
        for h in $(seq 0 $GFSFORECASTLENGTH) ; do
            hhh=$(printf "%03d" $h)
            file="gfs.t${cyc}z.pgrb2.0p25.f$hhh"
            remotePath="%2Fgfs.${yyyymmdd}%2F${cyc}%2Fatmos"
            finalURL=$baseURL"?file="$file$levels$vars$domain"&dir="$remotePath
            # subset and download with curl
            if [[ ! -e $instanceGfsDir/$yyyymmdd ]]; then
                mkdir -p $instanceGfsDir/$yyyymmdd 2>> $SYSLOG
            fi
            # don't download again if the files are already in place
            # (may want to turn off this conditional for end-to-end testing)
            localFile=$instanceGfsDir/$yyyymmdd/$file
            subsetSuccess=0
            numRetries=0
            cmd="curl -s '$finalURL' > $localFile 2>> $SYSLOG"
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] Subsetting GFS with curl using the following command: '$cmd'." >> $downloadGfsLog
            while [[ $subsetSuccess -eq 0 && $numRetries -lt $maxRetries ]]; do
                how="download"
                if [[ ! -s $localFile ]]; then
                    curl -s "$finalURL" > $localFile 2>> $SYSLOG
                    if [[ $? != 0 ]]; then
                        logMessage "Subsetting GFS with curl failed when using the following command: '$cmd'."
                        if [[ -e $localFile ]]; then
                            mv $localFile ${localFile}.curlerr
                        fi
                    fi
                else
                    how="have"
                fi
                # perform quality checks
                if [[ -s $localFile ]]; then
                    #logMessage "Performing quality check on '$localFile'."
                    localFileType="$(file $localFile 2>> $SYSLOG)"
                    localFileSize=$(stat -c "%s" $localFile 2>> $SYSLOG)
                    localFileLines=$(wgrib2 $localFile -match 'PRMSL' -inv /dev/null -text - 2>> $SYSLOG | wc -l)
                    if [[ ! $localFileType =~ "(GRIB) version 2" && ! $localFileType =~ "data" ]]; then
                        logMessage "Subsetting GFS with curl failed to produce a local grib2 file '$localFile'."
                        if [[ "$localFileType" =~ "HTML document" ]]; then
                            logMessage "The file '$localFile' is html instead of grib2."
                            if [[ $(cat $localFile) =~ "data file is not present" ]]; then
                                logMessage "The html message is: 'data file is not present'."
                                mv $localFile not_present.html 2>> $SYSLOG
                            else
                                mv $localFile ${localFile}.html 2>> $SYSLOG
                            fi
                        else
                            logMessage "The file type of '$localFile' is unrecognized: '$localFileType'."
                            mv $localFile ${localFile}.unrecognized 2>> $SYSLOG
                        fi
                    # make sure the downloaded file is at least 1MB (these files
                    # seem to be about 2-3MB)
                    elif [[ $localFileSize -lt 1000000 ]]; then
                        logMessage "The file '$localFile' seems to be incomplete because it is only '$localFileSize' bytes."
                        mv $localFile ${localFile}.toosmall 2>> $SYSLOG
                    # smoke test
                    elif [[ $localFileLines -eq 0 ]]; then
                        logMessage "The file '$localFile' does not seem to have barometric pressure data."
                        mv $localFile ${localFile}.nodata 2>> $SYSLOG
                    fi
                else
                    logMessage "Subsetting GFS with curl failed to produce a local grib2 file (or a file with zero length) when using the following command: '$cmd'."
                fi
                # if the file is still there, it passed its quality checks
                if [[ -s $localFile ]]; then
                    subsetSuccess=1
                    if [[ $how == "download" ]]; then
                        downloaded+=( $localFile )
                    else
                        have+=( $localFile )
                    fi
                    gfsForecastFiles+=( $localFile )
                else
                    numRetries=$(( numRetries + 1 ))
                    if [[ $how == "download" ]]; then
                        logMessage "GFS subsetting with curl failed. Sleeping 60 seconds before retry."
                        sleep 60
                    fi
                fi
            done
            if [[ $numRetries -ge $maxRetries ]]; then
                logMessage "Exceeded the max number of retries for downloading this file. Continuing with the forecast files that were downloaded successfully."
                break
            fi
        done
        #
        # now regrid to lat lon coordinates with wgrib2
        logMessage "$THIS: Regridding GFS forecast grib2 files to latlon."
        consoleMessage "$I Regridding GFS forecast grib2 files to latlon."
        unset gfsFileList
        declare -a gfsFileList
        for origFile in ${gfsForecastFiles[@]} ; do
            latLonFile=${origFile}.latlon
            if [[ ! -s $latLonFile ]]; then
                echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] Regridding GFS with wgrib2 using the following command: 'wgrib2 $origFile -inv /dev/null -set_grib_type same -new_grid_winds earth -new_grid latlon $lonSpec $latSpec $latLonFile 2>> $SYSLOG'." >> $downloadGfsLog
                wgrib2 $origFile          \
                    -inv /dev/null        \
                    -set_grib_type same   \
                    -new_grid_winds earth \
                    -new_grid latlon $lonSpec $latSpec \
                    $latLonFile
                    2>> $SYSLOG
            fi
            # add to the list of files that will have their contents
            # extracted for conversion to OceanWeather WIN/PRE ASCII format
            gfsFileList+=( $latLonFile )
        done
        #
        # grab the start time (YYYYMMDDHH) of the files from the
        # inventory in the first file
        owiWinPre["startDateTime"]=$(wgrib2 ${gfsFileList[0]} -match "PRMSL" 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
        date=${owiWinPre["startDateTime"]}
        owiStartEpochSeconds=$(TZ=UTC date -u -d "${date:0:4}-${date:4:2}-${date:6:2} ${date:8:2}:00:00" "+%s" 2>>$SYSLOG)
        #
        # determine the WTIMINC (time increment between datasets in seconds,
        # needed for the ADCIRC fort.15 file)
        incr=( $(wgrib2 ${gfsFileList[1]} -match "PRMSL" 2>> $SYSLOG | cut -d : -f 6) )
        WTIMINC=${incr[0]}
        if [[ ${incr[1]} == "hour" ]]; then
            WTIMINC=$(( $WTIMINC * 3600 ))
        else
            fatal "$THIS: ERROR: The time increment was specified as '${incr[1]}' which is not recognized."
        fi
        #
        # determine the NWBS (number of blank snaps between the
        # hotstart time and the start of the OWI WIN/PRE data)
        # under normal circumstances, will be equal to 0
        owiWinPre["NWBS"]=$(echo "scale=0; ($owiStartEpochSeconds - $hsEpochSeconds)/$WTIMINC" | bc)
        # write the fort.22 file
        fort22="fort.22"
        if [[ $BACKGROUNDMET == "gfsBlend" ]]; then
            fort22="owi_fort.22"
        fi
        echo ${owiWinPre["NWSET"]} > $fort22
        echo ${owiWinPre["NWBS"]} >> $fort22
        echo ${owiWinPre["DWM"]}  >> $fort22
        #
        # find the end time for use in the main file header
        incr=( $(wgrib2 ${gfsFileList[-1]} -match "PRMSL" 2>> $SYSLOG | cut -d : -f 6) )
        duration=${incr[0]}
        local sdt=${owiWinPre["startDateTime"]}
        if [[ ${incr[1]} == "hour" ]]; then
            owiWinPre["endDateTime"]=$(date -u --date="${sdt:0:4}-${sdt:4:2}-${sdt:6:2} ${sdt:8:10}:00:00 $duration hours" '+%Y%m%d%H' 2>>$SYSLOG)
        else
            fatal "$THIS: ERROR: The time increment was specified as '${incr[1]}' which is not recognized."
        fi
        #
        # write the headers to the win/pre files
        preFileName=GFS_${stage^^}_${owiWinPre["startDateTime"]}_${owiWinPre["endDateTime"]}.221
        winFileName=GFS_${stage^^}_${owiWinPre["startDateTime"]}_${owiWinPre["endDateTime"]}.222
        headerLineTemplate="$(printf "%s%38s%15s\n" "Oceanweather WIN/PRE Format" ${owiWinPre["startDateTime"]} ${owiWinPre["endDateTime"]})"
        headerLine="${headerLineTemplate:0:30}#${gfsDomain['coverage']}${headerLineTemplate:$(expr 31 + ${#gfsDomain['coverage']}):${#headerLineTemplate}}"
        echo "$headerLine" > $preFileName # fort.221
        echo "$headerLine" > $winFileName # fort.222
        #
        # extract the data from the grib2 files as ascii, reformat
        # into eight columns, and append the dataset to the corresponding file
        logMessage "$THIS: Writing ASCII WIN/PRE GFS forecast files."
        consoleMessage "$I Writing ASCII WIN/PRE GFS forecast files."
        unset winPreTimes
        for file in ${gfsFileList[@]}; do
            incr=( $(wgrib2 $file -match "PRMSL" 2>> $SYSLOG | cut -d : -f 6) )
            duration=${incr[0]}  # assumes this is in hours
            if [[ ${incr[0]} == "anl" ]]; then
                duration="0"
            fi
            snapDateTime=$(date -u --date="${sdt:0:4}-${sdt:4:2}-${sdt:6:2} ${sdt:8:10}:00:00 $duration hours" '+%Y%m%d%H' )
            headerLine="$(printf "iLat=%4diLong=%4dDX=%6.3fDY=%6.3fSWLat=%8.3fSWLon=%8.3fDT=%8d00" ${gfsLatLonGrid['nlat']} ${gfsLatLonGrid['nlon']} ${gfsLatLonGrid['dlon']} ${gfsLatLonGrid['dlat']} $SWLat $SWLon $snapDateTime)"
            winPreTimes+=( $snapDateTime )
            echo "$headerLine" >> $preFileName
            echo "$headerLine" >> $winFileName
            # convert barometric pressure from Pa to millibar in the process
            wgrib2 $file -match 'PRMSL' -inv /dev/null -text - 2>> $SYSLOG \
                | awk 'NR!=1 { printf("%10s",(sprintf("%4.4f",$1/100.0))); if ((NR-1)%8 == 0) print ""; }' \
                >> $preFileName 2>> $SYSLOG
            for var in "UGRD:10" "VGRD:10"; do
                wgrib2 $file -match "$var" -inv /dev/null -text - 2>> $SYSLOG \
                    | awk 'NR!=1 { printf("%10f",$1); if ((NR-1)%8 == 0) print ""; }' \
                    >> $winFileName 2>> $SYSLOG
            done
        done
        #
        # write metadata to JSON
        bashJSON.pl \
            --mapscalar get="$THIS" \
            --mapscalar winPreHeader="$(printf "%s%38s%15s" "Oceanweather WIN/PRE Format" ${owiWinPre["startDateTime"]} ${owiWinPre["endDateTime"]})" \
            --mapscalar winPreVelocityFile="$winFileName" \
            --mapscalar winPrePressureFile="$preFileName" \
            --mapscalar winPreRecordLength=$(( ${gfsLatLonGrid['nlon']} * ${gfsLatLonGrid['nlat']} )) \
            --mapscalar gfsForecastValidStart="${owiWinPre["startDateTime"]}0000" \
            --mapscalar winPreWtimincSeconds="$WTIMINC" \
            --mapscalar winPreNumRecords="${#winPreTimes[*]}" \
            --maparray winPreDataTimes="$(echo ${winPreTimes[@]})" \
            --maparray filesDownloaded="$(echo ${downloaded[@]})" \
            --maparray filesFromCache="$(echo ${have[@]})" \
            < select_gfs_nowcast.pl.json \
            > ${THIS}.json
            2>> $SYSLOG
        #
        # put files in scenario directory
        mv $winFileName $preFileName fort.22 $SCENARIODIR
        cp ${THIS}.json "${winFileName%.*}.json"
        cp get_gfs_status.pl.* ${THIS}.json "${winFileName%.*}.json" forecast.properties $SCENARIODIR
        cat forecast.properties >> ${SCENARIODIR}/run.properties
        cd $SCENARIODIR 2>> $SYSLOG
        # create links to the OWI WIN/PRE files with names that  ADCIRC expects
        ln -s $(basename $preFileName) fort.221 2>> $SYSLOG
        ln -s $(basename $winFileName) fort.222 2>> $SYSLOG
        cd ..
    fi
}
