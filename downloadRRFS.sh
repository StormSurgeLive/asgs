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
        # see if wgrib2 can return the inventory successfully
        wgrib2 $1 -inv /dev/null
        if [[ $? != 0 ]]; then
            msg="$THIS: The grib2 file '$1' appears to be corrupted."
            logMessage "$msg"
            consoleMessage "$I: $msg"
            appMessage "$msg" $downloadRrfsLog
        fi
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
        appMessage "The requested RRFS file '$1' was not found." $downloadRrfsLog
        mv $1 $instanceRrfsDir/${1}.errxml 2>> $SYSLOG
        return 1
    fi
    # make sure the downloaded file is at least 80kB (these files
    # seem to be about 100kB)
    if [[ $localFileSize -lt ${quality['size']} ]]; then
        appMessage "The file '$1' seems to be incomplete because it is only '$localFileSize' bytes." $downloadRrfsLog
        mv $1 $instanceRrfsDir/${1}.toosmall 2>> $SYSLOG
        return 1
    fi
    # I found 1691 lines in the one that I looked at
    if [[ $localFileLines -lt ${quality['lines']} ]]; then
        appMessage "The file '$1' only has '$localFileLines' line which does not seem to be enough." $downloadRrfsLog
        mv $1 $instanceRrfsDir/${1}.tooshort 2>> $SYSLOG
        return 1
    fi

    return 0
}



downloadRRFS()
{
    #
    local THIS="asgs_main.sh>downloadRRFS.sh"
    msg="$SCENARIO: $THIS: Polling for new RRFS meteorological data."
    logMessage "$msg"
    consoleMessage "$I $msg"
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
    # constant parameters related to RRFS forcing
    lonSpec="${rrfsLatLonGrid['lon0']}:${rrfsLatLonGrid['nlon']}:${rrfsLatLonGrid['dlon']}"
    latSpec="${rrfsLatLonGrid['lat0']}:${rrfsLatLonGrid['nlat']}:${rrfsLatLonGrid['dlat']}"
    declare -A rrfsVar
    rrfsVar['UGRD']='UGRD:10 m above ground'
    rrfsVar['VGRD']='VGRD:10 m above ground'
    rrfsVar['PRES']='PRES:surface'
    delare -a winPreExt
    case ${rrfsDomain['coverage']} in
    "basin")
        winPreExt=( 221 222 )
        ;;
    "region")
        winPreExt=( 223 224 )
        ;;
    "landfall")
        winPreExt=( 217 218 )
        ;;
    *)
        # should be unreachable
        fatal "$THIS: Invalid RRFS domain coverage: '${rrfs[coverage]}'; must be basin, region, or landfall."
        ;;
    esac
    # example data path
    # aws s3 ls --no-sign-request s3://noaa-rrfs-pds/rrfs_a/rrfs.20251004/18/
    # -> 2025-10-04 15:12:46     102829 rrfs.t18z.natlev.3km.f000.na.grib2.idx
    #-------------------------------------------------------
    #
    #                  N O W C A S T
    #
    #-------------------------------------------------------
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
            -e "s?%NULLSCRIPTDIR%?${SCRIPTDIR//$USER/\$USER}?" \
            -e "s?%LOCALDATADIR%?${instanceRrfsDir//$USER/\$USER}?" \
            -e "s?\"%NULLFORECASTCYCLES%\"?$(echo ${forecastCycleArray%?})?" \
            -e "s?%NULLDOMAIN%?${rrfsDomain['coverage']}?" \
            -e "s?\"%NULLLON0%\"?${rrfsLatLonGrid['lon0']}?" \
            -e "s?\"%NULLNLON%\"?${rrfsLatLonGrid['nlon']}?" \
            -e "s?\"%NULLDLON%\"?${rrfsLatLonGrid['dlon']}?" \
            -e "s?\"%NULLLAT0%\"?${rrfsLatLonGrid['lat0']}?" \
            -e "s?\"%NULLNLAT%\"?${rrfsLatLonGrid['nlat']}?" \
            -e "s?\"%NULLDLAT%\"?${rrfsLatLonGrid['dlat']}?" \
            < $SCRIPTDIR/${rrfs['TemplateName']} \
            > "$instanceRrfsDir/${rrfs['FilledTemplateName']}" \
            2>> $SYSLOG
        if [[ $? != 0 ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Failed to fill in RRFS data request template with sed."
        fi
        # stop here if we are only testing the filling of the JSON template
        if [[ $breakPoint == "rrfs.template" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #   C A T A L O G   A V A I L A B L E   D A T A
        #
        # determine the status of the latest RRFS cycle posted by NCEP,
        # along with the range of cycles posted since the adcirc hotstart time
        sleepSeconds=$(( ${rrfs['PollingInterval']} * 60 ))
        latestCycle=0
        tries=0
        printf "." # progress bar
        while [[ $latestCycle -le $lastCycle ]]; do
            if [[ $tries -ne 0 ]]; then
                spinner $sleepSeconds
                printf "\b.."
            fi
            ((tries++))
            #
            #   D O W N L O A D   C Y C L E   I N D E X   F I L E S
            #
            # getting rrfs status : start with a list of dates beginning on the hotstart
            # date and progressing for a number of days equal to rrfsLookAhead, check for
            # the existence of a grib2 index file for each cycle
            unset cycleList
            declare -a cycleList
            for d in $(seq 0 ${rrfs['LookAhead']}); do
                cycleDate=$( TZ=UTC date --date="${lastCycle:0:8} +$d day" +"%Y%m%d" )
                for h in 00 06 12 18; do
                    # don't need it if it is before the current hotstart time
                    if [[ $cycleDate$h -lt $lastCycle ]]; then
                        continue
                    fi
                    indexFileName=rrfs.t${h}z.natlev.3km.f000.na.grib2.idx
                    if [[ -e $instanceRrfsDir/$cycleDate/$h/$indexFileName ]]; then
                        cycleList+=( $cycleDate$h )
                    else
                        # check to see if the grib2 index file is available from NCEP
                        # be a good citizen and just ask for the http header
                        curlCommand="curl --silent --head ${rrfs['BaseURL']}/rrfs.$cycleDate/$h/$indexFileName"
                        indexFileStatus=$($curlCommand | head -n 1 | tr -d '\r')
                        if [[ ! $indexFileStatus =~ "200" ]]; then
                            appMessage "Could not find the grib2 index file on the remote web server; the HTTP status was '$indexFileStatus' using the curl command '$curlCommand'." $downloadRrfsLog
                            break 2
                        fi
                        # this index file has been posted, apparently; download it
                        curlCommand="curl --silent -O ${rrfs['BaseURL']}/rrfs.$cycleDate/$h/$indexFileName"
                        appMessage "curlCommand=$curlCommand" $downloadRrfsLog
                        $curlCommand 2>> $SYSLOG
                        exitCode=$?
                        if [[ $exitCode != 0 ]]; then
                            appMessage "The curl command '$curlCommand' to download the RRFS index file has failed with exit code '$exitCode'." $downloadRrfsLog
                            break 2
                        else
                            performQualityChecksRRFS $indexFileName indexFile
                            if [[ $? -ne 0 ]]; then
                                break 2
                            fi
                        fi
                        # if the index file was downloaded successfully and passed quality checks,
                        # place it in the local cache
                        cycleList+=( $cycleDate$h )
                        if [[ ! -e $instanceRrfsDir/$cycleDate/$h/$indexFileName ]]; then
                            if [[ ! -d $instanceRrfsDir/$cycleDate/$h ]]; then
                                mkdir -p $instanceRrfsDir/$cycleDate/$h 2>> $SYSLOG
                                mv $indexFileName $instanceRrfsDir/$cycleDate/$h 2>> $SYSLOG
                            fi
                        fi
                    fi
                done
            done
            if [[ ${#cycleList[@]} -ne 0 ]]; then
                latestCycle=${cycleList[-1]}
                # update the cyclelist in json
                cycleListStr=$( (IFS=","; echo "${cycleList[*]}") )
                sed -e "s?%NULLCYCLELIST%?$cycleListStr?" < $instanceRrfsDir/${rrfs['FilledTemplateName']} > $instanceRrfsDir/get_rrfs_status.json 2>> $SYSLOG
            else
                latestCycle=0
            fi
        done
        # stop here if we are only testing through the catalogging of the remote site
        if [[ $breakPoint == "rrfs.template.catalog" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #     N E W   N O W C A S T   C Y C L E
        #        H A S   B E E N   P O S T E D
        #
        # get the latest cycle that we want to nowcast to, if specific
        # forecast cycles have been specified; remove any forecast
        # cycles after the most recent one that we want to nowcast to
        msg="$THIS: A new RRFS nowcast cycle has been posted: '$latestCycle'."
        logMessage "$msg"
        consoleMessage "$I $msg"
        forecastCycles=( $(echo ${FORECASTCYCLE//,/' '}) )
        if [[ ${#forecastCycles[@]} -gt 0 ]]; then
            forecastFound=0
            extraCycles=0
            for (( c=${#cycleList[@]}-1; c>=1; c-- )) ; do
                if [[ ${cycleList[$c]} -le $lastCycle ]]; then
                    break
                fi
                cycleday=${cycleList[$c]:0:8}
                cyclehour=${cycleList[$c]:8:2}
                # loop over the forecast cycles to see if this
                # cycle has been specified
                for f in ${forecastCycles[@]}; do
                    if [[ $f == $cyclehour ]]; then
                        forecastFound=1
                        break 2
                    fi
                done
                ((extraCycles++))
            done
            # now remove the excess cycles that are beyond the end of the
            # required nowcast period, if any
            if [[ $forecastFound == 1 ]]; then
                cycleList=( ${cycleList[@]:0:(( ${#cycleList[@]} - $extraCycles ))} )
            fi
        fi
        cycleListStr=$( (IFS=","; echo "${cycleList[*]}") )
        sed -e "s?\"%NULLCYCLELIST%\"?$cycleListStr?" < $instanceRrfsDir/${rrfs['FilledTemplateName']} > $instanceRrfsDir/select_rrfs_nowcast.json 2>> $SYSLOG
        # stop here if we are only testing through the removal of extra cycles
        if [[ $breakPoint == "rrfs.template.catalog.select" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        thisCycle=${cycleList[-1]}
        #
        #   G E T    H O U R L Y   N O W C A S T
        #     G R I B 2   I N D E X   F I L E S
        #
        msg="$THIS: Downloading hourly RRFS nowcast grib2 files through cycle '$thisCycle'."
        logMessage "$msg"
        consoleMessage "$I $msg"
        numHourlyCycles=$(( ((${#cycleList[@]} - 1 ) * 6 ) + 1 ))
        for c in ${cycleList[@]:0:${#cycleList[@]}-1} ; do
            cycleDate=${c:0:8}
            cycleHour=${c:8:2}
            # don't need any hourly cycles after the cycle
            # we are nowcasting to (i.e., after thisCycle)
            if [[ $c == $thisCycle ]]; then
                break
            fi
            # need the index files for the nowcast or "analysis" at each hour
            # between the cycle times
            for h in $(seq 0 5); do
                hh=$(printf "%02d" $(( $cycleHour + $h )) )
                indexFileName=rrfs.t${hh}z.natlev.3km.f000.na.grib2.idx
                if [[ ! -e $instanceRrfsDir/$cycleDate/$hh/$indexFileName ]]; then
                    # check to see if it is available from NCEP
                    curlCommand="curl --silent -O ${rrfs['BaseURL']}/rrfs.$cycleDate/$hh/$indexFileName"
                    appMessage "curlCommand=$curlCommand" $downloadRrfsLog
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
                        if [[ ! -d $instanceRrfsDir/$cycleDate/$hh ]]; then
                            mkdir -p $instanceRrfsDir/$cycleDate/$hh 2>> $SYSLOG
                        fi
                        mv $indexFileName $instanceRrfsDir/$cycleDate/$hh 2>> $SYSLOG
                    fi
                fi
            done
        done
        # stop here if we are only testing through the download of hourly
        # nowcast index files
        if [[ $breakPoint == "rrfs.template.catalog.select.hourly" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #    C O M P U T E   B Y T E   R A N G E S
        #   A N D   D O W N L O A D   S U B S E T S
        #         B Y   V A R I A B L E
        #
        msg="$THIS: Computing byte ranges and downloading subsets by variable."
        logMessage "$msg"
        consoleMessage "$I $msg"
        needed=$(( $numHourlyCycles * 3)) # separate downloads for UGRD, VGRD, and PRES
        succeeded=0
        tries=0
        r=$instanceRrfsDir/ranges
        while [[ $succeeded -lt $needed ]]; do
            if [[ $tries -ne 0 ]]; then
                msg="$THIS: Failed to download grib2 subsets of meteorological data. Succeeded for '$succeeded' out of '$needed' files. Waiting 60 seconds before trying again."
                logMessage "$msg"
                consoleMessage "$W $msg"
                spinner 60
            fi
            unset downloaded
            unset have
            for c in ${cycleList[@]} ; do
                cycleDate=${c:0:8}
                cycleHour=${c:8:2}
                # need the index files for the nowcast or "analysis" at each hour
                # between the cycle times
                for h in $(seq 0 5); do
                    # on the last cycle we only need the data for the
                    # cycle time itself, not the hourly data after that
                    if [[ $c == ${cycleList[-1]} && $h -gt 0 ]]; then
                        break
                    fi
                    hh=$(printf "%02d" $(( $cycleHour + $h )) )
                    indexFileDir=$instanceRrfsDir/$cycleDate/$hh
                    indexFileName=rrfs.t${hh}z.natlev.3km.f000.na.grib2.idx
                    if [[ ! -e $indexFileDir/$indexFileName.range ]]; then
                        awk 'BEGIN { FS=":" ; startRange=0 } NR==1 { startRange=$2 } NR>1 { print "range="startRange"-"($2-1) ; startRange=$2 }' $indexFileDir/$indexFileName > $r 2>> $SYSLOG
                        paste -d "" $indexFileDir/$indexFileName $r > $indexFileDir/$indexFileName.range 2>> $SYSLOG
                    fi
                    # attempt to download specific byte ranges via curl
                    for v in UGRD VGRD PRES; do
                        byteRange=$(grep "${rrfsVar[$v]}" $indexFileDir/$indexFileName.range | grep -Eo '[0-9]*-[0-9]*')
                        grib2FileName=${indexFileName%.idx}
                        if [[ ! -e $indexFileDir/$v.$grib2FileName ]]; then
                            curlCommand="curl --range $byteRange --silent -o $v.$grib2FileName ${rrfs['BaseURL']}/rrfs.$cycleDate/$hh/$grib2FileName 2>> $SYSLOG"
                            appMessage "curlCommand=$curlCommand" $downloadRrfsLog
                            $curlCommand
                            performQualityChecksRRFS $v.$grib2FileName grib2File
                            if [[ $? -ne 0 ]]; then
                                rm $v.$grib2FileName 2>> $SYSLOG
                                break 3
                            fi
                            # the subset downloaded successfully and passed quality checks,
                            # copy it to the local grib2 file cache
                            mv $v.$grib2FileName $indexFileDir 2>> $SYSLOG
                            downloaded+=( $cycleDate/$hh/$v.$grib2FileName )
                        else
                            have+=( $cycleDate/$hh/$v.$grib2FileName )
                        fi
                    done
                    # concatenate into a single grib2 file so that UGRD and VGRD can be
                    # regridded and reprojected as vectors
                    if [[ ! -s $grib2FileName ]]; then
                        cat $indexFileDir/UGRD.$grib2FileName $indexFileDir/VGRD.$grib2FileName $indexFileDir/PRES.$grib2FileName > $indexFileDir/$grib2FileName
                    fi
                done
            done
            succeeded=$(( ${#downloaded[@]} + ${#have[@]} ))
            ((tries++))
        done
        # stop here if we are only testing through the download of hourly
        # nowcast grib2 subset files
        if [[ $breakPoint == "rrfs.template.catalog.select.hourly.download" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #            R E G R I D   T O   G E O G R A P H I C
        #     P R O J E C T I O N   O N   S P E C I F I E D   D O M A I N
        #
        msg="$THIS: Regridding RRFS grib2 files to latlon."
        logMessage "$msg"
        consoleMessage "$I $msg"
        tries=0
        succeeded=0
        while [[ $succeeded -lt $numHourlyCycles ]]; do
            if [[ $tries -ne 0 ]]; then
                msg="$THIS: Tried '$tries' time(s) and Failed to regrid/reproject meteorological data. Waiting 60 seconds before trying again."
                logMessage "$msg"
                consoleMessage "$W $msg"
                spinner 60
                succeeded=0
            fi
            for c in ${cycleList[@]} ; do
                cycleDate=${c:0:8}
                cycleHour=${c:8:2}
                for h in $(seq 0 5); do
                    # on the last cycle we only need the data for the
                    # cycle time itself, not the hourly data after that
                    if [[ $c == ${cycleList[-1]} && $h -gt 0 ]]; then
                        break 2
                    fi
                    hh=$(printf "%02d" $(( $cycleHour + $h )) )
                    origFile=$instanceRrfsDir/$cycleDate/$hh/rrfs.t${hh}z.natlev.3km.f000.na.grib2
                    latLonFile=${origFile}.latlon_lonSpec.${lonSpec}_latSpec.$latSpec
                    # if this file has not been regridded, or the regridded file
                    # is 0 length
                    if [[ ! -s $latLonFile ]]; then
                        regridCommand="wgrib2 $origFile -inv /dev/null -set_grib_type same -new_grid_winds earth -new_grid latlon $lonSpec $latSpec $latLonFile"
                        appMessage "Regridding RRFS using '$regridCommand'." $downloadRrfsLog
                        $regridCommand 2>> $SYSLOG
                        if [[ $? -ne 0 ]]; then
                            logMessage "$THIS: The regridding command '$regridCommand' failed with exit code '$?'."
                            if [[ -e $latLonFile ]]; then
                                rm $latLonFile 2>> $SYSLOG
                            fi
                        else
                            ((succeeded++))
                        fi
                    else
                        appMessage "The regridded file '$latLonFile' was already available in the local RRFS cache." $downloadRrfsLog
                        ((succeeded++))
                    fi
                done
            done
            ((tries++))
        done
        # stop here if we are only testing through the regridding of hourly
        # nowcast grib2 subset files
        if [[ $breakPoint == "rrfs.template.catalog.select.hourly.download.regrid" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #    W R I T E   A S C I I   O W I
        #     W I N / P R E   F I L E S
        #
        # form win/pre file header line
        SWLat=$(printf "%3.4f" ${rrfsLatLonGrid['lat0']})
        SWLon=$(printf "%3.4f" ${rrfsLatLonGrid['lon0']})
        # form the file names of the first, second, and last grib2 files
        # to extract timing information
        firstFile=$instanceRrfsDir/${cycleList[0]:0:8}/${cycleList[0]:8:2}/rrfs.t${cycleList[0]:8:2}z.natlev.3km.f000.na.grib2
        secondFile=$instanceRrfsDir/${cycleList[1]:0:8}/${cycleList[1]:8:2}/rrfs.t${cycleList[1]:8:2}z.natlev.3km.f000.na.grib2
        lastFile=$instanceRrfsDir/${cycleList[-1]:0:8}/${cycleList[-1]:8:2}/rrfs.t${cycleList[-1]:8:2}z.natlev.3km.f000.na.grib2
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
        preFileName=RRFS_${stage^^}_${owiWinPre["startDateTime"]}_${owiWinPre["endDateTime"]}.${winPreExt[0]}
        winFileName=RRFS_${stage^^}_${owiWinPre["startDateTime"]}_${owiWinPre["endDateTime"]}.${winPreExt[1]}
        headerLineTemplate="$(printf "%s%38s%15s\n" "Oceanweather WIN/PRE Format" ${owiWinPre["startDateTime"]} ${owiWinPre["endDateTime"]})"
        winPreHeader="${headerLineTemplate:0:30}#${rrfsDomain['coverage']}${headerLineTemplate:$(expr 31 + ${#rrfsDomain['coverage']}):${#headerLineTemplate}}"
        echo "$winPreHeader" > $preFileName
        echo "$winPreHeader" > $winFileName
        #
        # extract the data from the grib2 files as ascii, reformat
        # into eight columns, and append the dataset to the corresponding file
        msg="$THIS: Writing RRFS ASCII WIN/PRE files."
        logMessage "$msg"
        consoleMessage "$I $msg"
        appMessage "$msg" $downloadRrfsLog
        unset winPreTimes
        for c in ${cycleList[@]} ; do
            cycleDate=${c:0:8}
            cycleHour=${c:8:2}
            for h in $(seq 0 5); do
                # on the last cycle we only need the data for the
                # cycle time itself, not the hourly data after that
                if [[ $c == ${cycleList[-1]} && $h -gt 0 ]]; then
                    break 2
                fi
                hh=$(printf "%02d" $(( $cycleHour + $h )) )
                f=$instanceRrfsDir/$cycleDate/$hh/rrfs.t${hh}z.natlev.3km.f000.na.grib2.latlon_lonSpec.${lonSpec}_latSpec.$latSpec
                d=$(wgrib2 $f -match 'PRES' 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
                headerLine="$(printf "iLat=%4diLong=%4dDX=%6.3fDY=%6.3fSWLat=%8.3fSWLon=%8.3fDT=%8d00" ${rrfsLatLonGrid['nlat']} ${rrfsLatLonGrid['nlon']} ${rrfsLatLonGrid['dlon']} ${rrfsLatLonGrid['dlat']} $SWLat $SWLon $d)"
                winPreTimes+=( $d"00" )
                echo "$headerLine" >> $preFileName
                echo "$headerLine" >> $winFileName
                # replace missing surface pressure values with 101300 Pa (ADCIRC default)
                # convert barometric pressure from Pa to millibar in the process
                wgrib2 $f -match 'PRES' -inv /dev/null -text - 2>>$SYSLOG \
                    | sed 's/9.999e+20/101300.0/g' \
                    | awk 'NR!=1 { printf("%10s",(sprintf("%4.4f",$1/100.0))); if ((NR-1)%8 == 0) print ""; }' \
                    >> $preFileName 2>> $SYSLOG
                for v in UGRD VGRD ; do
                    # replace missing velocity values with 0.0 m/s
                    wgrib2 $f -match $v -inv /dev/null -text - 2>> $SYSLOG \
                        | sed 's/9.999e+20/0.0/g' \
                        | awk 'NR!=1 { printf("%10f",$1); if ((NR-1)%8 == 0) print ""; }' \
                        >> $winFileName 2>> $SYSLOG
                done
            done
        done
        # stop here if we are only testing through the writing of owi win/pre
        # nowcast files
        if [[ $breakPoint == "rrfs.template.catalog.select.hourly.download.regrid.owiwinpre" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #          W R I T E   M E T A D A T A   T O   J S O N
        #
        # write metadata to JSON
        winPreRecordLength=$(( ${rrfsLatLonGrid['nlon']} * ${rrfsLatLonGrid['nlat']} ))
        rrfsForecastValidStart="${owiWinPre["startDateTime"]}0000"
        downloadedFilesArray=$(printf "\"%s\"," ${downloaded[@]})
        haveFilesArray=$(printf "\"%s\"," ${have[@]})
        winPreTimesArray=$(printf "\"%s\"," ${winPreTimes[@]})
        sed \
            -e "s?%WINPREHEADER%?$winPreHeader?" \
            -e "s?%WINPREVELOCITYFILE%?$winFileName?" \
            -e "s?%WINPREPRESSUREFILE%?$preFileName?" \
            -e "s?%WINPRERECORDLENGTH%?$winPreRecordLength?" \
            -e "s?%WINPREWTIMINCSECONDS%?$WTIMINC?" \
            -e "s?%WINPRENUMRECORDS%?${#winPreTimes[*]}?" \
            -e "s?\"%WINPREDATATIMES%\"?$(echo ${winPreTimesArray%?})?" \
            -e "s?\"%NULLDOWNLOADED%\"?$(echo ${downloadedFilesArray%?})?" \
            -e "s?\"%NULLFOUND%\"?$(echo ${haveFilesArray%?})?" \
            < $instanceRrfsDir/select_rrfs_nowcast.json \
            > $instanceRrfsDir/downloadRRFS.json \
            2>> $SYSLOG
        if [[ $? != 0 ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Failed to fill in RRFS data request template with sed."
        fi
        # stop here if we are only testing through the writing of metadata for the
        # owi win/pre nowcast files
        if [[ $breakPoint == "rrfs.template.catalog.select.hourly.download.regrid.owiwinpre.metadata" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #   M O V E   F I L E S   T O   S C E N A R I O   D I R E C T O R Y
        #
        if [[ $BACKGROUNDMET == "RRFS" ]]; then # don't need to do this for "rrfsBlend"
            CYCLE=$thisCycle
            ADVISORY=$CYCLE
            CYCLEDIR=$RUNDIR/$CYCLE
            SCENARIODIR=$CYCLEDIR/$SCENARIO
            CYCLELOG=$CYCLEDIR/cycle.log
            NOWCASTDIR=$SCENARIODIR
            SCENARIOLOG=$SCENARIODIR/scenario.log
            mkdir -p $SCENARIODIR 2>> $SYSLOG
            ADVISDIR=$CYCLEDIR
            #
            # record the new advisory number to the statefile
            debugMessage "$THIS: $SCENARIO: The new RRFS cycle is '$CYCLE'."
            cp -f $STATEFILE ${STATEFILE}.old 2>> ${SYSLOG} 2>&1
            sed 's/ADVISORY=.*/ADVISORY='$CYCLE'/' $STATEFILE > ${STATEFILE}.new
            debugMessage "Updating statefile $STATEFILE with new cycle number '$CYCLE'."
            cp -f ${STATEFILE}.new $STATEFILE 2>> ${SYSLOG} 2>&1
        fi
        #
        # put files in scenario directory
        mv $winFileName $preFileName $fort22 $SCENARIODIR 2>> $SYSLOG
        mv $instanceRrfsDir/downloadRRFS.json "$SCENARIODIR/${winFileName%.*}.json" 2>> $SYSLOG
        #
        cd $SCENARIODIR 2>> $SYSLOG
        # create links to the OWI WIN/PRE files with names that  ADCIRC expects
        ln -s $(basename $preFileName) fort.221 2>> $SYSLOG
        ln -s $(basename $winFileName) fort.222 2>> $SYSLOG
        if [[ $BACKGROUNDMET == "rrfsBlend" ]]; then
            ln -s $fort22 fort.22 2>> $SYSLOG
        fi
        cd $RUNDIR
        # nowcast files
        if [[ $breakPoint == "rrfs.template.catalog.select.hourly.download.regrid.owiwinpre.metadata.scenariodir" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
    else
        #-------------------------------------------------------
        #
        #                F O R E C A S T
        #
        #-------------------------------------------------------
        # epoch seconds associated with cold start and hotstart times
        CYCLE=$ADVISORY
        CYCLEDIR=$RUNDIR/$CYCLE
        SCENARIODIR=$RUNDIR/$CYCLE/$SCENARIO
        if [[ ! -d $SCENARIODIR ]]; then
            mkdir -p $SCENARIODIR
        fi
        # write the forecast.properties file
        echo "forecastValidStart : ${CYCLE}0000" > $instanceRrfsDir/forecast.properties
        #
        #       D O W N L O A D   G R I B 2   I N D E X
        #         F I L E S   F O R   F O R E C A S T
        #
        # form the list of files to download
        declare -a rrfsForecastFiles
        msg="$THIS: Downloading hourly RRFS forecast grib2 files for cycle '$CYCLE'."
        logMessage "$msg"
        consoleMessage "$I $msg"
        cycleDate=${CYCLE:0:8}
        hh=$(printf "%02d" ${CYCLE:8:2})
        succeeded=0
        tries=0
        r=$instanceRrfsDir/ranges
        while [[ $succeeded -lt  ${rrfs['ForecastLength']} ]]; do
            if [[ $tries -ne 0 ]]; then
                msg="$THIS: Tried '$tries' time(s) and failed to download all meteorological forecast data for cycle '$CYCLE'. Waiting 60 seconds before trying again."
                logMessage "$msg"
                consoleMessage "$W $msg"
                spinner 60
                succeeded=0
            fi
            unset downloaded have
            for h in $(seq 0 ${rrfs['ForecastLength']}) ; do
                hhh=$(printf "%03d" $h)
                indexFileName=rrfs.t${hh}z.natlev.3km.f${hhh}.na.grib2.idx
                if [[ ! -s $instanceRrfsDir/$cycleDate/$hh/$indexFileName ]]; then
                    # check to see if the grib2 index file is available
                    # be a good citizen and just ask for the http header
                    curlCommand="curl --silent --head ${rrfs['BaseURL']}/rrfs.$cycleDate/$hh/$indexFileName"
                    indexFileStatus=$($curlCommand | head -n 1 | tr -d '\r')
                    if [[ ! $indexFileStatus =~ "200" ]]; then
                        appMessage "Could not find the grib2 index file on the remote web server; the HTTP status was '$indexFileStatus' using the curl command '$curlCommand'." $downloadRrfsLog
                        break
                    fi
                    curlCommand="curl --silent -O ${rrfs['BaseURL']}/rrfs.$cycleDate/$hh/$indexFileName"
                    appMessage "curlCommand=$curlCommand" $downloadRrfsLog
                    $curlCommand
                    exitCode=$?
                    if [[ $exitCode != 0 ]]; then
                        appMessage "The curl command '$curlCommand' to download the RRFS index file has failed with exit code '$exitCode'." $downloadRrfsLog
                        break
                    else
                        performQualityChecksRRFS $indexFileName indexFile
                        if [[ $? -ne 0 ]]; then
                            break
                        fi
                        # if the index file was downloaded successfully and passed quality checks
                        # place the index file with computed ranges in the local cache
                        if [[ ! -d $instanceRrfsDir/$cycleDate/$hh ]]; then
                            mkdir -p $instanceRrfsDir/$cycleDate/$hh 2>> $SYSLOG
                        fi
                        mv $indexFileName $instanceRrfsDir/$cycleDate/$hh 2>> $SYSLOG
                        downloaded+=( $cycleDate/$hh/$indexFileName )
                    fi
                else
                    have+=( $cycleDate/$hh/$indexFileName )
                fi
            done
            succeeded=$(( ${#downloaded[@]} + ${#have[@]} ))
            ((tries++))
        done
        if [[ $breakPoint == "rrfs.forecast.index" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #    C O M P U T E   B Y T E   R A N G E S   A N D
        #     D O W N L O A D   G R I B 2   S U B S E T S
        #
        msg="$THIS: Computing byte ranges and downloading forecast subsets by variable."
        logMessage "$msg"
        consoleMessage "$I $msg"
        needed=$(( ${rrfs['ForecastLength']} * 3)) # separate downloads for UGRD, VGRD, and PRES
        succeeded=0
        tries=0
        while [[ $succeeded -lt $needed ]]; do
            if [[ $tries -ne 0 ]]; then
                msg="$THIS: Failed to download grib2 subsets of forecast meteorological data. Succeeded for '$succeeded' out of '$needed' files. Waiting 60 seconds before trying again."
                logMessage "$msg"
                consoleMessage "$W $msg"
                spinner 60
            fi
            unset downloaded
            unset have
            for h in $(seq 0 ${rrfs['ForecastLength']}) ; do
                hhh=$(printf "%03d" $h)
                indexFileDir=$instanceRrfsDir/$cycleDate/$hh
                indexFileName=rrfs.t${hh}z.natlev.3km.f${hhh}.na.grib2.idx
                if [[ ! -e $indexFileDir/$indexFileName.range ]]; then
                    awk 'BEGIN { FS=":" ; startRange=0 } NR==1 { startRange=$2 } NR>1 { print "range="startRange"-"($2-1) ; startRange=$2 }' $indexFileDir/$indexFileName > $r 2>> $SYSLOG
                    paste -d "" $indexFileDir/$indexFileName $r > $indexFileDir/$indexFileName.range 2>> $SYSLOG
                fi
                # attempt to download specific byte ranges via curl
                for v in UGRD VGRD PRES; do
                    byteRange=$(grep "${rrfsVar[$v]}" $indexFileDir/$indexFileName.range 2>>$SYSLOG | grep -Eo '[0-9]*-[0-9]*')
                    grib2FileName=${indexFileName%.idx}
                    if [[ ! -e $indexFileDir/$v.$grib2FileName ]]; then
                        curlCommand="curl --range $byteRange --silent -o $v.$grib2FileName ${rrfs['BaseURL']}/rrfs.$cycleDate/$hh/$grib2FileName 2>> $SYSLOG"
                        appMessage "curlCommand=$curlCommand" $downloadRrfsLog
                        $curlCommand
                        performQualityChecksRRFS $v.$grib2FileName grib2File
                        if [[ $? -ne 0 ]]; then
                            rm $v.$grib2FileName 2>> $SYSLOG
                            break 2
                        fi
                        # the subset downloaded successfully and passed quality checks,
                        # move it to the local grib2 file cache
                        mv $v.$grib2FileName $indexFileDir 2>> $SYSLOG
                        downloaded+=( $cycleDate/$hh/$v.$grib2FileName )
                    else
                        have+=( $cycleDate/$hh/$v.$grib2FileName )
                    fi
                done
                # concatenate into a single grib2 file so that UGRD and VGRD can be
                # regridded and reprojected as vectors
                if [[ ! -s $indexFileDir/grib2FileName ]]; then
                    cat $indexFileDir/UGRD.$grib2FileName $indexFileDir/VGRD.$grib2FileName $indexFileDir/PRES.$grib2FileName > $indexFileDir/$grib2FileName 2>>$SYSLOG
                fi
            done
            succeeded=$(( ${#downloaded[@]} + ${#have[@]} ))
            ((tries++))
        done
        if [[ $breakPoint == "rrfs.forecast.index.subset" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #       R E G R I D   A N D   R E P R O J E C T
        #                  F O R E C A S T
        #
        msg="$THIS: Regridding RRFS grib2 forecast files to latlon."
        logMessage "$msg"
        consoleMessage "$I $msg"
        tries=0
        succeeded=0
        while [[ $succeeded -lt ${rrfs['ForecastLength']} ]]; do
            if [[ $tries -ne 0 ]]; then
                msg="$THIS: Tried '$tries' time(s) and Failed to regrid/reproject meteorological forecast data. Waiting 60 seconds before trying again."
                logMessage "$msg"
                consoleMessage "$W $msg"
                spinner 60
                succeeded=0
            fi
            for h in $(seq 0 ${rrfs['ForecastLength']}) ; do
                hhh=$(printf "%03d" $h)
                origFile=$instanceRrfsDir/$cycleDate/$hh/rrfs.t${hh}z.natlev.3km.f${hhh}.na.grib2
                latLonFile=${origFile}.latlon_lonSpec.${lonSpec}_latSpec.$latSpec
                # if this file has not been regridded, or the regridded file
                # is 0 length
                if [[ ! -s $latLonFile ]]; then
                    regridCommand="wgrib2 $origFile -inv /dev/null -set_grib_type same -new_grid_winds earth -new_grid latlon $lonSpec $latSpec $latLonFile"
                    appMessage "Regridding RRFS forecast using '$regridCommand'." $downloadRrfsLog
                    $regridCommand 2>> $SYSLOG
                    if [[ $? -ne 0 ]]; then
                        logMessage "$THIS: The regridding command '$regridCommand' failed with exit code '$?'."
                        if [[ -e $latLonFile ]]; then
                            rm $latLonFile 2>> $SYSLOG
                        fi
                    else
                        ((succeeded++))
                    fi
                else
                    appMessage "The regridded file '$latLonFile' was already available in the local RRFS cache." $downloadRrfsLog
                    ((succeeded++))
                fi
            done
            ((tries++))
        done
        if [[ $breakPoint == "rrfs.forecast.index.subset.regrid" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #    W R I T E   A S C I I   O W I
        #     W I N / P R E   F I L E S
        #
        msg="$THIS: Writing ASCII OWI WIN/PRE formatted forecast files."
        logMessage "$msg"
        consoleMessage "$I $msg"
        # make a list of the files available
        rrfsFileList=( $(ls $instanceRrfsDir/$cycleDate/$hh/rrfs.t${hh}z.natlev.3km.f???.na.grib2.latlon_lonSpec.${lonSpec}_latSpec.$latSpec | sort) )
        # grab the start time (YYYYMMDDHH) of the files from the
        # inventory in the first file
        owiWinPre["startDateTime"]=$(wgrib2 ${rrfsFileList[0]} -match "PRES" 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
        d=${owiWinPre["startDateTime"]}
        owiStartEpochSeconds=$(TZ=UTC date -u -d "${d:0:4}-${d:4:2}-${d:6:2} ${d:8:2}:00:00" "+%s" 2>>$SYSLOG)
        #
        # determine the WTIMINC (time increment between datasets in seconds,
        # needed for the ADCIRC fort.15 file)
        incr=( $(wgrib2 ${rrfsFileList[1]} -match "PRES" 2>> $SYSLOG | cut -d : -f 6) )
        WTIMINC=${incr[0]}
        if [[ ${incr[1]} == "hour" ]]; then
            WTIMINC=$(( $WTIMINC * 3600 ))
        else
            fatal "$THIS: ERROR: The forecast time increment was specified as '${incr[1]}' which is not recognized."
        fi
        #
        # determine the NWBS (number of blank snaps between the
        # hotstart time and the start of the OWI WIN/PRE data)
        # under normal circumstances, will be equal to 0
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
        incr=( $(wgrib2 ${rrfsFileList[-1]} -match "PRES" 2>> $SYSLOG | cut -d : -f 6) )
        duration=${incr[0]}
        local sdt=${owiWinPre["startDateTime"]}
        if [[ ${incr[1]} == "hour" ]]; then
            owiWinPre["endDateTime"]=$(date -u --date="${sdt:0:4}-${sdt:4:2}-${sdt:6:2} ${sdt:8:10}:00:00 $duration hours" '+%Y%m%d%H' 2>>$SYSLOG)
        else
            fatal "$THIS: ERROR: The forecast time increment was specified as '${incr[1]}' which is not recognized."
        fi
        #
        # write the headers to the win/pre files
        preFileName=RRFS_${stage^^}_${owiWinPre["startDateTime"]}_${owiWinPre["endDateTime"]}.${winPreExt[0]}
        winFileName=RRFS_${stage^^}_${owiWinPre["startDateTime"]}_${owiWinPre["endDateTime"]}.${winPreExt[1]}
        headerLineTemplate="$(printf "%s%38s%15s\n" "Oceanweather WIN/PRE Format" ${owiWinPre["startDateTime"]} ${owiWinPre["endDateTime"]})"
        winPreHeader="${headerLineTemplate:0:30}#${rrfsDomain['coverage']}${headerLineTemplate:$(expr 31 + ${#rrfsDomain['coverage']}):${#headerLineTemplate}}"
        echo "$winPreHeader" > $preFileName
        echo "$winPreHeader" > $winFileName
        #
        # extract the data from the grib2 files as ascii, reformat
        # into eight columns, and append the dataset to the corresponding file
        unset winPreTimes
        for file in ${rrfsFileList[@]}; do
            incr=( $(wgrib2 $file -match "PRES" 2>> $SYSLOG | cut -d : -f 6) )
            duration=${incr[0]}  # assumes this is in hours
            if [[ ${incr[0]} == "anl" ]]; then
                duration="0"
            fi
            snapDateTime=$(date -u --date="${sdt:0:4}-${sdt:4:2}-${sdt:6:2} ${sdt:8:10}:00:00 $duration hours" '+%Y%m%d%H' )
            headerLine="$(printf "iLat=%4diLong=%4dDX=%6.3fDY=%6.3fSWLat=%8.3fSWLon=%8.3fDT=%8d00" ${rrfsLatLonGrid['nlat']} ${rrfsLatLonGrid['nlon']} ${rrfsLatLonGrid['dlon']} ${rrfsLatLonGrid['dlat']} $SWLat $SWLon $snapDateTime)"
            winPreTimes+=( $snapDateTime )
            echo "$headerLine" >> $preFileName
            echo "$headerLine" >> $winFileName
            # convert barometric pressure from Pa to millibar in the process
            wgrib2 $file -match 'PRES' -inv /dev/null -text - 2>> $SYSLOG \
                | sed 's/9.999e+20/101300.0/g' \
                | awk 'NR!=1 { printf("%10s",(sprintf("%4.4f",$1/100.0))); if ((NR-1)%8 == 0) print ""; }' \
                >> $preFileName 2>> $SYSLOG
            for var in "UGRD" "VGRD"; do
                wgrib2 $file -match "$var" -inv /dev/null -text - 2>> $SYSLOG \
                    | sed 's/9.999e+20/0.0/g' \
                    | awk 'NR!=1 { printf("%10f",$1); if ((NR-1)%8 == 0) print ""; }' \
                    >> $winFileName 2>> $SYSLOG
            done
        done
        if [[ $breakPoint == "rrfs.forecast.index.subset.regrid.owiwinpre" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #   W R I T E   M E T A D A T A   T O   J S O N
        #
        msg="$THIS: Writing RRFS forecast ASCII OWI WIN/PRE metadata."
        logMessage "$msg"
        consoleMessage "$I $msg"
        winPreRecordLength=$(( ${rrfsLatLonGrid['nlon']} * ${rrfsLatLonGrid['nlat']} ))
        downloadedFilesArray=$(printf "\"%s\"," ${downloaded[@]})
        haveFilesArray=$(printf "\"%s\"," ${have[@]})
        winPreTimesArray=$(printf "\"%s\"," ${winPreTimes[@]})
        sed \
            -e "s?NOWCAST?FORECAST?" \
            -e "s?%WINPREHEADER%?$winPreHeader?" \
            -e "s?%WINPREVELOCITYFILE%?$winFileName?" \
            -e "s?%WINPREPRESSUREFILE%?$preFileName?" \
            -e "s?%WINPRERECORDLENGTH%?$winPreRecordLength?" \
            -e "s?%WINPREWTIMINCSECONDS%?$WTIMINC?" \
            -e "s?%WINPRENUMRECORDS%?${#winPreTimes[*]}?" \
            -e "s?\"%WINPREDATATIMES%\"?$(echo ${winPreTimesArray%?})?" \
            -e "s?\"%NULLDOWNLOADED%\"?$(echo ${downloadedFilesArray%?})?" \
            -e "s?\"%NULLFOUND%\"?$(echo ${haveFilesArray%?})?" \
            < $instanceRrfsDir/select_rrfs_nowcast.json \
            > $instanceRrfsDir/downloadRRFS.json \
            2>> $SYSLOG
        if [[ $? != 0 ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Failed to fill in RRFS data request template with sed."
        fi
        if [[ $breakPoint == "rrfs.forecast.index.subset.regrid.owiwinpre.metadata" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
        #
        #   M O V E   F I L E S   T O   S C E N A R I O   D I R E C T O R Y
        #
        msg="$THIS: Moving RRFS ASCII OWI WIN/PRE and metadata to forecast scenario directory '$SCENARIODIR'."
        logMessage "$msg"
        consoleMessage "$I $msg"
        mv $winFileName $preFileName fort.22 $SCENARIODIR
        mv $instanceRrfsDir/downloadRRFS.json $SCENARIODIR/"${winFileName%.*}.json"
        cat $instanceRrfsDir/forecast.properties >> ${SCENARIODIR}/run.properties
        mv $instanceRrfsDir/forecast.properties $SCENARIODIR
        cd $SCENARIODIR 2>> $SYSLOG
        # create links to the OWI WIN/PRE files with names that  ADCIRC expects
        ln -s $(basename $preFileName) fort.221 2>> $SYSLOG
        ln -s $(basename $winFileName) fort.222 2>> $SYSLOG
        cd $RUNDIR
        if [[ $breakPoint == "rrfs.forecast.index.subset.regrid.owiwinpre.metadata.scenariodir" ]]; then
            echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] $THIS: Stopping at break point '$breakPoint'."
            exit
        fi
    fi
}