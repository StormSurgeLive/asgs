#!/bin/bash

#trap read debug
#----------------------------------------------------------------
# downloadGFS.sh: subroutine that polls an external
# ftp site to determine status of GFS data and then subsets
# and downloads via curl.
#----------------------------------------------------------------
# Copyright(C) 2022 Jason Fleming
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
downloadGFS()
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
    THIS="asgs_main.sh>downloadGFS.sh"
    CURRENT_STATE="WAIT"
    RMQMessage "INFO" "$CURRENT_EVENT" "$THIS>$ENSTORM" "$CURRENT_STATE"  "Downloading NAM meteorological data for $ENSTORM."
    logMessage "$ENSTORM: $THIS: Downloading GFS meteorological data."
    cd $RUNDIR 2>> ${SYSLOG}
    #
    # if there isn't an archive directory for GFS data inside
    # the instance directory, make one
    instanceGfsDir=$RUNDIR/gfs
    if [[ ! -d $instanceGfsDir ]]; then
       mkdir -p $instanceGfsDir 2>> $SYSLOG
    fi
    # if there isn't an archive directory for GFS data in the
    # ASGS WORK directory, make one
    platformGfsDir=$WORK/gfs
    if [[ ! -d $platformGfsDir ]]; then
       mkdir -p $platformGfsDir 2>> $SYSLOG
    fi
    #
    # set up the URL for subsetting and downloading
    baseURL="https://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl"
    levels="&lev_10_m_above_ground=on&lev_mean_sea_level=on"
    vars="&var_UGRD=on&var_VGRD=on&var_PRMSL=on"
    domain="&leftlon=${gfsDomain['leftlon']}"
    domain+="&rightlon=${gfsDomain['rightlon']}"
    domain+="&toplat=${gfsDomain['toplat']}"
    domain+="&bottomlat=${gfsDomain['bottomlat']}"
    # create the json file to act as input to the
    # status checker and GFS downloader
    gfsTemplateName="get_gfs_template.json"
    filledGfsTemplateName="asgs_main.sh_get_gfs_status.json"
    #
    # N O W C A S T
    if [[ $stage == "NOWCAST" ]]; then
        # determine the cycle time corresponding to the current state of the simulation
        csEpochSeconds=$(TZ=UTC date -u -d "${CSDATE:0:4}-${CSDATE:4:2}-${CSDATE:6:2} ${CSDATE:8:2}:00:00" "+%s" 2>>$SYSLOG)
        hsEpochSeconds=$((csEpochSeconds + ${HSTIME%.*}))
        lastCycle=$(TZ=UTC date -u -d "1970-01-01 UTC $hsEpochSeconds seconds" +"%Y%m%d%H" 2>>$SYSLOG)
        DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
        sed \
            -e "s/%NULLGETGFSTEMPLATEFILE%/$gfsTemplateName/" \
            -e "s/%NULLGETGFSTEMPLATEFILLEDFILE%/$filledGfsTemplateName/" \
            -e "s/%NULLBACKSITE%/$BACKSITE/" \
            -e "s/%NULLCYCLE%/$lastCycle/" \
            -e "s/%NULLSTAGE%/$stage/"                   \
            -e "s/\"%NULLGFSNOWCASTDOWNLOADED%\"/null/" \
            -e "s/\"%NULLGFSNOWCASTFOUND%\"/null/" \
            -e "s/\"%NULLGFSFORECASTDOWNLOADED%\"/null/" \
            -e "s/\"%NULLGFSFORECASTFOUND%\"/null/" \
            -e "s/\"%NULLGFSSTATUSFILE%\"/null/" \
            -e "s/\"%NULLGFSSELECTFILE%\"/null/" \
            -e "s/\"%NULLGETGFSFILE%\"/null/" \
            -e "s/%NULLLASTUPDATER%/$THIS/" \
            -e "s/%NULLLASTUPDATETIME%/$DATETIME/" \
            < $SCRIPTDIR/$gfsTemplateName \
            > "part_$filledGfsTemplateName"
           2>> $SYSLOG
        if [[ $? != 0 ]]; then
            fatal "$THIS: Failed to fill in GFS data request template with sed."
        fi
        # add directories and arrays that would confuse sed
        bashJSON.pl \
            --mapscalar siteDir="$BACKDIR" \
            --mapscalar scriptDir="$SCRIPTDIR" \
            --mapscalar localDataDir="$instanceGfsDir" \
            --maparray configDailyForecastCycles="$( ( echo ${FORECASTCYCLE//,/' '} ) )" \
            --maparray cyclelist="$( ( echo $lastCycle ) )" \
            < "part_$filledGfsTemplateName" \
            > $filledGfsTemplateName
           2>> $SYSLOG
        # determine the status of the latest GFS cycle posted by NCEP,
        # along with the range of cycles posted since the adcirc hotstart time
        latestCycle=0
        TRIES=0
        while [[ $latestCycle -le $lastCycle ]]; do
            if [[ $TRIES -ne 0 ]]; then
                sleep 60
            fi
            ((TRIES++))
            # getting gfs status
            get_gfs_status.pl < $filledGfsTemplateName > get_gfs_status.pl.json 2>> $SYSLOG
            if [[ $? != 0 ]]; then
                warn "$THIS: Failed to get status of GFS cycles with get_gfs_status.pl."
                RMQMessage "INFO" "$CURRENT_EVENT" "$THIS>$ENSTORM" "$CURRENT_STATE" \
                    "Waiting on NCEP data for $ENSTORM. Sleeping 60 secs (TRY=$TRIES) ..."
                sleep 60
                continue
            fi
            bashJSON.pl --key cyclelist --last < get_gfs_status.pl.json > latestCycle 2>> $SYSLOG
            if [[ $? != 0 ]]; then
                warn "$THIS: Failed to extract the latest GFS cycle from get_gfs_status.pl.json with bashJSON.pl"
                RMQMessage "INFO" "$CURRENT_EVENT" "$THIS>$ENSTORM" "$CURRENT_STATE" \
                    "Waiting on NCEP data for $ENSTORM. Sleeping 60 secs (TRY=$TRIES) ..."
                sleep 60
                continue
            fi
            latestCycle=$(<"latestCycle")
        done
        # refine the list of NAM cycles to end the nowcast on the correct cycle
        select_nam_nowcast.pl < get_gfs_status.pl.json > select_gfs_nowcast.pl.json 2>> $SYSLOG
        if [[ $? != 0 ]]; then
            warn "$THIS: Failed to select the proper GFS cycle to end the nowcast with select_nam_nowcast.pl."
        fi
        # then download the actual nowcast data for the time range of interest
        gfsCycleList=( $(bashJSON.pl --key cyclelist < select_gfs_nowcast.pl.json 2>> $SYSLOG) )
        thisCycle=${gfsCycleList[-1]}
        #
        # subset and download with curl
        unset downloaded have
        logMessage "$THIS: Subsetting and downloading GFS grib2 files."
        for cycle in ${gfsCycleList[@]} ; do
            cyc=${cycle:8:2}
            hhh="000" # FIXME: this only applies to a nowcast
            file="gfs.t${cyc}z.pgrb2.0p25.f$hhh"
            yyyymmdd=${cycle:0:8}
            remotePath="%2Fgfs.${yyyymmdd}%2F${cyc}%2Fatmos"
            finalURL=$baseURL"?file="$file$levels$vars$domain"&dir="$remotePath
            if [[ ! -e $instanceGfsDir/$yyyymmdd ]]; then
                mkdir -p $instanceGfsDir/$yyyymmdd
            fi
            # don't download again if the files are already in place
            # (may want to turn off this conditional for end-to-end testing)
            subsetSuccess=0
            localFile=$instanceGfsDir/$yyyymmdd/$file
            cmd="curl -s "$finalURL" > $localFile 2>> $SYSLOG" # used in error messages
            while [[ $subsetSuccess -eq 0 ]]; do
                how="download"
                if [[ ! -s $localFile ]]; then
                    curl -s "$finalURL" > $localFile 2>> $SYSLOG
                    if [[ $? != 0 ]]; then
                        warn "Subsetting GFS with curl failed when using the following command: '$cmd'."
                        if [[ -e $localFile ]]; then
                            mv $localFile ${localFile}.curlerr
                        fi
                    fi
                else
                    how="have"
                fi
                # perform quality checks
                if [[ -s $localFile ]]; then
                    localFileType="$(file $localFile 2>> $SYSLOG)"
                    localFileSize=$(stat -c "%s" $localFile 2>> $SYSLOG)
                    localFileLines=$(wgrib2 $localFile -match 'PRMSL' -inv /dev/null -text - 2>> $SYSLOG | wc -l)
                    if [[ ! $localFileType =~ "(GRIB) version 2" && ! $localFileType =~ "data" ]]; then
                        warn "Subsetting GFS with curl failed to produce a local grib2 file when using the following command: '$cmd'."
                        if [[ "$localFileType" =~ "HTML document" ]]; then
                            warn "The file '$localFile' is html instead of grib2."
                            mv $localFile ${localFile}.html 2>> $SYSLOG
                        else
                            warn "The file type of '$localFile' is unrecognized: '$localFileType'."
                            mv $localFile ${localFile}.unrecognized 2>> $SYSLOG
                        fi
                    # make sure the downloaded file is at least 1MB (these files
                    # seem to be about 2-3MB)
                    elif [[ $localFileSize -lt 1000000 ]]; then
                        warn "The file '$localFile' seems to be incomplete because it is only '$localFileSize' bytes."
                        mv $localFile ${localFile}.toosmall 2>> $SYSLOG
                    # smoke test
                    elif [[ $localFileLines -eq 0 ]]; then
                        warn "The file '$localFile' does not seem to have barometric pressure data."
                        mv $localFile ${localFile}.nodata 2>> $SYSLOG
                    fi
                else
                    warn "Subsetting GFS with curl failed to produce a local grib2 file (or a file with zero length) when using the following command: '$cmd'."
                fi
                # if the file is still there, it passed its quality checks
                if [[ -s $localFile ]]; then
                    subsetSuccess=1
                    if [[ $how == "download" ]]; then
                        downloaded+=( $localFile )
                    else
                        have+=( $localFile )
                    fi
                else
                    warn "GFS subsetting with curl failed. Sleeping 60 seconds before retry."
                    sleep 60
                fi
            done
        done
        #
        # now regrid to lat lon coordinates with wgrib2
        logMessage "$THIS: Regridding GFS grib2 files to latlon."
        unset gfsFileList
        declare -a gfsFileList
        for cycle in ${gfsCycleList[@]} ; do
            cyc=${cycle:8:2}
            yyyymmdd=${cycle:0:8}
            hhh="000" # this only applies to a nowcast
            origFile="$instanceGfsDir/$yyyymmdd/gfs.t${cyc}z.pgrb2.0p25.f${hhh}"
            latLonFile=${origFile}.latlon
            lonSpec="${gfsLatLonGrid['lon0']}:${gfsLatLonGrid['nlon']}:${gfsLatLonGrid['dlon']}"
            latSpec="${gfsLatLonGrid['lat0']}:${gfsLatLonGrid['nlat']}:${gfsLatLonGrid['dlat']}"
            if [[ ! -s $latLonFile ]]; then
                wgrib2 $origFile          \
                    -inv /dev/null        \
                    -set_grib_type same   \
                    -new_grid_winds earth \
                    -new_grid latlon $lonSpec $latSpec \
                    $latLonFile 2>> $SYSLOG
            fi
            # add to the list of files that will have their contents
            # extracted for conversion to OceanWeather WIN/PRE ASCII format
            gfsFileList+=( $latLonFile )
        done
        #
        # form win/pre file header line
        SWLat=$(printf "%3.4f" ${gfsLatLonGrid['lat0']})
        # wgrib2 regridding longitudes have a range of 0 to 360
        # but win/pre format expects -180 to 180
        SWLon=$(printf "%3.4f" $(echo ${gfsLatLonGrid['lon0']} - 360.0 | bc -q))
        #
        # grab the start time (YYYYMMDDHH) of the files from the
        # inventory in the first file
        date=$(wgrib2 ${gfsFileList[0]} -match "PRMSL" 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
        sdate=$(date --date="${date:0:4}-${date:4:2}-${date:6:2} ${date:8:10}:00:00" '+%s' 2>>$SYSLOG)
        startDateTime=$date
        #
        # determine the wtiminc (time increment between files in seconds,
        # needed for the ADCIRC fort.15 file)
        date=$(wgrib2 ${gfsFileList[1]} -match "PRMSL" 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
        ndate=$(date --date="${date:0:4}-${date:4:2}-${date:6:2} ${date:8:10}:00:00" '+%s' 2>>$SYSLOG)
        wtiminc=$(( (ndate - sdate) ))
        # write to a temp/pseudo fort.22 file
        echo "# $wtiminc <-set WTIMINC to this value in ADCIRC fort.15" > fort.22
        #
        # find the end time for use in the main file header
        endDateTime=$(wgrib2 ${gfsFileList[-1]} -match "PRMSL" 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
        #
        # write the headers to the win/pre files
        preFileName=GFS_${stage^^}_${startDateTime}_${endDateTime}.221
        winFileName=GFS_${stage^^}_${startDateTime}_${endDateTime}.222
        printf "%s%38s%15s\n" "Oceanweather WIN/PRE Format" $startDateTime $endDateTime > $preFileName # fort.221
        printf "%s%38s%15s\n" "Oceanweather WIN/PRE Format" $startDateTime $endDateTime > $winFileName # fort.222
        #
        # extract the data from the grib2 files as ascii, reformat
        # into eight columns, and append the dataset to the corresponding file
        logMessage "$THIS: Writing ASCII WIN/PRE files."
        unset winPreTimes
        for file in ${gfsFileList[@]}; do
            date=$(wgrib2 $file -match 'PRMSL' 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
            headerLine="$(printf "iLat=%4diLong=%4dDX=%6.3fDY=%6.3fSWLat=%8.3fSWLon=%8.3fDT=%8d00" ${gfsLatLonGrid['nlat']} ${gfsLatLonGrid['nlon']} ${gfsLatLonGrid['dlon']} ${gfsLatLonGrid['dlat']} $SWLat $SWLon $date)"
            winPreTimes+=( $date"00" )
            echo "$headerLine" >> $preFileName
            echo "$headerLine" >> $winFileName
            # convert barometric pressure from Pa to millibar in the process
            wgrib2 $file -match 'PRMSL' -inv /dev/null -text - 2>>$SYSLOG \
                | awk 'NR!=1 { printf("%10s",(sprintf("%4.4f",$1/100.0))); if ((NR-1)%8 == 0) print ""; }' \
                >> $preFileName 2>> $SYSLOG
            for var in "UGRD:10" "VGRD:10" ; do
                wgrib2 $file -match "$var" -inv /dev/null -text - 2>> $SYSLOG \
                    | awk 'NR!=1 { printf("%10f",$1); if ((NR-1)%8 == 0) print ""; }' \
                    >> $winFileName 2>> $SYSLOG
            done
        done
        #
        # write metadata to JSON
        bashJSON.pl \
            --mapscalar get="$THIS" \
            --mapscalar winPreHeader="$(printf "%s%38s%15s" "Oceanweather WIN/PRE Format" $startDateTime $endDateTime)" \
            --mapscalar winPreVelocityFile="$winFileName" \
            --mapscalar winPrePressureFile="$preFileName" \
            --mapscalar winPreRecordLength=$(( ${gfsLatLonGrid['nlon']} * ${gfsLatLonGrid['nlat']} )) \
            --mapscalar gfsForecastValidStart="${startDateTime}0000" \
            --mapscalar winPreWtimincSeconds="$wtiminc" \
            --mapscalar winPreNumRecords="${#winPreTimes[*]}" \
            --maparray winPreDataTimes="$(echo ${winPreTimes[@]})" \
            --maparray filesDownloaded="$(echo ${downloaded[@]})" \
            --maparray filesFromCache="$(echo ${have[@]})" \
            < select_gfs_nowcast.pl.json \
            > ${THIS}.json
            2>> $SYSLOG
        ADVISDIR=$RUNDIR/$thisCycle
        CYCLEDIR=$ADVISDIR
        CYCLELOG=$CYCLEDIR/cycle.log
        NOWCASTDIR=$ADVISDIR/$ENSTORM
        SCENARIODIR=$CYCLEDIR/$SCENARIO
        SCENARIOLOG=$SCENARIODIR/scenario.log
        mkdir -p $SCENARIODIR 2>> $SYSLOG
        #
        # put files in scenario directory
        mv $winFileName $preFileName fort.22 run.properties $SCENARIODIR 2>> $SYSLOG
        cp ${THIS}.json "${winFileName%.*}.json" 2>> $SYSLOG
        cp get_gfs_status.pl.* ${THIS}.json "${winFileName%.*}.json" $SCENARIODIR 2>> $SYSLOG
        #
        # record the new advisory number to the statefile
        debugMessage "$THIS: $ENSTORM: The new NAM cycle is $thisCycle."
        cp -f $STATEFILE ${STATEFILE}.old 2>> ${SYSLOG} 2>&1
        sed 's/ADVISORY=.*/ADVISORY='$thisCycle'/' $STATEFILE > ${STATEFILE}.new
        debugMessage "Updating statefile $STATEFILE with new cycle number ${thisCycle}."
        cp -f ${STATEFILE}.new $STATEFILE 2>> ${SYSLOG} 2>&1

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
        logMessage "$THIS: Subsetting and downloading GFS grib2 files."
        maxRetries=10
        for h in $(seq 0 120) ; do
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
            while [[ $subsetSuccess -eq 0 && $numRetries -lt $maxRetries ]]; do
                how="download"
                if [[ ! -s $localFile ]]; then
                    curl -s "$finalURL" > $localFile 2>> $SYSLOG
                    if [[ $? != 0 ]]; then
                        warn "Subsetting GFS with curl failed when using the following command: '$cmd'."
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
                        warn "Subsetting GFS with curl failed to produce a local grib2 file '$localFile'."
                        if [[ "$localFileType" =~ "HTML document" ]]; then
                            logMessage "The file '$localFile' is html instead of grib2."
                            if [[ $(cat $localFile) =~ "data file is not present" ]]; then
                                logMessage "The html message is: 'data file is not present'."
                                mv $localFile not_present.html 2>> $SYSLOG
                            else
                                mv $localFile ${localFile}.html 2>> $SYSLOG
                            fi
                        else
                            warn "The file type of '$localFile' is unrecognized: '$localFileType'."
                            mv $localFile ${localFile}.unrecognized 2>> $SYSLOG
                        fi
                    # make sure the downloaded file is at least 1MB (these files
                    # seem to be about 2-3MB)
                    elif [[ $localFileSize -lt 1000000 ]]; then
                        warn "The file '$localFile' seems to be incomplete because it is only '$localFileSize' bytes."
                        mv $localFile ${localFile}.toosmall 2>> $SYSLOG
                    # smoke test
                    elif [[ $localFileLines -eq 0 ]]; then
                        warn "The file '$localFile' does not seem to have barometric pressure data."
                        mv $localFile ${localFile}.nodata 2>> $SYSLOG
                    fi
                else
                    warn "Subsetting GFS with curl failed to produce a local grib2 file (or a file with zero length) when using the following command: '$cmd'."
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
                        warn "GFS subsetting with curl failed. Sleeping 60 seconds before retry."
                        sleep 60
                    fi
                fi
            done
            if [[ $numRetries -ge $maxRetries ]]; then
                warn "Exceeded the max number of retries for downloading this file. Continuing with the forecast files that were downloaded successfully."
                break
            fi
        done
        #
        # now regrid to lat lon coordinates with wgrib2
        logMessage "$THIS: Regridding GFS forecast grib2 files to latlon."
        unset gfsFileList
        declare -a gfsFileList
        for origFile in ${gfsForecastFiles[@]} ; do
            latLonFile=${origFile}.latlon
            if [[ ! -s $latLonFile ]]; then
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
        startDateTime=$(wgrib2 ${gfsFileList[0]} -match "PRMSL" 2>> $SYSLOG | cut -d : -f 3 | cut -d = -f 2)
        #
        # determine the wtiminc (time increment between files in seconds,
        # needed for the ADCIRC fort.15 file)
        incr=( $(wgrib2 ${gfsFileList[1]} -match "PRMSL" 2>> $SYSLOG | cut -d : -f 6) )
        wtiminc=${incr[0]}
        if [[ ${incr[1]} == "hour" ]]; then
            wtiminc=$(( $wtiminc * 3600 ))
        else
            fatal "$THIS: ERROR: The time increment was specified as '${incr[1]}' which is not recognized."
        fi
        # write to a temp/pseudo fort.22 file
        echo "# $wtiminc <-set WTIMINC to this value in ADCIRC fort.15" > fort.22
        #
        # find the end time for use in the main file header
        incr=( $(wgrib2 ${gfsFileList[-1]} -match "PRMSL" 2>> $SYSLOG | cut -d : -f 6) )
        duration=${incr[0]}
        if [[ ${incr[1]} == "hour" ]]; then
            endDateTime=$(date -u --date="${startDateTime:0:4}-${startDateTime:4:2}-${startDateTime:6:2} ${startDateTime:8:10}:00:00 $duration hours" '+%Y%m%d%H' 2>>$SYSLOG)
        else
            fatal "$THIS: ERROR: The time increment was specified as '${incr[1]}' which is not recognized."
        fi
        #
        # write the headers to the win/pre files
        preFileName=GFS_${stage^^}_${startDateTime}_${endDateTime}.221
        winFileName=GFS_${stage^^}_${startDateTime}_${endDateTime}.222
        printf "%s%38s%15s\n" "Oceanweather WIN/PRE Format" $startDateTime $endDateTime > $preFileName # fort.221
        printf "%s%38s%15s\n" "Oceanweather WIN/PRE Format" $startDateTime $endDateTime > $winFileName # fort.222
        #
        # extract the data from the grib2 files as ascii, reformat
        # into eight columns, and append the dataset to the corresponding file
        logMessage "$THIS: Writing ASCII WIN/PRE files."
        unset winPreTimes
        for file in ${gfsFileList[@]}; do
            incr=( $(wgrib2 $file -match "PRMSL" 2>> $SYSLOG | cut -d : -f 6) )
            duration=${incr[0]}  # assumes this is in hours
            if [[ ${incr[0]} == "anl" ]]; then
                duration="0"
            fi
            snapDateTime=$(date -u --date="${startDateTime:0:4}-${startDateTime:4:2}-${startDateTime:6:2} ${startDateTime:8:10}:00:00 $duration hours" '+%Y%m%d%H' )
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
            --mapscalar winPreHeader="$(printf "%s%38s%15s" "Oceanweather WIN/PRE Format" $startDateTime $endDateTime)" \
            --mapscalar winPreVelocityFile="$winFileName" \
            --mapscalar winPrePressureFile="$preFileName" \
            --mapscalar winPreRecordLength=$(( ${gfsLatLonGrid['nlon']} * ${gfsLatLonGrid['nlat']} )) \
            --mapscalar gfsForecastValidStart="${startDateTime}0000" \
            --mapscalar winPreWtimincSeconds="$wtiminc" \
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
