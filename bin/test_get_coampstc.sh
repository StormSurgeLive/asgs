#!/bin/bash
#
# set variables to be used in json template
SCRIPTDIR=~/Campaigns/Development/2022/COAMPS-TC/asgs
RUNDIR=$PWD
metGetURL=https://api.metget.zachcobell.com
instanceCoampstcDir=$PWD/coampstc
FORECASTCYCLE="00,18"
DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
THIS=test_get_coampstc.sh
#lastCycle=$(date -d yesterday +%Y%m%d00)
# ts bonnie
lastCycle=2022062900
STORM=02
#
coampstcTemplateName="get_coampstc_template.json"
filledCoampstcTemplateName="asgs_main.sh_get_coamps_status.json"
stage="NOWCAST"
SCENARIO=nowcast
SCENARIODIR=$RUNDIR/$SCENARIO
if [[ ! -d $SCENARIODIR ]]; then
    mkdir -p $SCENARIODIR
fi
dryRun="true"
# COAMPS-TC subset
declare -A coampstcDomain
coampstcDomain['leftlon']='-110'
coampstcDomain['rightlon']='-45'
coampstcDomain['toplat']='50'
coampstcDomain['bottomlat']='0'
coampstcDomain['dlon']='0.1'
coampstcDomain['dlat']='0.1'
coampstcDomain['dt']='3600'       # time increment in seconds
# make a directory to hold the raw data
instanceCoampstcDir=$RUNDIR/coampstc
if [[ ! -d $instanceCoampstcDir ]]; then
    mkdir -p $instanceCoampstcDir
fi
#
#    n o w c a s t
#
# getting coampstc status
echo "$THIS: Determining status of COAMPS-TC files available for $stage."
curl -s -H "Content-Type: application/json" \
        -H "x-api-key: $METGET_API_KEY" \
        $metGetURL/status | json_pp \
        > metget_status.json
#
# refine the list of COAMPS-TC cycles to end the nowcast on the correct cycle
select_coampstc_nowcast.pl \
    --source coampstc \
    --startcycle $lastCycle \
    --forecastcycle $FORECASTCYCLE \
    --backgroundmet $BACKGROUNDMET \
    --tropicalcyclone $TROPICALCYCLONE \
    --storm $STORM \
    < metget_status.json \
    > select_coampstc_nowcast.pl.json
#
# grab the list of cycles
coampstcCycleList=( $(bashJSON.pl --key cyclelist < select_coampstc_nowcast.pl.json) )
#
# fill in template with data needed to build COAMPS-TC WIN/PRE ASCII file with MetGet
#
start=${coampstcCycleList[0]}
start_date="${start:0:4}-${start:4:2}-${start:6:2} ${start:10:2}:00:00"
end=${coampstcCycleList[-1]}
end_date="${end:0:4}-${end:4:2}-${end:6:2} ${end:10:2}:00:00"
coampstcStormName=$(printf "coamps-tc-%0.2dL" $STORM)
coampstcStorm=$(printf "%0.2dL" $STORM)
fileName=COAMPS-TC_${stage^^}_${start_date}_${end_date}
#
sed \
    -e "s/%NULLSTARTDATE%/$start_date/" \
    -e "s/%NULLENDDATE%/$end_date/" \
	-e "s/%NULLTIMEINCREMENT%/${coampstcDomain[dt]}/" \
	-e "s/%NULLSTORMNAME%/$coampstcStormName/" \
	-e "s/%NULLSTORM%/$coampstcStorm/" \
	-e "s/%NULLDRYRUN%/$dryRun/" \
	-e "s/%NULLFILENAME%/$fileName/" \
     < $SCRIPTDIR/$coampstcTemplateName \
     > "part_$filledCoampstcTemplateName"
if [[ $? != 0 ]]; then
    echo "$THIS: Failed to fill in GFS data request template with sed."
fi
# add directories and arrays that would confuse sed
bashJSON.pl \
    --mapscalar nowcast="true" \
    --mapscalar x_init=${coampstcDomain[leftlon]} \
    --mapscalar y_init=${coampstcDomain[bottomlat]} \
    --mapscalar x_end=${coampstcDomain[rightlon]} \
    --mapscalar y_end=${coampstcDomain[toplat]} \
    --mapscalar di=${coampstcDomain[dlon]} \
    --mapscalar dj=${coampstcDomain[dlat]} \
    < "part_$filledCoampstcTemplateName" \
    > $filledCoampstcTemplateName
#
# write to a temp/pseudo fort.22 file
echo "# ${coampstcDomain[dt]} <-set WTIMINC to this value in ADCIRC fort.15" > fort.22
#
# write the headers to the win/pre files
preFileName=$fileName.221
winFileName=$fileName.222
# jgfdebug
exit
#
# request that the files be built
curl -s -X POST
        -H "Content-Type: application/json" \
        -H "x-api-key: $METGET_API_KEY" \
        --form "fileupload=@$filledCoampstcTemplateName" \
        $metGetURL/build
#
#


#
# put files in scenario directory
mv $winFileName $preFileName fort.22 $SCENARIODIR
cp ${THIS}.json "${winFileName%.*}.json"
cp get_gfs_status.pl.* ${THIS}.json "${winFileName%.*}.json" $SCENARIODIR
#
#   f o r e c a s t
#
stage=FORECAST
SCENARIO=gfsforecast
echo "$THIS: INFO: Downloading files for scenario '$SCENARIO'."
SCENARIODIR=$RUNDIR/gfsforecast
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
echo "$THIS: Subsetting and downloading GFS grib2 files."
for h in $(seq 0 120) ; do
    hhh=$(printf "%03d" $h)
    file="gfs.t${cyc}z.pgrb2.0p25.f$hhh"
    remotePath="%2Fgfs.${yyyymmdd}%2F${cyc}%2Fatmos"
    finalURL=$baseURL"?file="$file$levels$vars$domain"&dir="$remotePath
    # subset and download with curl
    if [[ ! -e $instanceGfsDir/$yyyymmdd ]]; then
        mkdir -p $instanceGfsDir/$yyyymmdd
    fi
    # don't download again if the files are already in place
    # (may want to turn off this conditional for end-to-end testing)
    localFile=$instanceGfsDir/$yyyymmdd/$file
    if [[ ! -f $localFile ]]; then
        success=0
        numRetries=1
        maxRetries=10
        while [[ $success -eq 0 && $numRetries -lt $maxRetries ]]; do
            echo "$THIS: INFO: Downloading '$file'."
            curl -s "$finalURL" > $localFile
            if [[ $? -eq 0 && -s $localFile ]]; then
                success=1
                downloaded+=( $localFile )
                break
            else
                echo "$THIS: WARNING: Failed to download '$finalURL'."
                numRetries=$(( $numRetries + 1 ))
                sleep 60
            fi
        done
    else
        echo "$THIS: INFO: Already have '$file'."
        have+=( $localFile )
    fi
done
#
# now regrid to lat lon coordinates with wgrib2
echo "$THIS: Regridding GFS grib2 files to latlon."
unset gfsFileList
declare -a gfsFileList
for h in $(seq 0 120) ; do
    hhh=$(printf "%03d" $h)
    origFile="$instanceGfsDir/$yyyymmdd/gfs.t${cyc}z.pgrb2.0p25.f${hhh}"
    latLonFile=${origFile}.latlon
    if [[ ! -s $latLonFile ]]; then
        echo "$THIS: Already have GFS grib2 file '$latLonFile' regridded to latlon."
        wgrib2 $origFile          \
            -inv /dev/null        \
            -set_grib_type same   \
            -new_grid_winds earth \
            -new_grid latlon $lonSpec $latSpec \
            $latLonFile
    fi
    # add to the list of files that will have their contents
    # extracted for conversion to OceanWeather WIN/PRE ASCII format
    gfsFileList+=( $latLonFile )
done
#
# grab the start time (YYYYMMDDHH) of the files from the
# inventory in the first file
startDateTime=$(wgrib2 ${gfsFileList[0]} -match "PRMSL" | cut -d : -f 3 | cut -d = -f 2)
#
# determine the wtiminc (time increment between files in seconds,
# needed for the ADCIRC fort.15 file)
incr=( $(wgrib2 ${gfsFileList[1]} -match "PRMSL" | cut -d : -f 6) )
wtiminc=${incr[0]}
if [[ ${incr[1]} == "hour" ]]; then
    wtiminc=$(( $wtiminc * 3600 ))
else
    echo "$THIS: ERROR: The time increment was specified as '${incr[1]}' which is not recognized."
fi
# write to a temp/pseudo fort.22 file
echo "# $wtiminc <-set WTIMINC to this value in ADCIRC fort.15" > fort.22
#
# find the end time for use in the main file header
incr=( $(wgrib2 ${gfsFileList[-1]} -match "PRMSL" | cut -d : -f 6) )
duration=${incr[0]}
if [[ ${incr[1]} == "hour" ]]; then
    endDateTime=$(date -u --date="${startDateTime:0:4}-${startDateTime:4:2}-${startDateTime:6:2} ${startDateTime:8:10}:00:00 $duration hours" '+%Y%m%d%H' )
else
    echo "$THIS: ERROR: The time increment was specified as '${incr[1]}' which is not recognized."
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
echo "$THIS: Writing ASCII WIN/PRE files."
unset winPreTimes
for file in ${gfsFileList[@]}; do
    incr=( $(wgrib2 $file -match "PRMSL" | cut -d : -f 6) )
    duration=${incr[0]}  # assumes this is in hours
    if [[ ${incr[0]} == "anl" ]]; then
        duration="0"
    fi
    snapDateTime=$(date -u --date="${startDateTime:0:4}-${startDateTime:4:2}-${startDateTime:6:2} ${startDateTime:8:10}:00:00 $duration hours" '+%Y%m%d%H' )
    headerLine=$(printf "iLat=%4siLong=%4sDX=%6sDY=%6sSWLat=%8sSWLon=%8sDT=%8s00" ${gfsLatLonGrid['nlat']} ${gfsLatLonGrid['nlon']} ${gfsLatLonGrid['dlon']} ${gfsLatLonGrid['dlat']} $SWLat $SWLon $snapDateTime)
    winPreTimes+=( $snapDateTime )
    echo $headerLine >> $preFileName
    echo $headerLine >> $winFileName
    # convert barometric pressure from Pa to millibar in the process
    wgrib2 $file -match 'PRMSL' -inv /dev/null -text - \
        | awk 'NR!=1 { printf("%10s",(sprintf("%4.4f",$1/100.0))); if ((NR-1)%8 == 0) print ""; }' \
         >> $preFileName
    for var in "UGRD:10" "VGRD:10"; do
        wgrib2 $file -match "$var" -inv /dev/null -text - \
            | awk 'NR!=1 { printf("%10f",$1); if ((NR-1)%8 == 0) print ""; }' \
             >> $winFileName
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
#
# put files in scenario directory
mv $winFileName $preFileName fort.22 $SCENARIODIR
cp ${THIS}.json "${winFileName%.*}.json"
cp get_gfs_status.pl.* ${THIS}.json "${winFileName%.*}.json" $SCENARIODIR