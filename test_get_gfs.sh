#!/bin/bash
#
# set variables to be used in json template
SCRIPTDIR=~/Campaigns/Development/asgs
RUNDIR=$PWD
BACKSITE=ftp.ncep.noaa.gov
BACKDIR=/pub/data/nccf/com/gfs/v16.2
instanceGfsDir=$PWD/gfs
FORECASTCYCLE="00,18"
DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
THIS=test_get_gfs.sh
lastCycle=$(date -d yesterday +%Y%m%d00)
gfsTemplateName="get_gfs_template.json"
filledGfsTemplateName="asgs_main.sh_get_gfs_status.json"
stage="NOWCAST"
SCENARIO=nowcast
SCENARIODIR=$RUNDIR/$SCENARIO
if [[ ! -d $SCENARIODIR ]]; then
    mkdir -p $SCENARIODIR
fi
escSCENARIODIR=${SCENARIODIR////'\/'}
# GFS subset
declare -A gfsDomain
gfsDomain['leftlon']='-110'
gfsDomain['rightlon']='-45'
gfsDomain['toplat']='50'
gfsDomain['bottomlat']='0'
#
# lat lon grid for conversion to WIN/PRE ASCII format with wgrib2
# e.g.
# wgrib2 IN.grib2 -new_grid latlon lon0:nlon:dlon lat0:nlat:dlat OUT.grib2
# lat0, lon0 = degrees of lat/lon for 1st grid point
# nlon = number of longitudes
# nlat = number of latitudes
# dlon = grid cell size in degrees of longitude
# dlat = grid cell size in degrees of latitude
declare -A gfsLatLonGrid
gfsLatLonGrid['lon0']="260"
gfsLatLonGrid['nlon']="240"
gfsLatLonGrid['dlon']="0.25"
gfsLatLonGrid['lat0']="5"
gfsLatLonGrid['nlat']="240"
gfsLatLonGrid['dlat']="0.25"
# keep sed from getting confused by escaping slashes
escBACKDIR=${BACKDIR////'\/'}
escRUNDIR=${RUNDIR////'\/'}
escSCRIPTDIR=${SCRIPTDIR////'\/'}
escInstanceGfsDir=${instanceGfsDir////'\/'}
arrFORECASTCYCLE=${FORECASTCYCLE//,/\",\"}
# make a directory to hold the raw data
instanceGfsDir=$RUNDIR/gfs
if [[ ! -d $instanceGfsDir ]]; then
    mkdir -p $instanceGfsDir
fi
#
# fill in template with data needed to get GFS status
#
# asgs_main.sh->downloadBackgroundMet :
sed \
    -e "s/%NULLGETGFSTEMPLATEFILE%/$gfsTemplateName/" \
    -e "s/%NULLGETGFSTEMPLATEFILLEDFILE%/$filledGfsTemplateName/" \
    -e "s/%NULLBACKSITE%/$BACKSITE/" \
    -e "s/%NULLBACKDIR%/$escBACKDIR/" \
    -e "s/%NULLCYCLE%/$lastCycle/" \
    -e "s/%NULLFORECASTCYCLE%/$arrFORECASTCYCLE/" \
    -e "s/%NULLSTAGE%/$stage/"                   \
    -e "s/%NULLSCRIPTDIR%/$escSCRIPTDIR/"        \
    -e "s/%NULLGFSDATAPATH%/$escInstanceGfsDir/" \
    -e "s/\"%NULLGFSNOWCASTDOWNLOADED%\"/null/" \
    -e "s/\"%NULLGFSNOWCASTFOUND%\"/null/" \
    -e "s/\"%NULLGFSFORECASTDOWNLOADED%\"/null/" \
    -e "s/\"%NULLGFSFORECASTFOUND%\"/null/" \
    -e "s/\"%NULLGFSSTATUSFILE%\"/null/" \
    -e "s/\"%NULLGFSSELECTFILE%\"/null/" \
    -e "s/\"%NULLGETGFSFILE%\"/null/" \
    -e "s/%NULLGFSWINPREDATAPATH%/$escSCENARIODIR/" \
    -e "s/%NULLLASTUPDATER%/$THIS/" \
    -e "s/%NULLLASTUPDATETIME%/$DATETIME/" \
     < $SCRIPTDIR/$gfsTemplateName \
     > $filledGfsTemplateName
if [[ $? != 0 ]]; then
    echo "$THIS: Failed to fill in GFS data request template with sed."
fi
#
baseURL="https://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl"
levels="&lev_10_m_above_ground=on&lev_mean_sea_level=on"
vars="&var_UGRD=on&var_VGRD=on&var_PRMSL=on"
domain="&leftlon=${gfsDomain['leftlon']}"
domain+="&rightlon=${gfsDomain['rightlon']}"
domain+="&toplat=${gfsDomain['toplat']}"
domain+="&bottomlat=${gfsDomain['bottomlat']}"
#
#    n o w c a s t
#
# getting gfs status
echo "$THIS: Determining status of GFS files available for $stage."
$SCRIPTDIR/bin/get_gfs_status.pl < $filledGfsTemplateName > get_gfs_status.pl.json
#
# refine the list of GFS cycles to end the nowcast on the correct cycle
select_nam_nowcast.pl < get_gfs_status.pl.json > select_gfs_nowcast.pl.json
#
# grab the list of cycles
gfsCycleList=( $(bashJSON.pl --key cyclelist < select_gfs_nowcast.pl.json) )
#
# curl "https://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?file=gfs.t00z.pgrb2.0p25.f000&lev_10_m_above_ground=on&lev_mean_sea_level=on&var_PRMSL=on&var_UGRD=on&var_VGRD=on&leftlon=-100&rightlon=-45&toplat=48&bottomlat=5&dir=%2Fgfs.20220601%2F00%2Fatmos" > IN.grb
#
# subset and download with curl
unset downloaded have
echo "$THIS: Subsetting and downloading GFS grib2 files."
for cycle in ${gfsCycleList[@]} ; do
    cyc=${cycle:8:2}
    hhh="000" # FIXME: this only applies to a nowcast
    file="gfs.t${cyc}z.pgrb2.0p25.f$hhh"
    yyyymmdd=${cycle:0:8}
    remotePath="%2Fgfs.${yyyymmdd}%2F${cyc}%2Fatmos"
    finalURL=$baseURL"?file="$file$levels$vars$domain"&dir="$remotePath
    #echo $finalURL
    if [[ ! -e $instanceGfsDir/$yyyymmdd ]]; then
        mkdir -p $instanceGfsDir/$yyyymmdd
    fi
    # don't download again if the files are already in place
    # (may want to turn off this conditional for end-to-end testing)
    if [[ ! -f $instanceGfsDir/$yyyymmdd/$file ]]; then
        curl -s "$finalURL" > $instanceGfsDir/$yyyymmdd/$file
        downloaded+=( $instanceGfsDir/$yyyymmdd/$file )
    else
        have+=( $instanceGfsDir/$yyyymmdd/$file )
    fi
done
#
# now regrid to lat lon coordinates with wgrib2
# e.g.
#./wgrib2 IN.grb -new_grid_winds earth -new_grid latlon 260:200:1 7:160:1 OUT.grb
echo "$THIS: Regridding GFS grib2 files to latlon."
unset gfsFileList
declare -a gfsFileList
for cycle in ${gfsCycleList[@]} ; do
    cyc=${cycle:8:2}
    yyyymmdd=${cycle:0:8}
    hhh="000" # FIXME: this only applies to a nowcast
    origFile="$instanceGfsDir/$yyyymmdd/gfs.t${cyc}z.pgrb2.0p25.f${hhh}"
    latLonFile=${origFile}.latlon
    lonSpec="${gfsLatLonGrid['lon0']}:${gfsLatLonGrid['nlon']}:${gfsLatLonGrid['dlon']}"
    latSpec="${gfsLatLonGrid['lat0']}:${gfsLatLonGrid['nlat']}:${gfsLatLonGrid['dlat']}"
    wgrib2 $origFile          \
        -inv /dev/null        \
        -set_grib_type same   \
        -new_grid_winds earth \
        -new_grid latlon $lonSpec $latSpec \
        $latLonFile
    # add to the list of files that will have their contents
    # extracted for conversion to OceanWeather WIN/PRE ASCII format
    gfsFileList+=( $latLonFile )
done
#
# form win/pre file header line
SWLat=$(printf "%3.4f" ${gfsLatLonGrid['lon0']})
# wgrib2 regridding longitudes have a range of 0 to 360
# but win/pre format expects -180 to 180
SWLon=$(printf "%3.4f" $(echo ${gfsLatLonGrid['lon0']} - 360.0 | bc -q))
#
# grab the start time (YYYYMMDDHH) of the files from the
# inventory in the first file
date=$(wgrib2 ${gfsFileList[0]} -match "PRMSL" | cut -d : -f 3 | cut -d = -f 2)
sdate=$(date --date="${date:0:4}-${date:4:2}-${date:6:2} ${date:8:10}:00:00" '+%s')
startDateTime=$date
#
# determine the wtiminc (time increment between files in seconds,
# needed for the ADCIRC fort.15 file)
date=$(wgrib2 ${gfsFileList[1]} -match "PRMSL" | cut -d : -f 3 | cut -d = -f 2)
ndate=$(date --date="${date:0:4}-${date:4:2}-${date:6:2} ${date:8:10}:00:00" '+%s')
wtiminc=$(( (ndate - sdate) ))
# write to a temp/pseudo fort.22 file
echo "# $wtiminc <-set WTIMINC to this value in ADCIRC fort.15" > fort.22
#
# find the end time for use in the main file header
endDateTime=$(wgrib2 ${gfsFileList[-1]} -match "PRMSL" | cut -d : -f 3 | cut -d = -f 2)
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
    date=$(wgrib2 $file -match 'PRMSL' | cut -d : -f 3 | cut -d = -f 2)
    headerLine=$(printf "iLat=%4siLong=%4sDX=%6sDY=%6sSWLat=%8sSWLon=%8sDT=%8s00" ${gfsLatLonGrid['nlat']} ${gfsLatLonGrid['nlon']} ${gfsLatLonGrid['dlon']} ${gfsLatLonGrid['dlat']} $SWLat $SWLon $date)
    winPreTimes+=( $date"00" )
    echo $headerLine >> $preFileName
    echo $headerLine >> $winFileName
    # convert barometric pressure from Pa to millibar in the process
    wgrib2 $file -match 'PRMSL' -inv /dev/null -text - \
        | awk 'NR!=1 { printf("%10s",(sprintf("%4.4f",$1/100.0))); if ((NR-1)%8 == 0) print ""; }' \
         >> $preFileName
    for var in "UGRD:10" "VGRD:10" ; do
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