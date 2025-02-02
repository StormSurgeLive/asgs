#!/bin/bash
#
# This script should be run within the asgs shell environment
# (needed so that the values of SCRIPTDIR etc are available)
#
# set variables to be used in json template
RUNDIR=$PWD
BACKSITE=ftp.ncep.noaa.gov
BACKDIR=/pub/data/nccf/com/nam/prod
instanceNamDir=$PWD/nam
FORECASTCYCLE="00,18"
DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
THIS=cmd_line_test
lastCycle=$(date -d yesterday +%Y%m%d00)
namTemplateName="get_nam_template.json"
filledNamTemplateName="asgs_main.sh_get_nam_status.json"
# keep sed from getting confused by escaping slashes
escBACKDIR=${BACKDIR////'\/'}
escRUNDIR=${RUNDIR////'\/'}
escSCRIPTDIR=${SCRIPTDIR////'\/'}
escInstanceNamDir=${instanceNamDir////'\/'}
arrFORECASTCYCLE=${FORECASTCYCLE//,/\",\"}
#  n o w c a s t
stage=NOWCAST
#
# fill in template with data needed to get NAM status
#
# asgs_main.sh->downloadBackgroundMet :
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
# getting nam status
get_nam_status.pl < $filledNamTemplateName > get_nam_status.pl.json
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to get status of NAM cycles with get_nam_status.pl."
    exit $err
fi
latest.pl < get_nam_status.pl.json > latestCycle
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to extract the latest NAM cycle from get_nam_status.pl.json with latest.pl"
    exit $err
fi
latestCycle=$(<"latestCycle")
# refine the list of NAM cycles to end the nowcast on the correct cycle
select_nam_nowcast.pl < get_nam_status.pl.json > select_nam_nowcast.pl.json
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to select the proper NAM cycle to end the nowcast with select_nam_nowcast.pl."
    exit $err
fi
# then download the actual nowcast data for the time range of interest
get_nam_data.pl < select_nam_nowcast.pl.json > get_nam_data.pl.json
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to download NAM nowcast data with get_nam_data.pl.json."
    exit $err
fi
thisCycle=$(latest.pl < select_nam_nowcast.pl.json)
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to get the latest cycle from select_nam_nowcast.pl.json."
    exit $err
fi
echo $thisCycle
# for extracting the grib2 and converting to ascii win/pre (owi) format
SCENARIO=nowcast
SCENARIODIR=$RUNDIR/nowcast
if [[ ! -d $SCENARIODIR ]]; then mkdir $SCENARIODIR ; fi
escSCENARIODIR=${SCENARIODIR////'\/'}
SPATIALEXTRAPOLATIONRAMPDISTANCE=1.0
VELOCITYMULTIPLIER=1.0
boolApplyRamp=true
ptFilePath=${SCRIPTDIR}/input/ptFile_oneEighth.txt
escPtFilePath=${ptFilePath////'\/'}
# NAMtoOWIRamp.pl writes the ascii OWI WIN/PRE files to
# the scenario directories
sed -e "s/%NULLNAMWINPREDATAPATH%/$escSCENARIODIR/" \
    -e "s/%NULLNAMWINPREGRID%/$escPtFilePath/" \
    -e "s/\"%NULLNAMAWIPGRID%\"/218/" \
    -e "s/%NULLNAMRAWFORMAT%/grib2/" \
    -e "s/\"%NULLVELMULT%\"/$VELOCITYMULTIPLIER/" \
    -e "s/\"%NULLPRESSMULT%\"/0.01/" \
    -e "s/\"%NULLAPPLYRAMP%\"/$boolApplyRamp/" \
    -e "s/\"%NULLRAMPDIST%\"/$SPATIALEXTRAPOLATIONRAMPDISTANCE/" \
     < get_nam_data.pl.json \
     | $SCRIPTDIR/NAMtoOWIRamp.pl > NAMtoOWIRamp.pl.json
preFile=$($SCRIPTDIR/bin/bashJSON.pl --key winPrePressureFile < NAMtoOWIRamp.pl.json)
winFile=$($SCRIPTDIR/bin/bashJSON.pl --key winPreVelocityFile < NAMtoOWIRamp.pl.json)
mv fort.22 $SCENARIODIR
mv NAMtoOWIRamp.pl.* get_nam_status.pl.* asgs_main.sh_get_nam_status.json rotatedNAM.txt select_nam_nowcast.pl.json reproject.log lambert_diag.out $SCENARIODIR
cp get_nam_data.pl.* $SCENARIODIR  # needed for forecast
#  f o r e c a s t
stage=FORECAST
SCENARIO=namforecast
SCENARIODIR=$RUNDIR/namforecast
if [[ ! -d $SCENARIODIR ]]; then mkdir $SCENARIODIR ; fi
escSCENARIODIR=${SCENARIODIR////'\/'}
sed "s/NOWCAST/FORECAST/" < get_nam_data.pl.json > get_nam_forecast.json
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to replace NOWCAST with FORECAST in select_nam_nowcast.pl.json with sed."
    exit $err
fi
# download the forecast data
get_nam_data.pl < get_nam_forecast.json > get_nam_data.pl.json
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to download NAM forecast data with get_nam_data.pl."
    exit $err
fi
# NAMtoOWIRamp.pl writes the ascii OWI WIN/PRE files to
# the scenario directories
sed -e "s/%NULLNAMWINPREDATAPATH%/$escSCENARIODIR/" \
    -e "s/%NULLNAMWINPREGRID%/$escPtFilePath/" \
    -e "s/\"%NULLNAMAWIPGRID%\"/218/" \
    -e "s/%NULLNAMRAWFORMAT%/grib2/" \
    -e "s/\"%NULLVELMULT%\"/$VELOCITYMULTIPLIER/" \
    -e "s/\"%NULLPRESSMULT%\"/0.01/" \
    -e "s/\"%NULLAPPLYRAMP%\"/$boolApplyRamp/" \
    -e "s/\"%NULLRAMPDIST%\"/$SPATIALEXTRAPOLATIONRAMPDISTANCE/" \
     < get_nam_data.pl.json \
     | $SCRIPTDIR/NAMtoOWIRamp.pl > NAMtoOWIRamp.pl.json
preFile=$($SCRIPTDIR/bin/bashJSON.pl --key winPrePressureFile < NAMtoOWIRamp.pl.json)
winFile=$($SCRIPTDIR/bin/bashJSON.pl --key winPreVelocityFile < NAMtoOWIRamp.pl.json)
mv fort.22 $SCENARIODIR
mv get_nam_forecast.json get_nam_data.pl.* NAMtoOWIRamp.pl.* rotatedNAM.txt reproject.log lambert_diag.out forecast.properties $SCENARIODIR
#
#  N O W C A S T   F R O M   F I L E S Y S T E M
#
# test the construction of a nowcast from grib2 files already
# on the filesystem
# set variables to be used in json template
RUNDIR=$PWD
namArchiveDir=$RUNDIR/nam-filesystem
if [[ -d $namArchiveDir ]]; then
    rm -rf $namArchiveDir
fi
# use the files already downloaded to construct a nowcast
# from the filesystem
mv $instanceNamDir $namArchiveDir
BACKSITE=filesystem          # <--
BACKDIR=$namArchiveDir       # <--
FORECASTCYCLE="00,18"
DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
THIS=cmd_line_test
lastCycle=$(date -d yesterday +%Y%m%d00)
namTemplateName="get_nam_template.json"
filledNamTemplateName="asgs_main.sh_get_nam_status.json"
# keep sed from getting confused by escaping slashes
escBACKDIR=${BACKDIR////'\/'}
escRUNDIR=${RUNDIR////'\/'}
escSCRIPTDIR=${SCRIPTDIR////'\/'}
escInstanceNamDir=${instanceNamDir////'\/'}
arrFORECASTCYCLE=${FORECASTCYCLE//,/\",\"}
stage=NOWCAST
#
# fill in template with data needed to get NAM status
#
# asgs_main.sh->downloadBackgroundMet :
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
# getting nam status
get_nam_status.pl < $filledNamTemplateName > get_nam_status.pl.json
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to get status of NAM cycles with get_nam_status.pl."
    exit $err
fi
latest.pl < get_nam_status.pl.json > latestCycle
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to extract the latest NAM cycle from get_nam_status.pl.json with latest.pl"
    exit $err
fi
latestCycle=$(<"latestCycle")
# refine the list of NAM cycles to end the nowcast on the correct cycle
select_nam_nowcast.pl < get_nam_status.pl.json > select_nam_nowcast.pl.json
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to select the proper NAM cycle to end the nowcast with select_nam_nowcast.pl."
    exit $err
fi
# then download the actual nowcast data for the time range of interest
get_nam_data.pl < select_nam_nowcast.pl.json > get_nam_data.pl.json
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to download NAM nowcast data with get_nam_data.pl.json."
    exit $err
fi
thisCycle=$(latest.pl < select_nam_nowcast.pl.json)
err=$?
if [[ $err != 0 ]]; then
    echo "ERROR: Failed to get the latest cycle from select_nam_nowcast.pl.json."
    exit $err
fi
# for extracting the grib2 and converting to ascii win/pre (owi) format
SCENARIO=nowcast-filesystem
SCENARIODIR=$RUNDIR/$SCENARIO
if [[ ! -d $SCENARIODIR ]]; then mkdir $SCENARIODIR ; fi
escSCENARIODIR=${SCENARIODIR////'\/'}
SPATIALEXTRAPOLATIONRAMPDISTANCE=1.0
VELOCITYMULTIPLIER=1.0
boolApplyRamp=true
ptFilePath=${SCRIPTDIR}/input/ptFile_oneEighth.txt
escPtFilePath=${ptFilePath////'\/'}
# NAMtoOWIRamp.pl writes the ascii OWI WIN/PRE files to
# the scenario directories
sed -e "s/%NULLNAMWINPREDATAPATH%/$escSCENARIODIR/" \
    -e "s/%NULLNAMWINPREGRID%/$escPtFilePath/" \
    -e "s/\"%NULLNAMAWIPGRID%\"/218/" \
    -e "s/%NULLNAMRAWFORMAT%/grib2/" \
    -e "s/\"%NULLVELMULT%\"/$VELOCITYMULTIPLIER/" \
    -e "s/\"%NULLPRESSMULT%\"/0.01/" \
    -e "s/\"%NULLAPPLYRAMP%\"/$boolApplyRamp/" \
    -e "s/\"%NULLRAMPDIST%\"/$SPATIALEXTRAPOLATIONRAMPDISTANCE/" \
     < get_nam_data.pl.json \
     | $SCRIPTDIR/NAMtoOWIRamp.pl > NAMtoOWIRamp.pl.json
preFile=$($SCRIPTDIR/bin/bashJSON.pl --key winPrePressureFile < NAMtoOWIRamp.pl.json)
winFile=$($SCRIPTDIR/bin/bashJSON.pl --key winPreVelocityFile < NAMtoOWIRamp.pl.json)
mv fort.22 $SCENARIODIR
mv get_nam_data.pl.* NAMtoOWIRamp.pl.* get_nam_status.pl.* asgs_main.sh_get_nam_status.json rotatedNAM.txt select_nam_nowcast.pl.json reproject.log lambert_diag.out $SCENARIODIR
# move the files already downloaded back to where they
# can be used to run this test again (for use as a cache to
# avoid having to download them again)
mv $namArchiveDir $instanceNamDir