#!/bin/bash
#
# set variables to be used in json template
SCRIPTDIR=~/Campaigns/Development/asgs
RUNDIR=$PWD
BACKSITE=ftp.ncep.noaa.gov
BACKDIR=/pub/data/nccf/com/nam/prod
instanceNamDir=$PWD/nam
FORECASTCYCLE="00,18"
DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
THIS=cmd_line_test
lastCycle=$(date -d yesterday +%Y%m%d00)
namTemplateName="get_nam.json.template"
filledNamTemplateName="asgs_main.sh_get_nam_status.json"
# keep sed from getting confused by escaping slashes
escBACKDIR=${BACKDIR////'\/'}
escRUNDIR=${RUNDIR////'\/'}
escSCRIPTDIR=${SCRIPTDIR////'\/'}
escInstanceNamDir=${instanceNamDir////'\/'}
arrFORECASTCYCLE=${FORECASTCYCLE//,/\",\"}
#
# fill in template with data needed to get NAM status
#
# asgs_main.sh->downloadBackgroundMet :
sed -e "s/%NULLSCRIPTDIR%/$escSCRIPTDIR/" \
    -e "s/%NULLNAMDATAPATH%/$escInstanceNamDir/" \
    -e "s/%NULLGETNAMTEMPLATEFILE%/$namTemplateName/" \
    -e "s/%NULLGETNAMTEMPLATEFILLEDFILE%/$filledNamTemplateName/" \
    -e "s/%NULLBACKSITE%/$BACKSITE/" \
    -e "s/%NULLBACKDIR%/$escBACKDIR/" \
    -e "s/%NULLCYCLE%/$lastCycle/" \
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
     > $filledNamTemplateName
# getting nam status
latestCycle=$($SCRIPTDIR/get_nam_status.pl  \
             < $filledNamTemplateName       \
             | $SCRIPTDIR/latest.pl         \
            )
# selecting
thisCycle=$(sed "s/%NULLFORECASTCYCLE%/$arrFORECASTCYCLE/" \
            < get_nam_status.pl.json                       \
            | $SCRIPTDIR/select_nam_nowcast.pl             \
            | $SCRIPTDIR/latest.pl)
# get nowcast data
stage=NOWCAST
sed "s/%NULLSTAGE%/$stage/"  \
     < select_nam_nowcast.pl.json \
     | $SCRIPTDIR/get_nam_data.pl \
     > /dev/null
# for converting to ascii owi format
SCENARIO=nowcast
SCENARIODIR=$RUNDIR/nowcast
if [[ ! -d $SCENARIODIR ]]; then mkdir $SCENARIODIR ; fi
escSCENARIODIR=${SCENARIODIR////'\/'}
SPATIALEXTRAPOLATIONRAMPDISTANCE=1.0
VELOCITYMULTIPLIER=1.0
boolApplyRamp=true
ptFilePath=${SCRIPTDIR}/input/ptFile_oneEighth.txt
escPtFilePath=${ptFilePath////'\/'}
sed -e "s/%NULLNAMOWIDATAPATH%/$escSCENARIODIR/" \
    -e "s/%NULLNAMOWIGRID%/$escPtFilePath/" \
    -e "s/\"%NULLNAMAWIPGRID%\"/218/" \
    -e "s/%NULLNAMRAWFORMAT%/grib2/" \
    -e "s/\"%NULLVELMULT%\"/$VELOCITYMULTIPLIER/" \
    -e "s/\"%NULLPRESSMULT%\"/0.01/" \
    -e "s/\"%NULLAPPLYRAMP%\"/$boolApplyRamp/" \
    -e "s/\"%NULLRAMPDIST%\"/$SPATIALEXTRAPOLATIONRAMPDISTANCE/" \
     < get_nam_data.pl.json \
     | $SCRIPTDIR/NAMtoOWIRamp.pl > /dev/null
mv get_nam_data.pl.json NAMtoOWIRamp.pl.json $SCENARIODIR
