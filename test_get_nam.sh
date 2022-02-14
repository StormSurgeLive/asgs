#!/bin/bash
#
# set variables to be used in json template
SCRIPTDIR=~/Campaigns/Development/asgs
RUNDIR=$PWD
escRUNDIR=${RUNDIR////'\/'}
escSCRIPTDIR=${SCRIPTDIR////'\/'}
BACKSITE=ftp.ncep.noaa.gov
BACKDIR=/pub/data/nccf/com/nam/prod
escBACKDIR=${BACKDIR////'\/'}
instanceNamDir=$PWD/nam
escInstanceNamDir=${instanceNamDir////'\/'}
FORECASTCYCLE="00,18"
arrFORECASTCYCLE=${FORECASTCYCLE//,/\",\"}
boolApplyRamp=true
ptFilePath=${SCRIPTDIR}/input/ptFile_oneEighth.txt
escPtFilePath=${ptFilePath////'\/'}
DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
THIS=cmd_line_test
stage=NOWCAST
lastCycle=$(date -d yesterday +%Y%m%d00)
SPATIALEXTRAPOLATIONRAMPDISTANCE=1.0
VELOCITYMULTIPLIER=1.0
