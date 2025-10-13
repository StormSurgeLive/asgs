#!/bin/bash
#
source $SCRIPTDIR/variables_init.sh
variables_init
source $SCRIPTDIR/monitoring/spinner.sh
source $SCRIPTDIR/monitoring/logging.sh
source $SCRIPTDIR/config/forcing_defaults.sh
source $SCRIPTDIR/downloadRRFS.sh
#
BACKGROUNDMET=RRFS
STATEFILE="test_rrfs.state"
RUNDIR=$PWD
echo "ADVISORY=0" > $STATEFILE
CSDATE=$(date -d yesterday +%Y%m%d00)
HSTIME=21600.00000
#CSDATE=2025101300
#HSTIME=0.000
FORECASTCYCLE="00,18"
# keep files smaller (but still realistic) for testing
rrfsLatLonGrid['lon0']='-100'
rrfsLatLonGrid['nlon']='600'
rrfsLatLonGrid['dlon']='0.1'
rrfsLatLonGrid['lat0']='5'
rrfsLatLonGrid['nlat']='600'
rrfsLatLonGrid['dlat']='0.1'
#
SYSLOG=$RUNDIR/test.downloadRRFS.log
breakPoint=test
#
#      N O W C A S T
#
stage=NOWCAST
SCENARIO=nowcast
SCENARIODIR=$RUNDIR/$SCENARIO
#breakPoint=rrfs.template
#breakPoint=${breakPoint}.catalog
#breakPoint=${breakPoint}.select
#breakPoint=${breakPoint}.hourly
#breakPoint=${breakPoint}.download
#breakPoint=${breakPoint}.regrid
#breakPoint=${breakPoint}.owiwinpre
#breakPoint=${breakPoint}.metadata
#breakPoint=${breakPoint}.scenariodir
#downloadRRFS
#
#      F O R E C A S T
#
#rrfsForecastCycle=$(cat $instanceRrfsDir/select_rrfs_nowcast.json | jq '.cyclelist[-1]')
ADVISORY=$( cat $SCENARIODIR/RRFS_NOWCAST_??????????_??????????.json | jq '.cyclelist[-1]' )
stage=FORECAST
SCENARIO=rrfsforecast
#breakPoint=rrfs.forecast.index
#breakPoint=${breakPoint}.subset
#breakPoint=${breakPoint}.regrid
#breakPoint=${breakPoint}.owiwinpre
#breakPoint=${breakPoint}.metadata
#breakPoint=${breakPoint}.scenariodir
#
downloadRRFS