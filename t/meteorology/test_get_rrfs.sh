#!/bin/bash
#
source $SCRIPTDIR/variables_init.sh
variables_init
source $SCRIPTDIR/monitoring/spinner.sh
source $SCRIPTDIR/monitoring/logging.sh
source $SCRIPTDIR/config/forcing_defaults.sh
source $SCRIPTDIR/downloadRRFS.sh
#
STATEFILE="test_rrfs.state"
echo "ADVISORY=0" > $STATEFILE
echo "script : test_get_rrfs.sh" > run.properties
BACKGROUNDMET=RRFS
RUNDIR=$PWD
CSDATE=$(date -d yesterday +%Y%m%d00)
HSTIME=21600.00000
stage=NOWCAST
SCENARIO=nowcast
SCENARIODIR=$RUNDIR/$SCENARIO
FORECASTCYCLE="00,18"
# keep files small (but realistic) for testing
rrfsLatLonGrid['lon0']='-100'
rrfsLatLonGrid['nlon']='600'
rrfsLatLonGrid['dlon']='0.1'
rrfsLatLonGrid['lat0']='5'
rrfsLatLonGrid['nlat']='600'
rrfsLatLonGrid['dlat']='0.1'
#
breakPoint=rrfs.template
SYSLOG=$RUNDIR/test.$breakPoint.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
breakPoint=${breakPoint}.catalog
SYSLOG=$RUNDIR/test.$breakPoint.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
breakPoint=${breakPoint}.select
SYSLOG=$RUNDIR/test.$breakPoint.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
breakPoint=${breakPoint}.hourly
SYSLOG=$RUNDIR/test.$breakPoint.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
breakPoint=${breakPoint}.download
SYSLOG=$RUNDIR/test.$breakPoint.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
breakPoint=${breakPoint}.regrid
SYSLOG=$RUNDIR/test.$breakPoint.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
breakPoint=${breakPoint}.owiwinpre
SYSLOG=$RUNDIR/test.$breakPoint.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
breakPoint=${breakPoint}.metadata
SYSLOG=$RUNDIR/test.$breakPoint.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
breakPoint=${breakPoint}.scenariodir
SYSLOG=$RUNDIR/test.$breakPoint.downloadRRFS.log
downloadRRFS  # <- uncomment only this invokation to run to this breakpoint