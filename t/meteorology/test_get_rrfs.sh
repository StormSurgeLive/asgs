#!/bin/bash
#
source $SCRIPTDIR/variables_init.sh
variables_init
source $SCRIPTDIR/monitoring/spinner.sh
source $SCRIPTDIR/monitoring/logging.sh
source $SCRIPTDIR/config/forcing_defaults.sh
source $SCRIPTDIR/downloadRRFS.sh
#
RUNDIR=$PWD
CSDATE=$(date -d yesterday +%Y%m%d00)
HSTIME=21600.00000
stage=NOWCAST
SCENARIO=nowcast
SCENARIODIR=$RUNDIR/$SCENARIO
FORECASTCYCLE="00,18"
#
unitTest=rrfs.template
SYSLOG=$RUNDIR/test.$unitTest.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
unitTest=${unitTest}.catalog
SYSLOG=$RUNDIR/test.$unitTest.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
unitTest=${unitTest}.select
SYSLOG=$RUNDIR/test.$unitTest.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
unitTest=${unitTest}.hourly
SYSLOG=$RUNDIR/test.$unitTest.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
unitTest=${unitTest}.download
SYSLOG=$RUNDIR/test.$unitTest.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
unitTest=${unitTest}.regrid
SYSLOG=$RUNDIR/test.$unitTest.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
unitTest=${unitTest}.owiwinpre
SYSLOG=$RUNDIR/test.$unitTest.downloadRRFS.log
#downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
unitTest=${unitTest}.metadata
SYSLOG=$RUNDIR/test.$unitTest.downloadRRFS.log
downloadRRFS  # <- uncomment only this invokation to run to this breakpoint
#
unitTest=${unitTest}.scenariodir
SYSLOG=$RUNDIR/test.$unitTest.downloadRRFS.log
downloadRRFS  # <- uncomment only this invokation to run to this breakpoint