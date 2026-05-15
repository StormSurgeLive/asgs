#!/bin/bash
#
#  T E S T   0 1 4
#
# Description: EC95d with default parameter package
# branching ensemble member 01
#
# Initialize variables accessed from ASGS config parameters to reasonable values
source ${SCRIPTDIR}/config/config_defaults.sh
# Initialize model parameters to appropriate values
source ${SCRIPTDIR}/config/model_defaults.sh
# set default output file formats and frequencies
source ${SCRIPTDIR}/config/io_defaults.sh
# set default values related to forcing URLs etce
source ${SCRIPTDIR}/config/forcing_defaults.sh
# source the script that fills in the yaml template
source $SCRIPTDIR/generateDynamicInput.sh
# initialize test number and log files
#
# dynamic input
GRIDNAME=EC95d
parameterPackage=default
createWind10mLayer="no"  # don't need this because there are no wind roughnesses
source $SCRIPTDIR/config/mesh_defaults.sh
#
TIDEFAC=on           # tide factor recalc
BACKGROUNDMET=off    # NAM/GFS download/forcing
TROPICALCYCLONE=on   # tropical cyclone forcing
STORM=13             # storm number, e.g. 05=ernesto in 2006
YEAR=2020            # year of the storm
STORMNAME="LAURA"    # <---<< FIXME: this is not populated in asgs_main.sh
ADVISORY=18
FDIR=$WORK
HDIR="$FDIR"
RSSSITE=filesystem
FTPSITE=filesystem
WAVES=off            # wave forcing
REINITIALIZESWAN=no  # used to bounce the wave solution
VARFLUX=off          # variable river flux forcing
CSDATE=2020072300
HINDCASTLENGTH=30.0
BASENWS=20
NWS=20
test_adcirc_version="v56.0.4.live.0"
#
# branching ensemble tracks
b=11   # branch number
branchNum=$(printf "%02d" $b)
SCENARIO=branching$branchNum
#       01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17   # branch
tau=( 0 48 60  0 60 48 36 48 60  0 60 48 36 48 60  0 60 48 ) # hotstart time (hours beyond base forecast)
HSTIME=$(( 2786400 + ( ${tau[$b]} * 3600 ) ))
endTime=2020082906   # tcEnd=$(grep "forcing.tropicalcyclone.fcst.time.end" run.properties | sed 's/forcing.tropicalcyclone.fcst.time.end.*://' | sed 's/^\s//' 2>> ${SYSLOG}) ; echo $tcEnd
runLength=37.25       # only used to compute tide nodal factors