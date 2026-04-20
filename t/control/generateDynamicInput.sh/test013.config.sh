#!/bin/bash
#
#  T E S T   0 1 3
#
# Description: EGOMv20b with default parameter package
# tides-only hindcast
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
GRIDNAME=EGOMv20b
parameterPackage=default
source $SCRIPTDIR/config/mesh_defaults.sh
NAFILE=EGOM-RT_v20b_asgs_chk_header.13.template # avoid handling the whole nodal attributes file
#
CSDATE=2024010100
HINDCASTLENGTH=2.0
HSTIME=0.0
endTime=2024010300 # 2 days
runLength=2.0 # only used to compute tide nodal factors
SCENARIO=hindcast
BASENWS=0
NWS=0
test_adcirc_version="v53.05-modified"
