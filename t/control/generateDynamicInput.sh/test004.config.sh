#!/bin/bash
#
# SCRIPTDIR should be set if this test script is executed with
# the ASGS shell
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
#
# dynamic input
CSDATE=2024010100
HINDCASTLENGTH=2.0
HSTIME=172800.0 # 2 days
endTime=2024010400  # 3 days from coldstart
runLength=3.0 # only used to compute tide nodal factors
SCENARIO=nowcast
ADVISORY=1
BASENWS=20
NWS=20
WAVES="off"
storm_name="TESTVORTEX" # <---<< FIXME: this is not populated in asgs_main.sh
GRIDNAME=EGOMv20b
parameterPackage="default"
source $SCRIPTDIR/config/mesh_defaults.sh
NAFILE=EGOM-RT_v20b_asgs_chk_header.13.template # avoid handling the whole nodal attributes file
# turn off output except for fulldomain water surface elevation
fort61["format"]="off"
fort62["format"]="off"
fort63["format"]="netcdf4"
fort7172["format"]="off"
fort64["format"]="off"
fort7374["format"]="off"
