#!/bin/bash
#
#  T E S T   0 0 6
#
# Description: Shinnecock Inlet with default parameter package
# (not hardcoded into template) nowcast with GAHM
# parametric vortex forcing and more extensive namelists
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
GRIDNAME=Shinnecock
parameterPackage="default"
source $SCRIPTDIR/config/mesh_defaults.sh
NAFILE=EGOM-RT_v20b_asgs_chk_header.13.template # avoid handling the whole nodal attributes file
# specify namelist parameters
CONTROLTEMPLATE=shinnecock.15.2026.1.template
metControl["WindDragLimit"]="0.003"   # max wind drag coefficient, unitless
metControl["DragLawString"]="garratt" # "garratt" or "powell"
metControl["outputWindDrag"]="no"     # "yes" or "no" to write fulldomain time varying wind drag coefficient
wetDryControl["windlim"]="on"         # on|off to limit wind stress calculations in shallow water
wetDryControl["StatPartWetFix"]="on"  # on|off to use nearby node in elements with less than 3 wet nodes
inundationOutputControl["inundationOutput"]="no" # yes|no to write extra fulldomain inundation data at end of execution
waveCoupling["WaveWindMultiplier"]="1.0"
waveCoupling["Limit_WaveStressGrad"]="no"
waveCoupling["WaveStressGrad_Cap"]="0.1"
# control the writing of namelist parameters
SWANOutputControl["write_SWAN_OutputTPS"]="yes"
SWANOutputControl["write_SWAN_OutputTM01"]="yes"
SWANOutputControl["write_SWAN_OutputHS"]="yes"
SWANOutputControl["write_SWAN_OutputDIR"]="yes"
SWANOutputControl["write_SWAN_OutputTMM10"]="yes"
SWANOutputControl["write_SWAN_OutputTM02"]="yes"
metControl["write_WindDragLimit"]="yes"
metControl["write_DragLawString"]="yes"
metControl["write_outputWindDrag"]="yes"
waveCoupling["WaveWindMultiplier"]="yes"
waveCoupling["write_Limit_WaveStressGrad"]="yes"
waveCoupling["write_WaveStressGrad_Cap"]="yes"