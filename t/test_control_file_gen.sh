#!/bin/bash
#----------------------------------------------------------------
# test_control_file_gen.sh: Driver script for the creating
# yaml to feed control_file_gen.pl and to create fort.15 and
# fort.26 control files for testing purposes
#----------------------------------------------------------------
# Copyright(C) 2024--2025 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------
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
# initialize test number and log files
t=1
ADVISDIR=$SCRIPTDIR/t
SCENARIODIR=$SCRIPTDIR/t
SYSLOG="$(basename $0)-syslog.log"
SCENARIOLOG="$(basename $0)-scenario.log"
#
# dynamic input
HSTIME=86400.0
runLength=$(echo "scale=2; ($HSTIME)/86400" | bc)
SCENARIO=nowcast
ADVISORY=20
CSDATE=2024010100
ENDTIME=2024010300 # FIXME: for some scenarios, this is computed by control_file_gen.pl ; whereas for others (tc and tidal init) it must be provided to control_file_gen.pl
NWS=20
#x
#  T E S T   0 0 1
#
# Description: shinnecock inlet as-is
# nowcast with GAHM forcing
#
GRIDNAME=Shinnecock
source $SCRIPTDIR/config/mesh_defaults.sh
adcirc_version="v53.05-modified"
# build command line for control_file_gen.pl
C="--name $SCENARIO"
C="$C --advisorynum $ADVISORY"
C="$C --cst $CSDATE"
C="$C --endtime $ENDTIME"
C="$C --dt $TIMESTEPSIZE"
C="$C --nws $NWS"
C="$C --bladj $BLADJ"
C="$C --pureVortex $PUREVORTEX"
C="$C --pureBackground $PUREBACKGROUND"
C="$C --hsformat $HOTSTARTFORMAT"
C="$C --hstime $HSTIME"
C="$C --elevstations $INPUTDIR/$ELEVSTATIONS"
C="$C --velstations $INPUTDIR/$VELSTATIONS"
C="$C --metstations $INPUTDIR/$METSTATIONS"
C="$C --gridname $GRIDNAME"              # to be recorded in run.properties
C="$C --nscreen $NSCREEN"
C="$C --swantemplate $SWANTEMPLATE"
C="$C $OUTPUTOPTIONS"
C="$C --controltemplate $INPUTDIR/$CONTROLTEMPLATE"
CONTROLOPTIONS="$C"
# fill in yaml template for control parameters and execute control_file_gen.pl
generateDynamicInput
# rename control parameters file
control_parameters=$(printf "ct-%03d-control-parameters.yaml" $t)
mv $SCENARIODIR/$SCENARIO.control_parameters.yaml $control_parameters >> $SYSLOG 2>&1
fort15=$(printf "ct-%03d-fort.15" $t)
mv ${SCENARIO}.fort.15 $fort15
fort13=$(printf "ct-%03d-fort.13" $t)
mv fort.13 $fort13 >> $SYSLOG 2>&1        # give nodal attributes file a unique name
rp=$(printf "ct-%03d-run.properties" $t)
mv run.properties $rp >> $SYSLOG 2>&1    # give run.properties file a unique name
t=$((t + 1))
#
#  T E S T   0 0 2
#
# Description: EGOMv20b with hardcoded parameter package
# nowcast with GAHM forcing
#
GRIDNAME=EGOMv20b
source $SCRIPTDIR/config/mesh_defaults.sh
NAFILE=EGOM-RT_v20b_asgs_chk_header.13.template # avoid handling the whole nodal attributes file
# ** other parameters are same as defined above unless redefined below **
# fill in yaml template for control parameters and execute control_file_gen.pl
C="--name $SCENARIO"
C="$C --advisorynum $ADVISORY"
C="$C --cst $CSDATE"
C="$C --endtime $ENDTIME"
C="$C --dt $TIMESTEPSIZE"
C="$C --nws $NWS"
C="$C --bladj $BLADJ"
C="$C --pureVortex $PUREVORTEX"
C="$C --pureBackground $PUREBACKGROUND"
C="$C --hsformat $HOTSTARTFORMAT"
C="$C --hstime $HSTIME"
C="$C --elevstations $INPUTDIR/$ELEVSTATIONS"
C="$C --velstations $INPUTDIR/$VELSTATIONS"
C="$C --metstations $INPUTDIR/$METSTATIONS"
C="$C --gridname $GRIDNAME"              # to be recorded in run.properties
C="$C --nscreen $NSCREEN"
C="$C --swantemplate $SWANTEMPLATE"
C="$C $OUTPUTOPTIONS"
C="$C --controltemplate $INPUTDIR/$CONTROLTEMPLATE"
CONTROLOPTIONS="$C"
generateDynamicInput
# rename control parameters file
control_parameters=$(printf "ct-%03d-control-parameters.yaml" $t)
mv $SCENARIODIR/$SCENARIO.control_parameters.yaml $control_parameters >> $SYSLOG 2>&1
fort15=$(printf "ct-%03d-fort.15" $t)
mv ${SCENARIO}.fort.15 $fort15
fort13=$(printf "ct-%03d-fort.13" $t)
mv fort.13 $fort13 >> $SYSLOG 2>&1        # give nodal attributes file a unique name
rp=$(printf "ct-%03d-run.properties" $t)
mv run.properties $rp >> $SYSLOG 2>&1    # give run.properties file a unique name
t=$((t + 1))
#
#  T E S T   0 0 3
#
# Description: EGOMv20b with default parameter package (not hardcoded into template)
# nowcast with GAHM forcing
#
GRIDNAME=EGOMv20b
parameterPackage="default"
source $SCRIPTDIR/config/mesh_defaults.sh
NAFILE=EGOM-RT_v20b_asgs_chk_header.13.template # avoid handling the whole nodal attributes file
# ** other parameters are same as defined above unless redefined below **
# build command line for control_file_gen.pl
C="--name $SCENARIO"
C="$C --advisorynum $ADVISORY"
C="$C --cst $CSDATE"
C="$C --endtime $ENDTIME"
C="$C --dt $TIMESTEPSIZE"
C="$C --nws $NWS"
C="$C --bladj $BLADJ"
C="$C --pureVortex $PUREVORTEX"
C="$C --pureBackground $PUREBACKGROUND"
C="$C --hsformat $HOTSTARTFORMAT"
C="$C --hstime $HSTIME"
C="$C --elevstations $INPUTDIR/$ELEVSTATIONS"
C="$C --velstations $INPUTDIR/$VELSTATIONS"
C="$C --metstations $INPUTDIR/$METSTATIONS"
C="$C --gridname $GRIDNAME"              # to be recorded in run.properties
C="$C --nscreen $NSCREEN"
C="$C --swantemplate $SWANTEMPLATE"
C="$C $OUTPUTOPTIONS"
C="$C --controltemplate $INPUTDIR/$CONTROLTEMPLATE"
CONTROLOPTIONS="$C"
# fill in yaml template for control parameters
generateDynamicInput
#
# rename control parameters file
control_parameters=$(printf "ct-%03d-control-parameters.yaml" $t)
mv $SCENARIODIR/$SCENARIO.control_parameters.yaml $control_parameters
fort15=$(printf "ct-%03d-fort.15" $t)
mv ${SCENARIO}.fort.15 $fort15
fort13=$(printf "ct-%03d-fort.13" $t)
mv fort.13 $fort13 >> $SYSLOG 2>&1        # give nodal attributes file a unique name
rp=$(printf "ct-%03d-run.properties" $t)
mv run.properties $rp >> $SYSLOG 2>&1    # give run.properties file a unique name
t=$((t + 1))
#
#  T E S T   0 0 4
#
# Description: EGOMv20b with default parameter package (not hardcoded into template)
# nowcast with GAHM forcing, only producing fort.63.nc
#
GRIDNAME=EGOMv20b
parameterPackage="default"
source $SCRIPTDIR/config/mesh_defaults.sh
NAFILE=EGOM-RT_v20b_asgs_chk_header.13.template # avoid handling the whole nodal attributes file
# ** other parameters are same as defined above unless redefined below **
# fill in yaml template for control parameters

# build command line for control_file_gen.pl
OUTPUTOPTIONS="--fort63freq 3600.0 --fort63netcdf"  # <--<< only produce fort.63.nc
C="--name $SCENARIO"
C="$C --advisorynum $ADVISORY"
C="$C --cst $CSDATE"
C="$C --endtime $ENDTIME"
C="$C --dt $TIMESTEPSIZE"
C="$C --nws $NWS"
C="$C --bladj $BLADJ"
C="$C --pureVortex $PUREVORTEX"
C="$C --pureBackground $PUREBACKGROUND"
C="$C --hsformat $HOTSTARTFORMAT"
C="$C --hstime $HSTIME"
C="$C --elevstations $INPUTDIR/$ELEVSTATIONS"
C="$C --velstations $INPUTDIR/$VELSTATIONS"
C="$C --metstations $INPUTDIR/$METSTATIONS"
C="$C --gridname $GRIDNAME"              # to be recorded in run.properties
C="$C --nscreen $NSCREEN"
C="$C --swantemplate $SWANTEMPLATE"
C="$C $OUTPUTOPTIONS"
C="$C --controltemplate $INPUTDIR/$CONTROLTEMPLATE"
CONTROLOPTIONS="$C"
generateDynamicInput
# rename control parameters file
control_parameters=$(printf "ct-%03d-control-parameters.yaml" $t)
mv $SCENARIODIR/$SCENARIO.control_parameters.yaml $control_parameters
#
fort15=$(printf "ct-%03d-fort.15" $t)
mv ${SCENARIO}.fort.15 $fort15
fort13=$(printf "ct-%03d-fort.13" $t)
mv fort.13 $fort13 >> $SYSLOG 2>&1        # give nodal attributes file a unique name
rp=$(printf "ct-%03d-run.properties" $t)
mv run.properties $rp >> $SYSLOG 2>&1    # give run.properties file a unique name
t=$((t + 1))