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
finishTest()
{
   # rename result file according to the test number
   declare -a resultFiles=( "$SCENARIO.control_parameters.yaml" )
   resultFiles+=( "${SCENARIO}.fort.15" )
   resultFiles+=( "fort.13" "fort.26" "run.properties" )
   for f in ${resultFiles[@]} ; do
      if [[ -e $f ]]; then
         n=$(printf "ct-%03d-%s" $t $f) # new file name specific to test number
         mv $f $n >> $SYSLOG 2>&1
      fi
   done
   # TODO: compare with expected output
   #
   # increment test number
   t=$((t + 1))
}
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
HINDCASTLENGTH=20.0
HSTIME=86400.0
runLength=$(echo "scale=2; ($HSTIME)/86400" | bc)
SCENARIO=nowcast
ADVISORY=20
CSDATE=2024010100
endTime=2024010300 # FIXME: for some scenarios, this is computed by control_file_gen.pl ; whereas for others (tc and tidal init) it must be provided to control_file_gen.pl
BASENWS=20
NWS=20
WAVES="off"
storm_name="KATRINA" # <---<< FIXME: this is not populated in asgs_main.sh
#
#  T E S T   0 0 1
#
# Description: shinnecock inlet as-is
# nowcast with GAHM forcing
#
GRIDNAME=Shinnecock
source $SCRIPTDIR/config/mesh_defaults.sh
adcirc_version="v53.05-modified"
# fill in yaml template for control parameters and execute control_file_gen.pl
generateDynamicInput
# rename control parameters and other result files
finishTest
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
generateDynamicInput
# rename control parameters file
finishTest
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
# fill in yaml template for control parameters
generateDynamicInput
# rename control parameters file
finishTest
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
generateDynamicInput
# rename control parameters file
finishTest
#
#  T E S T   0 0 5
#
# Description: EGOMv20b with default parameter package (not hardcoded into template)
# nowcast with GAHM forcing and SWAN coupling only producing fort.63.nc
#
GRIDNAME=EGOMv20b
parameterPackage="default"
source $SCRIPTDIR/config/mesh_defaults.sh
NAFILE=EGOM-RT_v20b_asgs_chk_header.13.template # avoid handling the whole nodal attributes file
NWS=320
WAVES=on
HOTSWAN=on
# ** other parameters are same as defined above unless redefined below **
# fill in yaml template for control parameters

# build command line for control_file_gen.pl
OUTPUTOPTIONS="--fort63freq 3600.0 --fort63netcdf"  # <--<< only produce fort.63.nc
generateDynamicInput
# rename control parameters file
finishTest