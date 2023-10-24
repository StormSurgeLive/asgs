#!/bin/bash
#-----------------------------------------------------------------------
# includeWind10m.sh : Make symbolic links to results from corresponding
# Wind10m results.
#-----------------------------------------------------------------------
# Copyright(C) 2018--2019 Jason Fleming
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
#-----------------------------------------------------------------------
#
THIS=$(basename -- $0)
# Count command line arguments; use them if provided or use
# run.properties if not.
declare -A properties
SCENARIODIR=$PWD
RUNPROPERTIES=$SCENARIODIR/run.properties
if [[ $# -eq 1 ]]; then
   RUNPROPERTIES=$1
fi
# this script can be called with just one command line option: the
# full path to the run.properties file
# echo "Loading properties."
# get loadProperties function
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' $RUNPROPERTIES`
source $SCRIPTDIR/properties.sh
# load run.properties file into associative array
loadProperties $RUNPROPERTIES
# echo "Finished loading properties."
# now set variables that would otherwise be set by command line arguments
CONFIG=${properties['config.file']}
CYCLEDIR=${properties['path.advisdir']}
CYCLE=${properties['advisory']}
HPCENV=${properties['hpc.hpcenv']}
SCENARIO=${properties['scenario']}
CSDATE=${properties['adcirc.time.coldstartdate']}
HSTIME=${properties['InitialHotStartTime']}
GRIDFILE=${properties['adcirc.file.input.gridfile']}
OUTPUTDIR=${properties['path.outputdir']}
SYSLOG=${properties['monitoring.logging.file.syslog']}
SSHKEY=${properties['post.file.sshkey']}
HPCENVSHORT=${properties['hpc.hpcenvshort']}
HPCENV=${properties['hpc.hpcenv']}
TROPICALCYCLONE=${properties['forcing.tropicalcyclone']}
if [[ $TROPICALCYCLONE != "off" ]]; then
   STORM=${properties['forcing.tropicalcyclone.stormnumber']}
   STORM=${properties['forcing.tropicalcyclone.stormname']}
   YEAR=${properties['forcing.tropicalcyclone.year']}
else
   STORM="null"
   YEAR=${CYCLE:0:4}
fi
#
SCENARIODIR=${CYCLEDIR}/${SCENARIO}       # shorthand
CYCLELOG=${properties['monitoring.logging.file.cyclelog']}
SCENARIOLOG=${properties['monitoring.logging.file.scenariolog']}
source ${SCRIPTDIR}/monitoring/logging.sh
source ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${HPCENVSHORT}
logMessage "$SCENARIO: $THIS: Starting post processing for adding Wind10m layer."
scenarioMessage "$THIS: SCENARIO=$SCENARIO ; SCENARIODIR=$SCENARIODIR"
cd ${SCENARIODIR} 2>&1 > errmsg || warn "cycle $CYCLE: $SCENARIO: $THIS: Could not change directory to $SCENARIODIR: `cat $errmsg`"
#
#-----------------------------------------------------------------------
#          I N C L U S I O N   O F   10 M   W I N D S
#-----------------------------------------------------------------------
# If winds at 10m (i.e., wind velocities that do not include the effect
# of land interaction from nodal attributes line directional wind roughness
# and canopy coefficient) were produced by another ensemble member,
# then include these winds in the post processing
wind10mFound=no
dirWind10m=$CYCLEDIR/${SCENARIO}Wind10m
if [[ -d $dirWind10m ]]; then
   scenarioMessage "$THIS: Corresponding 10m wind ensemble member was found."
   wind10mFound=yes
   for file in fort.72.nc fort.74.nc maxwvel.63.nc ; do
      if [[ -e $dirWind10m/$file && ! -e ./wind10m.${file} ]]; then
         scenarioMessage "$THIS: Found $dirWind10m/${file}."
         cp $dirWind10m/${file} ./wind10m.${file} 2>&1 > errmsg || warn "cycle $CYCLE: $SCENARIO: $THIS: Could not copy from Wind10m directory: `cat $errmsg`"
         # update the run.properties file
         case $file in
         "fort.72.nc")
            echo "Wind Velocity 10m Stations File Name : wind10m.fort.72.nc" >> run.properties
            echo "Wind Velocity 10m Stations Format : netcdf" >> run.properties
            ;;
         "fort.74.nc")
            echo "Wind Velocity 10m File Name : wind10m.fort.74.nc" >> run.properties
            echo "Wind Velocity 10m Format : netcdf" >> run.properties
            ;;
         "maxwvel.63.nc")
            echo "Maximum Wind Speed 10m File Name : wind10m.maxwvel.63.nc" >> run.properties
            echo "Maximum Wind Speed 10m Format : netcdf" >> run.properties
            ;;
         *)
            warn "cycle $CYCLE: $SCENARIO: $THIS: The file $file was not recognized."
         ;;
         esac
      else
         warn "cycle $CYCLE: $SCENARIO: $THIS: The file $dirWind10m/${file} was not found."
      fi
   done
else
   warn "cycle $CYCLE: $SCENARIO: $THIS: Corresponding 10m wind ensemble member was not found."
fi
