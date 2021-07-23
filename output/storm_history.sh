#!/bin/bash
#-----------------------------------------------------------------------
# storm_history.sh : Collect continuous storm history using
# nowcasts from previous cycle. 
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
echo "Loading properties."
# get loadProperties function
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' $RUNPROPERTIES`
source $SCRIPTDIR/properties.sh
# load run.properties file into associative array
loadProperties $RUNPROPERTIES
echo "Finished loading properties."
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
allMessage "$SCENARIO: $THIS: Starting post processing."
scenarioMessage "$THIS: SCENARIO=$SCENARIO ; SCENARIODIR=$SCENARIODIR"
cd ${SCENARIODIR} 2>&1 > errmsg || warn "cycle $CYCLE: $SCENARIO: $THIS: Could not change directory to $SCENARIODIR: `cat $errmsg`"
#
#-----------------------------------------------------------------------
#     A C C U M U L A T E   M I N   /   M A X 
#------------------------------------------------------------------------
# get path to hotstart file that started this run
fromdir=${properties['path.fromdir']}
# set previous advisory number with leading zero if appropriate
# FIXME: this makes an assumption that previous advisory number is one
# less than the current one
previousAdvisory=$(printf "%02d" `expr $CYCLE - 1`)
for file in maxele.63.nc maxinundepth.63.nc maxrs.63.nc maxvel.63.nc maxwvel.63.nc swan_HS_max.63.nc swan_TPS_max.63.nc ; do 
   if [[ -e $file ]]; then
      # create backup copy of the file just in case
      cp $file backup_${file}
      # merge nowcast min/max with current one
      if [[ -e $fromdir/$file ]]; then
         ${OUTPUTDIR}/collectMinMax.x --source $fromdir/$file --destination $file
      fi
      # merge previous min/max with current one
      previousPath=../../$previousAdvisory/nowcast
      if [[ -e $previousPath/$file ]]; then
         ${OUTPUTDIR}/collectMinMax.x --source ../../$previousAdvisory/nowcast/$file --destination $file       
      fi
   fi
done
