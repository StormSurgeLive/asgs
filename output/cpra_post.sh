#!/bin/bash
#-----------------------------------------------------------------------
# cpra_post.sh : Minimal post processing to get data onto THREDDS
# server for dissemination via CERA and to create slide deck for CPRA.
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
THIS=cpra_post.sh
# Count command line arguments; use them if provided or use 
# run.properties if not.
declare -A properties
if [[ $# -gt 2 ]]; then
   CONFIG=$1
   CYCLEDIR=$2
   STORM=$3
   YEAR=$4
   CYCLE=$5
   HPCENV=$6
   SCENARIO=$7
   CSDATE=$8
   HSTIME=$9
   GRIDFILE=${10}
   OUTPUTDIR=${11}
   SYSLOG=${12}
   SSHKEY=${13}
else
   # this script can be called with just one command line option: the
   # full path to the run.properties file
   RUNPROPERTIES=$1
   echo "Loading properties."
   # get loadProperties function
   SCRIPTDIR=`sed -n 's/[ ^]*$//;s/config.path.scriptdir\s*:\s*//p' $RUNPROPERTIES`
   source $SCRIPTDIR/properties.sh
   # load run.properties file into associative array
   loadProperties $RUNPROPERTIES
   echo "Finished loading properties."
   # now set variables that would otherwise be set by command line arguments
   CONFIG=${properties['config.file']}
   CYCLEDIR=${properties['asgs.path.advisdir']}
   CYCLE=${properties['advisory']}
   HPCENV=${properties['hpc.hpcenv']}
   SCENARIO=${properties['scenario']}
   CSDATE=${properties['config.adcirc.time.coldstartdate']}
   HSTIME=${properties['InitialHotStartTime']}
   GRIDFILE=${properties['adcirc.file.input.gridfile']}
   OUTPUTDIR=${properties['config.path.outputdir']}
   SYSLOG=${properties['monitoring.logging.file.syslog']}
   SSHKEY=${properties['post.file.sshkey']}
   TROPICALCYCLONE=${properties['config.forcing.tropicalcyclone']}
   if [[ $TROPICALCYCLONE != "off" ]]; then
      STORM=${properties['config.forcing.tropicalcyclone.stormnumber']}
      YEAR=${properties['config.forcing.tropicalcyclone.year']}
   else
      STORM="null"
      YEAR=${CYCLE:0:4}
   fi      
fi
# get the forecast ensemble member number 
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${SCENARIODIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
# grab all config info
si=$ENMEMNUM
. ${CONFIG} #FIXME: this should not be needed 
# Bring in logging functions
. ${SCRIPTDIR}/monitoring/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
SCENARIODIR=${CYCLEDIR}/${SCENARIO}       # shorthand

# get loadProperties function
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/config.path.scriptdir\s*:\s*//p' run.properties`
source $SCRIPTDIR/properties.sh
# load run.properties file into associative array
loadProperties ./run.properties
CYCLELOG=${properties['monitoring.logging.file.cyclelog']}
SCENARIOLOG==${properties['monitoring.logging.file.scenariolog']}
allMessage "$SCENARIO: $THIS: Starting post processing."
scenarioMessage "$THIS: SCENARIO=$SCENARIO ; SCENARIODIR=$SCENARIODIR"
cd ${SCENARIODIR} 2>&1 > errmsg || warn "cycle $CYCLE: $SCENARIO: $THIS: Could not change directory to $SCENARIODIR: `cat $errmsg`"
#
#-----------------------------------------------------------------------
#     A C C U M U L A T E   M I N   /   M A X 
#------------------------------------------------------------------------
# get path to hotstart file that started this run
fromdir=${properties['asgs.path.fromdir']}
# set previous advisory number with leading zero if appropriate
# FIXME: this makes an assumption that previous advisory number is one
# less than the current one
previousAdvisory=$(printf "%02d" `expr $CYCLE - 1`)
#for file in maxele.63.nc maxinundepth.63.nc maxrs.63.nc maxvel.63.nc maxwvel.63.nc swan_HS_max.63.nc swan_TPS_max.63.nc ; do 
#   if [[ -e $file ]]; then
#      # create backup copy of the file just in case
#      cp $file backup_${file}
#      # merge nowcast min/max with current one
#      if [[ -e $fromdir/$file ]]; then
#         #${OUTPUTDIR}/collectMinMax.x --source $fromdir/$file --destination $file
#      fi
#      # merge previous min/max with current one
#      previousPath=../../$previousAdvisory/nowcast
#      if [[ -e $previousPath/$file ]]; then
#         #${OUTPUTDIR}/collectMinMax.x --source ../../$previousAdvisory/nowcast/$file --destination $file       
#      fi
#   fi
#done
#
#-----------------------------------------------------------------------
#     C R E A T E   M A X   C S V  
#------------------------------------------------------------------------
${OUTPUTDIR}/make_max_csv.sh $CONFIG $CYCLEDIR $STORM $YEAR $CYCLE $HOSTNAME $SCENARIO $CSDATE $HSTIME $GRIDFILE $OUTPUTDIR $SYSLOG >> ${SCENARIOLOG} 2>&1
csvFileName=`grep "Maximum Values Point CSV File Name" ${SCENARIODIR}/run.properties | sed 's/Maximum Values Point CSV File Name.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
#-----------------------------------------------------------------------
#     C R E A T E   C P R A   S L I D E   D E C K 
#------------------------------------------------------------------------
# ATTN: Operator must compile FigureGen with netCDF4 support in cpra_postproc.
# This also requires installation and configuration of GMT, gdal, etc
${OUTPUTDIR}/cpra_slide_deck_post.sh
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
   scenarioMessage "Corresponding 10m wind ensemble member was found."
   wind10mFound=yes
   for file in fort.72.nc fort.74.nc maxwvel.63.nc ; do
      if [[ -e $dirWind10m/$file && ! -e ./wind10m.${file} ]]; then
         scenarioMessage "$SCENARIO: $THIS: Found $dirWind10m/${file}."
         ln -s $dirWind10m/${file} ./wind10m.${file} 2>&1 > errmsg || warn "cycle $CYCLE: $SCENARIO: $THIS: Could not link to Wind10m directory: `cat $errmsg`"
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
#-------------------------------------------------------------------
#               C E R A   F I L E   P R I O R I T Y
#-------------------------------------------------------------------
# @jasonfleming: Hack in a notification email once the bare minimum files
# needed by CERA have been posted. 
#
#FILES=(`ls *.nc al${STORM}${YEAR}.fst bal${STORM}${YEAR}.dat fort.15 fort.22 CERA.tar run.properties 2>> /dev/null`)
scenarioMessage "$SCENARIO: $THIS: Creating list of files to post to opendap."
fcstFile=../al${STORM}${YEAR}.fst
bestFile=../bal${STORM}${YEAR}.dat
for file in $fcstFile $bestFile ; do
   if [[ -e $file ]]; then
      cp $file . 2>&1 > errmsg || warn "cycle $CYCLE: $SCENARIO: $THIS: Could not link to $file: `cat $errmsg`"
   fi
done
ceraNonPriorityFiles=( `ls $CONFIG $SYSLOG $CYCLELOG $SCENARIOLOG cpra.post.log $csvFileName endrisinginun.63.nc everdried.63.nc fort.64.nc fort.68.nc fort.71.nc fort.72.nc fort.73.nc initiallydry.63.nc inundationtime.63.nc maxinundepth.63.nc maxrs.63.nc maxvel.63.nc minpr.63.nc rads.64.nc swan_DIR.63.nc swan_DIR_max.63.nc swan_TMM10.63.nc swan_TMM10_max.63.nc 2>> $SCENARIOLOG` )
ceraPriorityFiles=(`ls run.properties maxele.63.nc fort.63.nc fort.61.nc fort.15 fort.22 *.jpg 2>> $SCENARIOLOG`)
if [[ $TROPICALCYCLONE = on ]]; then
   ceraPriorityFiles=( ${ceraPriorityFiles[*]} `ls al${STORM}${YEAR}.fst bal${STORM}${YEAR}.dat 2>> $SCENARIOLOG` )
fi
if [[ $WAVES = on ]]; then
   ceraPriorityFiles=( ${ceraPriorityFiles[*]} `ls swan_HS_max.63.nc swan_TPS_max.63.nc swan_HS.63.nc swan_TPS.63.nc 2>> $SCENARIOLOG` )
fi
dirWind10m=$CYCLEDIR/${SCENARIO}Wind10m
if [[ -d $dirWind10m ]]; then
   ceraPriorityFiles=( ${ceraPriorityFiles[*]} `ls wind10m.maxwvel.63.nc wind10m.fort.74.nc 2>> $SCENARIOLOG` )
   ceraNonPriorityFiles=( ${ceraNonPriorityFiles[*]} `ls maxwvel.63.nc fort.74.nc 2>> $SCENARIOLOG` )
else
   ceraPriorityFiles=( ${ceraPriorityFiles[*]} `ls maxwvel.63.nc fort.74.nc 2>> $SCENARIOLOG` )
fi
FILES=( ${ceraPriorityFiles[*]} "sendNotification" ${ceraNonPriorityFiles[*]} )
#
#-----------------------------------------------------------------------
#         O P E N  D A P    P U B L I C A T I O N 
#-----------------------------------------------------------------------
#
OPENDAPDIR=""
#
# For each opendap server in the list in ASGS config file.
primaryCount=0
for server in ${TDS[*]}; do
   allMessage "cycle $CYCLE: $SCENARIO: $THIS: Posting to $server opendap using the following command: ${OUTPUTDIR}/opendap_post.sh $CONFIG $CYCLEDIR $CYCLE $HPCENV $SCENARIO $HSTIME $SYSLOG $server \"${FILES[*]}\" $OPENDAPNOTIFY"
   ${OUTPUTDIR}/opendap_post.sh $CONFIG $CYCLEDIR $CYCLE $HPCENV $SCENARIO $HSTIME $SYSLOG $server "${FILES[*]}" $OPENDAPNOTIFY >> ${SYSLOG} 2>&1
done
