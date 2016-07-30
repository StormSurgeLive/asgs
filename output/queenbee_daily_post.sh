#!/bin/bash
#------------------------------------------------------------------------
# queenbee_daily_post.sh : Posting to opendap from queenbee. 
#------------------------------------------------------------------------
# Copyright(C) 2015,2016 Jason Fleming
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
#------------------------------------------------------------------------
# Example of invocation:
# ~/asgs/trunk/output/queenbee_daily_post.sh ~/asgs/config/asgs_config_phil_garnet_hsdrrs2014.sh /lustre/work1/jgflemin/asgs19368/38 99 2008 38 garnet.erdc.hpc.mil nhcConsensus 2008080800  2743200.000000000 HSDRRS2014_MRGO_leveeupdate_fixSTC_MX.grd  ~/asgs/trunk/output /u/jgflemin/asgs/log/asgs-2014-Apr-23-T05:33:41.19368.log ~/.ssh/id_rsa.pub
#
CONFIG=$1
ADVISDIR=$2
STORM=$3
YEAR=$4
ADVISORY=$5
HOSTNAME=$6
ENSTORM=$7
CSDATE=$8
HSTIME=$9
GRIDFILE=${10}
OUTPUTDIR=${11}
SYSLOG=${12}
SSHKEY=${13}
#
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR}
# get the forecast ensemble member number for use in CERA load balancing
# as well as picking up any bespoke configuration for this ensemble
# member in the configuration files
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
si=$ENMEMNUM
#
# grab all config info
. ${CONFIG} 
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
# write the intended audience to the run.properties file for CERA
echo "intendedAudience : $INTENDEDAUDIENCE" >> run.properties
#*********************************************
# MS river properties for CERA, these variables must be set in ASGS config
#*********************************************
echo "msRiverBoundaryType : $MSRIVERBOUNDARYTYPE" >> run.properties
echo "msRiverBoundaryCondition : $MSRIVERBOUNDARYCONDITION" >> run.properties
#echo "remark : $BONNETCARRESPILLWAYOPENPERCENT" >> run.properties
#
# write the target area to the run.properties file for the CERA
# web app
echo "asgs : ng" >> run.properties 2>> $SYSLOG
echo "enstorm : $ENSTORM" >> run.properties 2>> $SYSLOG
#
# record the sea_surface_height_above_geoid nodal attribute to the
# run.properties file
isUsed=`grep -c sea_surface_height_above_geoid fort.15`
if [[ $isUsed = 0 ]]; then
   # this nodal attribute is not being used; report this to run.properties file
   echo "sea_surface_height_above_geoid : null" >> run.properties
else
   # get the line number where the start of this nodal attribute is specified
   # in the header of the fort.13 (nodal attributes) file
   linenum=`grep --line-number --max-count 1 sea_surface_height_above_geoid fort.13 | awk 'BEGIN { FS=":" } { print $1 }'`
   # get the actual default value, which is specified three lines after the
   # the name of the nodal attribute in the fort.13 header
   datumOffsetDefaultValueLine=`expr $linenum + 3`
   datumOffsetDefaultValue=`awk -v linenum=$datumOffsetDefaultValueLine 'NR==linenum { print $0 }' fort.13`
   echo "sea_surface_height_above_geoid : $datumOffsetDefaultValue" >> run.properties
fi
#
#--------------------------------------------------------------------------
#              I N U N D A T I O N    M  A S K  
#--------------------------------------------------------------------------
# When presenting inundation data on Google Maps, the ADCIRC extent of
# initially dry area is often landward of the Google Maps shoreline, 
# resulting in the erroneous depiction of non-inundated land areas 
# seaward of inundated areas. The inundationMask program expands the 
# inundation area presented on Google Maps to cover the full land area
# as depicted by Google Maps. 
# 
if [ -e ${STORMDIR}/initiallyDry.63.nc ]; then
   if [ -e ${OUTPUTDIR}/inundationMask.x ]; then
      ${OUTPUTDIR}/inundationMask.x --filename initiallyDry.63.nc --netcdf4 --numpasses 2 2>> ${SYSLOG} 2>&1
      ERROVALUE=$?
      if [ $ERROVALUE == 0 ]; then
         echo "Inundation Mask File Name : inundationMask.63.nc" >> run.properties
         echo "Inundation Mask Format : netcdf" >> run.properties
      else
         error "Failed to create inundationMask.63.nc file."
      fi
   else
      error "The initiallyDry.63.nc file was found in $STORMDIR but the inundationMask.x executable was not found in ${OUTPUTDIR}."
   fi
fi
#
#-----------------------------------------------------------------------
#         O P E N  D A P    P U B L I C A T I O N 
#-----------------------------------------------------------------------
logMessage "Creating list of files to post to opendap."
FILES=(`ls *.nc ${ADVISDIR}/al*.fst ${ADVISDIR}/bal*.dat fort.15 fort.22 run.properties`)
#
# For each opendap server in the list in ASGS config file.
for server in ${TDS[*]}; do 
   logMessage "Posting to $server opendap with opendap_post.sh using the following command: ${OUTPUTDIR}/opendap_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server \"${FILES[*]}\" $OPENDAPNOTIFY"
   ${OUTPUTDIR}/opendap_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server "${FILES[*]}" $OPENDAPNOTIFY >> ${SYSLOG} 2>&1
done
