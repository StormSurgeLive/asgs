#!/bin/bash
#-----------------------------------------------------------------------
# wind10m_post.sh : Post processing to generate CERA contours for 10m 
# winds. 
#-----------------------------------------------------------------------
# Copyright(C) 2017 Jason Fleming
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
# example invokation:
# grep -i cold run.properties
# grep -i hot run.properties
# bash ~/asgs/2014stable/output/wind10m_post.sh ~/asgs/2014stable/config/2017/asgs_config_nam_swan_hatteras_hsofs.sh /projects/ncfs/data/asgs19797/2017072706 99 2017 2017072706 hatteras.renci.org namforecastWind10m 2017061700 3477600.0 fort.14 ~/asgs/2014stable/output stuff.log ~/.ssh/id_rsa.pub
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
cd ${STORMDIR} 2>> ${SYSLOG}
THIS=$(basename -- $0)
# get the forecast ensemble member number 
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
# grab all config info
si=$ENMEMNUM
. ${CONFIG}
# Bring in logging functions
. ${SCRIPTDIR}/monitoring/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
# write the intended audience to the run.properties file for CERA
echo "intendedAudience : $INTENDEDAUDIENCE" >> run.properties
#
#-----------------------------------------------------------------------
#          G E N E R A T E   C E R A   C O N T O U R S     
#-----------------------------------------------------------------------
# list the files and time steps that should be contoured
FILES="maxwvel.63.nc fort.74.nc"
# pass these to the cera_contour_post.sh script
${OUTPUTDIR}/cera_contour_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $SERVER "$FILES"
#
# no need to monitor these, they will be passed along if they are complete
# according to the post processing for the ensemble member that they match
