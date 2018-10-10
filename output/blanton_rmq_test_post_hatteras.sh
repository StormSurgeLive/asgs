#!/bin/bash
#
# Copyright(C) 2012 Jason Fleming
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
# grab all config info
si=-1
. ${CONFIG}
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
echo "asgs : nc" >> run.properties
#

#
# make the directory where the results will be posted
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
STORMNAME=null
if [[ $BACKGROUNDMET = on ]]; then
   # the NAM cycle time is the last two digits of the "advisory"
   namcyclehour=${ADVISORY:8:2}
   STORMNAME="NAM${namcyclehour}Z"
fi
if [[ $TROPICALCYCLONE = on ]]; then
   STORMNAME=`grep "storm name" ${STORMDIR}/run.properties | sed 's/storm name.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
fi
STORMNAMEPATH=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
#
# make opendap directory
OPENDAPBASEDIR=/projects/ees/DataLayers/asgs
OPENDAPDIR=${OPENDAPBASEDIR}/tc/$STORMNAME/$ADVISORY/$GRIDNAME/$HOSTNAME/$INSTANCENAME/$ENSTORM
mkdir -p $OPENDAPDIR 2>> ${SYSLOG}
cd $OPENDAPDIR 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/fort.14 .  2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/fort.15 . 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/fort.22 . 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/fort.*.nc . 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/max*.nc . 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/min*.nc . 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/run.properties . 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/*.xmf . 2>> ${SYSLOG}
#
cp  ${ADVISDIR}/${ENSTORM}/fort.14 .  2>> ${SYSLOG}
cp  ${ADVISDIR}/${ENSTORM}/fort.15 . 2>> ${SYSLOG}
cp  ${ADVISDIR}/${ENSTORM}/fort.22 . 2>> ${SYSLOG}
cp  ${ADVISDIR}/${ENSTORM}/fort.*.nc . 2>> ${SYSLOG}
cp  ${ADVISDIR}/${ENSTORM}/max*.nc . 2>> ${SYSLOG}
cp  ${ADVISDIR}/${ENSTORM}/min*.nc . 2>> ${SYSLOG}
cp  ${ADVISDIR}/${ENSTORM}/run.properties . 2>> ${SYSLOG}
cp  ${ADVISDIR}/${ENSTORM}/*.xmf . 2>> ${SYSLOG}
#

