#!/bin/bash
#
# cera_post.sh 
# 
# Copies output from the ASGS for post processing.
# 
# Copyright(C) 2008, 2009 Jason Fleming
# Copyright(C) 2010--2013 Nate Dill
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
#
# grab all static configuration data
. ${CONFIG}
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
# env_dispatch ${TARGET}  #nld - shouldn't matter for ng cera
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
umask 002

STORMDIR=${ADVISDIR}/${ENSTORM}   # this is the ensemble member directory for this advisory 
cd $STORMDIR


#########################################################################
# get the forecast ensemble member number for use in CERA load balancing
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
# cera1, cera2, etc. We alternate the forecast ensemble members evenly 
# we expect the ASGS config file to tell us how many cera servers there
# are with CERASERVERNUM and assume they are consecutively named 
# among them
# nld - need to add NUMCERASERVERS to config 
CERASERVERNUM=`expr $ENMEMNUM % $NUMCERASERVERS + 1`
CERASERVER=cera$CERASERVERNUM
echo "ceraServer : $CERASERVER" >> run.properties
########################################################################


# copy the forecast and bestrack files to the STORMDIR
cp $ADVISDIR/al${STORM}${YEAR}.fst .
cp $ADVISDIR/bal${STORM}${YEAR}.dat .

if [ -f fort.63 ]; then
  echo "INFO: cera_post.sh: gzipping fort.63"
  gzip fort.63
fi
if [ -f fort.74 ]; then
  echo "INFO: cera_post.sh: gzipping fort.74"
   gzip fort.74
fi
if [ -f fort.64 ]; then
  echo "INFO: cera_post.sh: gzipping fort.64"
   gzip fort.64
fi
if [ -f fort.73 ]; then
  echo "INFO: cera_post.sh: gzipping fort.73"
   gzip fort.73
fi

# ng for northern gulf CERA
echo "asgs : ng" >> run.properties

# AUDIENCE  must be set in config file
echo "intendedAudience : $INTENDEDAUDIENCE" >> run.properties

#download path for scping files
DOWNLOADURL=${HOSTNAME}:$STORMDIR
echo "downloadurl : $DOWNLOADURL" >> run.properties

# use nhcConsensus, veer, windSpeed, overlandSPeed, or vMax when setting ENSTORM in configu file
#echo "track_modified : $ENSTORM" >> run.properties 
#echo "variation_veer : $PERCENT" >> run.properties

if [[ "veer" =~ $ENSTORM ]]; then
  echo "track_modified : veer" >> run.properties
  echo "variation_veer : $PERCENT" >> run.properties
fi
if [[ "windSpeed" =~ $ENSTORM ]]; then
  echo "track_modified : windSpeed" >> run.properties
  echo "variation_windSpeed : $PERCENT" >> run.properties
fi
if [[ "rMax" =~ $ENSTORM ]]; then
  echo "track_modified : rMax" >> run.properties
  echo "variation_rMax : $PERCENT" >> run.properties
fi
if [[ "overlandSpeed" =~ $ENSTORM ]]; then
  echo "track_modified : overlandSpeed" >> run.properties
  echo "variation_overlandSpeed : $PERCENT" >> run.properties
fi


#metalink=`ls ${YEAR}${STORM}${ADVISORY}?_w???o???v????r???`
#cp $metalink $POSTADVISORYDIR   #$ENSTORM
#cp $ASGSADVISORYDIR/$ENSTORM/fort.22 $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/fort.22.meta $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/fort.61 $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/maxele.63 $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$RM/maxwvel.64 $POSTADVISORYDIR
