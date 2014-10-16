#!/bin/bash
#------------------------------------------------------------------------
# Copyright(C) 2014 Jason Fleming
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
# get the forecast ensemble member number 
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
# grab all config info
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
# write the target area to the run.properties file for the CERA
# web app
echo "asgs : qatest" >> run.properties 2>> $SYSLOG
echo "enstorm : $ENSTORM" >> run.properties 2>> $SYSLOG
#
# get the "gold" dataset
#mkdir gold 2>> ${SYSLOG}
#curl http://www.unc.edu/ims/adcirc/test_cases/quarter_annular/parallel_quarter_annular_v50_99.tar > ./gold/parallel_quarter_annular_v50_99.tar 2>> curl.log
#cd gold 2>> ${SYSLOG}
#tar xvf parallel_quarter_annular_v50_99.tar 2>> ${SYSLOG}
#rm parallel_quarter_annular_v50_99.tar 2>> ${SYSLOG}
#bunzip2 *.bz2 2>> ${SYSLOG}
#diff fort.63 ../fort.63 > fort.63.diff 2>> ${SYSLOG}
#
# TODO: Fix availability/acquisition of gold data ... for now, I've
# manually created the gold data set using the latest v50release of serial
# ADCIRC. And hardwired its location on my desktop machine into this
# script.
GOLDDIR=/srv/asgs/qatests.gold/99999/nowriters 
#
# use the ADCIRC compare tool that is included with ADCIRC
RELERR=1e-6     # set the relative error threshold for passing the test
ABSERR=1e-6   # set the absolute error threshold for passing the test
if [[ -e fort.63 || -e fort.63.nc ]]; then
   if [[ -e fort.63.nc ]]; then
      ${OUTPUTDIR}/netcdf2adcirc.x --datafile fort.63.nc 2>> ${SYSLOG}
   fi
   ${ADCIRCDIR}/adccmp ${GOLDDIR} . ETA2 $RELERR $ABSERR > ETA2.results
   if [[ -e ETA2.results ]]; then
      RESULT=`awk '$9=="failed" { print $9 ; exit }' ETA2.results 2>> ${SYSLOG}`
      if [[ $RESULT = failed ]]; then 
         echo "ERROR: quarter annulus $ENSTORM ETA2 test FAILED." >> ${SYSLOG}
      else 
         echo "INFO: quarter annulus $ENSTORM ETA2 test PASSED." >> ${SYSLOG}
      fi
   else
      echo "ERROR: Failed to run $ENSTORM ETA2 test." >> ${SYSLOG} 
   fi
else
   echo "ERROR: The run did not produce a fort.63 or fort.63.nc file. ETA2 test FAILED." >> ${SYSLOG} 
fi
