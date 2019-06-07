#!/bin/bash
#------------------------------------------------------------------------
# cera_contour_post.sh : Runs cera_contour in parallel.
#------------------------------------------------------------------------
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
#------------------------------------------------------------------------
#
CONFIG=$1
ADVISDIR=$2
ADVISORY=$3
HOSTNAME=$4
ENSTORM=$5
HSTIME=$6
SYSLOG=$7
FILES=($8) # array of files to contour
#
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR}
# get the forecast ensemble member number for use in 
# picking up any bespoke configuration for this ensemble
# member in the configuration files
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
si=$ENMEMNUM
#
## grab all config info
. ${CONFIG} 
# Bring in logging functions
. ${SCRIPTDIR}/monitoring/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#-------------------------------------------------------------------------
#            P A R A L L E L   C E R A   C O N T O U R
#-------------------------------------------------------------------------
CERACONTOURTEMPLATE=${OUTPUTDIR}/cera_contour/ceraContourJobArray.template.slurm
TASKTYPE=cera_contour
IFSTORM="-s"
TOTALNUMTASKS=0
CONCURRENTNUMTASKS=0
CONTOURDIR=/home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring
TASKTIMELIMITMINUTES=3  
WRITESHP="-s"
FILELIST=""
mkdir $STORMDIR/CERA 2>> ${SYSLOG} 2>&1
mkdir $STORMDIR/$TASKTYPE 2>> ${SYSLOG} 2>&1
# 
# turn off storm switch if this is mundane met forcing
if [[ $BACKGROUNDMET = on ]]; then
   IFSTORM=""
fi
#
# count the number of tasks required
tasknum=1
for file in ${FILES[*]}; do 
   # if the file exists
   if [[ -e ${file} ]]; then
      # add the file to the file list
      FILELIST="$FILELIST $file"
      # determine the number of timesteps in the file
      timeStepEnd=`ncdump -h ${file} | grep currently | egrep -o '[0-9]+'`
      # increase the number of tasks according to the number of timesteps
      TOTALNUMTASKS=`expr $TOTALNUMTASKS + $timeStepEnd`
      while [[ $tasknum -le $TOTALNUMTASKS ]]; do
         LAYER=nullLayer
         fnum=`printf "%03d" $timestep`
         taskfnum=`printf "%03d" $tasknum`
         case $file in
         "fort.74.nc")
            LAYER=wvel
            ;;
         "fort.63.nc")
            LAYER=elev
            ;;
         "swan_HS.63.nc")
            LAYER=hsign
            ;;
         "swan_TPS.63.nc")
            LAYER=tps
            ;;
         "maxele.63.nc")
            LAYER=maxelev
            ;;
         "maxwvel.63.nc")
            LAYER=maxwvel
            ;;
         "swan_HS_max.63.nc")
            LAYER=maxhsign
            ;;
         "swan_TPS_max.63.nc")
            LAYER=maxtps
            ;;
         *)
            error "Files named $file are not supported."
            break
            ;;
         esac
         echo "cera_contour_post.sh: Creating task." > ./$TASKTYPE/$LAYER${taskfnum}.held
         tasknum=`expr $tasknum + 1`
     done
   fi
done
# if there are any tasks fill in the template and submit
if [[ $TOTALNUMTASKS -ne 0 ]]; then
   CONCURRENTNUMTASKS=$TOTALNUMTASKS  # default to submitting them all at once
   cp $CERACONTOURTEMPLATE . 2>> $SYSLOG 2>&1 
   # use sed script to substitute the values
   sed "s/%TASKTYPE%/$TASKTYPE/g
        s/%TOTALNUMTASKS%/$TOTALNUMTASKS/g
        s/%CONCURRENTNUMTASKS%/$CONCURRENTNUMTASKS/g
        s/%FILELIST%/$FILELIST/g
        s/%IFSTORM%/$IFSTORM/g 
        s/%WRITESHP%/$WRITESHP/g 
        s;%STORMDIR%;$STORMDIR;g
        s;%CONTOURDIR%;$CONTOURDIR;g
        s/%TASKTIMELIMITMINUTES%/$TASKTIMELIMITMINUTES/g" \
        $CERACONTOURTEMPLATE > cera_contour.slurm
   # fire and forget   
   sbatch cera_contour.slurm
else
   logMessage "There were no files or time steps to contour."
fi
