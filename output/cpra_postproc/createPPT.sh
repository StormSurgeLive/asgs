#!/bin/bash
#--------------------------------------------------------------------------
# createPPT.sh 
#--------------------------------------------------------------------------
# Workhorse script to call Matlab and generate hydrograph images,
# build final PPT slide deck, and email slide deck as attachment.
#--------------------------------------------------------------------------
# 
# Copyright(C) 2018 Matthew V Bilskie
# Copyright(C) 2018 Jason Fleming
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
#--------------------------------------------------------------------------
THIS="cpra_postproc/createPPT.sh"
#
#
#--------------------------------------------------------------------------
#       GATHER PROPERTIES
#--------------------------------------------------------------------------
POSTPROCDIR=`sed -n 's/[ ^]*$//;s/post.path.cpra.post.postprocdir\s*:\s*//p' run.properties`
STORMDIR=`sed -n 's/[ ^]*$//;s/asgs.path.stormdir\s*:\s*//p' run.properties`
fname=`sed -n 's/[ ^]*$//;s/post.file.cpra.post.maxele.fname\s*:\s*//p' run.properties`
coldStartTime=`sed -n 's/[ ^]*$//;s/ColdStartTime\s*:\s*//p' run.properties`
# Parse run.properties to get storm name and ensemble
storm=`sed -n 's/[ ^]*$//;s/storm class\s*:\s*//p' run.properties`
enstorm=`sed -n 's/[ ^]*$//;s/asgs.enstorm\s*:\s*//p' run.properties`
# Parse run.properties to get advisory
advisory=`sed -n 's/[ ^]*$//;s/advisory\s*:\s*//p' run.properties`
# Parse run.properties to get forecast advisory start time
forecastValidStart=`sed -n 's/[ ^]*$//;s/forecastValidStart\s*:\s*//p' run.properties`
# get mesh file name
grid=`sed -n 's/[ ^]*$//;s/adcirc.gridname\s*:\s*//p' run.properties`
#
# create strings to represent time in UTC and CDT
coldStartTimeUTC="${coldStartTime:0:8} ${coldStartTime:8:4} UTC"
coldStartTimeCDT=$(TZ="America/Chicago" date -d "${coldStartTimeUTC}" "+%Y-%m-%d %H:%M:%S")
forecastValidStartUTC="${forecastValidStart:0:8} ${forecastValidStart:8:4} UTC"
forecastValidStartCDT=$(TZ="America/Chicago" date -d "${forecastValidStartUTC}" "+%Y%m%d%H%M%S")
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#      WRITE PROPERTIES FOR MATLAB 
#--------------------------------------------------------------------------
oFile=cpraHydro.info
echo $storm > $oFile
echo $enstorm > $oFile
echo $grid >> $oFile
echo $forecastValidStartCDT >> $oFile
echo $coldStartTimeCDT >> $oFile
echo $advisory >> $oFile
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       SET MATLABPATH TO POINT TO MATLAB SCRIPTS
#--------------------------------------------------------------------------
export MATLABPATH=${POSTPROCDIR}/
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       RUN MATLAB SCRIPT TO GENERATE HYDROGRAPH IMAGES
#--------------------------------------------------------------------------
matlab -nodisplay -nosplash -nodesktop -r "run plot_usace_adcirc.m, exit"
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       WAIT UNTIL FIGUREGEN IMAGE(S) ARE FINISHED
#--------------------------------------------------------------------------
# If there is a maxele.63.nc, wait until associated FigureGen job is complete
if [[ -f maxele.63.nc ]]; then
   STARTPOST=`sed -n 's/[ ^]*$//;s/cpra.post.${storm}.start\s*:\s*//p' run.properties`  
   if [[ ! -z $STARTPOST ]]; then
      until [[ -f ${STORMDIR}/cpra.post.${storm}.run.finish || -f ${STORMDIR}/cpra.post.${storm}.run.error ]]; do
         echo "THIS: INFO: Waiting for FigureGen maxele job to complete."
         sleep 5
      done
   fi
fi
#--------------------------------------------------------------------------
#       RUN PYTHON SCRIPT TO GENERATE PPT SLIDE DECK
#--------------------------------------------------------------------------
cp ${POSTPROCDIR}/LSU_template.pptx ${STORMDIR}
python ${POSTPROCDIR}/buildPPT.py ${fname}
#rm LSU_template.pptx
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       E-MAIL PPT AS ATTACHMENT
#--------------------------------------------------------------------------
#emailList='mbilsk3@lsu.edu matt.bilskie@gmail.com jason.fleming@seahorsecoastal.com ckaiser@cct.lsu.edu'
#emailList='mbilsk3@lsu.edu'
emailList='jason.fleming@seahorsecoastal.com'
subjectLine="$storm Advisory $advisory PPT"
message="This is an automated message from the ADCIRC Surge Guidance System (ASGS).
New results are attached for STORM $storm ADVISORY $advisory issued on $forecastValidStartCDT CDT"
attachFile=$(cat pptFile.temp)
echo "$message" | mail -s "$subjectLine" -a "$attachFile" $emailList
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       CLEAN UP
#--------------------------------------------------------------------------
#rm cpraHydro.info
#rm pptFile.temp
#--------------------------------------------------------------------------
