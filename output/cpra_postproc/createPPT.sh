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
batchJOBTYPE=cpra.figuregen
postJOBTYPE=cpra.post
#
#
#--------------------------------------------------------------------------
#       GATHER PROPERTIES
#--------------------------------------------------------------------------
# STORMDIR: path where this ensemble member is supposed to run 
STORMDIR=`sed -n 's/[ ^]*$//;s/asgs.path.stormdir\s*:\s*//p' run.properties`
LOGFILE=${STORMDIR}/${postJOBTYPE}.log
# ensemble member name
ENSTORM=`sed -n 's/[ ^]*$//;s/asgs.enstorm\s*:\s*//p' run.properties`
echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Creating pptx." >> $LOGFILE
#
POSTPROCDIR=`sed -n 's/[ ^]*$//;s/post.path.cpra.post.postprocdir\s*:\s*//p' run.properties`
fname=`sed -n 's/[ ^]*$//;s/post.file.cpra.figuregen.maxele.fname\s*:\s*//p' run.properties`
coldStartTime=`sed -n 's/[ ^]*$//;s/ColdStartTime\s*:\s*//p' run.properties`
# Parse run.properties to get storm name and ensemble
storm=`sed -n '0,/stormname/{s/[ ^]*$//;s/stormname\s*:\s*//p}' run.properties`
enstorm=`sed -n 's/[ ^]*$//;s/asgs.enstorm\s*:\s*//p' run.properties`
# Parse run.properties to get advisory
advisory=`sed -n 's/[ ^]*$//;s/advisory\s*:\s*//p' run.properties`
# Parse run.properties to get forecast advisory start time
forecastValidStart=`sed -n 's/[ ^]*$//;s/forecastValidStart\s*:\s*//p' run.properties`
# get mesh file name
grid=`sed -n 's/[ ^]*$//;s/adcirc.gridname\s*:\s*//p' run.properties`
# HPCENVSHORT: shorthand for the HPC environment: queenbee, hatteras, etc
HPCENVSHORT=`sed -n 's/[ ^]*$//;s/hpc.hpcenvshort\s*:\s*//p' run.properties`
#
# create strings to represent time in UTC and CDT
coldStartTimeUTC="${coldStartTime:0:8} ${coldStartTime:8:4} UTC"
coldStartTimeCDT=$(TZ="America/Chicago" date -d "${coldStartTimeUTC}" "+%Y-%m-%d %H:%M:%S")
echo "time.coldstart.cdt : $coldStartTimeCDT" >> run.properties
forecastValidStartUTC="${forecastValidStart:0:8} ${forecastValidStart:8:4} UTC"
forecastValidStartCDT=$(TZ="America/Chicago" date -d "${forecastValidStartUTC}" "+%Y%m%d%H%M%S")
echo "time.forecast.valid.cdt : $forecastValidStartCDT" >> run.properties
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#      WRITE PROPERTIES FOR MATLAB 
#--------------------------------------------------------------------------
#echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Writing properties for matlab." >> $LOGFILE
#oFile=cpraHydro.info
#echo $storm > $oFile
#echo $enstorm >> $oFile
#echo $grid >> $oFile
#echo $forecastValidStartCDT >> $oFile
#echo $coldStartTimeCDT >> $oFile
#echo $advisory >> $oFile
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
echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Executing matlab." >> $LOGFILE
PLOTCMD=""
case $HPCENVSHORT in
    queenbee)
        PLOTCMD="${POSTPROCDIR}/Matlab_QB2/run_plot_usace_adcirc.sh /usr/local/packages/license/matlab/r2017a"
        ;;
    hatteras)
        PLOTCMD='matlab -nodisplay -nosplash -nodesktop -r "run plot_usace_adcirc.m, exit"'
        ;;
    *)
        error "HPC platform $HPCENVSHORT not recognized."
        ;;
esac
eval "$PLOTCMD" >> $LOGFILE 2>&1
echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Finished executing matlab." >> $LOGFILE
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       WAIT UNTIL FIGUREGEN IMAGE(S) ARE FINISHED
#--------------------------------------------------------------------------
# If there is a maxele.63.nc, wait until associated FigureGen job is complete
if [[ -f maxele.63.nc ]]; then
   echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Checking to see if FigureGen job is complete." >> $LOGFILE
   STARTPOST=`sed -n 's/[ ^]*$//;s/time.hpc.job.cpra.figuregen.*.start\s*:\s*//p' run.properties`  
   if [[ ! -z $STARTPOST ]]; then
      waitSeconds=0
      until [[ $waitSeconds -gt 300 ]]; do 
         FINISHPOST=""
         FINISHPOST=`sed -n 's/[ ^]*$//;s/time.hpc.job.cpra.figuregen.*.finish\s*:\s*//p' run.properties`            
         if [[ $FINISHPOST != "" ]]; then
            echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: FigureGen maxele job finished." >> $LOGFILE
            break
         fi
         ERRORPOST=""
         ERRORPOST=`sed -n 's/[ ^]*$//;s/time.hpc.job.cpra.figuregen.*.error\s*:\s*//p' run.properties`            
         if [[ $ERRORPOST != "" ]]; then
            echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: FigureGen maxele job exited with an error." >> $LOGFILE
            break
         fi
         echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Waiting for FigureGen maxele job to complete." >> $LOGFILE
         sleep 10
         waitSeconds=`expr $waitSeconds + 10`
      done
   fi
fi
#--------------------------------------------------------------------------
#       RUN PYTHON SCRIPT TO GENERATE PPT SLIDE DECK
#--------------------------------------------------------------------------
echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Building pptx with buildPPT.py." >> $LOGFILE
cp ${POSTPROCDIR}/LSU_template.pptx ${STORMDIR}
python ${POSTPROCDIR}/buildPPT.py ${fname}
#rm LSU_template.pptx
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       E-MAIL PPT AS ATTACHMENT
#--------------------------------------------------------------------------
echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Sending email." >> $LOGFILE
#emailList='mbilsk3@lsu.edu matt.bilskie@gmail.com jason.fleming@seahorsecoastal.com ckaiser@cct.lsu.edu'
#emailList='mbilsk3@lsu.edu'
emailList='jason.fleming@scimaritan.org'
subjectLine="$storm Advisory $advisory PPT"
message="This is an automated message from the ADCIRC Surge Guidance System (ASGS).
New results are attached for STORM $storm ADVISORY $advisory issued on $forecastValidStartCDT CDT"
# double quotes because the file name may have a space in it
attachFile="$(cat pptFile.temp)" 
# 
# now set the email list based on the HPC platform ... queenbee is the primary
# for CPRA while stampede, hatteras, and lonestar are backup HPC machines
# 
# the ASGS on the primary HPC should be set to email clients directly while
# backup machines should only email their results to ASGS Operators who can
# forward the slide deck if the primary HPC has failed ... this prevents
# clients being bombarded with redundant successful slide decks from 
# primary and multiple backup ASGSes ... however all Operators will get
# slide decks from all ASGSes on every ensemble member of every advisory ...
case $HPCENVSHORT in
   queenbee)
      emailList='jason.fleming@scimaritan.org mbilsk3@lsu.edu nathan.dill@ransomenv.com ckaiser@cct.lsu.edu shagen@lsu.edu rtwilley@lsu.edu Ignacio.Harrouch@la.gov rick_luettich@unc.edu Billy.Wall@la.gov Stephen.Amato@la.gov Heath.E.Jones@usace.army.mil Maxwell.E.Agnew@usace.army.mil David.A.Ramirez@usace.army.mil'
      #emailList='jason.fleming@scimaritan.org mbilsk3@lsu.edu nathan.dill@ransomenv.com'
      ;;
   hatteras|stampede|lonestar)
      emailList='jason.fleming@scimaritan.org mbilsk3@lsu.edu nathan.dill@ransomenv.com'
      ;;
   *)
      error "HPC platform $HPCENVSHORT not recognized."
      ;;
esac
echo "$message" | mail -s "$subjectLine" -a "$attachFile" $emailList
#--------------------------------------------------------------------------
#
#--------------------------------------------------------------------------
#
#       E-MAIL FIGUREGEN TO OPERATORS IN CASE MANUAL PPT PRODUCTION
#       IS NEEDED
#--------------------------------------------------------------------------
emailList='mbilsk3@lsu.edu'
subjectLine="$storm Advisory $advisory FigureGen"
message="This is an automated message from the ADCIRC Surge Guidance System (ASGS).
FigureGen results are attached for STORM $storm ADVISORY $advisory issued on $forecastValidStartCDT CDT"
attachFile=$fname
#--------------------------------------------------------------------------
#       CLEAN UP
#--------------------------------------------------------------------------
#rm cpraHydro.info
#rm pptFile.temp
#--------------------------------------------------------------------------
