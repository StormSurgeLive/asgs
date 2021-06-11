#!/bin/bash
#--------------------------------------------------------------------------
# createPPT.sh 
#--------------------------------------------------------------------------
# Workhorse script to call Matlab and generate hydrograph images,
# build final PPT slide deck, and email slide deck as attachment.
#--------------------------------------------------------------------------
# 
# Copyright(C) 2018--2019 Matthew V Bilskie
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
#
#--------------------------------------------------------------------------
THIS="output/cpra_postproc/createPPT.sh"
batchJOBTYPE=cpra.figuregen
postJOBTYPE=cpra.post
#
#--------------------------------------------------------------------------
#       GATHER PROPERTIES
#--------------------------------------------------------------------------
# SCRIPTDIR: path to asgs scripts like asgs_main.sh; assumes $PWD is same as $SCENARIODIR
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' run.properties` 
. ${SCRIPTDIR}/monitoring/logging.sh
. ${SCRIPTDIR}/platforms.sh           # contains hpc platform configurations
. ${SCRIPTDIR}/properties.sh          # contains loadProperties subroutine
# load properties
declare -A properties
loadProperties run.properties # assumes $PWD is same as $SCENARIODIR
SCENARIODIR=${properties["path.scenariodir"]}
LOGFILE=${SCENARIODIR}/${postJOBTYPE}.log
SYSLOG=${properties["monitoring.logging.file.syslog"]}
CYCLELOG=${properties["monitoring.logging.file.cyclelog"]}
SCENARIOLOG=${properties["monitoring.logging.file.scenariolog"]}
SCENARIO=${properties["scenario"]}
scenarioMessage "$SCENARIO: $THIS: Creating pptx." $LOGFILE
POSTPROCDIR=${properties["post.path.cpra.post.postprocdir"]}
fname=${properties["post.file.cpra.figuregen.maxele.fname"]}
coldStartTime=${properties["adcirc.time.coldstartdate"]}
TROPCIALCYCLONE=${properties["forcing.tropicalcyclone"]} 
if [[ $TROPICALCYCLONE = "on" ]]; then
   # Parse run.properties to get storm name and ensemble
   stormname=${properties["forcing.tropicalcyclone.stormname"]}
else
   stormname=NAM #FIXME: make this more general
fi
advisory=${properties["advisory"]}
forecastValidStart=${properties["forecastValidStart"]}
grid=${properties["adcirc.gridname"]}
HPCENVSHORT=${properties["hpc.hpcenvshort"]}
# pull in platform-specific configuration (specifically MCRROOT and MATLABEXE)
env_dispatch $HPCENVSHORT
THIS="output/cpra_postproc/createPPT.sh" # must reset after env_dispatch()
#
echo "post.path.cpra.post.mcrroot : $MCRROOT" >> $SCENARIODIR/run.properties
echo "post.cpra.post.matlabexe : $MATLABEXE" >> $SCENARIODIR/run.properties
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
#echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $SCENARIO: $THIS: Writing properties for matlab." >> $LOGFILE
#oFile=cpraHydro.info
#echo $stormname > $oFile
#echo $SCENARIO >> $oFile
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
scenarioMessage "$SCENARIO: $THIS: Executing matlab." $LOGFILE
PLOTCMD=""
if [[ $MATLABEXE = "mex" ]]; then
   # MCRROOT is set in platforms.sh
   MEXFILE="${POSTPROCDIR}/MEX/cpra_hydrograph_plotter_${HPCENVSHORT}.mex"
   PLOTCMD="${POSTPROCDIR}/MEX/run_mex.sh $MCRROOT $MEXFILE"
else
   PLOTCMD='matlab -nodisplay -nosplash -nodesktop -r "run cpra_hydrograph_plotter.m, exit"'
fi
eval "$PLOTCMD" 2>&1 | tee -a $LOGFILE >> $SCENARIOLOG
scenarioMessage "$SCENARIO: $THIS: Finished executing matlab." $LOGFILE
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       WAIT UNTIL FIGUREGEN IMAGE(S) ARE FINISHED
#--------------------------------------------------------------------------
# If there is a maxele.63.nc, wait until associated FigureGen job is complete
if [[ -f maxele.63.nc ]]; then
   scenarioMessage "$SCENARIO: $THIS: Checking to see if FigureGen job is complete." $LOGFILE
   loadProperties $SCENARIODIR/run.properties 
   STARTPOST=${properties["time.hpc.job.cpra.figuregen.start"]}  
   if [[ ! -z $STARTPOST ]]; then
      waitSeconds=0
      until [[ $waitSeconds -gt 300 ]]; do 
         FINISHPOST=""
         FINISHPOST=${properties["time.hpc.job.cpra.figuregen.finish"]}            
         if [[ $FINISHPOST != "" ]]; then
            scenarioMessage "$SCENARIO: $THIS: FigureGen maxele job finished." $LOGFILE
            break
         fi
         ERRORPOST=""
         ERRORPOST=${properties["time.hpc.job.cpra.figuregen.error"]}            
         if [[ $ERRORPOST != "" ]]; then
            warn "$SCENARIO: $THIS: FigureGen maxele job exited with an error." $LOGFILE
            break
         fi
         scenarioMessage "$SCENARIO: $THIS: Waiting for FigureGen maxele job to complete." $LOGFILE
         sleep 10
         waitSeconds=`expr $waitSeconds + 10`
      done
   fi
fi
#--------------------------------------------------------------------------
#       RUN PYTHON SCRIPT TO GENERATE PPT SLIDE DECK
#--------------------------------------------------------------------------
scenarioMessage "$SCENARIO: $THIS: Building pptx with buildPPT.py." $LOGFILE
cp ${POSTPROCDIR}/LSU_template.pptx ${SCENARIODIR} > errmsg 2>&1 || warn "cycle $advisory: $SCENARIO: $THIS: Could not copy ${POSTPROCDIR}/LSU_template.pptx to '$SCENARIODIR': `cat errmsg`." $LOGFILE
python ${POSTPROCDIR}/buildPPT.py ${fname} 2>&1 | tee -a $LOGFILE >> $SCENARIOLOG
#rm LSU_template.pptx
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       E-MAIL PPT AS ATTACHMENT
#--------------------------------------------------------------------------
scenarioMessage "$SCENARIO: $THIS: Sending email." $LOGFILE
#emailList='mbilsk3@lsu.edu matt.bilskie@gmail.com jason.fleming@seahorsecoastal.com ckaiser@cct.lsu.edu'
#emailList='mbilsk3@lsu.edu'
emailList='jason.fleming@scimaritan.org'
if [[ $TROPICALCYCLONE != "off" ]]; then
   subjectLine="$stormname Advisory $advisory PPT"
   message="This is an automated message from the ADCIRC Surge Guidance System (ASGS).
New results are attached for STORM $stormname ADVISORY $advisory issued on $forecastValidStartCDT CDT"
else
   subjectLine="$stormname Cycle $advisory PPT"
   message="This is an automated message from the ADCIRC Surge Guidance System (ASGS).
New results are attached for $stormname Cycle $advisory issued on $forecastValidStartCDT CDT"
fi
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
#emailList='jason.fleming@scimaritan.org mbilsk3@lsu.edu'
#emailList='jason.fleming@scimaritan.org mbilsk3@lsu.edu rluettich1@gmail.com'
emailList='jason.fleming@seahorsecoastal.com'
#
#case $HPCENVSHORT in
#   queenbee)
      #emailList='jason.fleming@scimaritan.org mbilsk3@lsu.edu nathan.dill@ransomenv.com ckaiser@cct.lsu.edu shagen@lsu.edu rtwilley@lsu.edu Ignacio.Harrouch@la.gov rick_luettich@unc.edu Billy.Wall@la.gov Stephen.Amato@la.gov Heath.E.Jones@usace.army.mil Maxwell.E.Agnew@usace.army.mil David.A.Ramirez@usace.army.mil'
      #emailList='jason.fleming@scimaritan.org mbilsk3@lsu.edu nathan.dill@ransomenv.com ckaiser@cct.lsu.edu shagen@lsu.edu rtwilley@lsu.edu rick_luettich@unc.edu'
#      emailList='jason.fleming@scimaritan.org mbilsk3@lsu.edu'
#      ;;
#   hatteras|stampede|lonestar)
#      emailList='jason.fleming@scimaritan.org mbilsk3@lsu.edu'
#      ;;
#   *)
#      error "HPC platform $HPCENVSHORT not recognized."
#      ;;
#esac
# load asgs operator email address for the reply-to field
ASGSADMIN=${properties["notification.email.asgsadmin"]}
echo "$message" | mail -S "replyto=$ASGSADMIN" -s "$subjectLine" -a "$attachFile" $emailList > errmsg 2>&1 || warn "cycle $advisory: $SCENARIO: $THIS: Failed to send CPRA slide deck email to $emailList: `cat errmsg`." $LOGFILE
#--------------------------------------------------------------------------
#
#--------------------------------------------------------------------------
#
#       E-MAIL FIGUREGEN TO OPERATORS IN CASE MANUAL PPT PRODUCTION
#       IS NEEDED
#--------------------------------------------------------------------------
#emailList='mbilsk3@lsu.edu'
emailList='jason.fleming@seahorsecoastal.com'
subjectLine="$stormname Advisory $advisory FigureGen"
message="This is an automated message from the ADCIRC Surge Guidance System (ASGS).
FigureGen results are attached for STORM $stormname ADVISORY $advisory issued on $forecastValidStartCDT CDT"
attachFile=$fname
#--------------------------------------------------------------------------
#       CLEAN UP
#--------------------------------------------------------------------------
#rm cpraHydro.info
#rm pptFile.temp
#--------------------------------------------------------------------------
