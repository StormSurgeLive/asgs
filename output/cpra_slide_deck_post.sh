#!/bin/bash
#--------------------------------------------------------------------------
# cpra_slide_deck_post.sh 
#--------------------------------------------------------------------------
# Driver script for building CPRA hydrograph slide deck.
#--------------------------------------------------------------------------
# 
# Copyright(C) 2018 Matthew V Bilskie
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
#                       R E A D  M E
#--------------------------------------------------------------------------
# This script assumes that it is executed in the same directory with
# the results it is post processing.
#
# FigureGen.F90 must be compiled in the cpra_postproc subdirectory 
# with netCDF4 support and all dependencies (GMT, gdal, etc) must be
# installed and configured in order for this script to function. 
#--------------------------------------------------------------------------
#                        S E T U P 
#--------------------------------------------------------------------------
umask 002
THIS=$(basename -- $0)
batchJOBTYPE=cpra.figuregen
postJOBTYPE=cpra.post
# SCENARIODIR: path where this ensemble member is supposed to run 
SCENARIODIR=$PWD  # default to post processing files in the current directory
# if there is only one command line argument, it is the directory where
# the files are that should be post processed
currentDir=$PWD # save in case we need to switch back at the end of the script 
if [[ $# -eq 1 ]]; then
   SCENARIODIR=$1 # only 1 cmd line option means we are given the scenario directory
fi
# SCRIPTDIR: path to asgs scripts like asgs_main.sh
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' run.properties`

. ${SCRIPTDIR}/monitoring/logging.sh
. ${SCRIPTDIR}/platforms.sh           # contains hpc platform configurations
. ${SCRIPTDIR}/properties.sh          # contains loadProperties subroutine
# load properties
declare -A properties
loadProperties $SCENARIODIR/run.properties
#
CYCLE=${properties['advisory']}
SCENARIO=${properties['scenario']}
SCENARIOLOG=${properties["monitoring.logging.file.scenariolog"]}
CYCLELOG=${properties["monitoring.logging.file.cyclelog"]}
SYSLOG=${properties["monitoring.logging.file.syslog"]}
LOGFILE=${SCENARIODIR}/${postJOBTYPE}.log
scenarioMessage "DEBUG: SCRIPTDIR is '${SCRIPTDIR}'." $LOGFILE 
cd $SCENARIODIR > errmsg 2>&1 || warn "cycle $CYCLE: $SCENARIO: $THIS: Failed to change directory to '$SCENARIODIR': `cat errmsg`." 
touch $LOGFILE 2>> $SCENARIOLOG
scenarioMessage "$SCENARIO: $THIS: Starting $postJOBTYPE post processing in ${SCENARIODIR}." $LOGFILE
#
# HPCENVSHORT: shorthand for the HPC environment: queenbee, hatteras, etc
HPCENVSHORT=${properties['hpc.hpcenvshort']}
scenarioMessage "$SCENARIO: $THIS: Setting up for execution on ${HPCENVSHORT}." $LOGFILE 
# set variables as defaults according to the hpc platform
env_dispatch $HPCENVSHORT
THIS="output/cpra_slide_deck_post.sh" # must reset after executing env_dispatch()
#
# now pick up properties related to this particular scenario
#
# ACCOUNT, SERQUEUE, QUEUENAME: by default, use whatever account was used by adcprep, 
# padcirc or padcswan for this post processing 
ACCOUNT=${properties['hpc.job.default.account']}
QUEUENAME=${properties['hpc.job.default.queuename']}
SERQUEUE=${properties['hpc.job.default.serqueue']}
PPN=1
# use the same ppn and account as was used in adcprep
for key in "${!properties[@]}" ; do
   if [[ $key == hpc.job.prep*.ppn ]]; then
      PPN=${properties[$key]}
   fi
done
#
# check to see if this is a tropical cyclone
TROPICALCYCLONE=${properties['forcing.tropicalcyclone']}
if [[ $TROPICALCYCLONE != "off" ]]; then
   STORM=${properties['forcing.tropicalcyclone.stormnumber']}
   YEAR=${properties['forcing.tropicalcyclone.year']}
   STORMNAME=${properties['forcing.tropicalcyclone.stormname']}
else
   STORM="null"
   YEAR=${ADVISORY:0:4}
   STORMNAME=NAM  #FIXME: make this more general
fi
# cycle ID
ADVISORY=${properties['advisory']}
#
POSTPROCDIR=${SCRIPTDIR}/output/cpra_postproc
echo "post.path.${postJOBTYPE}.postprocdir : $POSTPROCDIR" >> $SCENARIODIR/run.properties
#
echo "hpc.job.${batchJOBTYPE}.serqueue : $SERQUEUE" >> $SCENARIODIR/run.properties
echo "hpc.job.${batchJOBTYPE}.queuename : $QUEUENAME" >> $SCENARIODIR/run.properties
echo "hpc.job.${batchJOBTYPE}.account : $ACCOUNT" >> $SCENARIODIR/run.properties
echo "hpc.job.${batchJOBTYPE}.ppn : $PPN" >> $SCENARIODIR/run.properties
#
# set command for finding max water surface elevation (ft) within a lat/lon box
export MATLABPATH=${POSTPROCDIR}
# queenbee, supermic
if [[ $MATLABEXE = "mex" ]]; then
   FINDMAXZCMD="${POSTPROCDIR}/MEX/run_mex.sh $MCRROOT ${POSTPROCDIR}/MEX/FindMaxZ_${HPCENVSHORT}.mex"
else
   # hatteras, stampede2, lonestar5
   FINDMAXZCMD="(matlab -nodisplay -nosplash -nodesktop -r 'run FindMaxZ.m, exit')"
fi
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#                    F I G U R E   G E N  
#--------------------------------------------------------------------------
if [[ -f maxele.63.nc ]]; then
    scenarioMessage "$SCENARIO: $THIS: Building FigureGen job." $LOGFILE
    # Create storm track from fort.22 file
    awk 'BEGIN { FS="," } { printf "-%0.2f  %0.2f\n", $8/10.0, $7/10.0 }' fort.22 > fort.22.trk 2>> $LOGFILE
    # copy in color palette
    cp ${POSTPROCDIR}/Default2.pal ${SCENARIODIR}/ 2>> $LOGFILE
    cp ${POSTPROCDIR}/FG51_SELA_maxele.inp.template ${SCENARIODIR}/FG51_SELA_maxele.inp 2>> $LOGFILE
    # fill in figuregen template file with values for this plot
    fname="LA_SELA_${STORMNAME}_${ADVISORY}_${SCENARIO}_maxele_"
    sed -i "s/%FileName%/${fname}/g" FG51_SELA_maxele.inp 2>> $LOGFILE
    sed -i "s/%Title%/Peak Water Levels/g" FG51_SELA_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_SELA_maxele.inp 2>> $LOGFILE
    # Find Maximum WSE
    eval "$FINDMAXZCMD"
    etaMax=$(head -n 1 etaMax.txt) 2>> $LOGFILE
    etaMax=${etaMax%.*} 2>> $LOGFILE # Converts floating point to integer
    # set contour range based on maximum water level
    if [[ $etaMax -lt 6 ]]; then
        sed -i "s/%zmax%/6/g" FG51_SELA_maxele.inp 2>> $LOGFILE
        sed -i "s/%contourscaleinterval%/2/g" FG51_SELA_maxele.inp 2>> $LOGFILE
        sed -i "s/%scalelabel%/1/g" FG51_SELA_maxele.inp 2>> $LOGFILE
    elif [[ $etaMax -lt 16 ]]; then
        sed -i "s/%zmax%/16/g" FG51_SELA_maxele.inp 2>> $LOGFILE
        sed -i "s/%contourscaleinterval%/4/g" FG51_SELA_maxele.inp 2>> $LOGFILE
        sed -i "s/%scalelabel%/2/g" FG51_SELA_maxele.inp 2>> $LOGFILE
    else
        sed -i "s/%zmax%/32/g" FG51_SELA_maxele.inp 2>> $LOGFILE
        sed -i "s/%contourscaleinterval%/4/g" FG51_SELA_maxele.inp 2>> $LOGFILE
        sed -i "s/%scalelabel%/4/g" FG51_SELA_maxele.inp 2>> $LOGFILE
    fi
    # FigureGen contour plot of maxele.63.nc 
    #
    # write properties to describe the FigureGen job
    echo "hpc.job.${batchJOBTYPE}.file.qscripttemplate : $SCRIPTDIR/qscript.template" >> $SCENARIODIR/run.properties   
    echo "hpc.job.${batchJOBTYPE}.parallelism : serial" >> $SCENARIODIR/run.properties   
    echo "hpc.job.${batchJOBTYPE}.ncpu : 1" >> $SCENARIODIR/run.properties
    echo "hpc.job.${batchJOBTYPE}.ppn : $PPN" >> $SCENARIODIR/run.properties
    echo "hpc.job.${batchJOBTYPE}.account : $ACCOUNT" >> $SCENARIODIR/run.properties
    echo "hpc.job.${batchJOBTYPE}.limit.walltime : 01:00:00" >> $SCENARIODIR/run.properties
    echo "hpc.job.${batchJOBTYPE}.serialmodules : $SERIALMODULES" >> $SCENARIODIR/run.properties 
    echo "hpc.job.${batchJOBTYPE}.path.jobenvdir : $JOBENVDIR" >> $SCENARIODIR/run.properties 
    JOBENV=( )
    JOBENVSTRING="("
    for string in ${JOBENV[*]}; do
       JOBENVSTRING="$JOBENVSTRING $string"
    done
    JOBENVSTRING="$JOBENVSTRING )" 
    echo "hpc.job.${batchJOBTYPE}.jobenv : $JOBENVSTRING" >> $SCENARIODIR/run.properties 
    echo "hpc.job.${batchJOBTYPE}.cmd : ${POSTPROCDIR}/FigureGen -I FG51_SELA_maxele.inp" >> $SCENARIODIR/run.properties 
    # now submit the job
    if [[ ! -e ${POSTPROCDIR}/FigureGen ]]; then
       error "Need to compile ${POSTPROCDIR}/FigureGen." $LOGFILE
    fi
    scenarioMessage "$SCENARIO: $THIS: Submitting FigureGen job." $LOGFILE
    perl ${SCRIPTDIR}/qscript.pl --jobtype $batchJOBTYPE 2>&1 | tee -a $SCENARIOLOG >> $LOGFILE
    #
    fname="LA_SELA_${STORMNAME}_${ADVISORY}_${SCENARIO}_maxele_0001.jpg"
    echo "post.file.${batchJOBTYPE}.maxele.fname : $fname" >> $SCENARIODIR/run.properties
else
    warn "$SCENARIO: $THIS: The maxele.63.nc file was not found, so the FigureGen job will not be built." $LOGFILE
fi
#
# submit FigureGen job
#$SUBMITSTRING ${STORMDIR}/${QSFILE} >> ${SYSLOG} 2>&1
loadProperties $SCENARIODIR/run.properties # reload properties to get the name of the qscript
submitString=${properties["hpc.submitstring"]}
qscript=${properties["hpc.job.${batchJOBTYPE}.file.qscript"]}
if [[ -e $qscript ]]; then
   while [ true ];  do
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
      echo "time.hpc.job.${batchJOBTYPE}.submit : $DATETIME" >> ${SCENARIODIR}/run.properties
      $submitString $SCENARIODIR/$qscript 2>&1 | tee -a $LOGFILE >> ${SCENARIOLOG}
      if [[ $? = 0 ]]; then
         break # job submission command returned a "success" status
      else 
         warn "$SCENARIO: $THIS: $submitString $SCENARIODIR/$qscript failed; will retry in 60 seconds." $LOGFILE
         sleep 60
      fi
   done
else 
   warn "$SCENARIO: $THIS: The qscript file $qsript was not found in $SCENARIODIR." $LOGFILE
fi
#--------------------------------------------------------------------------
#       GENERATE HYDROGRAPHS & BUILD PPT
#--------------------------------------------------------------------------
# copy in the spreadsheets that matlab needs
cp ${POSTPROCDIR}/Gate_Closure_Trigger.xlsx . > errmsg 2>&1 || warn "cycle $CYCLE: $SCENARIO: $THIS: Failed to copy ${POSTPROCDIR}/Gate_Closure_Trigger.xlsx to '$SCENARIODIR': `cat errmsg`." $LOGFILE
cp ${POSTPROCDIR}/Datum_Conversion.xlsx . > errmsg 2>&1 || warn "cycle $CYCLE: $SCENARIO: $THIS: Failed to copy ${POSTPROCDIR}/Datum_Conversion.xlsx to '$SCENARIODIR': `cat errmsg`." $LOGFILE
cp ${POSTPROCDIR}/Static_Offset.xlsx . > errmsg 2>&1 || warn "cycle $CYCLE: $SCENARIO: $THIS: Failed to copy ${POSTPROCDIR}/Static_Offset.xlsx to '$SCENARIODIR': `cat errmsg`." $LOGFILE
# Run createPPT.sh
scenarioMessage "$SCENARIO: $THIS: Running ${POSTPROCDIR}/createPPT.sh." $LOGFILE
${POSTPROCDIR}/createPPT.sh 2>&1 | tee -a $SCENARIOLOG >> $LOGFILE
#--------------------------------------------------------------------------
# in case we changed directories at the beginning and another script follows this
cd $currentDir > errmsg 2>&1 || warn "cycle $CYCLE: $SCENARIO: $THIS: Failed to change directory to '$currentDir': `cat errmsg`." $LOGFILE
