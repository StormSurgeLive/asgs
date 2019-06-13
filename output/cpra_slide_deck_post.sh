#!/bin/bash
#--------------------------------------------------------------------------
# cpra_slide_deck_post.sh 
#--------------------------------------------------------------------------
# Driver script for building CPRA hydrograph slide deck.
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
# This script assumes that it is executed in the same directory with
# the results it is post processing.
#
# FigureGen.F90 must be compiled in the cpra_postproc subdirectory 
# in order for this script to function. 
#--------------------------------------------------------------------------
#                        S E T U P 
#--------------------------------------------------------------------------
umask 002
THIS="cpra_slide_deck_post.sh"
batchJOBTYPE=cpra.figuregen
postJOBTYPE=cpra.post
# STORMDIR: path where this ensemble member is supposed to run 
STORMDIR=$PWD
LOGFILE=${STORMDIR}/${postJOBTYPE}.log
# ensemble member name
ENSTORM=`sed -n 's/[ ^]*$//;s/asgs.enstorm\s*:\s*//p' run.properties`
echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Starting post processing." >> $LOGFILE
echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Collecting properties." >> $LOGFILE
# SCRIPTDIR: path to asgs scripts like asgs_main.sh
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/config.path.scriptdir\s*:\s*//p' run.properties`
. ${SCRIPTDIR}/monitoring/logging.sh
# ACCOUNT: by default, use whatever account was used by padcirc or padcswan
ACCOUNT=`sed -n 's/[ ^]*$//;s/hpc.job.padcswan.account\s*:\s*//p' run.properties`
if [[ -z $ACCOUNT ]]; then
   ACCOUNT=`sed -n 's/[ ^]*$//;s/hpc.job.padcirc.account\s*:\s*//p' run.properties`
fi
# type of queueing system (e.g., PBS, SLURM, or mpiexec)
QUEUESYS=`sed -n 's/[ ^]*$//;s/hpc.queuesys\s*:\s*//p' run.properties`
# check to see if this is a tropical cyclone
TROPCIALCYCLONE=`sed -n 's/[ ^]*$//;0,/config.forcing.tropicalcyclone/{s/config.forcing.tropicalcyclone\s*:\s*//p}' run.properties`
if [[ $TROPICALCYCLONE != "off" ]]; then
   # name of tc (FIXME: need the 0 because this property may appear more than once)
   STORMNAME=`sed -n 's/[ ^]*$//;0,/stormname/{s/stormname\s*:\s*//p}' run.properties`
else
   STORMNAME=NAM  #FIXME: make this more general
fi
# advisory number
ADVISORY=`sed -n 's/[ ^]*$//;s/advisory\s*:\s*//p' run.properties`
#
POSTPROCDIR=${SCRIPTDIR}/output/cpra_postproc
echo "post.path.${postJOBTYPE}.postprocdir : $POSTPROCDIR" >> $STORMDIR/run.properties

export MATLABPATH=${POSTPROCDIR}
JOBMODULES=""    
JOBPATHS=""
JOBLIBS=""
FINDMAXZ=""
# HPCENVSHORT: shorthand for the HPC environment: queenbee, hatteras, etc
HPCENVSHORT=`sed -n 's/[ ^]*$//;s/hpc.hpcenvshort\s*:\s*//p' run.properties`
echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Setting up for execution on ${HPCENVSHORT}." >> $LOGFILE
#
case $HPCENVSHORT in
    queenbee)
        echo "hpc.job.${batchJOBTYPE}.serqueue : single" >> $STORMDIR/run.properties
        echo "hpc.job.${batchJOBTYPE}.queuename : workq" >> $STORMDIR/run.properties
        JOBMODULES="module load python/2.7.12-anaconda-tensorflow"
        $JOBMODULES
        FINDMAXZCMD="${POSTPROCDIR}/Matlab_QB2/run_FindMaxZ.sh /usr/local/packages/license/matlab/r2017a"
        if [[ $USER = jgflemin ]]; then
           ACCOUNT=loni_cera_2019
           GDAL_HOME=/home/jgflemin/asgs/gdal
           GMT_HOME=/home/jgflemin/asgs/gmt/gmt-4.5.18
           JOBPATHS="export PATH=${GDAL_HOME}/bin:${GMT_HOME}/bin:\$PATH GDAL_DATA=${GDAL_HOME}/share/gdal"
           JOBLIBS="export LD_LIBRARY_PATH=${GDAL_HOME}/lib:${GMT_HOME}/lib:\$LD_LIBRARY_PATH"
        fi
        ;;
    hatteras)
        JOBMODULES="module load python_modules/2.7 matlab/2017b"
        module unload zlib # avoid intel library conflict issues with matlab
        #FINDMAXZCMD='matlab -nodisplay -nosplash -nodesktop -r "run FindMaxZ.m, exit"'
        FINDMAXZCMD=(matlab -nodisplay -nosplash -nodesktop -r "run FindMaxZ.m, exit")
        # set location of gdal; this only works if the asgs is running
        # in the ncfs account
        if [[ $USER = ncfs ]]; then
           GDAL_HOME=/home/ncfs/asgs/gdal
           GMT_HOME=/home/ncfs/asgs/gmt/gmt-4.5.18
           JOBPATHS="export PATH=${GDAL_HOME}/bin:${GMT_HOME}/bin:\$PATH GDAL_DATA=${GDAL_HOME}/share/gdal"
           JOBLIBS="export LD_LIBRARY_PATH=${GDAL_HOME}/lib:${GMT_HOME}/lib:\$LD_LIBRARY_PATH"
           # use the ncfs priority level
           echo "hpc.job.${batchJOBTYPE}.partition : ncfs" >> ${STORMDIR}/run.properties
        fi
        $JOBMODULES
        ;;
    lonestar)
        JOBMODULES=""    
        ;;
    stampede)
        JOBMODULES=""
        ;;
    *)
        error "HPC platform $HPCENVSHORT not recognized."
        ;;
esac
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#                    F I G U R E   G E N  
#--------------------------------------------------------------------------
if [[ -f maxele.63.nc ]]; then
    echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Building FigureGen job." >> $LOGFILE
    # Create storm track from fort.22 file
    awk 'BEGIN { FS="," } { printf "-%0.2f  %0.2f\n", $8/10.0, $7/10.0 }' fort.22 > fort.22.trk 2>> $LOGFILE
    # copy in color palette
    cp ${POSTPROCDIR}/Default2.pal ${STORMDIR}/ 2>> $LOGFILE
    cp ${POSTPROCDIR}/FG51_SELA_maxele.inp.template ${STORMDIR}/FG51_SELA_maxele.inp 2>> $LOGFILE
    # fill in figuregen template file with values for this plot
    fname="LA_SELA_${STORMNAME}_${ADVISORY}_${ENSTORM}_maxele_"
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
    echo "hpc.path.${batchJOBTYPE}.template.qstdir : $SCRIPTDIR/input/queuesys/$QUEUESYS" >> $STORMDIR/run.properties
    echo "hpc.file.${batchJOBTYPE}.template.qstemplate : ${QUEUESYS,,}.template" >> $STORMDIR/run.properties
    echo "hpc.job.${batchJOBTYPE}.ncpu : 1" >> $STORMDIR/run.properties
    echo "hpc.job.${batchJOBTYPE}.account : $ACCOUNT" >> $STORMDIR/run.properties
    echo "hpc.job.${batchJOBTYPE}.limit.walltime : 01:00:00" >> $STORMDIR/run.properties
    echo "hpc.job.${batchJOBTYPE}.jobmodules : $JOBMODULES" >> $STORMDIR/run.properties 
    echo "hpc.job.${batchJOBTYPE}.jobpaths : $JOBPATHS" >> $STORMDIR/run.properties 
    echo "hpc.job.${batchJOBTYPE}.joblibs : $JOBLIBS" >> $STORMDIR/run.properties 
    echo "hpc.job.${batchJOBTYPE}.cmd : ${POSTPROCDIR}/FigureGen -I FG51_SELA_maxele.inp > ${batchJOBTYPE}.log 2>\&1" >> $STORMDIR/run.properties 
    # now submit the job
    echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Submitting FigureGen job." >> $LOGFILE
    ${SCRIPTDIR}/submitJob.sh $batchJOBTYPE
    fname="LA_SELA_${STORMNAME}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
    echo "post.file.${batchJOBTYPE}.maxele.fname : $fname" >> $STORMDIR/run.properties
else
    echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: The maxele.63.nc file was not found, so the FigureGen job will not be built." >> $LOGFILE
fi
#--------------------------------------------------------------------------
#       GENERATE HYDROGRAPHS & BUILD PPT
#--------------------------------------------------------------------------
# copy in the spreadsheet that matlab needs
cp ${POSTPROCDIR}/Gate_Closure_Trigger.xlsx . 2>> $LOGFILE
# Run createPPT.sh
echo "["`date +'%Y-%h-%d-T%H:%M:%S%z'`"]: $ENSTORM: $THIS: Running ${POSTPROCDIR}/createPPT.sh." >> $LOGFILE
${POSTPROCDIR}/createPPT.sh 
#--------------------------------------------------------------------------
