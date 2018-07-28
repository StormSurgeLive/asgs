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
# STORMDIR: path where this ensemble member is supposed to run 
STORMDIR=$PWD
# SCRIPTDIR: path to asgs scripts like asgs_main.sh
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/config.path.scriptdir\s*:\s*//p' run.properties`
. ${SCRIPTDIR}/logging.sh
# ACCOUNT: by default, use whatever account was used by padcirc or padcswan
ACCOUNT=`sed -n 's/[ ^]*$//;s/hpc.job.padcswan.account\s*:\s*//p' run.properties`
if [[ -z $ACCOUNT ]]; then
   ACCOUNT=`sed -n 's/[ ^]*$//;s/hpc.job.padcirc.account\s*:\s*//p' run.properties`
fi
# type of queueing system (e.g., PBS, SLURM, or mpiexec)
QUEUESYS=`sed -n 's/[ ^]*$//;s/hpc.queuesys\s*:\s*//p' run.properties`
# name of tc (FIXME: need the 0 because this property may appear more than once)
STORMNAME=`sed -n 's/[ ^]*$//;0,/stormname/{s/stormname\s*:\s*//p}' run.properties`
# advisory number
ADVISORY=`sed -n 's/[ ^]*$//;s/advisory\s*:\s*//p' run.properties`
# ensemble member name
ENSTORM=`sed -n 's/[ ^]*$//;s/asgs.enstorm\s*:\s*//p' run.properties`
#
JOBTYPE=cpra.post
POSTPROCDIR=${SCRIPTDIR}/output/cpra_postproc
echo "post.path.${JOBTYPE}.postprocdir : $POSTPROCDIR" >> $STORMDIR/run.properties
LOGFILE=${STORMDIR}/cpra.post.log
export MATLABPATH=${POSTPROCDIR}
JOBMODULES=""    
# HPCENVSHORT: shorthand for the HPC environment: queenbee, hatteras, etc
HPCENVSHORT=`sed -n 's/[ ^]*$//;s/hpc.hpcenvshort\s*:\s*//p' run.properties`
case $HPCENVSHORT in
    queenbee)
        JOBMODULES="module load python/2.7.12-anaconda-tensorflow matlab/r2015b"  
        $JOBMODULES
        ;;
    hatteras)
        JOBMODULES="module load python_modules/2.7 matlab/2017b"
        $JOBMODULES
        module unload zlib # causes intel library issues with matlab on hatteras
        echo "hpc.job.${JOBTYPE}.partition : ncfs" >> ${STORMDIR}/run.properties
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
    # Create storm track from fort.22 file
    awk 'BEGIN { FS="," } { printf "-%0.2f  %0.2f\n", $8/10.0, $7/10.0 }' fort.22 > fort.22.trk 2>> $LOGFILE
    # copy in color palette
    cp ${POSTPROCDIR}/Default2.pal ${STORMDIR}/ 2>> $LOGFILE
    cp ${POSTPROCDIR}/FG51_SELA_maxele.inp.template ${STORMDIR}/FG51_SELA_maxele.inp 2>> $LOGFILE
    # fill in figuregen template file with values for this plot
    fname="LA_SELA_${STORMNAME}_${ADVISORY}_${ENSTORM}_maxele_"
    echo "$THIS: fname is $fname" # debug
    sed -i "s/%FileName%/${fname}/g" FG51_SELA_maxele.inp 2>> $LOGFILE
    #sed -i "s/%Title%/${title}/g" FG51_SELA_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_SELA_maxele.inp 2>> $LOGFILE
    # Find Maximum WSE
    matlab -nodisplay -nosplash -nodesktop -r "run FindMaxZ.m, exit" 
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
    echo "hpc.path.${JOBTYPE}.template.qstdir : $SCRIPTDIR/input/queuesys/$QUEUESYS" >> $STORMDIR/run.properties
    echo "hpc.file.${JOBTYPE}.template.qstemplate : slurm.template" >> $STORMDIR/run.properties
    echo "hpc.job.${JOBTYPE}.ncpu : 1" >> $STORMDIR/run.properties
    echo "hpc.job.${JOBTYPE}.account : $ACCOUNT" >> $STORMDIR/run.properties
    echo "hpc.job.${JOBTYPE}.limit.walltime : 01:00:00" >> $STORMDIR/run.properties
    echo "hpc.job.${JOBTYPE}.jobmodules : $JOBMODULES" >> $STORMDIR/run.properties 
    echo "hpc.job.${JOBTYPE}.cmd : ${POSTPROCDIR}/FigureGen -I FG51_SELA_maxele.inp > ${JOBTYPE}.log 2>\&1" >> $STORMDIR/run.properties 
    # now submit the job
    ${SCRIPTDIR}/submitJob.sh $JOBTYPE
    fname="LA_SELA_${STORMNAME}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
    echo "post.file.${JOBTYPE}.maxele.fname : $fname" >> $STORMDIR/run.properties
fi
#--------------------------------------------------------------------------
#       GENERATE HYDROGRAPHS & BUILD PPT
#--------------------------------------------------------------------------
# Run createPPT.sh
${POSTPROCDIR}/createPPT.sh 
#--------------------------------------------------------------------------
