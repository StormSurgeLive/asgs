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
#--------------------------------------------------------------------------
#                        S E T U P 
#--------------------------------------------------------------------------
umask 002
# STORMDIR: path where this ensemble member is supposed to run 
STORMDIR=$PWD
# SCRIPTDIR: path to asgs scripts like asgs_main.sh
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/config.path.scriptdir\s*:\s*//p' run.properties`
. ${SCRIPTDIR}/logging.sh
#
POSTPROCDIR=${SCRIPTDIR}/output/cpra_postproc/
LOGFILE=${STORMDIR}/cpra.post.log
export MATLABPATH=${POSTPROCDIR}
JOBMODULES=""    
# HPCENVSHORT: shorthand for the HPC environment: queenbee, hatteras, etc
HPCENVSHORT=`sed -n 's/[ ^]*$//;s/hpc.hpcenvshort\s*:\s*//p' run.properties`
case $HPCENVSHORT in
    queenbee)
        JOBMODULES="module load python/2.7.12-anaconda-tensorflow ; module load matlab/r2015b"  
        ;;
    hatteras)
        JOBMODULES="module load python_modules/2.7 ; module load matlab/2017b"
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
    fname="LA_SELA_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    sed -i "s/%FileName%/${fname}/g" FG51_SELA_maxele.inp 2>> $LOGFILE
    #sed -i "s/%Title%/${title}/g" FG51_SELA_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_SELA_maxele.inp 2>> $LOGFILE
    # Find Maximum WSE
    matlab -nodisplay -nosplash -nodesktop -r "run FindMaxZ.m, exit" 2>> $LOGFILE
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
    JOBTYPE=cpra.post
    echo "hpc.path.${JOBTYPE}.template.qstdir : $POSTPROCDIR" >> $STORMDIR/run.properties
    echo "hpc.file.${JOBTYPE}.template.qstemplate : $INPUTDIR/queuesys/$QUEUESYS/serial.template" >> $STORMDIR/run.properties
    echo "hpc.job.${JOBTYPE}.ncpu : 1" >> $STORMDIR/run.properties
    echo "hpc.job.${JOBTYPE}.account : $ACCOUNT" >> $STORMDIR/run.properties
    echo "hpc.job.${JOBTYPE}.limit.walltime : 01:00:00" >> $STORMDIR/run.properties
    echo "hpc.job.${JOBTYPE}.jobmodules : $JOBMODULES" >> $STORMDIR/run.properties 
    echo "hpc.job.${JOBTYPE}.platformmodules : $PLATFORMMODULES" >> $STORMDIR/run.properties 
    echo "hpc.job.${JOBTYPE}.cmd : ${POSTPROCDIR}/FigureGen -I FG51_SELA_maxele.inp > ${JOBTYPE}.log 2>&1" >> $STORMDIR/run.properties 
    # now submit the job
    ${SCRIPTDIR}/submitJob.sh $JOBTYPE
fi
#--------------------------------------------------------------------------
#       GENERATE HYDROGRAPHS & BUILD PPT
#--------------------------------------------------------------------------
# Run createPPT.sh
fname="LA_SELA_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
${POSTPROCDIR}/createPPT.sh -i ${POSTPROCDIR} -s ${STORMDIR} -fig ${fname} 
#--------------------------------------------------------------------------
