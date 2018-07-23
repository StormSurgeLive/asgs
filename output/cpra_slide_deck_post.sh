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
#/project/mbilskie/repo/asgs/output/cpra_slide_deck_post.sh /scratch/mbilskie/LA_v17a/config/asgs_config_isaac_hindcast_qb2_LA_v17a_postproc_testing.sh /work/mbilskie/LA_v17a/isaac/asgs25945/25_almost/ 09 2012 25 qb.loni.org nhcOfficial 2012081100 2012082706 LA_v17a-WithUpperAtch_chk /work/mbilskie/LA_v17a/isaac/asgs25945/25_almost/nhcOfficial/ /scratch/mbilskie/LA_v17a/isaac/asgs-2018-May-15-T09\:57\:47.25945.log  ~/.ssh/authorized_keys
#
#bash ~/asgs/branches/nowcastarchive/output/cpra_slide_deck_post.sh ~/asgs/branches/nowcastarchive/config/2018/asgs_config_isaac_swan_hatteras_LAv17a.sh /projects/ncfs/data/asgs18825/20/nhcConsensus  09 2012 20 hatteras.renci.org nhcConsensus 2012072600 2678400.0 /projects/ncfs/data/input/LA_v17a-WithUpperAtch_chk.grd ~/asgs/branches/nowcastarchive/output syslog.log ~/.ssh/authorized_keys
#--------------------------------------------------------------------------
CONFIG=$1
ADVISDIR=$2
STORM=$3
YEAR=$4
ADVISORY=$5
HOSTNAME=$6
ENSTORM=$7
COLDSTARTDATE=$8
HSTIME=$9
GRIDFILE=$10
OUTPUTDIR=$11
SYSLOG=$12
SSHKEY=$13
#
# Grab all static configuration data
. ${CONFIG}
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# Dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info again (so the CONFIG file takes precendence)
. ${CONFIG}
#
umask 002
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       SETUP AND ENTER SIMULATION DIRECTORY
#--------------------------------------------------------------------------
POSTPROCDIR=${SCRIPTDIR}/output/cpra_postproc/
STORMDIR=${ADVISDIR}/${ENSTORM} # ensemble member directory for this advisory
cd $STORMDIR 2>> ${SYSLOG}
LOGFILE=${STORMDIR}/cpra.post.log
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       GENERATE FIGUREGEN IMAGES
#--------------------------------------------------------------------------
# Generate FigureGen Images
#${POSTPROCDIR}/cpra_FigureGen.sh -i ${POSTPROCDIR} -s ${STORMDIR}
export MATLABPATH=${POSTPROCDIR}
# Create storm track from fort.22 file
awk 'BEGIN { FS="," } { printf "-%0.2f  %0.2f\n", $8/10.0, $7/10.0 }' fort.22 > fort.22.trk 2>> $LOGFILE
#${POSTPROCDIR}/Extract_latlon.sh fort.22 fort.22.trk
# copy in color palette
cp ${POSTPROCDIR}/Default2.pal ${STORMDIR}/ 2>> $LOGFILE
if [[ -f maxele.63.nc ]]; then
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
    echo "hpc.job.${JOBTYPE}.cmd : ${POSTPROCDIR}/FigureGen -I FG51_SELA_maxele.inp > ${JOBTYPE}.log 2>&1" >> $STORMDIR/run.properties 
    MODULESCMD=""
    case $HPCENVSHORT in
    queenbee)
        MODULESCMD="module load python/2.7.12-anaconda-tensorflow"  
        ;;
    hatteras)
        MODULESCMD="module load python_modules/2.7 ; module load matlab/2017b"
        ;;
    lonestar)
        MODULESCMD=""    
        ;;
    stampede)
        MODULESCMD=""
        ;;
    *)
        error "HPC platform $HPCENVSHORT not recognized."
        ;;
    esac
    echo "hpc.job.${JOBTYPE}.modulescmd : $MODULESCMD" >> $STORMDIR/run.properties
    # now submit the job
    ${SCRIPTDIR}/submitJob.sh $JOBTYPE
fi
# 
# Launch submit script
#cp ${POSTPROCDIR}/submit-postproc.qb ${STORMDIR}/
#qsub submit-postproc.qb
#sleep 2
#logMessage "$ENSTORM: Submitting FigureGen runs"

### MOVED THIS TO createPPT.sh SO THE MATLAB HYDROGRAPHS CAN START 
# Wait until submit-postproc is finished
#until [ -f ${STORMDIR}/postproc.done ]
#do
#    sleep 5
#done
#sleep 5
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       GENERATE HYDROGRAPHS & BUILD PPT
#--------------------------------------------------------------------------
# Run createPPT.sh
fname="LA_SELA_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
${POSTPROCDIR}/createPPT.sh -i ${POSTPROCDIR} -s ${STORMDIR} -fig ${fname} 
#--------------------------------------------------------------------------
#
#
#POSTDIR=/dev/null
#
# do nothing and return
