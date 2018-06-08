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
cd $STORMDIR
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
${POSTPROCDIR}/Extract_latlon.sh fort.22 fort.22.trk

if [ -f maxele.63.nc ]; then
    cp ${POSTPROCDIR}/FG51_SELA_maxele.inp.template ${STORMDIR}/FG51_SELA_maxele.inp
    fname="LA_SELA_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    sed -i "s/%FileName%/${fname}/g" FG51_SELA_maxele.inp 
    #sed -i "s/%Title%/${title}/g" FG51_SELA_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_SELA_maxele.inp
    # Find Maximum WSE
    matlab -nodisplay -nosplash -nodesktop -r "run FindMaxZ.m, exit"
    etaMax=$(head -n 1 etaMax.txt)
    etaMax=${etaMax%.*} # Converts floating point to integer
    if [ $etaMax -lt 6 ]; then
        sed -i "s/%zmax%/6/g" FG51_SELA_maxele.inp
        sed -i "s/%contourscaleinterval%/2/g" FG51_SELA_maxele.inp
        sed -i "s/%scalelabel%/1/g" FG51_SELA_maxele.inp
    elif [ $etaMax -lt 16 ]; then
        sed -i "s/%zmax%/16/g" FG51_SELA_maxele.inp
        sed -i "s/%contourscaleinterval%/4/g" FG51_SELA_maxele.inp
        sed -i "s/%scalelabel%/2/g" FG51_SELA_maxele.inp
    elif [ $etaMax -lt 32 ]; then
        sed -i "s/%zmax%/32/g" FG51_SELA_maxele.inp
        sed -i "s/%contourscaleinterval%/4/g" FG51_SELA_maxele.inp
        sed -i "s/%scalelabel%/4/g" FG51_SELA_maxele.inp
    fi
fi
cp ${POSTPROCDIR}/Default2.pal ${STORMDIR}/
#--------------------------------------------------------------------------

# Launch submit script
cp ${POSTPROCDIR}/submit-postproc.qb ${STORMDIR}/
qsub submit-postproc.qb
#logMessage "$ENSTORM: Submitting FigureGen runs"

# Wait until submit-postproc is finished
until [ -f ${STORMDIR}/postproc.done ]
do
    sleep 5
done
sleep 5
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       GENERATE HYDROGRAPHS & BUILD PPT
#--------------------------------------------------------------------------
# Run createPPT.sh
#logMessage "$ENSTORM: Creating hydrographs and PPT slides ${POSTPROCDIR}/createPPT.sh -i {POSTPROCDIR}"
${POSTPROCDIR}/createPPT.sh -i ${POSTPROCDIR} -s ${STORMDIR} 
#--------------------------------------------------------------------------
#
#
#POSTDIR=/dev/null
#
# do nothing and return
