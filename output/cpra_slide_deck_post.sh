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
#
# grab all static configuration data
. ${CONFIG}
#Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# grab all config info again (so the CONFIG file takes precendence)
. ${CONFIG}
#
umask 002
#
#--------------------------------------------------------------------------
# Modules for QB2
#module load matlab/r2015b
#module load python/2.7.12-anaconda-tensorflow

# Modules for hatteras
#module load python_modules/2.7
#module load matlab/2017b
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
${POSTPROCDIR}/cpra_FigureGen.sh -i ${POSTPROCDIR} -s ${STORMDIR}
# Launch submit script
cp ${POSTPROCDIR}/submit-postproc.qb ${STORMDIR}/
qsub submit-postproc.qb

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
${POSTPROC}/createPPT.sh -i ${POSTPROCDIR} 
sleep 5
#--------------------------------------------------------------------------
#
#
#POSTDIR=/dev/null
#
# do nothing and return
