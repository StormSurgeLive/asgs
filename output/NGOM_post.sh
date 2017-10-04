#!/bin/bash
#--------------------------------------------------------------------------
# NGOM_post.sh 
#--------------------------------------------------------------------------
# Copies output from the ASGS to /dev/null.
#--------------------------------------------------------------------------
# 
# Copyright(C) 2008, 2009, 2010 Jason Fleming
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

# Copy FG51_WNAT_maxele.inp.template and edit.

STORMDIR=${ADVISDIR}/${ENSTORM} # ensemble member directory for this advisory
cd $STORMDIR

cp ${SCRIPTDIR}/output/NGOM_post/Extract_latlon.sh ${STORMDIR}/
./Extract_latlon.sh fort.22 fort.22.trk


if [ -f maxele.63.nc ]; then
    fname="NGOM_WNAT_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    title="NGOM_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_WNAT_maxele.inp.template ${STORMDIR}/FG51_WNAT_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_WNAT_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_WNAT_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_WNAT_maxele.inp
    
    fname="NGOM_GoM_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_GoM_maxele.inp.template ${STORMDIR}/FG51_GoM_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_GoM_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_GoM_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_GoM_maxele.inp
    
    fname="NGOM_NGOM_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_NGOM_maxele.inp.template ${STORMDIR}/FG51_NGOM_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_NGOM_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_NGOM_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_NGOM_maxele.inp
fi

if [ -f swan_HS_max.63.nc ]; then
    fname="NGOM_WNAT_${STORM}_${ADVISORY}_${ENSTORM}_maxHS_"
    title="NGOM_${STORM}_${ADVISORY}_${ENSTORM}_maxHS_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_WNAT_maxHS.inp.template ${STORMDIR}/FG51_WNAT_maxHS.inp
    sed -i "s/%FileName%/${fname}/g" FG51_WNAT_maxHS_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_WNAT_maxHS.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_WNAT_maxHS.inp
    
    fname="NGOM_GoM_${STORM}_${ADVISORY}_${ENSTORM}_maxHS_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_GoM_maxHS.inp.template ${STORMDIR}/FG51_GoM_maxHS.inp
    sed -i "s/%FileName%/${fname}/g" FG51_GoM_maxHS.inp 
    sed -i "s/%Title%/${title}/g" FG51_GoM_maxHS.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_GoM_maxHS.inp
    
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_NGOM_maxHS.inp
    fname="NGOM_NGOM_${STORM}_${ADVISORY}_${ENSTORM}_maxHS_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_NGOM_maxHS.inp.template ${STORMDIR}/FG51_NGOM_maxHS.inp
    sed -i "s/%FileName%/${fname}/g" FG51_NGOM_maxHS.inp 
    sed -i "s/%Title%/${title}/g" FG51_NGOM_maxHS.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_NGOM_maxHS.inp
fi

cp ${SCRIPTDIR}/output/NGOM_post/Default2.pal ${STORMDIR}/
cp ${SCRIPTDIR}/output/NGOM_post/Coastal_Resiliency_ppl_RGB.eps ${STORMDIR}/

cp ${SCRIPTDIR}/output/NGOM_post/submit-postproc.qb ${STORMDIR}/
qsub submit-postproc.qb

until [ -f ${STORMDIR}/postproc.done ]
do
    sleep 5
done


#
#POSTDIR=/dev/null
#
# do nothing and return
