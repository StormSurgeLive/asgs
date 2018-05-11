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

    fname="NGOM_MS_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_MS_maxele.inp.template ${STORMDIR}/FG51_MS_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_MS_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_MS_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_MS_maxele.inp

    fname="NGOM_AL_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_AL_maxele.inp.template ${STORMDIR}/FG51_AL_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_AL_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_AL_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_AL_maxele.inp

    fname="NGOM_Pensacola_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_Pensacola_maxele.inp.template ${STORMDIR}/FG51_Pensacola_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_Pensacola_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_Pensacola_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_Pensacola_maxele.inp

    fname="NGOM_Choc_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_Choc_maxele.inp.template ${STORMDIR}/FG51_Choc_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_Choc_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_Choc_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_Choc_maxele.inp

    fname="NGOM_StAndrews_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_StAndrews_maxele.inp.template ${STORMDIR}/FG51_StAndrews_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_StAndrews_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_StAndrews_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_StAndrews_maxele.inp

    fname="NGOM_Apalachicola_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_Apalachicola_maxele.inp.template ${STORMDIR}/FG51_Apalachicola_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_Apalachicola_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_Apalachicola_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_Apalachicola_maxele.inp

    fname="NGOM_Apalachee_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/NGOM_post/FG51_Apalachee_maxele.inp.template ${STORMDIR}/FG51_Apalachee_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_Apalachee_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_Apalachee_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_Apalachee_maxele.inp
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
#cp ${SCRIPTDIR}/output/NGOM_post/Coastal_Resiliency_ppl_RGB.eps ${STORMDIR}/
cp ${SCRIPTDIR}/output/NGOM_post/LSU_UCF.eps ${STORMDIR}/

cp ${SCRIPTDIR}/output/NGOM_post/submit-postproc.qb ${STORMDIR}/
qsub submit-postproc.qb

until [ -f ${STORMDIR}/postproc.done ]
do
    sleep 5
done

# Copy files to chenier
sdir=/data/CCR_data/ACTIVE/SHARE/asgs/NGOM/nate # Base directory
# Create new directory for the current advisory and ensemble member
ssh chenier.cct.lsu.edu "mkdir -p ${sdir}/${ADVISORY}"
ssh chenier.cct.lsu.edu "mkdir -p ${sdir}/${ADVISORY}/${ENSTORM}"
# Clear the current directory and create new ensemble memmber directory
ssh chenier.cct.lsu.edu "rm ${sdir}/Current/*"
ssh chenier.cct.lsu.edu "mkdir -p ${sdir}/Current/${ENSTORM}"
# Copy images for current advisory - Doesn't seem to work
#scp  "*.jpg" mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}

fname="NGOM_WNAT_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
scp ${fname} mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}/
fname2="NGOM_WNAT_${STORM}_${ENSTORM}_maxele_0001.jpg"
cp ${fname} ${fname2}
scp ${fname2} mbilskie@chenier.cct.lsu.edu:${sdir}/Current/${ENSTORM}/
rm ${fname2}

fname="NGOM_GoM_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
scp ${fname} mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}/
fname2="NGOM_GoM_${STORM}_${ENSTORM}_maxele_0001.jpg"
cp ${fname} ${fname2}
scp ${fname2} mbilskie@chenier.cct.lsu.edu:${sdir}/Current/${ENSTORM}/
rm ${fname2}

fname="NGOM_NGOM_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
scp ${fname} mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}/
fname2="NGOM_NGOM_${STORM}_${ENSTORM}_maxele_0001.jpg"
cp ${fname} ${fname2}
scp ${fname2} mbilskie@chenier.cct.lsu.edu:${sdir}/Current/${ENSTORM}/
rm ${fname2}

fname="NGOM_MS_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
scp ${fname} mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}/
fname2="NGOM_MS_${STORM}_${ENSTORM}_maxele_0001.jpg"
cp ${fname} ${fname2}
scp ${fname2} mbilskie@chenier.cct.lsu.edu:${sdir}/Current/${ENSTORM}/
rm ${fname2}

fname="NGOM_AL_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
scp ${fname} mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}/
fname2="NGOM_AL_${STORM}_${ENSTORM}_maxele_0001.jpg"
cp ${fname} ${fname2}
scp ${fname2} mbilskie@chenier.cct.lsu.edu:${sdir}/Current/${ENSTORM}/
rm ${fname2}

fname="NGOM_StAndrews_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
scp ${fname} mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}/
fname2="NGOM_StAndrews_${STORM}_${ENSTORM}_maxele_0001.jpg"
cp ${fname} ${fname2}
scp ${fname2} mbilskie@chenier.cct.lsu.edu:${sdir}/Current/${ENSTORM}/
rm ${fname2}

fname="NGOM_Pensacola_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
scp ${fname} mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}/
fname2="NGOM_Pensacola_${STORM}_${ENSTORM}_maxele_0001.jpg"
cp ${fname} ${fname2}
scp ${fname2} mbilskie@chenier.cct.lsu.edu:${sdir}/Current/${ENSTORM}/
rm ${fname2}

fname="NGOM_Choc_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
scp ${fname} mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}/
fname2="NGOM_Choc_${STORM}_${ENSTORM}_maxele_0001.jpg"
cp ${fname} ${fname2}
scp ${fname2} mbilskie@chenier.cct.lsu.edu:${sdir}/Current/${ENSTORM}/
rm ${fname2}

fname="NGOM_Apalachicola_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
scp ${fname} mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}/
fname2="NGOM_Apalachicola_${STORM}_${ENSTORM}_maxele_0001.jpg"
cp ${fname} ${fname2}
scp ${fname2} mbilskie@chenier.cct.lsu.edu:${sdir}/Current/${ENSTORM}/
rm ${fname2}

fname="NGOM_Apalachee_${STORM}_${ADVISORY}_${ENSTORM}_maxele_0001.jpg"
scp ${fname} mbilskie@chenier.cct.lsu.edu:${sdir}/${ADVISORY}/${ENSTORM}/
fname2="NGOM_Apalachee_${STORM}_${ENSTORM}_maxele_0001.jpg"
cp ${fname} ${fname2}
scp ${fname2} mbilskie@chenier.cct.lsu.edu:${sdir}/Current/${ENSTORM}/
rm ${fname2}

sleep 5

#
#POSTDIR=/dev/null
#
# do nothing and return
