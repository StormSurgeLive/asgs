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


# Copy FG51.inp.template and edit.

STORMDIR=${ADVISDIR}/${ENSTORM} # ensemble member directory for this advisory
cd $STORMDIR

cp ${SCRIPTDIR}/output/cpra_postproc/Extract_latlon.sh ${STORMDIR}/
chmod u+x Extrac_latlon.sh
./Extract_latlon.sh fort.22 fort.22.trk

if [ -f maxele.63.nc ]; then
    fname="LA_v17a_SELA${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    title="SELA_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    cp ${SCRIPTDIR}/output/cpra_postproc/FG51_SELA_maxele.inp.template ${STORMDIR}/FG51_SELA_maxele.inp
    sed -i "s/%FileName%/${fname}/g" FG51_SELA_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_SELA_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_SELA_maxele.inp
fi

#if [ -f swan_HS_max.63.nc ]; then
#    fname="NGOM_WNAT_${STORM}_${ADVISORY}_${ENSTORM}_maxHS_"
#    title="NGOM_${STORM}_${ADVISORY}_${ENSTORM}_maxHS_"
#    cp ${SCRIPTDIR}/output/NGOM_post/FG51_WNAT_maxHS.inp.template ${STORMDIR}/FG51_WNAT_maxHS.inp
#    sed -i "s/%FileName%/${fname}/g" FG51_WNAT_maxHS_maxele.inp 
#    sed -i "s/%Title%/${title}/g" FG51_WNAT_maxHS.inp 
#    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_WNAT_maxHS.inp
#fi

cp ${SCRIPTDIR}/output/cpra_postproc/Default2.pal ${STORMDIR}/

# Create FigureGen images
cp ${SCRIPTDIR}/output/cpra_postproc/submit-postproc.qb ${STORMDIR}/
qsub submit-postproc.qb

until [ -f ${STORMDIR}/postproc.done ]
do
    sleep 5
done

sleep 5

# Copy over files needed to create the hydrographs and PPT file
cp ${SCRIPTDIR}/output/cpra_postproc/createPPT.sh
cp ${SCRIPTDIR}/output/cpra_postproc/buildPPT.py
cp ${SCRIPTDIR}/output/cpra_postproc/LSU_template.pptx
cp ${SCRIPTDIR}/output/cpra_postproc/GetInfo4Hydrographs.sh
cp ${SCRIPTDIR}/output/cpra_postproc/Gate_Closure_Trigger.xlsx
cp ${SCRIPTDIR}/output/cpra_postproc/plot_usace_adcirc.m
cp ${SCRIPTDIR}/output/cpra_postproc/read61nc.m
cp ${SCRIPTDIR}/output/cpra_postproc/rivergages2.m
cp ${SCRIPTDIR}/output/cpra_postproc/xml2struct.m
chmod u+x createPPT.sh
./createPPT.sh
sleep 5
# Clean up
rm createPPT.sh
rm buildPPT.py
rm LSU_template.pptx
rm GetInfo4Hydrographs.sh
rm Gate_Closure_Trigger.xlsx
rm plot_usace_adcirc.m
rm read61nc.m
rm rivergages2.m
rm xml2struct.m

sleep 5

#
#POSTDIR=/dev/null
#
# do nothing and return
