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

####################################################
# JASON - Can we make this seamless pending the machine that is selected when launching asgs?

# Modules for QB2
#module load matlab/r2015b
#module load python/2.7.12-anaconda-tensorflow

# Modules for hatteras
#module load python_modules/2.7
#module load matlab/2017b
#####################################################


# Copy FG51.inp.template and edit.

STORMDIR=${ADVISDIR}/${ENSTORM} # ensemble member directory for this advisory
cd $STORMDIR

cp ${SCRIPTDIR}/output/cpra_postproc/Extract_latlon.sh ${STORMDIR}/
chmod u+x Extrac_latlon.sh
./Extract_latlon.sh fort.22 fort.22.trk

if [ -f maxele.63.nc ]; then
    sed -i "s/%FileName%/${fname}/g" FG51_SELA_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_SELA_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_SELA_maxele.inp
    # Find Maximum WSE
    matlab -nodisplay -nosplash -nodesktop -r "FindMaxZ, exit"
    etaMax=$(head -n 1 etaMax.txt)
    if [ etaMax -lt 6 ]
        sed -i "s/%zmax%/6/g" FG51_SELA_maxele.inp
        sed -i "s/%contourscale%/2/g" FG51_SELA_maxele.inp
        sed -i "s/%scalelabel%/1/g" FG51_SELA_maxele.inp
    elif [ etaMax -lt 16 ]
        sed -i "s/%zmax%/16/g" FG51_SELA_maxele.inp
        sed -i "s/%contourscale%/4/g" FG51_SELA_maxele.inp
        sed -i "s/%scalelabel%/2/g" FG51_SELA_maxele.inp
    elif [ etaMax -lt 32 ]
        sed -i "s/%zmax%/32/g" FG51_SELA_maxele.inp
        sed -i "s/%contourscale%/4/g" FG51_SELA_maxele.inp
        sed -i "s/%scalelabel%/4/g" FG51_SELA_maxele.inp
    fi
fi

cp ${SCRIPTDIR}/output/cpra_postproc/Default2.pal ${STORMDIR}/

# Create FigureGen images
cp ${SCRIPTDIR}/output/cpra_postproc/submit-postproc.qb ${STORMDIR}/
qsub submit-postproc.qb

until [ -f ${STORMDIR}/postproc.done ]
do
    sleep 5
done

sleep 5

# Run createPPT.sh
POSTPROCDIR=${SCRIPTDIR}/output/cpra_postproc/
${POSTPROC}/createPPT.sh -i ${POSTPROCDIR} 

sleep 5

#
#POSTDIR=/dev/null
#
# do nothing and return
