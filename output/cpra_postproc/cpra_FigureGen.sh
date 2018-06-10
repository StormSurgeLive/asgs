#!/bin/bash
#--------------------------------------------------------------------------
# cpra_FigureGen.sh
#--------------------------------------------------------------------------
# Script to prepare FigureGen inputs.
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
#
#
#--------------------------------------------------------------------------
#       GATHER COMMAND LINE ARGUMENTS
#--------------------------------------------------------------------------
POSITIONAL=()
while [[ $# -gt 0 ]]
do
    key="$1"

    case $key in
        -i)
            toolDir="$2"
            shift # past argument
            shift # past value
            ;;
        -s)
            stormDir="$2"
            shift # past argument
            shift # past value
            ;;
    esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters
#--------------------------------------------------------------------------
#
#
#--------------------------------------------------------------------------
#       PREPARE FIGUREGEN INPUT FILES
#--------------------------------------------------------------------------
export MATLABPATH=${toolDir}

# Create storm track from fort.22 file
${toolDir}/Extract_latlon.sh fort.22 fort.22.trk

if [ -f maxele.63.nc ]; then
    cp ${toolDir}/FG51_SELA_maxele.inp.template ${stormDir}/FG51_SELA_maxele.inp
    fname="LA_SELA_${STORM}_${ADVISORY}_${ENSTORM}_maxele_"
    sed -i "s/%FileName%/${fname}/g" FG51_SELA_maxele.inp 
    sed -i "s/%Title%/${title}/g" FG51_SELA_maxele.inp 
    sed -i "s/%TrackFile%/fort.22.trk/g" FG51_SELA_maxele.inp
    # Find Maximum WSE
    #matlab -nodisplay -nosplash -nodesktop -r "run FindMaxZ.m, exit"
    etaMax=$(head -n 1 etaMax.txt)
    etaMax=${etaMax%.*}
    echo $etaMax
    if [[ etaMax -lt 6 ]] ; then
        sed -i "s/%zmax%/6/g" FG51_SELA_maxele.inp
        sed -i "s/%contourscale%/2/g" FG51_SELA_maxele.inp
        sed -i "s/%scalelabel%/1/g" FG51_SELA_maxele.inp
    elif [[ etaMax -lt 16 ]] ; then
        sed -i "s/%zmax%/16/g" FG51_SELA_maxele.inp
        sed -i "s/%contourscale%/4/g" FG51_SELA_maxele.inp
        sed -i "s/%scalelabel%/2/g" FG51_SELA_maxele.inp
    elif [[ etaMax -lt 32 ]] ; then
        sed -i "s/%zmax%/32/g" FG51_SELA_maxele.inp
        sed -i "s/%contourscale%/4/g" FG51_SELA_maxele.inp
        sed -i "s/%scalelabel%/4/g" FG51_SELA_maxele.inp
    fi
fi
cp ${toolDir}/Default2.pal ${stormDir}/
#--------------------------------------------------------------------------
