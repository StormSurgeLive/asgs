#!/bin/bash
#------------------------------------------------------------------------
# run_adda.sh: computes DWLC from the nowcasts 
#------------------------------------------------------------------------
# Copyright(C) 2015--2019 Jason Fleming
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
#------------------------------------------------------------------------
#
THIS=$(basename -- $0)
#
declare -A properties
SCENARIODIR=$PWD
RUNPROPERTIES=$SCENARIODIR/run.properties
echo "RUNPROPERTIES=$RUNPROPERTIES"

# get ASGS functions
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' $RUNPROPERTIES`
echo "SCRIPTDIR=$SCRIPTDIR"

source $SCRIPTDIR/properties.sh
source $SCRIPTDIR/monitoring/logging.sh
source $SCRIPTDIR/platforms.sh

if [ ! -e "$RUNPROPERTIES" ] ; then
    allMessage "run_adda did NOT find the run.properties file."
    exit 1
fi

source "$SCRIPTDIR/output/adda_init.sh"
echo "ADDAHOME=$ADDAHOME"
#export ADDAHOME="/home/bblanton/GitHub/RENCI/ADCIRCDataAssimilation"
#export PYTHONPATH="/home/bblanton/GitHub/RENCI/ADCIRCDataAssimilation"
#export RUNTIMEDIR="./adda"
#PYTHON="/home/bblanton/miniconda3/bin/python"

DWLCfile="$RUNTIMEDIR/interpolated/ADCIRC_interpolated_wl.csv"
echo "DWLCfile=$DWLCfile"

# load run.properties file into associative array
loadProperties $RUNPROPERTIES
gridname=${properties['adcirc.gridname']}
instancename=${properties['instancename']}
adv=${properties['advisory']}

Dest="$ADDAStorage/current_"$gridname"_dwlc"
DestName="$Dest/dwlc_$gridname_$adv.csv"
DestLinkName="$Dest/dwlc_$gridname.csv"

echo "gridname=$gridname"
echo "adv=$adv"
echo "Dest=$Dest"
echo "DestName=$DestName"
echo "DestLinkName=$DestLinkName"

echo $PYTHON $ADDAHOME/ADDA/ADDA.py --grid $gridname --instance $instancename
$PYTHON $ADDAHOME/ADDA/ADDA.py --grid $gridname --instance $instancename

if [ $? == 0 ] ; then

    if [ ! -e $DWLCfile ] ; then 
        echo "Final ADDA output file DNE.  Terminal." 
    else
        echo "Pushing $DWLCfile to $Dest ... "
        if [ ! -d $Dest ] ; then
            mkdir $Dest
        fi
        cp $DWLCfile "$DestName"
        rm -rf "$DestLinkName"
        ln -sf "$DestName" "$DestLinkName"
    fi 

    date > adda.succeeded

else
    date > adda.failed
fi
