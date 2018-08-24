#!/bin/bash
#----------------------------------------------------------------
# issue_advisory.sh
#
# For use when the asgs is configured to retrieve tropical storm
# forecast advisories from the filesystem; mostly for testing
# purposes. 
#
# This script can be used to issue an advisory from sample files
# archived in the asgs input/sample_advisories. Typically it can
# be run from within the sample_advisories directory and the 
# asgs can be configured as follows.  
#
# i.e. your asgs_config file should contain the following
# 
# TRIGGER=rssembedded          
# RSSSITE=filesystem
# FTPSITE=filesystem
# HDIR=${SCRIPTDIR}/input/sample_advisories
# FDIR=${SCRIPTDIR}/input/sample_advisories
#
# Or alternatively use the optional --sampledir argument 
# if you have a reason for issusing advisories in a different
# HDIR or FDIR location. In that case run the script within 
# the HDIR and/or  FDIR location, which can be the same.  
#
# The script simply removes the old index-at.xml and balxxxxx.dat
# files and then makes symlinks to the new ones in the current
# working directory
#
# usage:
#  
# ./issue_advisory.sh --yyyy 2017 --stormnum 16 --advnum 12
#   [ --sampledir ~/home/$USER/asgs/input/sample_advisories ]
#
#----------------------------------------------------------------
# Copyright(C) 2018 Nate Dill
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
#----------------------------------------------------------------
#
SAMPLEDIR=""

if [[ $# < 6 ]]; then
   echo 1>&2  "Example usage: ` basename $0` --yyyy 2017 --stormnum 16 --advnum 12 [ --sampledir /home/nate/asgs/input/sample_advisories ] "  
exit 1
fi
# parse arguments
while [[ $# >1 ]]; do
   key=$1
   case $key in 
          --yyyy)
          YYYY="$2"
          shift
          ;;
          --stormnum)
          STORMNUM="$2"
          shift
          ;;
          --advnum)
          ADVNUM="$2"
          shift
          ;;
          --sampledir)
          SAMPLEDIR="$2"
          shift
          ;;
   esac
   shift
done

# the forecast xml file
if [[ $SAMPLEDIR == "" ]]; then 
   XMLFILE="$YYYY/$ADVNUM.${STORMNUM}${YYYY}.index-at.xml"
else
   XMLFILE="$SAMPLEDIR/$YYYY/$ADVNUM.${STORMNUM}${YYYY}.index-at.xml"
fi


# the best track atcf file
if [[ $SAMPLEDIR == "" ]]; then 
   BALFILE="$YYYY/$ADVNUM.bal${STORMNUM}${YYYY}.dat"
else
   BALFILE="$SAMPLEDIR/$YYYY/$ADVNUM.bal${STORMNUM}${YYYY}.dat"
fi

if [[ ! -f $XMLFILE ]]; then
   echo &>2 "Cannot find advisory sample index-at.xml file $XMLFILE"
   exit 1
fi
if [[ ! -f $BALFILE ]]; then
   echo &>2 "Cannot find advisory sample best track atcf file $XMLFILE"
   exit 1
fi

BAL=bal${STORMNUM}${YYYY}.dat
# remove old advisory files
if [[ -f index-at.xml ]]; then 
  echo "removing old index-at.xml"
  rm index-at.xml
fi
if [[ -f $BAL  ]]; then 
  echo "removing old $BAL"
  rm $BAL
fi

# create links to new advisory files
echo "creating symlinks to new index-at.xml and $BAL"
ln -s $XMLFILE index-at.xml
ln -s $BALFILE $BAL
