#!/bin/bash
#----------------------------------------------------------------
# get_advisories.sh
#
# Grabs a whole set of forecast/advisories from the National 
# Hurricane Center corresponding to a particular storm.
#
#----------------------------------------------------------------
# Copyright(C) 2015 Jason Fleming
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
# Alternatively, here is a one liner that will copy the advisory 
# data from an ASGS execution ; downsides are that this will likely
# not capture a complete set and may include erroneously issued
# advisory data and advisories that do not include later corrections
# 
#BASIN=al ; STORMNUMBER=29 ; YEAR=2020 ; NAMELC=eta ; ID=$BASIN$STORMNUMBER$YEAR ; for dir in ?? ; do echo $dir ; for file in $ID.fst $ID.fst.html b${ID}.dat index-at.xml ; do cp $dir/$file $SCRIPTDIR/input/sample_advisories/$YEAR/${ID}_${NAMELC}/${dir}.${file} ; done ; done 
#
ADVISORY=$1
MAXADVISORY=$2
STORMNUMBER=$3
YEAR=$4
BASIN=$5 # e.g. lowercase al for Atlantic basin
#
FSTORMNUMBER=`printf "%02d" $STORMNUMBER`
#
while [[ $ADVISORY -le $MAXADVISORY ]]; do 
   urlRoot=https://www.nhc.noaa.gov/archive/$YEAR
   file=`printf "$BASIN$FSTORMNUMBER$YEAR.fstadv.%03d.shtml" $ADVISORY` 
   if [[ $YEAR -lt 2006 ]]; then
      urlRoot="$urlRoot/mar"
   fi
   if [[ $YEAR -ge 2006 ]]; then
      urlRoot="$urlRoot/$BASIN$FSTORMNUMBER"
   fi
   echo "curl -s $urlRoot/$file > $file"
   curl -s $urlRoot/$file > $file
   ADVISORY=`expr $ADVISORY + 1`
done
