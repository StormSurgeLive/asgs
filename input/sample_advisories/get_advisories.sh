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
ADVISORY=$1
MAXADVISORY=$2
STORMNUMBER=$3
YEAR=$4
BASIN=$5
#
FSTORMNUMBER=`printf "%02d" $STORMNUMBER`
#
while [[ $ADVISORY -le $MAXADVISORY ]]; do 
   file=`printf "$BASIN$FSTORMNUMBER$YEAR.fstadv.%03d.shtml" $ADVISORY` 
   echo $file 
   if [[ $YEAR -lt 2006 ]]; then
      curl http://www.nhc.noaa.gov/archive/$YEAR/mar/$file > $file
   else
      curl http://www.nhc.noaa.gov/archive/$YEAR/$BASIN$FSTORMNUMBER/$file > $file
   fi  
   ADVISORY=`expr $ADVISORY + 1`
done
