#!/bin/bash
#--------------------------------------------------------------------------
# Extract_latlon.sh 
#--------------------------------------------------------------------------
# Generates a track file for FigureGen from NWS=20 fort.22 file.
#--------------------------------------------------------------------------
# 
# Copyright(C) 2018 Matthew V Bilskie
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

INPUT=$1
OUTPUT=$2
IFS=,

[ -f $OUTPUT ] && rm $OUTPUT

[ ! -f $INPUT ] && { echo "$INPUT file not found"; exit 99; }
while read f1 f2 f2 f3 f4 f5 f6 f7 f8 f9
do
   slat=${f6%?}
   slon=${f7%?}

   lat="$(echo $slat/10|bc -l)"
   lon="$(echo $slon/10|bc -l)"
   lon="$(echo $lon*-1|bc -l)"

   echo $(printf "%0.2f\t%0.2f\n" $lon $lat) >> $OUTPUT

done < $INPUT
#--------------------------------------------------------------------------
