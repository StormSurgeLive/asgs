#!/bin/sh
#------------------------------------------------------------------------
# extractNodalAttributesPropertis.sh : Extract and report nodal attributes
# properties for use in run.properties file.  
#------------------------------------------------------------------------
# Copyright(C) 2016 Jason Fleming
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
NODALATTRIBUTESFILE=$1
CONTROLFILE=$2
#
# record the default value of the sea_surface_height_above_geoid 
# nodal attribute to the nodal attributes properties file
isUsed=`grep -c sea_surface_height_above_geoid $CONTROLFILE`
if [ $isUsed = 0 ]; then
   # this nodal attribute is not being used; report this to run.properties file
   echo "sea_surface_height_above_geoid : null" 
else
   # get the line number where the start of this nodal attribute is specified
   # in the header of the fort.13 (nodal attributes) file
   linenum=`grep --line-number --max-count 1 sea_surface_height_above_geoid $NODALATTRIBUTESFILE | awk 'BEGIN { FS=":" } { print $1 }'`
   # get the actual default value, which is specified three lines after the
   # the name of the nodal attribute in the fort.13 header
   datumOffsetDefaultValueLine=`expr $linenum + 3`
   # get the default value from the correct line and strip off 
   # dos line ending characters, if any
   datumOffsetDefaultValue=`awk -v linenum=$datumOffsetDefaultValueLine 'NR==linenum { print $0 }' $NODALATTRIBUTESFILE | sed $'s/\r//'`
   # check to see if the datum offset is zero
   # TODO: This does not handle the case where the datum offset is variable
   if [ `echo "$datumOffsetDefaultValue==0.0" | bc -l` = 1 ]; then 
      echo "sea_surface_height_above_geoid : null"
   else 
      echo "sea_surface_height_above_geoid : $datumOffsetDefaultValue"
   fi
fi
