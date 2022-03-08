#!/usr/bin/env bash
#------------------------------------------------------------------------
# properties.sh : Reads run.properties file into associative array.
#------------------------------------------------------------------------
# Copyright(C) 2019 Jason Fleming
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
loadProperties () {
propertiesFile=$1
while read -r keyValuePair ; do 
   colonPosition=`expr index "$keyValuePair" ":" `
   key=${keyValuePair:0:$colonPosition-1}
   value=${keyValuePair:$colonPosition}
   # remove leading whitespace characters from key
   key="${key#"${key%%[![:space:]]*}"}" 
   # remove trailing whitespace characters from key
   key="${key%"${key##*[![:space:]]}"}"
   # remove leading whitespace characters from value
   value="${value#"${value%%[![:space:]]*}"}"
   # remove trailing whitespace characters from value
   value="${value%"${value##*[![:space:]]}"}"
   properties["$key"]="$value" 
done < $propertiesFile
}
