#!/bin/bash
#-----------------------------------------------------------------------
# createOPeNDAPFileList.sh : Construct a list of the files that
# should be posted to OPeNDAP and write the list to run.properties.
#-----------------------------------------------------------------------
# Copyright(C) 2018--2024 Jason Fleming
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
#-----------------------------------------------------------------------
#
THIS=$(basename -- $0)

# this is just the snipped that is in the Issue, this should actually be done
# using perl and Template Toolkit

echo "<h1>Output Files</h1>" > ./index.html;
ls -1 | awk 'BEGIN{print "<ul>"} {printf "<li><a href=\"%s\">%s</a></li>\n",$0,$0} END{print "</ul>"}' >> ./index.html 2>&1;
echo "<h1>run.properties</h1><pre>" >> ./index.html; cat ./run.properties >> ./index.html;
echo "</pre>" >> ./index.html
