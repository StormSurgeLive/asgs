#!/bin/bash
#----------------------------------------------------------------
# captureJobID.sh: Do the right thing on this platform to 
# capture the job ID from stdout from a successful batch
# job submision. 
#----------------------------------------------------------------
# Copyright(C) 2021 Jason Fleming
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
THIS=$(basename -- $0)
HPCENVSHORT=$1

# This script expects to be executed in the directory where
# the queue script was submitted, and the file "jobID" has
# been used to capture stdout. 
case $HPCENVSHORT in
"stampede2"|"frontera")
   # lots of info here that we don't need, but has
   # been appended to the scenario.log file
   grep 'batch job' jobID | grep -Eo [0-9]+ > jobID
   ;;
*)
   # on queenbee2 at least, the jobID file contains only
   # the jobID, so nothing needs to be done. 
   ;;
esac
