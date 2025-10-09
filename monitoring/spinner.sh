#!/bin/bash

#----------------------------------------------------------------
# spinner.sh: Adds an animated spinner to show that a process
# is still runnning.
#----------------------------------------------------------------
# Copyright(C) 2025 Jason Fleming
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
spinner()
{
   # $1 is the time limit in seconds to spin
   #    (0 to spin forever or until associated process exits)
   # $2 is the (optional) process ID to wait on
   #    (if both pid and time limit were provided, and
   #     time limit is exceeded before the process exits,
   #     this function returns an error code for the calling
   #     routine to interpret and deal with)
   local spin='-\|/'
   local i=0
   local j=0
   while [[ $j -le $1 ]]; do
      i=$(( (i+1) %4 ))
      printf "\b${spin:$i:1}" # to the console
      sleep 1
      # if there is a process ID, and the associated process
      # has finished, break out of the loop
      if [[ ! -z $2 ]]; then
         if ! kill -0 $2 >> /dev/null 2>&1 ; then
            return 0  # process we were waiting on has exited
         fi
         if [[ $1 -eq 0 ]]; then
            # wait indefinitely for process to end
            j=$(( (j-1) ))
         fi
      fi
      j=$(( (j+1) ))
   done
   # time limit has been reached; return success unless
   # process ID was also provided
   if [[ ! -z $2 ]]; then
      return 1  # process we are waiting on is still running
   else
      return 0  # we successfully waited for the right amount of time
   fi
}
