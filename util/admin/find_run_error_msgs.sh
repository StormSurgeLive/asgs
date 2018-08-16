#!/bin/bash
#------------------------------------------------------------------------
# find_run_error_msgs.sh : looks for error messages in adcirc log files.
#------------------------------------------------------------------------
# Copyright(C) 2018 Jason Fleming
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
# This script assumes it is executed in an asgs$PID directory. It looks
# for error messages in $jobtype.$enstorm.out (e.g., padcirc.nowcast.out)
# files and reports the job ID associated with them. 
#------------------------------------------------------------------------
#
for advisdir in `ls -d *`; do
   if [ -d $advisdir ]; then
      for stormdir in `ls $advisdir`; do
         if [ -d ${advisdir}/${stormdir} ]; then
            for jobtype in padcirc padcswan; do
               outfile=${advisdir}/${stormdir}/${jobtype}.${stormdir}.out
               if [[ -e $outfile ]]; then
                  errMsgCount=`grep MPI_Abort $outfile | wc -l`
                  if [[ $errMsgCount != 0 ]]; then
                     msg=`grep MPI_Abort $outfile`
                     jobid=`grep -o 'Job ID [0-9]\{1,\}' ${advisdir}/${stormdir}/${jobtype}.${stormdir}.run.start | sed "s/Job ID //g"`
                     echo "Found the message \"${msg}\" in $PWD/${outfile} for job ID ${jobid}."
                  fi
               fi
            done
         fi
      done
   fi
done
