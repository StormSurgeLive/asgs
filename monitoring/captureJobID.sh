#!/bin/bash
#----------------------------------------------------------------
# captureJobID.sh: Do the right thing on this platform to
# capture the job ID from stdout from a successful batch
# job submission.
#----------------------------------------------------------------
# Copyright(C) 2021--2024 Jason Fleming
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
"stampede3"|"frontera"|"ls6"|"queenbeeC"|"debian+slurm")
   # lots of info here that we don't need, but has
   # been appended to the scenario.log file
   # e.g. on qbc:
   # > sbatch partmesh.slurm
   # sbatch: 488040.79 SUs available in loni_ceraloni24  # to stderr
   # sbatch: 2.00 SUs estimated for this job.            # to stderr
   # sbatch: lua: Submitted job 568119                   # to stderr
   # Submitted batch job 568119                          # to stdout
   mv jobID jobID.tmp
   grep 'batch job' jobID.tmp | grep -Eo [0-9]+ > jobID
   rm jobID.tmp
   ;;
"mike"|"qbd"|"supermic")
   # SLURM returns information similar to the following when a
   # job is submitted:
   # <asgsh> sbatch prep13.slurm 2>stderr >stdout
   # <asgsh> cat stderr
   #  sbatch: Email specified differs from email associated with account. Using xxx@xxxx.com
   #  sbatch: Job estimates 2.00 SUs for -p single --nodes=1 --ntasks=1 --cpus-per-task=1
   #  sbatch: lua: Submitted job 274100
   # <asgsh> cat stdout
   #  Submitted batch job 274100
   mv jobID jobID.tmp
   cat jobID.tmp | tr -dc '0-9' > jobID
   rm jobID.tmp
   ;;
*)
   # on queenbee2 at least, the jobID file contains only
   # the jobID, so nothing needs to be done.
   ;;
esac
