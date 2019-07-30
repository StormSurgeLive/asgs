#!/bin/bash
#SBATCH -J %jobtype%.%enstorm%          # Job name
#SBATCH -o %advisdir%/%enstorm%/%jobtype%.%enstorm%_%j.out # Name of stdout output file (%j expands to jobId)
#SBATCH -p %queuename%                  # Queue name
#SBATCH --partition=%partition%
#SBATCH --reservation=%reservation%
#SBATCH --constraint=%constraint%
#SBATCH -N 1                            # Total number of nodes requested
#SBATCH -n 1                            # Total number of mpi tasks requested
#SBATCH -t %walltime%                   # Run time (hh:mm:ss) - 1.5 hours
#SBATCH -A %account%                    # Project/allocation name
#SBATCH --mail-user=%notifyuser%
#
echo "%enstorm% adcprep job script starting at `date`"
cd %advisdir%/%enstorm%
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] : %jobtype%.%enstorm% starting in %advisdir%/%enstorm%" > %jobtype%.%enstorm%.run.start
if [[ $USER = "jgflemin" ]]; then
   export PATH=$WORK/local/bin:$PATH
   export LD_LIBRARY_PATH=$WORK/local/lib:$LD_LIBRARY_PATH
fi
echo "PATH is $PATH" | tee --append %syslog%
echo "LD_LIBRARY_PATH is $LD_LIBRARY_PATH" | tee --append %syslog%
%adcircdir%/adcprep --np %ncpu% --%jobtype% --strict-boundaries >> adcprep.log 2>&1
ERROVALUE=$?
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
if [ $ERROVALUE == 0 ] ; then
   RUNSUFFIX="finish"
else
   RUNSUFFIX="error"
fi
echo "[${DATETIME}] : adcprep finished in %advisdir%/%enstorm% with return value = $ERROVALUE" > %jobtype%.%enstorm%.run.${RUNSUFFIX}
echo "%enstorm% adcprep job script finished at $DATETIME."
