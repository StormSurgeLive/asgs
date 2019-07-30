#!/bin/bash
#SBATCH -J %jobtype%.%enstorm%   # Job name
#SBATCH -o %advisdir%/%enstorm%/%jobtype%.%enstorm%_%j.out  # Name of stdout output file
#SBATCH -p %queuename%   # Queue name
#SBATCH -N %nnodes%      # Total number of nodes requested
#SBATCH -n %ncpu%        # Total number of mpi tasks requested
#SBATCH --partition=%partition%
#SBATCH --reservation=%reservation%
#SBATCH --constraint=%constraint%
#SBATCH -t %walltime%    # Run time (hh:mm:ss) - 1.5 hours
# The next line is required if the user has more than one project
#SBATCH -A %account%     # Project/allocation number
#SBATCH --mail-user=%notifyuser%

#Note:  ibrun does not bind tasks or threads by default
#       To bind task/threads to sockets or cores, you must use
#       ibrun with tacc_affinity or use the "srun" command directly
#       with the "--cpu_bind" option.

echo "%enstorm% %jobtype% job script starting at `date`"
cd %advisdir%/%enstorm%
if [[ $USER = "jgflemin" ]]; then
   export PATH=$WORK/local/bin:$PATH
   export LD_LIBRARY_PATH=$WORK/local/lib:$LD_LIBRARY_PATH
fi
echo "PATH is $PATH" | tee --append %syslog%
echo "LD_LIBRARY_PATH is $LD_LIBRARY_PATH" | tee --append %syslog%
CMD="ibrun %adcircdir%/%jobtype% %cloptions%"
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
RUNSUFFIX="start"
echo $CMD
echo "[${DATETIME}] : %jobtype%.%enstorm% starting in %advisdir%/%enstorm%" > %jobtype%.%enstorm%.run.start
$CMD
ERROVALUE=$?
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
if [ $ERROVALUE == 0 ] ; then
   RUNSUFFIX="finish"
else
   RUNSUFFIX="error"
fi
echo "[${DATETIME}] : %jobtype%.%enstorm% finished in %advisdir%/%enstorm% with return value = $ERROVALUE" > %jobtype%.%enstorm%.run.${RUNSUFFIX}
echo "%jobtype%.%enstorm% job script finished at $DATETIME."
