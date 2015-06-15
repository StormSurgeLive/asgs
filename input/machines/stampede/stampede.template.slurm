#!/bin/bash

#SBATCH -J %jobtype%.%enstorm% # job name
#SBATCH -t %walltime%          # wall time (hh:mm:ss)
#SBATCH -p %queuename%         # queue name e.g. large
#SBATCH -n %ncpu%              # number of mpi tasks
#SBATCH -N %nnodes%            # number of nodes requested
#SBATCH -o %advisdir%/%enstorm%/%jobtype%.%enstorm%.out
#SBATCH -e %advisdir%/%enstorm%/%jobtype%.%enstorm%.out
#SBATCH -A %account%           # Allocation name to charge job against

cd %advisdir%/%enstorm%

echo "Job Run on Nodes"
echo "----------------"
echo $SLURM_JOB_NODELIST
echo "----------------"

hostname > "CONTROL.TXT"
echo "Job Run on Nodes" >> "CONTROL.TXT"
echo "----------------" >> "CONTROL.TXT"
echo $SLURM_JOB_NODELIST >> "CONTROL.TXT"
echo "----------------" >> "CONTROL.TXT"

hostname

CMD="ibrun %adcircdir%/%jobtype% %cloptions%"
echo "%enstorm% job starting at `date`"
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
RUNSUFFIX="start"
echo $CMD 
echo "[${DATETIME}] : %jobtype%.%enstorm% starting in %advisdir%/%enstorm%." > %jobtype%.%enstorm%.run.${RUNSUFFIX}
$CMD > %advisdir%/%enstorm%/%jobtype%.%enstorm%.stdout.stderr 2>&1
ERROVALUE=$?
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
if [ $ERROVALUE == 0 ] ; then
   RUNSUFFIX="finish"
else
   RUNSUFFIX="error"
fi
echo "[${DATETIME}] : %jobtype%.%enstorm% finished in %advisdir%/%enstorm% with return value = $ERROVALUE" > %jobtype%.%enstorm%.run.${RUNSUFFIX}
echo "%jobtype%.%enstorm% job finished at $DATETIME."
