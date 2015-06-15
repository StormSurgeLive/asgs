#!/bin/bash

#SBATCH -J %jobtype%.%enstorm%  # job name
#SBATCH -t %walltime%           # wall time (hh:mm:ss)
#SBATCH -p serial               # queue name e.g. serial or normal
#SBATCH -n 1 
#SBATCH -N 1
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

module load netcdf/4.3.2

echo "%jobtype%.%enstorm% job starting at `date`"
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] : %jobtype%.%enstorm% starting in %advisdir%/%enstorm%" > %jobtype%.%enstorm%.run.start
%adcircdir%/adcprep --np %ncpu% --%jobtype% >> adcprep.log 2>&1
ERROVALUE=$?
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
if [ $ERROVALUE == 0 ] ; then
   RUNSUFFIX="finish"
else
   RUNSUFFIX="error"
fi
echo "[${DATETIME}] : adcprep finished in %advisdir%/%enstorm% with return value = $ERROVALUE" > %jobtype%.%enstorm%.run.${RUNSUFFIX}
echo "adcprep job finished at $DATETIME."
