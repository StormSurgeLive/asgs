#!/bin/bash

#SBATCH -J %jobtype%.%enstorm%
#SBATCH -t %wallminutes%
#      #SBATCH -p %account%
#SBATCH -n %ncpu% 

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

# Load modules
module load intelc/11.1_046
module load intelfort/11.1_046
module load mvapich2/1.9_intel-11.1_046
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/projects/ncfs/apps/croatan/netcdf/lib
CMD="srun %adcircdir%/%jobtype% %cloptions%"
echo "%enstorm% job starting at `date`"
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
RUNSUFFIX="start"
echo $CMD
echo "${DATETIME}" > %jobtype%.%enstorm%.run.start
$CMD 
ERROVALUE=$?
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
if [ $ERROVALUE == 0 ] ; then
   RUNSUFFIX="finish"
else
   RUNSUFFIX="error"
fi
echo "[${DATETIME}] : %jobtype%.%enstorm% finished in %advisdir%/%enstorm% with return value = $ERROVALUE" > %jobtype%.%enstorm%.run.${RUNSUFFIX}
echo "%jobtype%.%enstorm% job finished at $DATETIME."
