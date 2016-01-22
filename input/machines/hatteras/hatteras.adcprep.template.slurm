#!/bin/bash

#SBATCH -J %jobtype%.%enstorm%
#SBATCH -t %wallminutes%
#SBATCH -n 1 
#SBATCH -c 1
#SBATCH --partition=ncfs

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
#module load intelc/11.1_046
#module load intelfort/11.1_046
#module load mvapich2/1.9_intel-11.1_046
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/projects/ncfs/apps/croatan/netcdf/lib
module load intelc/14.0.3
module load intelfort/14.0.3
#module load mvapich2/2.0_intel-14.0.3_nemesis
module load mvapich2/2.0_intel-14.0.3_ch3_ofed-3.1
module load netcdf/4.1.3_intel-14.0.3
#
echo "%enstorm% job starting at `date`"
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] : %jobtype%.%enstorm% starting in %advisdir%/%enstorm%" > %jobtype%.%enstorm%.run.start
srun %adcircdir%/adcprep --np %ncpu% --%jobtype% >> adcprep.log 2>&1
ERROVALUE=$?
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
if [ $ERROVALUE == 0 ] ; then
   RUNSUFFIX="finish"
else
   RUNSUFFIX="error"
fi
echo "[${DATETIME}] : adcprep finished in %advisdir%/%enstorm% with return value = $ERROVALUE" > %jobtype%.%enstorm%.run.${RUNSUFFIX}
echo "adcprep job finished at $DATETIME."
