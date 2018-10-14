#!/bin/bash
#
#----------------------------------------------------------------------------
#                    S L U R M   D I R E C T I V E S
#----------------------------------------------------------------------------
#SBATCH -J %jobtype%.%enstorm%  # job name
#SBATCH -t %walltime%           # wall time (hh:mm:ss)
#SBATCH -p %queuename%          # queue name e.g. serial or normal or request
#SBATCH -n 1 
#SBATCH -N 1
#SBATCH -o %advisdir%/%enstorm%/%jobtype%.%enstorm%_%j.out
#SBATCH -A %account%           # Allocation name to charge job against
#SBATCH --mail-user=%notifyuser%
#
#----------------------------------------------------------------------------
#        L O G   M E S S A G E S   T O   S T A R T   T H E   J O B
#----------------------------------------------------------------------------
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] INFO: adcprep.%jobtype%.slurm: Executing %jobtype%.%enstorm% in %advisdir%/%enstorm% with SLURM Job ID ${SLURM_JOBID}; SLURM submit directory ${SLURM_SUBMIT_DIR}; and SLURM submit host ${SLURM_SUBMIT_HOST}." | tee --append %syslog% %jobtype%.%enstorm%.run.start
#
# log the command used to run adcprep
CMD="%adcircdir%/adcprep --np %ncpu% --%jobtype% --strict-boundaries"
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] INFO: adcprep.%jobtype%.slurm: Executing %jobtype%.%enstorm% with the command $CMD" | tee --append %syslog%
#
#----------------------------------------------------------------------------
#                      L O A D   M O D U L E S
#----------------------------------------------------------------------------
#module load netcdf
#
#----------------------------------------------------------------------------
#                  E X E C U T E   T H E   J O B
#----------------------------------------------------------------------------
cd %advisdir%/%enstorm%
$CMD 2>&1
# 
#----------------------------------------------------------------------------
#           C H E C K   S T A T U S   O F   R E S U L T S
#----------------------------------------------------------------------------
ERROVALUE=$?
# FIXME: insert code to detect failures in adcprep that still allow it to 
# finish with a normal exit code
if [ $ERROVALUE == 0 ] ; then
   RUNSUFFIX="finish"
else
   RUNSUFFIX="error"
fi
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] INFO: adcprep.%jobtype%.slurm: Finished in %advisdir%/%enstorm% with return value = $ERROVALUE" | tee --append %jobtype%.%enstorm%.run.${RUNSUFFIX}
