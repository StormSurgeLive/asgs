#!/bin/bash
#
#----------------------------------------------------------------------------
#                    S L U R M   D I R E C T I V E S
#----------------------------------------------------------------------------
#SBATCH -J %jobtype%.%enstorm%
#SBATCH -t %wallminutes%
#SBATCH -n 1 
#jgf20161003: commented out per Chris Rutledge #SBATCH -c 1
#SBATCH --partition=ncfs
#SBATCH -o %advisdir%/%enstorm%/%jobtype%.%enstorm%.out
#
#----------------------------------------------------------------------------
#        L O G   M E S S A G E S   T O   S T A R T   T H E   J O B
#----------------------------------------------------------------------------
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] INFO: adcprep.%jobtype%.slurm: Executing %jobtype%.%enstorm% in %advisdir%/%enstorm% with SLURM Job ID ${SLURM_JOBID}; SLURM submit directory ${SLURM_SUBMIT_DIR}; and SLURM submit host ${SLURM_SUBMIT_HOST}." | tee --append %jobtype%.${SLURM_JOBID}.adcprep.log %syslog% %jobtype%.%enstorm%.run.start
#
hostname | tee CONTROL.TXT
echo "Job Run on Nodes"  | tee --append CONTROL.TXT
echo "----------------"  | tee --append CONTROL.TXT
echo $SLURM_JOB_NODELIST | tee --append CONTROL.TXT
echo "----------------"  | tee --append CONTROL.TXT
#
# log the command used to run adcprep
CMD="srun %adcircdir%/adcprep --np %ncpu% --%jobtype%"
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] INFO: adcprep.%jobtype%.slurm: Executing %jobtype%.%enstorm% with the command $CMD" | tee --append %jobtype%.${SLURM_JOBID}.adcprep.log %syslog%
#
#----------------------------------------------------------------------------
#                      L O A D   M O D U L E S
#----------------------------------------------------------------------------
module load intelc/14.0.3
module load intelfort/14.0.3
module load mvapich2/2.0_intel-14.0.3_ch3_ofed-3.1
module load netcdf/4.1.3_intel-14.0.3
#
#----------------------------------------------------------------------------
#                  E X E C U T E   T H E   J O B
#----------------------------------------------------------------------------
cd %advisdir%/%enstorm%
$CMD >> %jobtype%.${SLURM_JOBID}.adcprep.log 2>&1
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
echo "[${DATETIME}] INFO: adcprep finished in %advisdir%/%enstorm% with return value = $ERROVALUE" > %jobtype%.%enstorm%.run.${RUNSUFFIX}
echo "adcprep job finished at $DATETIME."
