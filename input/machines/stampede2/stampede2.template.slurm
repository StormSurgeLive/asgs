#!/bin/bash
#SBATCH -J %jobtype%.%enstorm% # job name
#SBATCH -t %walltime%          # wall time (hh:mm:ss)
#SBATCH -p %queuename%         # queue name e.g. large
#SBATCH -n %ncpu%              # number of mpi tasks
#SBATCH -N %nnodes%            # number of nodes requested
#SBATCH -o %advisdir%/%enstorm%/%jobtype%.%enstorm%_%j.out
#SBATCH -A %account%           # Allocation name to charge job against
#SBATCH --mail-user=%notifyuser%
#
#----------------------------------------------------------------------------
#        L O G   M E S S A G E S   T O   S T A R T   T H E   J O B
#----------------------------------------------------------------------------
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] INFO: %jobtype%.slurm: Executing %jobtype%.%enstorm% in %advisdir%/%enstorm% with SLURM Job ID ${SLURM_JOBID}; SLURM submit directory ${SLURM_SUBMIT_DIR}; and SLURM submit host ${SLURM_SUBMIT_HOST}." | tee --append %syslog% %jobtype%.%enstorm%.run.start
#
# log the command used to run the job
CMD="ibrun %adcircdir%/%jobtype% %cloptions%"
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] INFO: %jobtype%.slurm: Executing %jobtype%.%enstorm%
 with the command $CMD" | tee --append %syslog% %jobtype%.%enstorm%.run.start
#
#----------------------------------------------------------------------------
#                      L O A D   M O D U L E S
#----------------------------------------------------------------------------
if [[ $USER = "jgflemin" ]]; then
   export PATH=$WORK/local/bin:$PATH
   export LD_LIBRARY_PATH=$WORK/local/lib:$LD_LIBRARY_PATH
fi
echo "PATH is $PATH" | tee --append %syslog%
echo "LD_LIBRARY_PATH is $LD_LIBRARY_PATH" | tee --append %syslog%
# 
#----------------------------------------------------------------------------
#                  E X E C U T E   T H E   J O B
#----------------------------------------------------------------------------
cd %advisdir%/%enstorm%
$CMD
# 
#----------------------------------------------------------------------------
#           C H E C K   S T A T U S   O F   R E S U L T S
#----------------------------------------------------------------------------
ERROMSG=""
ERROVALUE=$?  # capture exit status
if [ $ERROVALUE == 0 ] ; then
   # look for numerical instability errors in the stdout/stderr files
   for file in adcirc.log %advisdir%/%enstorm%/%jobtype%.%enstorm%_%j.out ; do
      if [ -e $file ]; then
         numMsg=`grep WarnElev $file | wc -l`
         if [ $numMsg = 0 ]; then
            DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
            echo "[${DATETIME}] INFO: %jobtype%.slurm: No numerical instability detected in $file after executing %jobtype%.%enstorm%." | tee --append %syslog%
         else
            ERROMSG="$ERROMSG Detected $numMsg numerical instability messages in $file."
            ERROVALUE=1
         fi
      fi
   done
else
   ERROMSG="The %jobtype%.%enstorm% job ended with an exit status that indicates an error occurred."
fi
#
if [ $ERROVALUE == 0 ] ; then
   RUNSUFFIX="finish"
else
   RUNSUFFIX="error"
fi
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] INFO: %jobtype%.slurm: %jobtype%.%enstorm% finished in %advisdir%/%enstorm% with return value = $ERROVALUE" | tee --append %jobtype%.%enstorm%.run.${RUNSUFFIX}
#
#  write reason for job failure
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
if [ $ERROVALUE == 1 ]; then
   echo "[${DATETIME}] ERROR: %jobtype%.slurm: $ERROMSG" | tee --append %syslog% %jobtype%.%enstorm%.run.${RUNSUFFIX}
fi
