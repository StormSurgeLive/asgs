#!/bin/bash
#
#----------------------------------------------------------------------------
#                    S L U R M   D I R E C T I V E S
#----------------------------------------------------------------------------
#SBATCH -J cera_contour
#SBATCH -t %PARALLELPOSTWALLTIME%
#SBATCH -n 1
#SBATCH --partition=ncfs
#SBATCH --reservation=ncfs
#SBATCH --array=%ARRAYRANGE%
#SBATCH -o %STORMDIR%/cera_contour.out
#
#----------------------------------------------------------------------------
#        L O G   M E S S A G E S   T O   S T A R T   T H E   J O B
#----------------------------------------------------------------------------
case $SLURM_ARRAY_TASK_ID in
   1)
   mkdir $STORMDIR/cera_contour
   DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
   echo "[${DATETIME}] INFO: SLURM Job ID ${SLURM_JOBID}; SLURM submit directory ${SLURM_SUBMIT_DIR}; and SLURM submit host ${SLURM_SUBMIT_HOST}." 
   #
   hostname | tee CONTROL.TXT
   echo "Job Run on Nodes"  | tee --append CONTROL.TXT
   echo "----------------"  | tee --append CONTROL.TXT
   echo $SLURM_JOB_NODELIST | tee --append CONTROL.TXT
   echo "----------------"  | tee --append CONTROL.TXT

   ;;
   *)
   # all other tasks don't need to write these log messages
   ;;
esac
#
#----------------------------------------------------------------------------
#                      L O A D   M O D U L E S
#----------------------------------------------------------------------------
module load intelc/14.0.3  
module load intelfort/14.0.3
module load netcdf/4.1.3_intel-14.0.3
module load mvapich2/2.0_intel-14.0.3_ch3_ofed-3.1  
# for cera_contour.py
module load gdal/1.11.1_gcc
source /projects/ncfs/apps/kalpana/env/bin/activate
# 
#----------------------------------------------------------------------------
#                  E X E C U T E   T H E   J O B
#----------------------------------------------------------------------------
cd %STORMDIR%
CONTOURDIR=%CERADIR%
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
taskID=`printf "%03d" $SLURM_ARRAY_TASK_ID` 
echo "$DATETIME start" > ./cera_contour/task${taskID}.log
#
task=1
for file in fort.74.nc fort.63.nc ; do 
   timeStepStart=1
   timeStepEnd=1
   if [[ -e ${file} ]]; then
      timeStepEnd=`ncdump -h ${file} | grep currently | egrep -o '[0-9]+'`
   else
      break
   fi
   #
   timestep=$timestepStart
   while [[ $timestep -le $timestepEnd ]]; do 
      fnum=`printf "%03d" $timestep`
      case $file in 
      "fort.74.nc")
         fname=wvel
         ;;
      "fort.63.nc")
         fname=elev
         ;;
      *)
         echo nothingtodo
         ;;
      esac 
      line="python ${CONTOURDIR}/cera_contour.py -i $file -t $timestep -s -o ./cera_contour/${fname}${fnum}" 
      if [[ $SLURM_ARRAY_TASK_ID -eq $task ]]; then
         $line >> ./task${taskID}.log
         ERROVALUE=$?  # capture exit status
         ERROMSG=""
         if [ $ERROVALUE == 0 ] ; then
            RUNSUFFIX="finish"
         else
            RUNSUFFIX="error"
         fi
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
         echo "[${DATETIME}] INFO: task${taskID} finished with return value = $ERROVALUE" >> ./cera_contour/task${taskID}.log
         if [ $ERROVALUE == 1 ]; then
            echo "[${DATETIME}] ERROR: $ERROMSG" >> ./cera_contour/task${taskID}.log
         fi
         exit
      fi
      timestep=`expr $timestep + 1`
      task=`expr $task + 1`
   done
done
#
#
