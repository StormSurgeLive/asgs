#!/bin/bash
#----------------------------------------------------------------------------
#                    S L U R M   D I R E C T I V E S
#----------------------------------------------------------------------------
#SBATCH -J %TASKTYPE%
#SBATCH -t %TASKTIMELIMITMINUTES%
#SBATCH -n 1
#SBATCH --array=1-%TOTALNUMTASKS%%%CONCURRENTNUMTASKS%
#SBATCH -o %STORMDIR%/%TASKTYPE%.out
#
#----------------------------------------------------------------------------
#        L O G   M E S S A G E S   T O   S T A R T   T H E   J O B
#----------------------------------------------------------------------------
if [[ $SLURM_ARRAY_TASK_ID -eq 1 ]]; then
   DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
   echo "[${DATETIME}] INFO: SLURM Job ID ${SLURM_JOBID}; SLURM submit directory ${SLURM_SUBMIT_DIR}; and SLURM submit host ${SLURM_SUBMIT_HOST}." 
   #
   hostname | tee CONTROL.TXT
   echo "Job Run on Nodes"  | tee --append CONTROL.TXT
   echo "----------------"  | tee --append CONTROL.TXT
   echo $SLURM_JOB_NODELIST | tee --append CONTROL.TXT
   echo "----------------"  | tee --append CONTROL.TXT
fi
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
#                  S E T   U P   T H E   J O B
#----------------------------------------------------------------------------
CONTOURDIR=%CONTOURDIR%
STORMDIR=%STORMDIR%
TASKTYPE=%TASKTYPE%  # this subdirectory must already exist for log files
taskID=`printf "%03d" $SLURM_ARRAY_TASK_ID` 
cd $STORMDIR 
#
task=1
taskFound=no
line=nullCommand
LAYER=nullLayer
#
# determine the targeted geographical area (asgs property); originates in mesh
# (fort.14) properties file 
asgs=`egrep 'asgs\s+' run.properties | sed 's/asgs.*://' | sed 's/\s//g'`
#
# determine the static datum offset; originates in nodal 
# attributes (fort.13) proprties file
sea_surface_height_above_geoid=`grep sea_surface_height_above_geoid run.properties | sed 's/sea_surface_height_above_geoid.*://' | sed 's/\s//g'`
#
# loop over all the files to find the one that this task is 
# supposed to process
for file in %FILELIST% ; do 
   timeStepStart=0 # CERA counts time steps starting from zero
   timeStepEnd=1
   if [[ -e ${file} ]]; then
      timeStepEnd=`ncdump -h ${file} | grep currently | egrep -o '[0-9]+'`
      #jgfdebug DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      #jgfdebug echo "[$DATETIME] INFO: There are $timeStepEnd time steps in the $file file" >> $TASKLOGFILESTART
      # CERA counts time steps starting at zero
      timeStepEnd=`expr $timeStepEnd - 1` 
   else
      #jgfdebug DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      #jgfdebug echo "[$DATETIME] INFO: The $file file does not exist." >> $TASKLOGFILESTART
      continue
   fi
   #
   # loop over all the timesteps in this file to find the one that 
   # this task is supposed to process
   timestep=$timeStepStart
   while [[ $timestep -le $timeStepEnd ]]; do 
      #
      # if the task number matches this SLURM task's ID, then this is
      # the file and timestep number to process; form the log file name 
      # and the command to execute 
      if [[ $SLURM_ARRAY_TASK_ID -eq $task ]]; then
         taskFound=yes
         fnum=`printf "%03d" $timestep`
         case $file in 
         "fort.74.nc")
            LAYER=wvel
            line="python ${CONTOURDIR}/cera_contour.py -i $file -t $timestep %WRITESHP% -o ${LAYER}${fnum}" 
            ;;
         "fort.63.nc")
            LAYER=elev
            line="python ${CONTOURDIR}/cera_contour.py -i $file -t $timestep %WRITESHP% -o ${LAYER}${fnum}" 
            ;;
         "swan_HS.63.nc")
            LAYER=hsign
            line="python ${CONTOURDIR}/cera_contour.py -i $file -t $timestep %WRITESHP% -o ${LAYER}${fnum}"        
            ;;
         "swan_TPS.63.nc")
            LAYER=tps
            line="python ${CONTOURDIR}/cera_contour.py -i $file -t $timestep %WRITESHP% -o ${LAYER}${fnum}"        
            ;;
         "maxele.63.nc")
            LAYER=maxelev
            line="python ${CONTOURDIR}/cera_contour_max.py -i $file -a $asgs %IFSTORM% -h $sea_surface_height_above_geoid" 
            ;;
         "maxwvel.63.nc")
            LAYER=maxwvel
            line="python ${CONTOURDIR}/cera_contour_max.py -i $file -a $asgs %IFSTORM% -h $sea_surface_height_above_geoid"
            ;;
         "swan_HS_max.63.nc")
            LAYER=maxhsign
            line="python ${CONTOURDIR}/cera_contour_max.py -i $file -a $asgs %IFSTORM% -h $sea_surface_height_above_geoid"
            ;;
         "swan_TPS_max.63.nc")
            LAYER=maxtps
            line="python ${CONTOURDIR}/cera_contour_max.py -i $file -a $asgs %IFSTORM% -h $sea_surface_height_above_geoid"
            ;;
         *)
            DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
            echo "[$DATETIME] ERROR: Files named $file are not supported."
            taskFound=no
            break
            ;;
         esac 
         TASKLOGFILEBASE=./$TASKTYPE/${LAYER}${taskID}
         # the .held files should already exist 
         if [[ ! -e ${TASKLOGFILEBASE}.held ]]; then
            DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
            echo "[$DATETIME] WARNING: ${TASKLOGFILEBASE}.held file did not exist." > ${TASKLOGFILEBASE}.held
         fi
         mv ${TASKLOGFILEBASE}.held ${TASKLOGFILEBASE}.start
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
         echo "[$DATETIME] INFO: Task $SLURM_ARRAY_TASK_ID started." >> ${TASKLOGFILEBASE}.start
         echo $line >> ${TASKLOGFILEBASE}.start
         break
      fi
      timestep=`expr $timestep + 1`
      task=`expr $task + 1`
   done
   if [[ $taskFound = yes ]]; then
      break
   fi
done
# 
# if this task has gone through all the files in the list, 
# and all the timesteps in those files, and still not found a task number 
# that matches this task's SLURM ID, then there are too many 
# SLURM tasks, so just exit gracefully
if [[ $taskFound = no ]]; then
   exit
fi
# 
#----------------------------------------------------------------------------
#                  E X E C U T E   T H E   J O B
#----------------------------------------------------------------------------
$line >> ${TASKLOGFILEBASE}.start 2>&1
ERROVALUE=$?  # capture exit status
ERROMSG=""
if [ $ERROVALUE == 0 ] ; then
   RUNSUFFIX="finish"
else
   RUNSUFFIX="error"
fi
mv ${TASKLOGFILEBASE}.start ${TASKLOGFILEBASE}.${RUNSUFFIX}
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] INFO: Task ${taskID} finished with return value = $ERROVALUE" >> ${TASKLOGFILEBASE}.${RUNSUFFIX}
if [ $ERROVALUE != 0 ]; then
   echo "[${DATETIME}] ERROR: $ERROMSG" >> ${TASKLOGFILEBASE}.${RUNSUFFIX}
fi
