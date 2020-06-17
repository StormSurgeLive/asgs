#!/bin/bash
#
CONTOURDIR="/home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring"
if [[ -e job_array.conf ]]; then
   rm job_array.conf
fi
echo 'case $SLURM_ARRAY_TASK_ID in' >> job_array.conf
task=1
for file in fort.74.nc fort.63.nc ; do 
   if [[ $file = "fort.74.nc" ]]; then
      fname=wvel
   fi
   if [[ $file = "fort.63.nc" ]]; then
      fname=elev
   fi
   timestepStart=1
   timestepEnd=84
   #
   timestep=$timestepStart
   while [[ $timestep -le $timestepEnd ]]; do 
      echo "   $task )" >> job_array.conf
      fnum=`printf "%03d" $timestep`
      line="      python ${CONTOURDIR}/cera_contour.py -i $file -t $timestep -s -o ${fname}${fnum}" 
      echo $line >> job_array.conf 
      echo "   ;;" >> job_array.conf
      timestep=`expr $timestep + 1`
      task=`expr $task + 1`
   done
done
echo "   * )" >> job_array.conf
echo "   echo nothingtodo" >> job_array.conf
echo "   ;;"  >> job_array.conf
echo "esac"  >> job_array.conf
