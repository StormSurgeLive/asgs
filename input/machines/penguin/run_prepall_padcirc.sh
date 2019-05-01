#!/usr/bin/bash
#
# initializations
# 
# send SIGTERM to all processes in the process group (i.e., all child 
# processes and subshells) when this script receives SIGTERM or exits
trap 'kill - SIGTERM && kill -- -$$' SIGINT SIGTERM EXIT
declare -a subshellPIDs # list of process IDs of subshells 
declare -a logFiles     # list of log files to be tailed into run.log
THIS=run_prepall_padcirc.sh
SCRIPTDIR=~/asgs/jasonfleming/master
#
#
for jobtype in prepall padcirc ; do
   unset logFiles
   if [[ $jobtype = prepall ]]; then
      logFiles=(fort.6 fort.16)
   fi
   if [[ $jobtype = padcirc ]]; then
      logFiles=(fort.6 fort.16 adcirc.log )
   fi
   if [[ $jobtype = padcswan ]]; then
      logFiles=(fort.6 fort.16 adcirc.log PE0000/asgs_swan.prt PE0000/Errfile )
   fi
   #
   # initialize log files if they do not exist so tail doesn't exit immediately
   for file in ${logFiles[*]} ; do
      echo "Initializing $file file." | awk -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk >> run.log 2>&1
      if [[ -e $file ]]; then
         rm $file
      fi
      # make a zero length file
      touch $file
      # execute logs monitoring in the background
      (
         tail -f $file >> run.log 2>&1
      ) &
      # add this process ID to the list of background subshell jobs
      subshellPIDs+=($!)
   done
   #
   # submit the queue script (unless this is a re-run and the compute job
   # has already run)
   if [[ ! -e ${jobtype}.run.error && ! -e ${jobtype}.run.finish ]]; then
      JOBID=`qsub ${jobtype}.pbs`
      if [[ $? == 0 ]]; then
         echo "The qsub command reported that the JOBID of the $jobtype job is $JOBID."  | awk -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk >> run.log 2>&1
      else 
         echo "The qsub command for ${jobtype}.pbs failed." | awk -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk | tee ${jobtype}.run.error >> run.log 2>&1 
      fi
   fi
   #
   # wait for notification that the compute job has started
   while [[ ! -e ${jobtype}.run.start && ! -e ${jobtype}.run.error && ! -e ${jobtype}.run.finish ]]; do
      sleep 5
   done
   # job has started; launch logfile timestamping subshell process in background
   (
      while [[ ! -e ${jobtype}.run.error && ! -e ${jobtype}.run.finish ]]; do
         sleep 5
         echo "${jobtype}: Timer." | awk -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk >> run.log 2>&1
      done
   ) &
   subshellPIDs+=($!)
   #
   # wait for notification that the job is complete
   while [[ ! -e ${jobtype}.run.error && ! -e ${jobtype}.run.finish ]]; do
      sleep 10
   done
   echo "Detected end of $jobtype job." | awk -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk >> run.log
   #
   # terminate all previously spawned background log monitoring processes
   for pid in ${subshellPIDs[*]}; do 
      echo "Terminating previously spawned subshell process." | awk -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk >> run.log 2>&1
      kill -TERM $pid 2>&1 | awk -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk >> run.log 2>&1
   done
   unset subshellPIDs   
done
exit 0
