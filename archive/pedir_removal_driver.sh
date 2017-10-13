#!/bin/bash
HOTTIFYPATH=$1
logFile="subdomaindir_removal_driver.log"
ageThresholdSec=2592000 # number of seconds in 30 days
rundir=$PWD # start in the directory containing all the asgsxxxxx directories
nowsec=`date +%s`
echo "Current time is $nowsec seconds." > $logFile
for instancedir in `ls -dtr asgs*`; do
   echo "instancedir is $instancedir" >> $logFile
   # operate on instance directories
   if [[ -d $instancedir ]]; then
      echo "Instance directory is $instancedir" >> $logFile
      # check last modification time on the directory
      lastModTimeSec=`date +%s -r $instancedir`
      echo "lastModTimeSec is $lastModTimeSec" >> $logFile
      # determine how long ago the directory was last modified
      ageSec=`expr $nowsec - $lastModTimeSec`
      echo "ageSec is $ageSec" >> $logFile
      # if longer than the threshold, then clean out the subdomain directories
      if [[ $ageSec -gt $ageThresholdSec ]]; then
         cd $instancedir
         echo "must clean out the directory $instancedir" >> ../$logFile
         ../subdomaindir_removal.sh $HOTTIFYPATH
         cd ..
      else
         echo "the directory $instancedir is too recent" >> $logFile
      fi
   fi
done
