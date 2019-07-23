#!/bin/bash
#----------------------------------------------------------------
#
# submitJob.sh: Reads run.properties file for a particular job type
# and its properties and submits that job to the queue.
#
#----------------------------------------------------------------
# Copyright(C) 2018 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------
#
# Submits a job to the local queueing system, assuming that it is 
# starting in the directory where the run.properties file is located
# and that all required properties use full paths rather than 
# relative paths.
#
#----------------------------------------------------------------
#       R E A D   J O B   P R O P E R T I E S
#----------------------------------------------------------------
# JOBTYPE: padcirc, padcswan, prep15, prep20, prepall, partmesh, etc
# This will be used to look up additional properties needed to submit
# this job. 
JOBTYPE=$1
# SYSLOG: asgs log file
SYSLOG=`sed -n 's/[ ^]*$//;s/file.syslog\s*:\s*//p' run.properties`
# SCRIPTDIR: path to asgs scripts like asgs_main.sh
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' run.properties`
# pull in logging functions
. ${SCRIPTDIR}/monitoring/logging.sh
#
# HPCENVSHORT: shorthand for the HPC environment: queenbee, hatteras, etc
HPCENVSHORT=`sed -n 's/[ ^]*$//;s/hpc.hpcenvshort\s*:\s*//p' run.properties`
# QUEUESYS: type of queueing system, e.g., SLURM, PBS, etc
QUEUESYS=`sed -n 's/[ ^]*$//;s/hpc.queuesys\s*:\s*//p' run.properties`
# PPN: processors (or cores) per node to specify for the job 
PPN=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.ppn\s*:\s*//p" run.properties`
# ADCIRDIR: path to adcirc executables e.g. ~/adcirc/work 
ADCIRCDIR=`sed -n 's/[ ^]*$//;s/path.adcircdir\s*:\s*//p' run.properties`
# SWANDIR: path to swan executables (i.e., to unhcat.exe)
SWANDIR=`sed -n 's/[ ^]*$//;s/path.swandir\s*:\s*//p' run.properties`
# STORMDIR: path where this ensemble member is supposed to run 
STORMDIR=`sed -n 's/[ ^]*$//;s/path.stormdir\s*:\s*//p' run.properties`
ADVISDIR=`sed -n 's/[ ^]*$//;s/path.advisdir\s*:\s*//p' run.properties`
# ENSTORM: name of this ensemble member
ENSTORM=`sed -n 's/[ ^]*$//;s/scenario\s*:\s*//p' run.properties`
# NOTIFYUSER: email address to put into the queue script that the queueing
# system will send email to if the hpc job fails
NOTIFYUSER=`sed -n "s/[ ^]*$//;s/notification.hpc.email.notifyuser\s*:\s*//p" run.properties`
# QSTDIR: template to use when generating queue script
QSTDIR=`sed -n "s/[ ^]*$//;s/hpc.path.${JOBTYPE}.template.qstdir\s*:\s*//p" run.properties`
# QSTEMPLATE: template to use when generating queue script
QSTEMPLATE=`sed -n "s/[ ^]*$//;s/hpc.file.${JOBTYPE}.template.qstemplate\s*:\s*//p" run.properties`
# NCPU: number of cores to request for this job
NCPU=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.ncpu\s*:\s*//p" run.properties`
# QUEUENAME: name of parallel queue to use on this platform
QUEUENAME=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.queuename\s*:\s*//p" run.properties`
# SERQUEUE: name of serial queue to use on this platform
SERQUEUE=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.serqueue\s*:\s*//p" run.properties`
# ACCOUNT: the account the job should be billed to
ACCOUNT=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.account\s*:\s*//p" run.properties`
# WALLTIME: limit of wall clock time for the job to run before being
# kicked out of the queue
WALLTIME=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.limit.walltime\s*:\s*//p" run.properties`
# JOBMODULES: command line to be executed to load resources specific to this job
JOBMODULES=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.jobmodules\s*:\s*//p" run.properties`
# JOBPATHS: command line to be executed to set paths to resources for this job
JOBPATHS=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.jobpaths\s*:\s*//p" run.properties`
# JOBLIBS: command line to be executed to set paths to libraries for this job
JOBLIBS=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.joblibs\s*:\s*//p" run.properties`
# PLATFORMMODULES: command line to be executed to load resources specific to this platform
PLATFORMMODULES=`sed -n "s/[ ^]*$//;s/hpc.platformmodules\s*:\s*//p" run.properties`
# SUBMITSTRING: command used to submit jobs to the queue
SUBMITSTRING=`sed -n "s/[ ^]*$//;s/hpc.submitstring\s*:\s*//p" run.properties`
# JOBLAUNCHER: command used inside a queue script to start a job
JOBLAUNCHER=`sed -n "s/[ ^]*$//;s/hpc.joblauncher\s*:\s*//p" run.properties`
# CMD: command line to be executed via queueing system
CMD=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.cmd\s*:\s*//p" run.properties`
#----------------------------------------------------------------
#     S E T   U P   J O B   C H A R A C T E R I S T I C S
#----------------------------------------------------------------
THIS="submitJob.sh"
LOGFILE=$STORMDIR/${JOBTYPE}.log
cd $STORMDIR 2>> $LOGFILE
echo "asgs.submitjob.${JOBTYPE}.pid : $$" >> ${STORMDIR}/run.properties 
#
CLOPTIONS=""     # command line options
LOCALHOTSTART=""
CPUREQUEST=$NCPU   
#
# deteremine command line options specific to padcirc and padcswan jobs
if [[ $JOBTYPE = padcirc || $JOBTYPE = padcswan ]]; then
   # NUMWRITERS: the number of dedicated writer processors for a padcirc
   # or padcswan job
   NUMWRITERS=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.numwriters\s*:\s*//p" run.properties`
   if [[ $NUMWRITERS != "0" ]]; then
      CLOPTIONS="-W $NUMWRITERS" # for command line execution with mpiexec
      CPUREQUEST=`expr $NCPU + $NUMWRITERS`
   fi
   # HOTSTARTCOMP: whether padcirc or padcswan should write subdomain 
   # hotstart files or fulldomain hotstart files (fulldomain is the default)
   HOTSTARTCOMP=`sed -n "s/[ ^]*$//;s/adcirc.hotstartcomp\s*:\s*//p" run.properties`
   if [[ $HOTSTARTCOMP = subdomain ]]; then
      CLOPTIONS="${CLOPTIONS} -S" # for command line execution with mpiexec
      LOCALHOTSTART="--localhotstart" 
   fi
fi
# convert HH:MM:SS wall time to integer minutes
WALLMINUTES=`echo "${WALLTIME:0:2} * 60 + ${WALLTIME:3:2} + 1" | bc` 
# compute number of nodes to request using processors per node (PPN)
NNODES=`python -c "from math import ceil; print int(ceil(float($CPUREQUEST)/float($PPN)))"`
#--------------------------------------------------------------------------
#       F I L L   I N   Q U E U E   S C R I P T   T E M P L A T E 
#--------------------------------------------------------------------------
# Any variables that contain forward slashes ("/") have to have the forward
# slashes escaped with a backslash so they don't confuse sed, 
# e.g., ${STORMDIR//\//\\/}
# 
case $QUEUESYS in
"PBS" | "SLURM" )
   # form queue script file name with downcased queueing system as suffix
   QSFILE=${JOBTYPE}.${QUEUESYS,,}
   # copy queue script template to storm directory
   cp $QSTDIR/$QSTEMPLATE $QSFILE 2>> $LOGFILE
   sed -i "s/%jobtype%/$JOBTYPE/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%enstorm%/$ENSTORM/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%stormdir%/${STORMDIR//\//\\/}/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%ncpu%/$CPUREQUEST/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%wallminutes%/$WALLMINUTES/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%walltime%/$WALLTIME/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%nnodes%/$NNODES/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%notifyuser%/$NOTIFYUSER/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%account%/$ACCOUNT/g" $QSFILE 2>> $LOGFILE
   if [[ $CPUREQUEST -gt 1 ]]; then
      sed -i "s/%queuename%/$QUEUENAME/g" $QSFILE 2>> $LOGFILE
      sed -i "s/%ppn%/$PPN/g" $QSFILE 2>> $LOGFILE
   else
      sed -i "s/%queuename%/$SERQUEUE/g" $QSFILE 2>> $LOGFILE
      sed -i "s/%ppn%/1/g" $QSFILE 2>> $LOGFILE
   fi
   sed -i "s/%advisdir%/${ADVISDIR//\//\\/}/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%syslog%/${SYSLOG//\//\\/}/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%platformmodules%/$PLATFORMMODULES/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%jobmodules%/${JOBMODULES//\//\\/}/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%jobpaths%/${JOBPATHS//\//\\/}/g" $QSFILE 2>> $LOGFILE
   sed -i "s/%joblibs%/${JOBLIBS//\//\\/}/g" $QSFILE 2>> $LOGFILE
   if [[ $CPUREQUEST -gt 1 ]]; then
      sed -i "s/%joblauncher%/$JOBLAUNCHER/g" $QSFILE 2>> $LOGFILE
   else 
      sed -i "s/%joblauncher%//g" $QSFILE 2>> $LOGFILE
   fi
   sed -i "s/%cmd%/${CMD//\//\\/}/g" $QSFILE 2>> $LOGFILE
   # slurm-specific settings
   if [[ $QUEUESYS = SLURM ]]; then 
      PARTITION=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.partition\s*:\s*//p" run.properties`   
      if [[ -z $PARTITION || $PARTITION = null ]]; then
         sed -i "/%partition%/d" $QSFILE 2>> $LOGFILE
      else
         sed -i "s/%partition%/$PARTITION/g" $QSFILE 2>> $LOGFILE
      fi
      RESERVATION=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.reservation\s*:\s*//p" run.properties`   
      if [[ -z $RESERVATION || $RESERVATION = null ]]; then
         sed -i "/%reservation%/d" $QSFILE 2>> $LOGFILE
      else
         sed -i "s/%reservation%/$RESERVATION/g" $QSFILE 2>> $LOGFILE
      fi
      CONSTRAINT=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.constraint\s*:\s*//p" run.properties`   
      if [[ -z $CONSTRAINT || $CONSTRAINT = null ]]; then
         sed -i "/%constraint%/d" $QSFILE 2>> $LOGFILE
      else
         sed -i "s/%constraint%/$CONSTRAINT/g" $QSFILE 2>> $LOGFILE
      fi
   fi
   ;;
"mpiexec") # do nothing because there is no queue script
   ;;
esac
#--------------------------------------------------------------------------
#              S U B M I T   J O B 
#--------------------------------------------------------------------------
case $QUEUESYS in 
#
#  Portable Batch System (PBS) or SLURM; both widely used
"PBS" | "SLURM")
      logMessage "$ENSTORM: $THIS: Submitting ${STORMDIR}/${QSFILE}."
      # submit job, check to make sure submission succeeded, and if not, retry
      while [ true ];  do
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
         echo "time.${JOBTYPE}.submit : $DATETIME" >> ${STORMDIR}/run.properties
         $SUBMITSTRING ${STORMDIR}/${QSFILE} >> ${SYSLOG} 2>&1
         if [[ $? = 0 ]]; then
            break # submission returned a "success" status
         else
            warn "$ENSTORM: $THIS: $SUBMITSTRING ${STORMDIR}/${QSFILE} failed; will retry in 60 seconds."
            sleep 60
         fi
      done
      ;;
#
#  No queueing system, just mpiexec (used on standalone computers)
"mpiexec")
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'%z`
      echo "time.${JOBTYPE}.start : $DATETIME" >> run.properties
      echo "[${DATETIME}] Starting ${JOBTYPE}.${ENSTORM} job in $PWD." >> ${STORMDIR}/${JOBTYPE}.${ENSTORM}.run.start
      logMessage "$ENSTORM: $THIS: Submitting job via \"$SUBMITSTRING -n $CPUREQUEST $CMD $CLOPTIONS >> ${SYSLOG} 2>&1"
      # submit the parallel job in a subshell
      #
      # write the process id for the subshell to the run.properties file
      # so that monitorJobs() can kill the job if it exceeds the expected
      # wall clock time
      (
         echo "asgs.submitjob.${JOBTYPE}.subshell.pid : $$" >> ${STORMDIR}/run.properties 
         $SUBMITSTRING -n $CPUREQUEST $CMD >> ${STORMDIR}/${JOBTYPE}.log 2>&1
         ERROVALUE=$?
         RUNSUFFIX="finish"
         if [ $ERROVALUE == 0 ] ; then
            if [[ $JOBTYPE = padcirc || $JOBTYPE = padcswan ]]; then
               for file in adcirc.log %advisdir%/%enstorm%/%jobtype%.%enstorm%.out ; do
                  if [ -e $file ]; then
                     numMsg=`grep WarnElev $file | wc -l`
                     if [ $numMsg = 0 ]; then
                        DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
                        echo "[${DATETIME}] INFO: %jobtype%.slurm: No numerical instability detected in $file after executing %jobtype%.%enstorm%." | tee --append %syslog%
                     else
                        ERROMSG="$ERROMSG Detected $numMsg numerical instability messages in $file."
                        ERROVALUE=1
                     fi
                  fi
               done
            fi
         else
            ERROMSG="The %jobtype%.%enstorm% job ended with an exit status that indicates an error occurred."
         fi
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'` 
         # select suffix based on whether the job finished successfully
         if [[ $ERROVALUE != 0 ]]; then
            RUNSUFFIX="error"
         fi
         echo "[${DATETIME}] Finished ${JOBTYPE}.${ENSTORM} job in $PWD with return value = $ERROVALUE." >> ${STORMDIR}/${JOBTYPE}.${ENSTORM}.run.${RUNSUFFIX}
         echo "time.${JOBTYPE}.${RUNSUFFIX} : $DATETIME" >> run.properties
      ) &
      ;;
   *)
      fatal "$ENSTORM: $THIS: Queueing system $QUEUESYS unrecognized."
      ;;
esac
