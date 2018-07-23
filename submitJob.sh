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
#
# JOBTYPE: padcirc, padcswan, prep15, prep20, prepall, partmesh, etc
# This will be used to look up additional properties needed to submit
# this job. 
JOBTYPE=$1
# SYSLOG: asgs log file
SYSLOG=`sed -n 's/[ ^]*$//;s/asgs.file.syslog\s*:\s*//p' run.properties`
# SCRIPTDIR: path to asgs scripts like asgs_main.sh
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/config.path.scriptdir\s*:\s*//p' run.properties`
# pull in logging functions
. ${SCRIPTDIR}/logging.sh
#
# HPCENVSHORT: shorthand for the HPC environment: queenbee, hatteras, etc
HPCENVSHORT=`sed -n 's/[ ^]*$//;s/hpc.hpcenvshort\s*:\s*//p' run.properties`
# QUEUESYS: type of queueing system, e.g., SLURM, PBS, etc
QUEUESYS=`sed -n 's/[ ^]*$//;s/hpc.queuesys\s*:\s*//p' run.properties`
# PPN: processors (or cores) per node to specify for the job 
PPN=`sed -n 's/[ ^]*$//;s/hpc.ppn\s*:\s*//p' run.properties`
# ADCIRDIR: path to adcirc executables e.g. ~/adcirc/work 
ADCIRCDIR=`sed -n 's/[ ^]*$//;s/config.path.adcircdir\s*:\s*//p' run.properties`
# SWANDIR: path to swan executables (i.e., to unhcat.exe)
SWANDIR=`sed -n 's/[ ^]*$//;s/config.path.swandir\s*:\s*//p' run.properties`
# INPUTDIR: path to input files, including subdirectories meshes and machines
INPUTDIR=`sed -n 's/[ ^]*$//;s/config.path.inputdir\s*:\s*//p' run.properties`
# ADVISDIR: path where all ensemble members are stored for a particular 
# advisory or nam cycle
ADVISDIR=`sed -n 's/[ ^]*$//;s/asgs.path.advisdir\s*:\s*//p' run.properties`
# ENSTORM: name of this ensemble member
ENSTORM=`sed -n 's/[ ^]*$//;s/asgs.enstorm\s*:\s*//p' run.properties`
# NOTIFYUSER: email address to put into the queue script that the queueing
# system will send email to if the hpc job fails
NOTIFYUSER=`sed -n "s/[ ^]*$//;s/notification.hpc.email.notifyuser\s*:\s*//p" run.properties`
# QSTDIR: template to use when generating queue script
QSTDIR=`sed -n "s/[ ^]*$//;s/hpc.file.${JOBTYPE}.template.qstdir\s*:\s*//p" run.properties`
# QSTEMPLATE: template to use when generating queue script
QSTEMPLATE=`sed -n "s/[ ^]*$//;s/hpc.file.${JOBTYPE}.template.qstemplate\s*:\s*//p" run.properties`
# NCPU: number of cores to request for this job
NCPU=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.ncpu\s*:\s*//p" run.properties`
# ACCOUNT: the account the job should be billed to
ACCOUNT=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.account\s*:\s*//p" run.properties`
# WALLTIME: limit of wall clock time for the job to run before being
# kicked out of the queue
WALLTIME=$`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.limit.walltime\s*:\s*//p" run.properties`
# CMD: command line to be executed via queueing system
CMD=`sed -n "s/[ ^]*$//;s/hpc.job.${JOBTYPE}.cmd\s*:\s*//p" run.properties`
#----------------------------------------------------------------
#
THIS="submitJob.sh"
STORMDIR=${ADVISDIR}/${ENSTORM}
cd $STORMDIR 2>> $LOGFILE
LOGFILE=$STORMDIR/${JOBTYPE}.log
#
CLOPTIONS=""     # command line options
LOCALHOTSTART=""
CPUREQUEST=$NCPU   
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
QSFILE=${JOBTYPE}.${QUEUESYS,,}
cp $QSTDIR/$QSTEMPLATE $QSFILE 2>> $LOGFILE
#
WALLMINUTES=`echo "${WALLTIME:0:2} * 60 + ${WALLTIME:3:2} + 1" | bc` 
#--------------------------------------------------------------------------
sed -i "s/%jobtype%/$JOBTYPE/g" $QSFILE 2>> $LOGFILE
sed -i "s/%enstorm%/$ENSTORM/g" $QSFILE 2>> $LOGFILE
sed -i "s/%wallminutes%/$WALLMINUTES/g" $QSFILE 2>> $LOGFILE
sed -i "s/%advisdir%/$ADVISDIR/g" $QSFILE 2>> $LOGFILE
sed -i "s/%syslog%/$SYSLOG/g" $QSFILE 2>> $LOGFILE
sed -i "s/%cmd%/$CMD/g" $QSFILE 2>> $LOGFILE

#--------------------------------------------------------------------------
#
case $QUEUESYS in 
#
#  Load Sharing Facility (LSF); used on topsail at UNC
"LSF")
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
      echo "time.${JOBTYPE}.submit : $DATETIME" >> ${STORMDIR}/run.properties
      bsub -x -n $NCPU -q $QUEUENAME -o log.%J -e err.%J -a mvapich mpirun $ADCIRCDIR/$JOBTYPE $CLOPTION >> ${SYSLOG}
      ;;
#
#  LoadLeveler (often used on IBM systems)
"LoadLeveler")
      perl $SCRIPTDIR/loadleveler.pl --cmd "$CMD" --jobtype $JOBTYPE --ncpu $NCPU --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --inputdir $INPUTDIR --enstorm $ENSTORM --notifyuser $NOTIFYUSER --numwriters $NUMWRITERS $LOCALHOTSTART > $ADVISDIR/$ENSTORM/${JOBTYPE}.ll 2>> ${SYSLOG}
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
      echo "time.${JOBTYPE}.submit : $DATETIME" >> ${STORMDIR}/run.properties
      llsubmit $ADVISDIR/$ENSTORM/${JOBTYPE}.ll >> ${SYSLOG} 2>&1
      ;;
#
#  Portable Batch System (PBS); widely used
"PBS")
      QSCRIPTOPTIONS="--jobtype $JOBTYPE --cmd \"$CMD\" --ncpu $NCPU --queuename $QUEUENAME --account $ACCOUNT --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --qscript ${QSTDIR}/${QSTEMPLATE} --enstorm $ENSTORM --notifyuser $NOTIFYUSER --walltime $WALLTIME $LOCALHOTSTART --syslog $SYSLOG"
      if [[ $PPN -ne 0 ]]; then
         QSCRIPTOPTIONS="$QSCRIPTOPTIONS --ppn $PPN"
      fi
      if [[ $NUMWRITERS != "0" ]]; then
         QSCRIPTOPTIONS="$QSCRIPTOPTIONS --numwriters $NUMWRITERS"
      fi
      logMessage "$ENSTORM: $THIS: QSCRIPTOPTIONS is $QSCRIPTOPTIONS"
      perl $SCRIPTDIR/$QSCRIPTGEN $QSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/${JOBTYPE}.pbs 2>> ${SYSLOG}
      logMessage "$ENSTORM: $THIS: Submitting $ADVISDIR/$ENSTORM/${JOBTYPE}.pbs"
      # submit job, check to make sure qsub succeeded, and if not, retry
      while [ true ];  do
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
         echo "time.${JOBTYPE}.submit : $DATETIME" >> ${STORMDIR}/run.properties
         qsub $ADVISDIR/$ENSTORM/${JOBTYPE}.pbs >> ${SYSLOG} 2>&1
         if [[ $? = 0 ]]; then
            break # qsub returned a "success" status
         else
            warn "$ENSTORM: $THIS: qsub $ADVISDIR/$ENSTORM/${JOBTYPE}.pbs failed; will retry in 60 seconds."
            sleep 60
         fi
      done
      ;;
#
#  SLURM
"SLURM")
      sed -i "s/%partition%/$PARTITION/g" $QSFILE 2>> $LOGFILE
      sed -i "s/%reservation%/$RESERVATION/g" $QSFILE 2>> $LOGFILE
      sed -i "s/%constraint%/$CONSTRAINT/g" $QSFILE 2>> $LOGFILE
      QSCRIPTOPTIONS="--jobtype $JOBTYPEE --cmd \"$CMD\" --ncpu $NCPU --queuename $QUEUENAME --account $ACCOUNT --partition $PARTITION --reservation $RESERVATION --constraint "$CONSTRAINT" --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --qscript ${QSTDIR}/${QSTEMPLATE} --enstorm $ENSTORM --notifyuser $NOTIFYUSER --walltime $WALLTIME $LOCALHOTSTART --syslog $SYSLOG"
      if [[ $PPN -ne 0 ]]; then
         QSCRIPTOPTIONS="$QSCRIPTOPTIONS --ppn $PPN"
      fi
      if [[ $NUMWRITERS != "0" ]]; then
         QSCRIPTOPTIONS="$QSCRIPTOPTIONS --numwriters $NUMWRITERS"
      fi
      logMessage "$ENSTORM: $THIS: QSCRIPTOPTIONS is $QSCRIPTOPTIONS"
      perl $SCRIPTDIR/$QSCRIPTGEN $QSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/${JOBTYPE}.slurm 2>> ${SYSLOG}
      logMessage "$ENSTORM: $THIS: Submitting $ADVISDIR/$ENSTORM/${JOBTYPE}.slurm"
      # submit job, check to make sure qsub succeeded, and if not, retry
      while [ true ];  do
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
         echo "time.${JOBTYPE}.submit : $DATETIME" >> ${STORMDIR}/run.properties
         sbatch $ADVISDIR/$ENSTORM/${JOBTYPE}.slurm >> ${SYSLOG} 2>&1
         if [[ $? = 0 ]]; then
            break # sbatch returned a "success" status
         else
            warn "$ENSTORM: $THIS: sbatch $ADVISDIR/$ENSTORM/${JOBTYPE}.slurm failed; will retry in 60 seconds."
            sleep 60
         fi
      done
      ;;
#
#  No queueing system, just mpiexec (used on standalone computers)
"mpiexec")
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'%z`
      echo "time.${JOBTYPE}.start : $DATETIME" >> run.properties
      echo "[${DATETIME}] Starting ${JOBTYPE}.${ENSTORM} job in $PWD." >> ${ADVISDIR}/${ENSTORM}/${JOBTYPE}.${ENSTORM}.run.start
      logMessage "$ENSTORM: $THIS: Submitting job via $SUBMITSTRING $CPUREQUEST $ADCIRCDIR/$JOBTYPE $CLOPTION >> ${SYSLOG} 2>&1"
      # submit the parallel job in a subshell
      #
      # write the process id for the subshell to the run.properties file
      # so that monitorJobs() can kill the job if it exceeds the expected
      # wall clock time
      (
         echo "$JOBTYPE subshell pid : $BASHPID" >> ${ADVISDIR}/${ENSTORM}/run.properties 2>> ${SYSLOG}
         mpiexec -n $CPUREQUEST $CMD >> ${ADVISDIR}/${ENSTORM}/${JOBTYPE}.log 2>&1
         ERROVALUE=$?
         RUNSUFFIX="finish"
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
         if [ $ERROVALUE != 0 ] ; then
            RUNSUFFIX="error"
         fi
         echo "[${DATETIME}] Finished ${JOBTYPE}.${ENSTORM} job in $PWD with return value = $ERROVALUE." >> ${ADVISDIR}/${ENSTORM}/${JOBTYPE}.${ENSTORM}.run.${RUNSUFFIX}
         echo "time.${JOBTYPE}.${RUNSUFFIX} : $DATETIME" >> run.properties
      ) &
      ;;
#
#  Sun Grid Engine (SGE); used on Sun and many Linux clusters
"SGE")
      QSCRIPTOPTIONS="--jobtype $JOBTYPE --cmd \"$CMD\" --ncpu $NCPU --ncpudivisor $NCPUDIVISOR --queuename $QUEUENAME --account $ACCOUNT --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --qscript ${QSTDIR}/${QSTEMPLATE} --enstorm $ENSTORM --notifyuser $NOTIFYUSER --walltime $WALLTIME --syslog $SYSLOG --numwriters $NUMWRITERS $LOCALHOTSTART"
      perl $SCRIPTDIR/$QSCRIPTGEN $QSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/${JOBTYPE}.sge 2>> ${SYSLOG}
      logMessage "$ENSTORM: $THIS: Submitting $ADVISDIR/$ENSTORM/${JOBTYPE}.sge"
      # submit job, check to make sure qsub succeeded, and if not, retry
      while [ true ];  do
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
         echo "time.${JOBTYPE}.submit : $DATETIME" >> ${STORMDIR}/run.properties
         qsub $ADVISDIR/$ENSTORM/${JOBTYPE}.sge >> ${SYSLOG} 2>&1
         if [[ $? = 0 ]]; then
            break # qsub returned a "success" status
         else
            warn "$ENSTORM: $THIS: qsub $ADVISDIR/$ENSTORM/${JOBTYPE}.sge failed; will retry in 60 seconds."
            sleep 60
         fi
      done
      ;;
   *)
      fatal "$ENSTORM: $THIS: Queueing system $QUEUESYS unrecognized."
      ;;
esac
