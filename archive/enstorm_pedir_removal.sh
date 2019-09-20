#!/bin/bash
#----------------------------------------------------------------
# esnstorm_pedir_removal.sh: Removes the PE* subdirectories that were
# created by adcprep for use in a parallel adcirc simulation.  
#----------------------------------------------------------------
# Copyright(C) 2017--2019 Jason Fleming
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
# This script gets all the information it needs from the 
# run.properties file instead of using command line arguments. 
#
# Tasks completed by this script:
# 0. Tar up the subdomain SWAN hotstart files for fast use by
#    dependent processes with the same number of compute cores. 
# 1. Copy the SWAN log file and error file out of the PE0000
#    subdomain directory and into SCENARIODIR.
# 2. Compose fulldomain hotstart file from subdomain hotstart files. 
# 3. Compress fulldomain SWAN hotstart file(s).
# 4. Tar up subdomain fort.16 files for any troubleshooting. 
# 5. Remove PE* subdomain directories. 
#
#----------------------------------------------------------------
THIS=archive/enstorm_pedir_removal.sh
# this assumes this script is executed in the dirctory that is to be archived
SCENARIODIR=$PWD
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' run.properties`
. ${SCRIPTDIR}/monitoring/logging.sh
. ${SCRIPTDIR}/platforms.sh           # contains hpc platform configurations
. ${SCRIPTDIR}/properties.sh          # contains loadProperties subroutine
# load properties
declare -A properties
loadProperties $SCENARIODIR/run.properties
CYCLE=${properties['advisory']}
SCENARIO=${properties['scenario']}
SCENARIOLOG=${properties["monitoring.logging.file.scenariolog"]}
CYCLELOG=${properties["monitoring.logging.file.cyclelog"]}
SYSLOG=${properties["monitoring.logging.file.syslog"]}
LOGFILE=${SCENARIODIR}/enstorm_pedir_removal.sh.log
ACCOUNT=${properties['hpc.job.default.account']}
QUEUENAME=${properties['hpc.job.default.queuename']}
SERQUEUE=${properties['hpc.job.default.serqueue']}
# run configuration
#
# pull in platform-specific value for the command used to remove directories
# (some platforms have a special command that is nicer for their filesystem)
REMOVALCMD="rm -rf"
#
# HPCENVSHORT: shorthand for the HPC environment: queenbee, hatteras, etc
HPCENVSHORT=${properties['hpc.hpcenvshort']}
scenarioMessage "$SCENARIO: $THIS: Setting up for execution on ${HPCENVSHORT}." $LOGFILE 
# set variables as defaults according to the hpc platform
env_dispatch $HPCENVSHORT   
THIS="archive/enstorm_pedir_removal.sh" # must reset after executing env_dispatch()
WAVES=${properties["coupling.waves"]}
#
scenarioMessage "$THIS: Starting cleanup of subdomain (PE*) subdirectories." $LOGFILE
#
# archiving/composing subdomain swan.67 files produced by nowcast
if [[ $WAVES = on && -e PE0000/swan.67 ]]; then
   hSWANExe=unhcat.exe
   SWANDIR=${properties["path.swandir"]} 
   scenarioMessage "$THIS: The path to SWAN executables is ${SWANDIR}." $LOGFILE
   scenarioMessage "$THIS: The SWAN hotstart composition executable is ${SWANDIR}/${hSWANExe}." $LOGFILE
   scenarioMessage "$THIS: Wave coupling with SWAN is active." $LOGFILE
   # Tar up the swan subdomain hotstart files written in the most recent
   # model run to create a fast start for hotstarted jobs using the same 
   # number of processors.
   properties[hpc.job.swanhotstartarchive.cmd]="tar cvzf swan.67.tar.gz PE*/swan.67"
   # compose a fulldomain hotstart file from the subdomain hotstart files 
   properties[hpc.job.swanhotstartcompose.cmd]="${SWANDIR}/$hSWANExe < unhcat.in ; gzip swan.67" 
   # create stdin for unhcat.exe
   echo "1" > unhcat.in ; echo "swan.67" >> unhcat.in ; echo "F" >> unhcat.in
   submitString=${properties["hpc.submitstring"]}
   for JOBTYPE in swanhotstartarchive swanhotstartcompose ; do 
      scenarioMessage "$THIS: Starting $JOBTYPE job."
      if [[ $QUEUESYS != serial && $QUEUESYS != mpiexec ]]; then
         # use same wall time estimate as is normally used for adcprep
         swanArchiveWallTime=${properties["hpc.job.limit.adcprepwalltime"]}
         echo "hpc.job.${JOBTYPE}.limit.walltime : $swanArchiveWallTime" >> $SCENARIODIR/run.properties
         echo "hpc.job.${JOBTYPE}.account : $ACCOUNT" >> $SCENARIODIR/run.properties
         echo "hpc.job.${JOBTYPE}.jobenv : ( null )" >> $SCENARIODIR/run.properties
         echo "hpc.job.${JOBTYPE}.path.jobenvdir : $JOBENVDIR" >> $SCENARIODIR/run.properties
         echo "hpc.job.${JOBTYPE}.file.qscripttemplate : $SCRIPTDIR/qscript.template" >> $SCENARIODIR/run.properties
         echo "hpc.job.${JOBTYPE}.parallelism : serial" >> $SCENARIODIR/run.properties
         echo "hpc.job.${JOBTYPE}.serqueue : $SERQUEUE" >> $SCENARIODIR/run.properties
         echo "hpc.job.${JOBTYPE}.serialmodules : $SERIALMODULES" >> $SCENARIODIR/run.properties
         echo "hpc.job.${JOBTYPE}.ppn : 1" >> $SCENARIODIR/run.properties
         echo "hpc.job.${JOBTYPE}.ncpu : 1" >> $SCENARIODIR/run.properties
         if [[ $QUEUESYS = "SLURM" ]]; then
            echo "hpc.slurm.job.${JOBTYPE}.reservation : $RESERVATION" >> $SCENARIODIR/run.properties
            echo "hpc.slurm.job.${JOBTYPE}.constraint : $CONSTRAINT" >> $SCENARIODIR/run.properties
            echo "hpc.slurm.job.${JOBTYPE}.qos : $QOS" >> $SCENARIODIR/run.properties
         fi
         echo "hpc.job.${JOBTYPE}.ncpu : ${properties["hpc.job.${JOBTYPE}.ncpu"]}" >> $SCENARIODIR/run.properties         
         # Write queue script
         scenarioMessage "$SCENARIO: $THIS: Writing queue script for $JOBTYPE job." $LOGFILE
         perl ${SCRIPTDIR}/qscript.pl --jobtype $JOBTYPE 2>&1 | tee -a $SCENARIOLOG >> $LOGFILE
         # reload properties to get the name of the queue script written by qscript.pl
         loadProperties $SCENARIODIR/run.properties 
         qscript=${properties["hpc.job.${JOBTYPE}.file.qscript"]}
         if [[ -e $qscript ]]; then
            # submit job, if it fails, retry 10 times
            (
               archiveSubmitRetry=0
               while [ $archiveSubmitRetry -lt 10 ];  do
                  DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
                  echo "time.hpc.job.${JOBTYPE}.submit : $DATETIME" >> ${SCENARIODIR}/run.properties
                  $submitString $SCENARIODIR/$qscript 2>&1 | tee -a $LOGFILE >> ${SCENARIOLOG}
                  if [[ $? = 0 ]]; then
                     break # job submission command returned a "success" status
                  else 
                     warn "$SCENARIO: $THIS: $submitString $SCENARIODIR/$qscript failed; will retry in 60 seconds." $LOGFILE
                     sleep 60
                  fi
                  archiveSubmitRetry=`expr $archiveSubmitRetry + 1`
               done
             ) &
         else 
            error "$SCENARIO: $THIS: Failed to generate queue script for $JOBTYPE job." $LOGFILE
         fi
      else 
         # not using a queueing system, do this on the command line
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
         echo "time.hpc.job.$JOBTYPE.start : $DATETIME" >> $SCENARIODIR/run.properties
         # run process in the background and record start and finish/error times
         (
            runSuffix=finish
            # execute command
            ${properties["hpc.job.${JOBTYPE}.cmd"]} 1> $LOGFILE 2> errmsg && cat tar.log  | tee -a $LOGFILE >> $SCENARIOLOG || warn "cycle $CYCLE: $SCENARIO: $THIS: Could not execute $JOBTYPE job: `cat errmsg`." $LOGFILE
            [[ $? != "0" ]] && runSuffix=error 
            DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
            echo "time.hpc.job.$JOBTYPE.$runSuffix : $DATETIME" >> $SCENARIODIR/run.properties
         ) &
      fi
      scenarioMessage "$THIS: Finished submitting $JOBTYPE job." $LOGFILE
   done
   #
   # Wait up to two hours for SWAN hotstart archiving process to finish. 
   archiveWaitRetry=0
   archiveExited=0
   # keep checking to see if the archiving of the SWAN hotstart files and
   # the re-composition of the SWAN hotstart files into a fulldomain
   # SWAN hotstart file are finished yet
   while [[ $archiveWaitRetry -lt 120 && $archiveExited -lt 2 ]]; do 
      archiveExited=0
      loadProperties $SCENARIODIR/run.properties
      # for each of the jobs we are waiting on 
      for JOBTYPE in swanhotstartarchive swanhotstartcompose ; do
         # for each of the ways the job can exit
         for endState in finish error ; do 
            # For every key in the properties array
            for propertykey in "${!properties[@]}"; do
               if [[ $propertykey = "time.hpc.job.$JOBTYPE.$endState" ]]; then
                  archiveExited=`expr $archiveExited + 1`
               fi
            done
         done
      done
      archiveWaitRetry=`expr $archiveWaitRetry +1`
      sleep 60
   done
   #
   # preserve the swan log file and swan Errfile (if any) so we can see it later
   # FIXME: the name of the asgs_swan.prt file is provided in swaninit but it
   # is hardcoded here as asgs_swan.prt
   for file in ./PE0000/asgs_swan.prt ./PE0000/Errfile ; do 
      if [[ -e $file ]]; then
         scenarioMessage "$THIS: Preserving SWAN $file file." $LOGFILE
         cp $file . > errmsg 2>&1 || warn "cycle $CYCLE: $SCENARIO: $THIS: Could not copy '$file' to $PWD: `cat errmsg`." $LOGFILE
      fi
   done
fi
#
# archive the subdomain fort.16 log files in case troubleshooting is required
tar cvzf fort.16.tar.gz ./PE*/fort.16 1> tar.log 2> errmsg && cat tar.log | tee -a $LOGFILE >> $SCENARIOLOG || warn "cycle $CYCLE: $SCENARIO: $THIS: Could not create a tar archive of subdomain fort.16 files: `cat errmsg`." $LOGFILE
#
# check to see if there is post processing going on, and if so, wait up to 2 hours
# for post processing to finish
loadProperties $SCENARIODIR/run.properties
# For every key in the properties array
for startPropertyKey in "${!properties[@]}"; do
   if [[ $startPropertykey = "time.post.start" ]]; then
      # post processing may still be underway
      postWaitRetry=0
      postExited=0
      while [[ $postWaitRetry -lt 120 && $postExited = "0" ]]; do 
         loadProperties $SCENARIODIR/run.properties
         # For every key in the properties array
         for exitPropertyKey in "${!properties[@]}"; do
            if [[ $exitPropertyKey = "time.post.finish" ]]; then
               postExited=1
            fi
         done
         postWaitRetry=`expr $postWaitRetry +1`
         sleep 60
      done
   fi
done
#
# now delete the PE* subdirectories using a platform-specific command
rm errmsg ; touch errmsg
for dir in `ls -d PE*`; do 
   $REMOVALCMD $dir 2>> errmsg | tee -a $LOGFILE >> $SCENARIOLOG 
done
# report any errors
if [[ -s errmsg  ]]; then 
   warn "cycle $CYCLE: $SCENARIO: $THIS: Could not remove PE subdirectories: `cat errmsg`." $LOGFILE
fi
# delete unnecessary files
for file in metis_graph.txt partmesh.txt fort.80 ; do
   if [[ -e $file ]]; then 
      rm $file 
   fi
done
scenarioMessage "$THIS: Finished cleanup of subdomain (PE*) subdirectories." $LOGFILE
