#!/bin/bash
#----------------------------------------------------------------
#
# asgs_main.sh: This is the main driver script for the ADCIRC Surge Guidance
# System (ASGS). It performs configuration tasks via config.sh, then enters a
# loop which is executed once per advisory cycle.
# 
#----------------------------------------------------------------
# Copyright(C) 2006, 2007, 2008, 2009 Jason Fleming
# Copyright(C) 2006, 2007 Brett Estrade
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
echoHelp()
{ clear
  echo "@@@ Help @@@"
  echo "Usage:"
  echo " bash  %$0 [-c /fullpath/of/asgs_config.sh] -e environment"
  echo
  echo "Options:"
  echo "-c : set location of configuration file"
  echo "-e (environment): set the computer that the ASGS is running on" 
  echo "-h : show help"
  exit;
}
#
# utility function to create progress indicator on console
function activity_indicator {
   activity=$1
   echo
   # calculate formatting for the activity message
   length=${#activity}
   backspace="\b"
   let i=0
   while [[ $i -le $length ]]; do
      backspace="$backspace\b"
      i=$[$i + 1]
   done

   chars=( "-" "\\" "|" "/" )
   charcount=0
   while true
   do
      pos=$(($charcount % 4))
      echo -en "${backspace}${chars[$pos]} $activity"
      charcount=$(($charcount + 1))
      sleep 1
   done
}
#
# utility function to stop progress indicator on console
function stop_activity_indicator {
  exec 2>/dev/null
  kill $1
  echo -en "\n"
}
#
# subroutine to check for the existence of required files that have
# been specified in config.sh
checkFileExistence()
{ FPATH=$1
  FTYPE=$2
  FNAME=$3
  if [[ -z $FNAME ]]; then
     fatal "The $FTYPE was not specified in the configuration file. When it is specified, the ASGS will look for it in the path ${FPATH}."
  fi
  if [ $FNAME ]; then
     if [ -e "${FPATH}/${FNAME}" ]; then 
        logMessage "The $FTYPE '${FPATH}/${FNAME}' was found." 
     else
        fatal "The $FTYPE '${FPATH}/${FNAME}' does not exist."
     fi
  fi
}
#
# subroutine to check for the existence of required directories 
# that have been specified in config.sh
checkDirExistence()
{ DIR=$1 
  TYPE=$2
  if [[ -z $DIR ]]; then
      fatal "The $TYPE was not specified in the configuration file." 
  fi 
  if [ -e $DIR ]; then 
     logMessage "The $TYPE '$DIR' was found." 
  else
     fatal "The $TYPE '$DIR' does not exist."
  fi
}
#
# subroutine to check for the existence and nonzero length of the 
# hotstart file
# the subroutine assumes that the hotstart file is named fort.67 and
# expects it to be found in $PWD
checkHotstart()
{ 
   if [ ! -e fort.67 ]; then
      fatal "The hotstart (fort.67) file was not found in $PWD. The preceding simulation run must have failed to produce it."
   else 
      hotstartSize=`stat -c %s fort.67`
      if [ $hotstartSize == "0" ]; then 
         fatal "The hotstart (fort.67) file in $PWD is of zero length. The preceding simulation run must have failed to produce it properly."
      else
         logMessage "The hotstart (fort.67) file was found in $PWD and it contains $hotstartSize bytes."
      fi
   fi  
}
#
# subroutine to create a symbolic link to the fort.22 file that has metadata in # to identify the type of data that is in the file 
createMetaDataLink()
{ STORM=$1
  YEAR=$2
  ADVISORY=$3
  ENSTORM=$4
  ADVISDIR=$5
  HOSTNAME=$6
  HSTIME=$7
  CSDATE=$8
  VARIATION=$9
  PERCENT=${10}  
#
  echo "# This metadata describes the storm data used in this run." >> $ADVISDIR/$ENSTORM/fort.22.meta
  echo "version:1" >> $ADVISDIR/$ENSTORM/fort.22.meta
  echo "year:$YEAR" >> $ADVISDIR/$ENSTORM/fort.22.meta
  echo "storm:$STORM" >> $ADVISDIR/$ENSTORM/fort.22.meta
  echo "advisory:$ADVISORY" >> $ADVISDIR/$ENSTORM/fort.22.meta
  echo "hostname:$HOSTNAME" >> $ADVISDIR/$ENSTORM/fort.22.meta  
  echo "directory advisory:$ADVISDIR" >> $ADVISDIR/$ENSTORM/fort.22.meta
  echo "directory storm:$ADVISDIR/$ENSTORM" >> $ADVISDIR/$ENSTORM/fort.22.meta
  echo "time hotstart seconds:$HSTIME" >> $ADVISDIR/$ENSTORM/fort.22.meta
  echo "time coldstart date:$CSDATE" >>  $ADVISDIR/$ENSTORM/fort.22.meta
  windPercent="+00"
  overlandSpeedPercent="+00"
  veerPercent="+000"
  rMaxPercent="+00"
#
  if [[ -z $PERCENT ]]; then
     $modpercent=0
     $modsign="+"
  else
     if [[ $PERCENT -lt 0 ]]; then
        $modsign="-"
        # lop off the minus sign since we already have it in modsign
        $modpercent=${modpercent:1}
     else 
        $modsign="+"
     fi
  fi
  if [[ ! -z $VARIATION ]]; then
     mod="m"
     echo "modified:y" >> $ADVISDIR/$ENSTORM/fort.22.meta
     case $VARIATION in
        "windSpeed") 
           $format="%02d"; windPercent=`printf "$format" $modpercent`
           $windPercent=$modsign$windPercent
           echo "variation wind speed:$windPercent" >> $ADVISDIR/$ENSTORM/fort.22.meta
           ;;
        "overlandSpeed") 
           $format="%02d"; overlandSpeedPercent=`printf "$format" $modpercent`
           $overlandSpeedPercent=$modsign$overlandSpeedPercent
           echo "variation overland speed:$overlandSpeedPercent" >>  $ADVISDIR/$ENSTORM/fort.22.meta
           ;;
        "veer") $modtype="v"
           $format="%03d"; veerPercent=`printf "$format" $modpercent`
           $veerPercent=$modsign$veerPercent
           echo "variation veer:$veerPercent" >> $ADVISDIR/$ENSTORM/fort.22.meta
           ;;
        "rMax") $modType="r"
           $format="%02d"; rMaxPercent=`printf "$format" $modpercent`
           $rMaxPercent=$modsign$rMaxPercent
           echo "variation rmax:$rMaxPercent" >> $ADVISDIR/$ENSTORM/fort.22.meta

            warn "rMax is not a supported variation at this time."
           ;;
         *) warn "'$VARIATION' is not a supported forecast variation. Supported variations are 'windSpeed', 'overlandSpeed', and 'veer'. The fort.22 will be labeled as 'consensus' although this may not be correct."
           ;;
     esac
  else 
      mod="c"
      echo "modified:n" >> $ADVISDIR/$ENSTORM/fort.22.meta    
  fi
  # assemble link name 
  linkName=${YEAR}${STORM}${ADVISORY}${mod}_w${windPercent}o${overlandSpeedPercent}v${veerPercent}r${rMaxPercent}
  ln -s $ADVISDIR/$ENSTORM/fort.22 $ADVISDIR/$ENSTORM/$linkName
}
#
# subroutine to run adcprep, using a pre-prepped archive of fort.13 and 
# fort.14 files
prep()
{   ADVISDIR=$1  # directory containing the current advisory
    INPUTDIR=$2 # directory where grid and nodal attribute files are found
    ENSTORM=$3  # ensemble member (nowcast, storm1, storm5, etc) 
    START=$4    # coldstart or hotstart
    OLDADVISDIR=$5 # directory containing last advisory
    ENV=$6     # machine to run on (jade, desktop, queenbee, etc)
    NCPU=$7     # number of CPUs to request in parallel jobs
    PREPPEDARCHIVE=$8 # preprocessed fort.13 and fort.14 package 
    GRIDFILE=$9 # fulldomain grid
    ACCOUNT=${10} # account to charge time to 
    NAFILE=${11}  # full domain nodal attributes file, must be last in the
                  # argument list, since it may be undefined
    TIMESTAMP=`date +%d%b%Y:%H:%M:%S`
    if [ ! -d $ADVISDIR/$ENSTORM ]; then 
	mkdir $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
    fi
    echo "$TIMESTAMP adcprep.log entry for $FILE for ensemble member $ENSTORM in $ADVISDIR as follows: " >> $ADVISDIR/$ENSTORM/adcprep.log
    cd $ADVISDIR/$ENSTORM
    logMessage "Copying fulldomain input files."
    # symbolically link grid 
    if [ ! -e $ADVISDIR/$ENSTORM/fort.14 ]; then 
        ln -s $INPUTDIR/$GRIDFILE $ADVISDIR/$ENSTORM/fort.14 2>> ${SYSLOG}
    fi
    # symbolically link nodal attributes
    if [ ! -e $ADVISDIR/$ENSTORM/fort.13 ]; then
        if [[ ! -z $NAFILE ]]; then
           ln -s $INPUTDIR/$NAFILE $ADVISDIR/$ENSTORM/fort.13 2>> ${SYSLOG}
        fi
    fi
    # if this is the cold start, we just prep the fort.15 and rely on a 
    # full adcprep that has already been done (to save time) and saved 
    # in a tar file
    if [ $START = coldstart ]; then
        # change to proper directory
        cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
        # copy in the files that have already been preprocessed
        logMessage "Copying input files that have already been decomposed."
        cp $INPUTDIR/${PREPPEDARCHIVE} . 2>> ${SYSLOG}
        gunzip -f ${PREPPEDARCHIVE} 2>> ${SYSLOG}
        # untar the uncompressed archive
        UNCOMPRESSEDARCHIVE=${PREPPEDARCHIVE%.gz}
        tar xvf $UNCOMPRESSEDARCHIVE > untarred_files.log 2>> ${SYSLOG}
        logMessage "Removing $UNCOMPRESSEDARCHIVE"
        rm $UNCOMPRESSEDARCHIVE 2>> ${SYSLOG}
        # run adcprep to decompose the new fort.15 file
        logMessage "Running adcprep to prepare new fort.15 file"
        prepControlFile $ENV $NCPU $ACCOUNT 
       # link to hurricane track file rather than prepping it with adcprep
       PE=0
       format="%04d"
       while [ $PE -lt $NCPU ]; do
           PESTRING=`printf "$format" $PE`
           ln -s $ADVISDIR/$ENSTORM/fort.22 $ADVISDIR/$ENSTORM/PE${PESTRING}/fort.22 2>> ${SYSLOG}
           PE=`expr $PE + 1`
       done
    else
       # this is a hotstart
       # set directory where data will be copied from     
       if [ $ENSTORM = nowcast ]; then
            FROMDIR=$OLDADVISDIR/nowcast
       else
            FROMDIR=$ADVISDIR/nowcast
       fi
       # link to fulldomain files
       ln $FROMDIR/fort.80 $ADVISDIR/$ENSTORM/fort.80 2>> ${SYSLOG}
       ln $FROMDIR/partmesh.txt $ADVISDIR/$ENSTORM/partmesh.txt 2>> ${SYSLOG}
       ln $FROMDIR/PE0000/fort.67 $ADVISDIR/$ENSTORM/fort.68
       # link to existing subdomain files
       PE=0
       format="%04d"
       while [ $PE -lt $NCPU ]; do
           PESTRING=`printf "$format" $PE`
           mkdir $ADVISDIR/$ENSTORM/PE${PESTRING} 2>> ${SYSLOG}
           ln -s $ADVISDIR/$ENSTORM/fort.22 $ADVISDIR/$ENSTORM/PE${PESTRING}/fort.22 2>> ${SYSLOG}
           PE=`expr $PE + 1`
       done
       for file in fort.13 fort.14 fort.18; do
          PE=0
          while [ $PE -lt $NCPU ]; do 
              PESTRING=`printf "$format" $PE`
              ln -s $FROMDIR/PE${PESTRING}/$file $ADVISDIR/$ENSTORM/PE${PESTRING}/$file 2>> ${SYSLOG}
              PE=`expr $PE + 1`
          done
       done
       for file in fort.61 fort.62 fort.63 fort.64 fort.71 fort.72 fort.73 fort.74; do
          if [ $ENSTORM = nowcast ]; then
             ln $FROMDIR/$file $ADVISDIR/$ENSTORM/$file 2>> ${SYSLOG}
          else 
             cp $FROMDIR/$file $ADVISDIR/$ENSTORM/$file 2>> ${SYSLOG}
          fi
       done
       # run adcprep to decompose the new fort.15 file
       logMessage "Running adcprep to prepare new fort.15 file."
       prepControlFile $ENV $NCPU $ACCOUNT 
       # run adcprep to decompose the hotstart file
       logMessage "Running adcprep to decompose hotstart file."
       prepHotstartFile $ENV $NCPU $ACCOUNT
    fi
}
#
# function to run adcprep in a platform dependent way to decompose 
# the fort.15 file
prepControlFile()
{   ENV=$1
    NCPU=$2
    ACCOUNT=$3
    if [ $ENV = jade || $ENV = sapphire ]; then
       qsub -l ncpus=0 -l walltime=02:00:00 -q debug -A erdcvenq -I
       cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
       $ADCIRCDIR/adcprep <<END >> $ADVISDIR/$ENSTORM/adcprep.log 2>&1
$NCPU
4
fort.14
fort.15
END
       exit
    elif [ $ENV = ranger ]; then
         cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
          echo $NCPU    > prep.in1
          echo 4       >> prep.in1
          echo fort.14 >> prep.in1
          echo fort.15 >> prep.in1
        SERQSCRIPT=ranger.template.serial
        SERQSCRIPTOPTIONS="--account $ACCOUNT --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --enstorm $ENSTORM --notifyuser $NOTIFYUSER --serqscript $INPUTDIR/$SERQSCRIPT"
        perl $SCRIPTDIR/ranger.serial.pl  $SERQSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/adcprep.serial.sge 2>> ${SYSLOG}
        logMessage "Submitting $ADVISDIR/$ENSTORM/adcprep.serial.sge"
        qsub $ADVISDIR/$ENSTORM/adcprep.serial.sge >> ${SYSLOG} 2>&1
    # check once per minute until all jobs have finished
    monitorJobs $QUEUESYS
    consoleMesssage "Job(s) complete."
    # prep-ing control finished, get on with it
    logMessage "adcprep control finished"
    consoleMessage "adcprep control finished"

    else
       $ADCIRCDIR/adcprep <<END >> $ADVISDIR/$ENSTORM/adcprep.log 2>&1
$NCPU
4
fort.14
fort.15
END
    fi
}
#
# function to run adcprep in a platform dependent way to decompose 
# the fort.68 file
prepHotstartFile()
{     ENV=$1
      NCPU=$2
      ACCOUNT=$3
      if [ $ENV = jade || $ENV = sapphire ]; then
         qsub -l ncpus=0 -l walltime=02:00:00 -q debug -A erdcvenq -I
         cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
         $ADCIRCDIR/adcprep <<END >> $ADVISDIR/$ENSTORM/adcprep.log 2>&1
$NCPU
6
68
END
         exit
    elif [ $ENV = ranger ]; then
         cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
          echo $NCPU    > prep.in1
          echo 6       >> prep.in1
          echo 68      >> prep.in1
        SERQSCRIPT=ranger.template.serial
        SERQSCRIPTOPTIONS="--account $ACCOUNT --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --enstorm $ENSTORM --notifyuser $NOTIFYUSER --serqscript $INPUTDIR/$SERQSCRIPT"
        perl $SCRIPTDIR/ranger.serial.pl  $SERQSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/adcprep.serial.sge 2>> ${SYSLOG}
        logMessage "Submitting $ADVISDIR/$ENSTORM/adcprep.serial.sge"
        qsub $ADVISDIR/$ENSTORM/adcprep.serial.sge >> ${SYSLOG} 2>&1
    # check once per minute until all jobs have finished
    monitorJobs $QUEUESYS
    consoleMesssage "Job(s) complete."
    # prep-ing hotstart finished, get on with it
    logMessage "adcprep hotstart finished"
    consoleMessage "adcprep hotstart finished"

      else
         $ADCIRCDIR/adcprep <<END >> $ADVISDIR/$ENSTORM/adcprep.log 2>&1
$NCPU
6
68
END
      fi
}
#
# subroutine that calls an external script over and over until it
# pulls down a new advisory (then it returns)
downloadWindData() 
{   STORM=$1
    YEAR=$2
    STORMDIR=$3
    SCRIPTDIR=$4
    OLDADVISDIR=$5
    TRIGGER=$6
    ADVISORY=$7
    FTPSITE=$8
    RSSSITE=$9
    FDIR=${10}
    HDIR=${11}
    activity_indicator "Checking remote site for new advisory..." &
    pid=$!; trap "stop_activity_indicator ${pid}; exit" EXIT
    cd $STORMDIR
    newAdvisory=false
    newAdvisoryNum=
    forecastFileName=al${STORM}${YEAR}.fst
    OPTIONS="--storm $STORM --year $YEAR --ftpsite $FTPSITE --fdir $FDIR --hdir $HDIR --rsssite $RSSSITE --trigger $TRIGGER --adv $ADVISORY"
    if [ "$START" = coldstart ]; then
       logMessage "Downloading initial hindcast/forecast."
    else
       logMessage "Checking remote site for new advisory..."
    fi
    while [ $newAdvisory = false ]; do
       newAdvisoryNum=`perl $SCRIPTDIR/get_atcf.pl $OPTIONS 2>> $SYSLOG`
       # check to see if we have a new one, and if so, determine the
       # new advisory number correctly
       case $TRIGGER in 
       "ftp")
          if [ $START = hotstart ]; then
             if ! diff $OLDADVISDIR/$forecastFileName ./$forecastFileName > /dev/null 2>> ${SYSLOG}; then
                # forecasts from NHC ftp site do not have advisory number
                newAdvisoryNum=$[$ADVISORY + 1]

                newAdvisory="true"
             fi 
          fi
          ;;
       "rss")
          # if there was a new advisory, the get_atcf.pl script
          # would have returned the advisory number in stdout
          if [ ! -z $newAdvisoryNum ]; then
             newAdvisory="true"
             if [ -e $forecastFileName ]; then 
                mv $forecastFileName $forecastFileName.ftp 2>> $SYSLOG
             fi
          fi       
          ;;
       *)
          fatal "Invalid 'TRIGGER' type: '$TRIGGER'; must be ftp or rss." 
          ;;
       esac
       if [ $START = coldstart ]; then
          if [ $TRIGGER = ftp ]; then
             newAdvisoryNum=$ADVISORY
          fi         
          newAdvisory="true"
       fi 
       sleep 60 # we are hotstarting, the advisory is same as last one
    done
    stop_activity_indicator ${pid}
    logMessage "New forecast detected." 
    printf "%02d" $newAdvisoryNum > $STORMDIR/advisoryNumber 2>> $SYSLOG 
    if [ $TRIGGER = rss ]; then
       perl ${SCRIPTDIR}/nhc_advisory_bot.pl --input ${forecastFileName}.html --output $forecastFileName >> ${SYSLOG} 2>&1
    fi
}
#
# checks the local queueing system over and over to see if a job has
# finished ... returns to the calling routine when the job has finished
monitorJobs() 
{   QUEUESYS=$1
    activity_indicator "Monitoring queue for run completion..." &
    pid=$!; trap "stop_activity_indicator ${pid}; exit" EXIT
    sleep 60
    if [ $QUEUESYS = LSF ]; then
        JOBDONE=`bjobs 2>&1`
        until [[ $JOBDONE = "No unfinished job found" ]]; do
           sleep 60
           JOBDONE=`bjobs 2>&1`
        done
    elif [ $QUEUESYS = LoadLeveler ]; then
        while [[ `llq | grep $USER` ]]; do
            sleep 60
        done
    elif [ $QUEUESYS = PBS ]; then
        while [[ `$QCHECKCMD | grep $USER` ]]; do
            sleep 60
        done
    elif [ $QUEUESYS = mpiexec ]; then
        # do nothing, mpiexec has returned at this point
        logMessage "mpiexec has returned"
    elif [ $QUEUESYS = SGE ]; then
        while [[ `$QCHECKCMD | grep $USER` ]]; do
            sleep 60
        done
    else 
       fatal "ERROR: Queueing system $QUEUESYS unrecognized." 
    fi
    logMessage "Job(s) complete."
    stop_activity_indicator ${pid}
}
#
# submits a job to the local queueing system
submitJob()
{   QUEUESYS=$1
    NCPU=$2
    ADCIRCDIR=$3
    ADVISDIR=$4
    SCRIPTDIR=$5
    INPUTDIR=$6
    ENSTORM=$7
    NOTIFYSER=$8
    ENV=$9
    ACCOUNT=${10}
    PPN=${11}
    if [ $QUEUESYS = LSF ]; then
        bsub -x -n $NCPU -q $QUEUENAME -o log.%J -e err.%J -a mvapich mpirun $ADCIRCDIR/padcirc >> ${SYSLOG}
    elif [ $QUEUESYS = LoadLeveler ]; then
        perl $SCRIPTDIR/loadleveler.pl --ncpu $NCPU --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --inputdir $INPUTDIR --enstorm $ENSTORM --notifyuser $NOTIFYUSER > $ADVISDIR/$ENSTORM/padcirc.ll 2>> ${SYSLOG}
        llsubmit $ADVISDIR/$ENSTORM/padcirc.ll >> ${SYSLOG} 2>&1
    elif [ $QUEUESYS = PBS ]; then
        QSCRIPTOPTIONS="--ncpu $NCPU --queuename $QUEUENAME --account $ACCOUNT --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --qscript $INPUTDIR/$QSCRIPT --enstorm $ENSTORM --notifyuser $NOTIFYUSER --walltime $WALLTIME --submitstring $SUBMITSTRING --syslog $SYSLOG"
        if [[ ! -z $PPN ]]; then
           QSCRIPTOPTIONS="$QSCRIPTOPTIONS --ppn $PPN"
        fi
        logMessage "QSCRIPTOPTIONS is $QSCRIPTOPTIONS"
        perl $SCRIPTDIR/$QSCRIPTGEN $QSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/padcirc.pbs 2>> ${SYSLOG}
        logMessage "Submitting $ADVISDIR/$ENSTORM/padcirc.pbs"
        qsub $ADVISDIR/$ENSTORM/padcirc.pbs >> ${SYSLOG} 2>&1
    elif [ $QUEUESYS = mpiexec ]; then
        $SUBMITSTRING $NCPU $ADCIRCDIR/padcirc >> ${SYSLOG} 2>&1 
    elif [ $QUEUESYS = SGE ]; then
        QSCRIPTOPTIONS="--ncpu $NCPU --ncpudivisor $NCPUDIVISOR --queuename $QUEUENAME --account $ACCOUNT --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --qscript $INPUTDIR/$QSCRIPT --enstorm $ENSTORM --notifyuser $NOTIFYUSER --walltime $WALLTIME --submitstring $SUBMITSTRING --syslog $SYSLOG"
        perl $SCRIPTDIR/$QSCRIPTGEN $QSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/padcirc.sge 2>> ${SYSLOG}
        logMessage "Submitting $ADVISDIR/$ENSTORM/padcirc.sge"
        qsub $ADVISDIR/$ENSTORM/padcirc.sge >> ${SYSLOG} 2>&1
    else 
        fatal "ERROR: Queueing system $QUEUESYS unrecognized."
    fi
}
#
# Log file forced to be in /tmp/$$.asgs
logMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] INFO: $@"
  echo ${MSG} >> ${SYSLOG} 
}
#
# log a warning message, execution continues
warn()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] WARNING: $@"
  echo ${MSG} >> ${SYSLOG} 
  echo ${MSG}  # send to console
}
#
# log an error message, execution halts
fatal()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] FATAL ERROR: $@"
  echo ${MSG} >> ${SYSLOG} 
  if [[ $EMAILNOTIFY = YES ]]; then
     cat ${SYSLOG} | mail -s "[ASGS] Fatal Error for PROCID ($$)" "${ASGSADMIN}"
  fi
  echo ${MSG} # send to console
  exit ${EXIT_NOT_OK} 
}
#
# send a message to the console (i.e., window where the script was started)
consoleMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] INFO: $@"
  echo ${MSG}
}
#
# initialization subroutines for the various machines/architectures 
#
init_sapphire()
{ #<- can replace the following with a custom script
  HOSTNAME=sapphire.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvhsp
  SUBMITSTRING="aprun"
  SCRATCHDIR=/work2/$USER
  SSHKEY=id_rsa_sapphire
  QSCRIPT=erdc.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
}

init_jade()
{ #<- can replace the following with a custom script
  HOSTNAME=jade.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvhsp
  SUBMITSTRING="aprun"
# INTERSTRING="qsub -l size=1,walltime=00:10:00 -A $ACCOUNT -q $QUEUENAME -I"
  INTERSTRING=
  SCRATCHDIR=/work/$USER
  SSHKEY=id_rsa_jade
  QSCRIPT=erdc.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
}

init_queenbee()
{ #<- can replace the following with a custom script
  HOSTNAME=queenbee.loni.org
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=loni_asgs2009
  SUBMITSTRING="mpirun"
  SCRATCHDIR=/work/$USER
  SSHKEY=id_rsa_queenbee
  QSCRIPT=queenbee.template.pbs
  QSCRIPTGEN=queenbee.pbs.pl
  PPN=8
}

init_tezpur()
{ #<- can replace the following with a custom script
  HOSTNAME=tezpur.hpc.lsu.edu
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=loni_asgs2009
  SUBMITSTRING="mpirun"
  SCRATCHDIR=/work/$USER
  SSHKEY=id_rsa_tezpur
  QSCRIPT=tezpur.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=4
}

init_ranger()
{ #<- can replace the following with a custom script
  HOSTNAME=ranger.tacc.utexas.edu
  QUEUESYS=SGE
  QCHECKCMD=qstat
  NCPUDIVISOR=16
  ACCOUNT=TG-DMS080016N
  SUBMITSTRING="ibrun tacc_affinity"
  SCRATCHDIR=$SCRATCH
  SSHKEY=id_rsa_ranger
  QSCRIPT=ranger.template.sge
  QSCRIPTGEN=ranger.sge.pl
}

init_lonestar()
{ #<- can replace the following with a custom script
  HOSTNAME=lonestar.tacc.utexas.edu
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=
  SUBMITSTRING="yod"
  SCRATCHDIR=$SCRATCH
  SSHKEY=id_rsa_lonestar
  QSCRIPT=lonestar.template.sge
  QSCRIPTGEN=ranger.sge.pl
}

init_desktop()
{
  HOSTNAME=jason-desktop
  QUEUESYS=mpiexec
  QCHECKCMD="ps -aux | grep mpiexec "
  SUBMITSTRING="mpiexec -n"
  SCRATCHDIR=/srv/asgs
  SSHKEY=id_rsa_jason-desktop
}

init_topsail() 
{ #<- can replace the following with a custom script
  HOSTNAME=topsail.unc.edu
  QUEUESYS=LSF
  INTERSTRING="bsub -q int -Ip"
  SCRATCHDIR=/ifs1/scr/$USER
  SSHKEY=id_rsa_topsail
}

init_test()
{ #<- can replace the following with a custom script
  QUEUESYS=Test
  NCPU=-1
}

# used to dispatch environmentally sensitive actions
# such as queue interactions
env_dispatch(){
 case $1 in
  "sapphire") logMessage "Sapphire (ERDC) configuration found."
          init_sapphire
	  ;;
  "jade") logMessage "Jade (ERDC) configuration found."
          init_jade
	  ;;
  "queenbee") logMessage "Queenbee (LONI) configuration found."
          init_queenbee
	  ;;
  "tezpur") logMessage "Tezpur (LSU) configuration found."
          init_tezpur
          ;;
  "topsail") logMessage "Topsail (UNC) configuration found."
             init_topsail
             ;;
  "ranger") logMessage "Ranger (TACC) configuration found."
          init_ranger
             ;;
  "lonestar") logMessage "Lonestar (TACC) configuration found."
          init_ranger
             ;;
  "desktop") logMessage "desktop configuration found."
          init_desktop 
           ;;
  "test") logMessage "test environment (default) configuration found."
          init_test
          ;; 
  *) fatal "'$1' is not a supported environment; currently supported options: sapphire, jade, ranger, lonestar, queenbee, topsail, desktop"
     ;;
  esac
}
# Option Summary
#  
# -c : set location of configuration file"
# -e (environment): set the computer that the ASGS is running on" 
# -h : show help"
#
# Example:
#   sh asgs_main.sh -c /path/to/config -e topsail 
#
# mail alert
ASGSADMIN="estrabd+lpfs@gmail.com jgflemin@email.unc.edu" #<-- purposefully not in config.sh
#ASGSADMIN="rjweaver@email.unc.edu" #<-- purposefully not in config.sh

# exit statuses
EXIT_NOT_OK=1
EXIT_OK=0

# need to determine standard time format to be used for pasting log files
STARTDATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
SYSLOG=/tmp/asgs-${STARTDATETIME}.$$.log
# create directories with default permissions of "775" and 
# files with the default permssion of "664"
umask 002
#
# Initialize variables accessed from config.sh
TRIGGER="rss"
STARTADVISORYNUM=null
DRY=1           
DEMO=       
STORM=        
YEAR=      
CSDATE=
HOTORCOLD=
LASTSUBDIR=  
FTPSITE=
FTPFCSTDIR=
FTPHCSTDIR=
ADCIRCDIR=
SCRATCHDIR=
MAILINGLIST=
ENV=
QUEUESYS=
QUEUENAME=
QCHECKCMD=
NCPU=
ACCOUNT=
SUBMITSTRING=
INTERSTRING=
RESULTSHOST=
RESULTSPATH=
RESULTSUSERNAME=
RESULTSPROMPT=
RESULTSPASSWORD=
NOTIFYUSER=
STORMDIR=
INPUTDIR=
PERL5LIB=
SSHKEY=
PPN=
logMessage "ASGS Start Up MSG: [PROCID] $$"
logMessage "ASGS Start Up MSG: [SYSLOG] ${SYSLOG}"
logMessage "The ADCIRC Surge Guidance System is activated."
consoleMessage "Please see ASGS log file for detailed information regarding system progress."
consoleMessage "ASGS Start Up MSG: [SYSLOG] The log file is ${SYSLOG}"

SCRIPTDIR=`pwd`
# first - look for SCRIPTDIR
while getopts "c:e:h" optname; do    #<- first getopts for SCRIPTDIR
  case $optname in
    c) CONFIG=${OPTARG}
       if [ -e "${CONFIG}" ]; then 
          logMessage "${CONFIG} found!" 
          # source config file
          . ${CONFIG}
       else
          fatal "${CONFIG} does not exist!"
       fi
       ;;
    e) ENV=${OPTARG} 
       ;;
    h) echoHelp 
       ;;
  esac
done

logMessage "ASGS script directory set to ${SCRIPTDIR}"
# dispatch environment
logMessage "Configuring the ASGS for the '${ENV}' platform..."
env_dispatch ${ENV} 
#
# check existence of all required files and directories
checkDirExistence $ADCIRCDIR "ADCIRC executables directory" 
checkDirExistence $INPUTDIR "directory for input files" 
checkDirExistence $OUTPUTDIR "directory for post processing scripts"
checkDirExistence $PERL5LIB "directory for the Date::Pcalc perl module"
#
checkFileExistence $ADCIRCDIR "ADCIRC preprocessing executable" adcprep
checkFileExistence $ADCIRCDIR "ADCIRC parallel executable" padcirc
checkFileExistence $ADCIRCDIR "hotstart time extraction executable" hstime
checkFileExistence $ADCIRCDIR "asymmetric metadata generation executable" aswip
#
checkFileExistence $INPUTDIR "ADCIRC mesh file" $GRIDFILE
checkFileExistence $INPUTDIR "ADCIRC template fort.15 file" $CONTROLTEMPLATE
# fort.13 (nodal attributes) file is optional
if [[ ! -z $NAFILE ]]; then
   checkFileExistence $INPUTDIR "ADCIRC nodal attributes (fort.13) file" $NAFILE
fi
checkFileExistence $INPUTDIR "preprocessed ADCIRC input archive" $PREPPEDARCHIVE
#
checkFileExistence $OUTPUTDIR "postprocessing initialization script" $INITPOST
checkFileExistence $OUTPUTDIR "postprocessing script" $POSTPROCESS
checkFileExistence $OUTPUTDIR "email notification script" $NOTIFY_SCRIPT
#
checkDirExistence ${PERL5LIB}/Date "subdirectory for the Pcalc.pm perl module"
checkFileExistence ${PERL5LIB}/Date "perl module for date calculations" Pcalc.pm
#
checkDirExistence ~/.ssh "subdirectory containing ssh key"
checkFileExistence ~/.ssh "ssh key file" $SSHKEY
#
# pull in subroutines for email notifications
if [[ $EMAILNOTIFY = YES ]]; then
   # source notifications file
   . ${OUTPUTDIR}/${NOTIFY_SCRIPT}
fi
#
# initialize the directory where the storm will run, based on info from 
# the machine-specific initialization and config.sh
STORMDIR=$SCRATCHDIR/$STORM$YEAR
logMessage "The directory $STORMDIR will be used for this storm"
# set directory to get perl date calcs module from
export PERL5LIB=${SCRIPTDIR}:${PERL5LIB} #<- augment, don't write over existing
# see if the storm directory already exists in the scratch space
if [ ! -d $STORMDIR ]; then
    # -p says make the entire path tree if intermediate dirs do not exist 
    mkdir -p $STORMDIR #
fi
#
if [[ $EMAILNOTIFY = YES ]]; then
   # send out an email to notify users that the ASGS is ACTIVATED
   activation_email $HOSTNAME $STORM $YEAR $STORMDIR $ACTIVATE_LIST
fi
#
OLDADVISDIR=null
CSDATE=$COLDSTARTDATE
if [ $HOTORCOLD = "" ]; then
    START=coldstart
else
    START=$HOTORCOLD
    OLDADVISDIR=$STORMDIR/$LASTSUBDIR
fi
printf "%02d" $STARTADVISORYNUM > $STORMDIR/advisoryNumber 2>> $SYSLOG 
ADVISORY=$STARTADVISORYNUM
#
###############################
#   BODY OF ASGS STARTS HERE    
###############################
ADVISDIR=   # determined below
HSTIME=     # determined below
while [ 1 -eq 1 ]; do
    . ${CONFIG}
    cd $STORMDIR 2>> ${SYSLOG}
    #
    # N O W C A S T
    if [ $START != coldstart ]; then
        logMessage "Checking for new advisory every 60 seconds ..."
    fi
    # download wind data from ftp site every 60 seconds to see if
    # there is a new advisory
    #echo "downloadWindData $STORM $YEAR $STORMDIR $SCRIPTDIR $OLDADVISDIR $TRIGGER $ADVISORY $FTPSITE $RSSSITE $FDIR $HDIR" >> ${SYSLOG}
    downloadWindData $STORM $YEAR $STORMDIR $SCRIPTDIR $OLDADVISDIR $TRIGGER $ADVISORY $FTPSITE $RSSSITE $FDIR $HDIR
    ADVISORY=`cat advisoryNumber`
    ADVISDIR=$STORMDIR/${ADVISORY}
    logMessage "$START Storm $STORM advisory $ADVISORY in $YEAR"
    consoleMessage "$START Storm $STORM advisory $ADVISORY in $YEAR"
    if [[ $EMAILNOTIFY = YES ]]; then
       if [ $START != coldstart ]; then
           new_advisory_email $HOSTNAME $STORM $YEAR $ADVISORY
       fi
    fi
    logMessage "Starting nowcast for advisory $ADVISORY"
    consoleMessage "Starting nowcast for advisory $ADVISORY"
    logMessage "Creating directory $ADVISDIR"
    #
    if [ ! -d $ADVISDIR ]; then 
        mkdir $ADVISDIR 2>> ${SYSLOG}
    fi
    if [ ! -d $ADVISDIR/nowcast ]; then
        mkdir $ADVISDIR/nowcast 2>> ${SYSLOG}
    fi
    # move raw ATCF files into advisory directory
    mv *.fst *.dat $ADVISDIR 2>> ${SYSLOG}
    #
    # perform any initialization of output that must be done once for each 
    # advisory, before the actual runs begin
    logMessage "Initializing post processing for advisory $ADVISORY."
    ${OUTPUTDIR}/${INITPOST} $ADVISDIR $STORM $YEAR $ADVISORY $HOSTNAME 2>> ${SYSLOG}
    if [[ $EMAILNOTIFY = YES ]]; then
       post_init_email $ADVISDIR $STORM $YEAR $ADVISORY $HOSTNAME 
    fi
    #
    # prepare nowcast met (fort.22) and control (fort.15) files 
    METOPTIONS="--dir $ADVISDIR --storm $STORM --year $YEAR --name nowcast --nws $NWS " 
    CONTROLOPTIONS=" --metfile $ADVISDIR/nowcast/fort.22 --name nowcast --dt $TIMESTEPSIZE --nws $NWS --advisorynum $ADVISORY --controltemplate ${INPUTDIR}/${CONTROLTEMPLATE}"
    if [ $START = hotstart ]; then
       cd $OLDADVISDIR/nowcast/PE0000 2>> ${SYSLOG}
       checkHotstart       
       HSTIME=`$ADCIRCDIR/hstime` 2>> ${SYSLOG}
       logMessage "The time in the hotstart file is '$HSTIME' seconds."
       METOPTIONS="$METOPTIONS --hotstartseconds $HSTIME "
       CONTROLOPTIONS="$CONTROLOPTIONS --hst $HSTIME"
    else
       HSTIME=0
       OLDADVISDIR=$ADVISDIR # initialize with dummy value when coldstarting
       logMessage "Coldstarting Storm $STORM in $YEAR"
       logMessage "Coldstart time is '$CSDATE'"
    fi
    cd $ADVISDIR/nowcast 2>> ${SYSLOG}
    if [[ $START = coldstart && -z $CSDATE ]]; then
       logMessage "Generating ADCIRC Met File (fort.22) for nowcast with the following options: $METOPTIONS."
       CSDATE=`${SCRIPTDIR}/storm_track_gen.pl $METOPTIONS 2>> ${SYSLOG}`
    else
       METOPTIONS="$METOPTIONS --coldstartdate $CSDATE " 
       logMessage "Generating ADCIRC Met File (fort.22) for nowcast with the following options: $METOPTIONS."
       ${SCRIPTDIR}/storm_track_gen.pl $METOPTIONS >> ${SYSLOG} 2>&1   
    fi
    # create a new file that contains metadata
    $ADCIRCDIR/aswip >> ${SYSLOG} 2>&1
    if [ -e NWS_19_fort.22 ]; then
       mv fort.22 fort.22.orig
       cp NWS_19_fort.22 fort.22 
    fi
    createMetaDataLink $STORM $YEAR $ADVISORY nowcast $ADVISDIR $HOSTNAME $HSTIME $CSDATE

    CONTROLOPTIONS="$CONTROLOPTIONS --cst $CSDATE " 
    logMessage "Generating ADCIRC Control File (fort.15) for nowcast with the following options: $CONTROLOPTIONS."
     perl $SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS >> ${SYSLOG} 2>&1
    # preprocess
    logMessage $ADVISDIR "Starting nowcast preprocessing."
    prep $ADVISDIR $INPUTDIR nowcast $START $OLDADVISDIR $ENV $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT $NAFILE
    # then submit the job
    logMessage "Submitting ADCIRC nowcast job."
    cd $ADVISDIR/nowcast 2>> ${SYSLOG}
    logMessage "submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR nowcast $NOTIFYUSER $ENV $ACCOUNT $PPN"
    submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR nowcast $NOTIFYUSER $ENV $ACCOUNT $PPN
    # check once per minute until all jobs have finished
    monitorJobs $QUEUESYS 
    consoleMesssage "Job(s) complete."
    # nowcast finished, get on with it
    logMessage "nowcast run finished"
    consoleMessage "nowcast run finished"
    cd $ADVISDIR 2>> ${SYSLOG}
    if [ $START = coldstart ]; then
        START=hotstart
    fi
    #
    # F O R E C A S T
    logMessage "Starting forecast for advisory $ADVISORY"
    consoleMessage "Starting forecast for advisory $ADVISORY"
    cd $ADVISDIR/nowcast/PE0000 2>> ${SYSLOG}
    checkHotstart
    HSTIME=`$ADCIRCDIR/hstime` 2>> ${SYSLOG}
    logMessage "The time in the hotstart file is '$HSTIME' seconds."
    METOPTIONS=" --dir $ADVISDIR --storm $STORM --year $YEAR --coldstartdate $CSDATE --hotstartseconds $HSTIME --nws $NWS "
    CONTROLOPTIONS="--cst $CSDATE --dt $TIMESTEPSIZE --nws $NWS --advisorynum $ADVISORY --controltemplate ${INPUTDIR}/${CONTROLTEMPLATE} --hst $HSTIME"
    let si=0
    while [ $si -lt $ENSEMBLESIZE ]; do  
        ENSTORM=${NAME[${STORMLIST[$si]}]}
        if [ ! -d $ADVISDIR/${ENSTORM} ]; then
           mkdir $ADVISDIR/${ENSTORM} 2>> ${SYSLOG}
        fi
        cd $ADVISDIR/${ENSTORM}
        METOPTIONS="$METOPTIONS --name $ENSTORM" 
        CONTROLOPTIONS="$CONTROLOPTIONS --metfile $ADVISDIR/${ENSTORM}/fort.22 --name $ENSTORM"
        logMessage "Generating ADCIRC Met File (fort.22) for $ENSTORM with the following options: $METOPTIONS."
        ${SCRIPTDIR}/storm_track_gen.pl $METOPTIONS >> ${SYSLOG} 2>&1
        # create a new file that contains metadata
        $ADCIRCDIR/aswip >> ${SYSLOG} 2>&1
        if [ -e NWS_19_fort.22 ]; then
           mv fort.22 fort.22.orig
           cp NWS_19_fort.22 fort.22 
        fi
        createMetaDataLink $STORM $YEAR $ADVISORY $ENSTORM $ADVISDIR $HOSTNAME $HSTIME $CSDATE
        logMessage "Generating ADCIRC Control File (fort.15) for $ENSTORM with the following options: $CONTROLOPTIONS."
        perl $SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS >> ${SYSLOG} 2>&1
        # preprocess
        logMessage "Starting $ENSTORM preprocessing."
        prep $ADVISDIR $INPUTDIR $ENSTORM $START $OLDADVISDIR $ENV $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT $NAFILE
        # then submit the job
        logMessage "Submitting ADCIRC ensemble member $ENSTORM for forecast."
        consoleMessage "Submitting ADCIRC ensemble member $ENSTORM for forecast."
        submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $NOTIFYUSER $ENV $ACCOUNT $PPN
        # check once per minute until job has completed
        monitorJobs $QUEUESYS 
        consoleMesssage "Job(s) complete."
        # execute post processing
        logMessage "$ENSTORM finished; postprocessing"
        # execute post processing
        ${OUTPUTDIR}/${POSTPROCESS} $ADVISDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM 2>> ${SYSLOG} 
        if [[ $EMAILNOTIFY = YES ]]; then
           post_email $ADVISDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM
        fi
        ${OUTPUTDIR}/${POSTPROCESS2} $ADVISDIR $OUTPUTDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $GRIDFILE  2>> ${SYSLOG} 
        si=$[$si + 1];
    done
    logMessage "Forecast complete for advisory $ADVISORY."
    consoleMessage "Forecast complete for advisory $ADVISORY."
    OLDADVISDIR=$ADVISDIR
done
