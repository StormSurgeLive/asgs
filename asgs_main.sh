#!/bin/sh
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
# subroutine to check for the existence of required files that have
# been specified in config.sh
checkFileExistence()
{ FPATH=$1
  FTYPE=$2
  FNAME=$3
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
         logMessage "The hotstart (fort.67) was found in $PWD and contains $hotstartSize bytes."
      fi
   fi  
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
    QUEUESYS=$6 # queueing system (LSF, LoadLeveler, etc)
    NCPU=$7     # number of CPUs to request in parallel jobs
    PREPPEDARCHIVE=$8 # preprocessed fort.13 and fort.14 package 
    GRIDFILE=$9 # fulldomain grid
    NAFILE=$10  # full domain nodal attributes file
    TIMESTAMP=`date +%d%b%Y:%H:%M:%S`
    echo "$TIMESTAMP adcprep.log entry for $FILE for ensemble member $ENSTORM in $ADVISDIR as follows: " >> $ADVISDIR/adcprep.log
    if [ ! -d $ADVISDIR/$ENSTORM ]; then 
	mkdir $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
    fi
    cd $ADVISDIR/$ENSTORM
    logMessage $PWD "Copying fulldomain input files."
    # symbolically link grid 
    if [ ! -e $ADVISDIR/$ENSTORM/fort.14 ]; then 
        ln -s $INPUTDIR/$GRIDFILE $ADVISDIR/$ENSTORM/fort.14 2>> ${SYSLOG}
    fi
    # symbolically link nodal attributes
    if [ ! -e $ADVISDIR/$ENSTORM/fort.13 ]; then
        if [[ -z $NAFILE ]]; then
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
        logMessage $PWD "Copying input files that have already been decomposed."
        cp $INPUTDIR/${PREPPEDARCHIVE} . 2>> ${SYSLOG}
        gunzip -f ${PREPPEDARCHIVE} 2>> ${SYSLOG}
        # untar the uncompressed archive
        UNCOMPRESSEDARCHIVE=${PREPPEDARCHIVE%.gz}
        tar xvf $UNCOMPRESSEDARCHIVE 
        logMessage $PWD "Removing $UNCOMPRESSEDARCHIVE"
        rm $UNCOMPRESSEDARCHIVE 2>> ${SYSLOG}
        # run adcprep to decompose the new fort.15 file
        $INTERSTRING $ADCIRCDIR/adcprep <<END >> $ADVISDIR/adcprep.log
$NCPU
4
fort.14
fort.15
END
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
       # copy fulldomain files
       cp $FROMDIR/fort.80 $ADVISDIR/$ENSTORM/fort.80 2>> ${SYSLOG}
       cp $FROMDIR/partmesh.txt $ADVISDIR/$ENSTORM/partmesh.txt 2>> ${SYSLOG}
       cp $FROMDIR/PE0000/fort.67 $ADVISDIR/$ENSTORM/fort.68
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
           cp $FROMDIR/$file $ADVISDIR/$ENSTORM/$file 2>> ${SYSLOG}
       done
       # run adcprep to decompose the new fort.15 file
       $INTERSTRING $ADCIRCDIR/adcprep <<END >> $ADVISDIR/adcprep.log
$NCPU
4
fort.14
fort.15
END
        # run adcprep to decompose the hotstart file
        $INTERSTRING $ADCIRCDIR/adcprep <<END >> $ADVISDIR/adcprep.log
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
    cd $STORMDIR
    OPTIONS="--storm $STORM --year $YEAR"
    OPTIONS="$OPTIONS --site $FTPSITE --fdir $FDIR --hdir $HDIR"
    if [ "$START" = coldstart ]; then
       logMessage $PWD "Downloading initial hindcast/forecast."
       perl $SCRIPTDIR/get_atcf.pl $OPTIONS 2>> $SYSLOG
    else
       logMessage $PWD "Waiting to download new advisory..."
       # move hindcast/forecast files to "old" if they exist
       if [ -e fort.22.hindcast ]; then
          mv fort.22.hindcast fort.22.hindcast.old 2>> ${SYSLOG}
          logMessage $PWD "Moving fort.22.hindcast to fort.22.hindcast.old."
       fi
       if [ -e fort.22.forecast ]; then
          mv fort.22.forecast fort.22.forecast.old 2>> ${SYSLOG}
          logMessage $PWD "Moving fort.22.forecast fort.22.forecast.old."
       fi
       while [ 1 -eq 1 ]; do
       perl $SCRIPTDIR/get_atcf.pl $OPTIONS 2>> $SYSLOG
          if ! diff fort.22.forecast fort.22.forecast.old > /dev/null 2>> ${SYSLOG}; then
             logMessage $PWD "New forecast detected."
             break
          else
             sleep 60 # new forecast not yet published by NHC
          fi
       done
    fi
}
#
# checks the local queueing system over and over to see if a job has
# finished ... returns to the calling routine when the job has finished
monitorJobs() 
{   QUEUESYS=$1
    if [ $QUEUESYS = LSF ]; then
        sleep 60
        JOBDONE=`bjobs 2>&1`
        until [[ $JOBDONE = "No unfinished job found" ]]; do
           sleep 60
           JOBDONE=`bjobs 2>&1`
        done
    elif [ $QUEUESYS = LoadLeveler ]; then
        sleep 60
        while [[ `llq | grep $USER` ]]; do
            sleep 60
        done
    elif [ $QUEUESYS = PBS ]; then
        sleep 60
        while [[ `$QCHECKCMD | grep $USER` ]]; do
            sleep 60
        done
    elif [ $QUEUESYS = mpiexec ]; then
        sleep 60
        while [[ `$QCHECKCMD | grep padcirc` ]]; do
            logMessage "Using '$QCHECKCMD | grep padcirc' to monitor the job." #jgfdebug
            sleep 60
        done
    else 
       fatal "ERROR: Queueing system $QUEUESYS unrecognized." 
    fi
    logMessage "Job(s) complete."
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
    if [ $QUEUESYS = LSF ]; then
        bsub -x -n $NCPU -q $QUEUENAME -o log.%J -e err.%J -a mvapich mpirun $ADCIRCDIR/padcirc >> ${SYSLOG}
    elif [ $QUEUESYS = LoadLeveler ]; then
        perl $SCRIPTDIR/loadleveler.pl --ncpu $NCPU --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --inputdir $INPUTDIR --enstorm $ENSTORM --notifyuser $NOTIFYUSER > $ADVISDIR/$ENSTORM/padcirc.ll 2>> ${SYSLOG}
        llsubmit $ADVISDIR/$ENSTORM/padcirc.ll >> ${SYSLOG} 2>&1
    elif [ $QUEUESYS = PBS ]; then
        perl $SCRIPTDIR/pbs.pl --ncpu $NCPU --queuename $QUEUENAME --account $ACCOUNT --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --inputdir $INPUTDIR --enstorm $ENSTORM --notifyuser $NOTIFYUSER --submitstring $SUBMITSTRING > $ADVISDIR/$ENSTORM/padcirc.pbs 2>> ${SYSLOG}
        logMessage "Submitting $ADVISDIR/$ENSTORM/padcirc.pbs"
        qsub $ADVISDIR/$ENSTORM/padcirc.pbs >> ${SYSLOG} 2>&1
    elif [ $QUEUESYS = mpiexec ]; then
        logMessage "Submitting job via $SUBMITSTRING $NCPU $ADCIRCDIR/padcirc 2>> ${SYSLOG}"
        $SUBMITSTRING $NCPU $ADCIRCDIR/padcirc 2>> ${SYSLOG}
    elif [ $QUEUESYS = SGE ]; then
        fatal "ERROR: The SGE system is not supported yet."
    else 
        fatal "ERROR: Queueing system $QUEUESYS unrecognized."
    fi
}
#
# Log file forced to be in /tmp/$$.asgs
logMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="MSG...[${DATETIME}]: $@"
  echo ${MSG} >> ${SYSLOG} 
  echo ${MSG}
}
#
# log a warning message, execution continues
warn()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="WARN..[${DATETIME}]: $@"
  echo ${MSG} >> ${SYSLOG} 
  echo ${MSG} 
}
#
# log an error message, execution halts
fatal()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="FATAL.[${DATETIME}]: $@"
  echo ${MSG} >> ${SYSLOG} 
  if [[ $EMAILNOTIFY = YES ]]; then
     cat ${SYSLOG} | mail -s "[ASGS] Fatal Error for PROCID ($$)" "${ASGSADMIN}"
  fi
  echo ${MSG}
  exit ${EXIT_NOT_OK} 
}
#
# initialization subroutines for the various machines/architectures 
#
init_sapphire()
{ #<- can replace the following with a custom script
  HOSTNAME=sapphire.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvenq
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
  ACCOUNT=erdcvenq
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
}

init_ranger()
{ #<- can replace the following with a custom script
  HOSTNAME=ranger.tacc.utexas.edu
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=
  SUBMITSTRING="yod"
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
  QCHECKCMD="ps aux | grep mpiexec "
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
#  -d dir      -> looks for config.sh here; defaults to current directory
#  -e env_tag  -> machine to config for (ranger, sapphire, queenbee, etc)
#  -h          -> show help and exit script
#
#
# Example:
#   sh asgs_main.sh -d /path/to/config -e topsail 
#
# mail alert
ASGSADMIN="estrabd+lpfs@gmail.com jgflemin@email.unc.edu" #<-- purposefully not in config.sh

# exit statuses
EXIT_NOT_OK=1
EXIT_OK=0

# need to determine standard time format to be used for pasting log files
STARTDATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
SYSLOG=/tmp/asgs-${STARTDATETIME}.$$.log

#
# Initialize variables accessed from config.sh
DRY=1           
DEMO=       
STORM=        
YEAR=      
COLDSTARTDATE=
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

logMessage "ASGS Start Up MSG: [PROCID] $$"
logMessage "ASGS Start Up MSG: [SYSLOG] ${SYSLOG}"

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
#
checkFileExistence $INPUTDIR "ADCIRC mesh file" $GRIDFILE
checkFileExistence $INPUTDIR "ADCIRC template fort.15 file" $CONTROLTEMPLATE
checkFileExistence $INPUTDIR "ADCIRC nodal attributes (fort.13) file" $NAFILE
checkFileExistence $INPUTDIR "preprocessed ADCIRC input archive" $PREPPEDARCHIVE
#
checkFileExistence $OUTPUTDIR "postprocessing initialization script" $INITPOST
checkFileExistence $OUTPUTDIR "postprocessing script" $POSTPROCESS
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
   . ${OUTPUTDIR}/notify.sh
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
   email_activation $HOSTNAME $STORM $YEAR $STORMDIR
fi
#
if [ $HOTORCOLD = "" ]; then
    START=coldstart
else
    START=$HOTORCOLD
    CSTIME=$COLDSTARTDATE
    OLDADVISDIR=$STORMDIR/$LASTSUBDIR
fi
#
logMessage "$STORMDIR $START Storm $STORM in $YEAR"
#
###############################
#   BODY OF ASGS STARTS HERE    
###############################
ADVISORY=   # determined below
ADVISDIR=   # determined below
while [ 1 -eq 1 ]; do
    . $SCRIPTDIR/config.sh
    cd $STORMDIR 2>> ${SYSLOG}
    #
    # N O W C A S T
    if [ $START != coldstart ]; then
        logMessage $PWD "Checking for new advisory every 60 seconds ..."
    fi
    # download wind data from ftp site every 60 seconds to see if
    # there is a new advisory
    downloadWindData $STORM $YEAR $STORMDIR $SCRIPTDIR 
    if [[ $EMAILNOTIFY = YES ]]; then
       if [ $START != coldstart ]; then
           new_advisory_email $HOSTNAME $STORM $YEAR $ADVISORY
       fi
    fi
    logMessage $PWD "Starting nowcast for advisory $ADVISORY"
    advformat="%02d"
    ADVISORY=`printf "$advformat" $ADVISORYNUM`
    ADVISDIR=$STORMDIR/${ADVISORY}
    logMessage $PWD "Creating directory $ADVISDIR"
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
    ${OUTPUTDIR}/${INITPOST} $HOSTNAME $STORM $YEAR 2>> ${SYSLOG}
    #
    # prepare nowcast met (fort.22) and control (fort.15) files 
    METOPTIONS="--dir $ADVISDIR --storm $STORM --year $YEAR --coldstartdate $COLDSTARTDATE --name nowcast" 
    CONTROLOPTIONS=" --cst $COLDSTARTDATE --metfile $ADVISDIR/nowcast/fort.22 --name nowcast --dt $TIMESTEPSIZE --controltemplate ${INPUTDIR}/${CONTROLTEMPLATE}"
    if [ $START = hotstart ]; then
       cd $OLDADVISDIR/nowcast/PE0000 2>> ${SYSLOG}
       checkHotstart       
       HSTIME=`$ADCIRCDIR/hstime` 2>> ${SYSLOG}
       logMessage "Time in hotstart file is $HSTIME."
       METOPTIONS="$METOPTIONS --hotstartseconds $HSTIME "
       CONTROLOPTIONS="$CONTROLOPTIONS --hst $HSTIME"
    else
       logMessage $ADVISDIR "Coldstarting Storm $STORM in $YEAR"
       logMessage $ADVISDIR "Coldstart time is $COLDSTARTDATE"
    fi
    cd $ADVISDIR/nowcast 2>> ${SYSLOG}
    logMessage "Generating ADCIRC Met File (fort.22) for nowcast with the following options: $METOPTIONS."
    ${SCRIPTDIR}/storm_track_gen.pl $METOPTIONS 2>> ${SYSLOG} 
    logMessage "Generating ADCIRC Control File (fort.15) for nowcast with the following options: $CONTROLOPTIONS."
     perl $SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS 2>> ${SYSLOG}
    # preprocess
    logMessage $ADVISDIR "Starting nowcast preprocessing."
    prep $ADVISDIR $INPUTDIR nowcast $START $OLDADVISDIR $QUEUESYS $NCPU $PREPPEDARCHIVE $GRIDFILE $NAFILE
    # then submit the job
    logMessage $PWD "Submitting ADCIRC nowcast job."
    cd $ADVISDIR/nowcast 2>> ${SYSLOG}
    submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR nowcast $NOTIFYUSER
    # check once per minute until all jobs have finished
    monitorJobs $QUEUESYS 
    # nowcast finished, get on with it
    logMessage $PWD "nowcast run finished"
    cd $ADVISDIR 2>> ${SYSLOG}
    if [ $START = coldstart ]; then
        START=hotstart
    fi
    #
    # F O R E C A S T
    logMessage $PWD "Starting forecast for advisory $ADVISORY"
    cd $ADVISDIR/nowcast/PE0000 2>> ${SYSLOG}
    checkHotstart
    HSTIME=`$ADCIRCDIR/hstime` 2>> ${SYSLOG}
    logMessage "Time in hotstart file is $HSTIME."
    METOPTIONS=" --dir $ADVISDIR --storm $STORM --year $YEAR --coldstartdate $COLDSTARTDATE --hotstartseconds $HSTIME "
    CONTROLOPTIONS="--cst $COLDSTARTDATE --dt $TIMESTEPSIZE --controltemplate ${INPUTDIR}/${CONTROLTEMPLATE} --hst $HSTIME"
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
        ${SCRIPTDIR}/storm_track_gen.pl $METOPTIONS 2>> ${SYSLOG} 
        logMessage "Generating ADCIRC Control File (fort.15) for $ENSTORM with the following options: $CONTROLOPTIONS."
        perl $SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS 2>> ${SYSLOG}
        # preprocess
        logMessage $PWD "Starting $ENSTORM preprocessing."
        prep $ADVISDIR $INPUTDIR $ENSTORM $START $OLDADVISDIR $QUEUESYS $NCPU $PREPPEDARCHIVE $GRIDFILE $NAFILE
        # then submit the job
        logMessage $PWD "Submitting ADCIRC ensemble member $ENSTORM for forecast."
        submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $NOTIFYUSER
        # check once per minute until job has completed
        monitorJobs $QUEUESYS
        # execute post processing
        logMessage $PWD "$ENSTORM finished; postprocessing"
        # execute post processing
        ${SCRIPTDIR}/output/post.sh 2>> ${SYSLOG} 
        si=$[$si + 1];
    done
    logMessage $PWD "Forecast complete for advisory $ADVISORY."
    OLDADVISDIR=$ADVISDIR
    ADVISORYNUM=$[$ADVISORYNUM + 1] # jgfdebug: this is temporary, until we get rss xml feed 
done
