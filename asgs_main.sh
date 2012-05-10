#!/bin/bash
#----------------------------------------------------------------
#
# asgs_main.sh: This is the main driver script for the ADCIRC Surge Guidance
# System (ASGS). It performs configuration tasks via config.sh, then enters a
# loop which is executed once per advisory cycle.
# 
#----------------------------------------------------------------
# Copyright(C) 2006, 2007, 2008, 2009, 2010, 2011 Jason Fleming
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
#####################################################################
#                B E G I N   F U N C T I O N S
#####################################################################
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
# the subroutine assumes that the hotstart file is named fort.$LUN or 
# fort.$LUN.nc depending on the format and expects it to be provided with 
# the full path if it is not in the current directory. 
checkHotstart()
{
   FROMDIR=$1
   HOTSTARTFORMAT=$2 
   LUN=$3
#
   HOTSTARTFILE=
   # set name and specific file location based on format (netcdf or binary)
   if [[ $HOTSTARTFORMAT = netcdf ]]; then
      HOTSTARTFILE=$FROMDIR/fort.$LUN.nc
   else
      HOTSTARTFILE=$FROMDIR/PE0000/fort.$LUN
   fi
   # check for existence of hotstart file
   if [ ! -e $HOTSTARTFILE ]; then
      fatal "The hotstart file '$HOTSTARTFILE' was not found. The preceding simulation run must have failed to produce it."
   # if it exists, check size to be sure its nonzero
   else 
      hotstartSize=`stat -c %s $HOTSTARTFILE`
      if [ $hotstartSize == "0" ]; then 
         fatal "The hotstart file '$HOTSTARTFILE' is of zero length. The preceding simulation run must have failed to produce it properly."
      else
         logMessage "The hotstart file '$HOTSTARTFILE' was found and it contains $hotstartSize bytes."
         # check time in hotstart file to be sure it can be found and that
         # it is nonzero
         HSTIME=
         if [[ $HOTSTARTFORMAT = netcdf ]]; then
            HSTIME=`$ADCIRCDIR/hstime -f $HOTSTARTFILE -n` 2>> ${SYSLOG}
         else
            HSTIME=`$ADCIRCDIR/hstime -f $HOTSTARTFILE` 2>> ${SYSLOG}
         fi
         errorOccurred=`expr index "$HSTIME" ERROR`
         if [[ $errorOccurred != 0 ]]; then
            fatal "The hstime utility could not read the ADCIRC time from the file '$HOTSTARTFILE'. The output from hstime was as follows: '$HSTIME'."
         else
            if float_cond '$HSTIME == 0.0'; then
               fatal "The time in the hotstart file '$HOTSTARTFILE' is zero. The preceding simulation run must have failed to produce a proper hotstart file."
            fi
         fi
      fi
   fi  
}
#
# Evaluate a floating point number conditional expression.
# From http://www.linuxjournal.com/content/floating-point-math-bash
function float_cond()
{
    local cond=0
    if [[ $# -gt 0 ]]; then
        cond=$(echo "$*" | bc -q 2>/dev/null)
        if [[ -z "$cond" ]]; then cond=0; fi
        if [[ "$cond" != 0  &&  "$cond" != 1 ]]; then cond=0; fi
    fi
    local stat=$((cond == 0))
    return $stat
}
#
# subroutine to create a symbolic link to the fort.22 file that has metadata in # to identify the type of data that is in the file 
createMetaDataLink()
{ 
  STORM=$1
  YEAR=$2
  GRIDNAME=$3
  ADVISORY=$4
  ENSTORM=$5
  ADVISDIR=$6
  HOSTNAME=$7
  HSTIME=$8
  CSDATE=$9
  VARIATION=${10}
  PERCENT=${11}  
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
  echo "mesh:$GRIDNAME" >>  $ADVISDIR/$ENSTORM/fort.22.meta
#
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
# subroutine to run adcprep, using a pre-prepped archive of fort.13,  
# fort.14 and fort.18 files
prep()
{   ADVISDIR=$1  # directory containing the now/forecast runs for this cycle
    INPUTDIR=$2 # directory where grid and nodal attribute files are found
    ENSTORM=$3  # ensemble member (nowcast, storm1, storm5, etc) 
    START=$4    # coldstart or hotstart
    OLDADVISDIR=$5 # directory containing last advisory
    ENV=$6     # machine to run on (jade, desktop, queenbee, etc)
    NCPU=$7     # number of CPUs to request in parallel jobs
    PREPPEDARCHIVE=$8 # preprocessed fort.13 and fort.14 package 
    GRIDFILE=$9 # fulldomain grid
    ACCOUNT=${10} # account to charge time to 
    OUTPUTOPTIONS="${11}" # contains list of args for appending files 
    HOTSTARTCOMP=${12} # fulldomain or subdomain
    WALLTIME=${13} # HH:MM:SS format
    HOTSTARTFORMAT=${14}   # "binary" or "netcdf"
    MINMAX=${15}           # "continuous" or "reset"  
    HOTSWAN=${16} # "yes" or "no" to reinitialize SWAN only
    NAFILE=${17}  # full domain nodal attributes file, must be last in the
                  # argument list, since it may be undefined
    TIMESTAMP=`date +%d%b%Y:%H:%M:%S`
#
    # set the name of the archive of preprocessed input files
    PREPPED=$PREPPEDARCHIVE
    if [[ $START = coldstart ]]; then
       PREPPED=$HINDCASTARCHIVE
    fi
    # determine if there is an archive of preprocessed input files
    HAVEARCHIVE=yes
    if [[ ! -e ${INPUTDIR}/${PREPPED} ]]; then
       HAVEARCHIVE=no
    fi
    # create directory to run in
    if [ ! -d $ADVISDIR/$ENSTORM ]; then 
	   mkdir $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
    fi
    echo "$TIMESTAMP adcprep.log entry for $FILE for ensemble member $ENSTORM in $ADVISDIR as follows: " >> $ADVISDIR/$ENSTORM/adcprep.log
    cd $ADVISDIR/$ENSTORM
    logMessage "Linking to full domain input files."
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
    if [[ $HAVEARCHIVE = yes ]]; then
        # copy in the files that have already been preprocessed
        logMessage "Copying input files that have already been decomposed."
        cp $INPUTDIR/${PREPPED} . 2>> ${SYSLOG}
        gunzip -f ${PREPPED} 2>> ${SYSLOG}
        # untar the uncompressed archive
        UNCOMPRESSEDARCHIVE=${PREPPED%.gz}
        tar xvf $UNCOMPRESSEDARCHIVE > untarred_files.log 2>> ${SYSLOG}
        logMessage "Removing $UNCOMPRESSEDARCHIVE"
        rm $UNCOMPRESSEDARCHIVE 2>> ${SYSLOG}
    fi
    #
    # this is a   C O L D S T A R T
    if [ $START = coldstart ]; then
       # if we have variable river flux, link the fort.20 and fort.88 files
       if [[ $VARFLUX = on ]]; then
          # jgf20110525: For now, just copy a static file to this location
          # and adcprep it. TODO: When real time flux data become available,
          # grab those instead of relying on a static file.
          ln -s ${INPUTDIR}/${HINDCASTRIVERFLUX} ./fort.20        
          # run adcprep to decompose the river elevation init (fort.88) file
          ln -s ${INPUTDIR}/${RIVERINIT} ./fort.88
       fi
       # now run adcprep to decompose the files
       if [[ $HAVEARCHIVE = no ]]; then
          logMessage "Running adcprep to partition the mesh for $NCPU compute processors."
          prepFile partmesh $NCPU $ACCOUNT $WALLTIME 
          logMessage "Running adcprep to prepare all files."
          prepFile prepall $NCPU $ACCOUNT $WALLTIME 
       else
          logMessage "Running adcprep to prepare new fort.15 file."      
          prepFile prep15 $NCPU $ACCOUNT $WALLTIME
          if [[ $VARFLUX = on ]]; then
             logMessage "Running adcprep to prepare new fort.20 file."      
             prepFile prep20 $NCPU $ACCOUNT $WALLTIME
             logMessage "Running adcprep to prepare fort.88 file."
             prepFile prep88 $NCPU $ACCOUNT $WALLTIME
          fi
       fi
    else
       # this is a   H O T S T A R T
       #
       # copy in the swaninit file which contains the name of the swan 
       # control file (conventionally named fort.26 when used with ADCIRC) 
       if [[ $WAVES = on ]]; then
          cp $INPUTDIR/swaninit.template $ADVISDIR/$ENSTORM/swaninit 2>> ${SYSLOG}
       fi
       # run adcprep to decompose the new files
       if [[ $HAVEARCHIVE = no ]]; then
          logMessage "Running adcprep to partition the mesh for $NCPU compute processors."
          prepFile partmesh $NCPU $ACCOUNT $WALLTIME 
          logMessage "Running adcprep to prepare all files."
          prepFile prepall $NCPU $ACCOUNT $WALLTIME 
       else
          logMessage "Running adcprep to prepare new fort.15 file."
          prepFile prep15 $NCPU $ACCOUNT $WALLTIME
          if [[ $VARFLUX = on ]]; then
             logMessage "Running adcprep to prepare new fort.20 file."      
             prepFile prep20 $NCPU $ACCOUNT $WALLTIME
          fi
          if [[ $WAVES = on ]]; then
             PE=0
             format="%04d"
             while [[ $PE -lt $NCPU ]]; do
                PESTRING=`printf "$format" $PE`
                ln -s $ADVISDIR/$ENSTORM/fort.26 $ADVISDIR/$ENSTORM/PE${PESTRING}/fort.26 2>> ${SYSLOG}
                PE=`expr $PE + 1`
             done
          fi
       fi
       # set directory where data will be copied from     
       if [[ $ENSTORM = nowcast ]]; then
          # this is a nowcast run, and we are getting the hotstart file
          # either from the ASGS's last nowcast run or from an initial 
          # hindcast run
          if [[ -d $OLDADVISDIR/nowcast ]]; then
             FROMDIR=$OLDADVISDIR/nowcast
          fi
          if [[ -d $OLDADVISDIR/hindcast ]]; then
             FROMDIR=$OLDADVISDIR/hindcast 
             HOTSWAN=off # hindcast runs don't include swan
          fi
       else 
          # this is a forecast run, and we get the hotstart file from
          # our own nowcast run in our own advisory directory
          FROMDIR=$ADVISDIR/nowcast
       fi
       logMessage "Copying existing output files to this directory."
       if [[ $MINMAX = continuous ]]; then
          # copy max and min files so that the max values will be 
          # preserved across hotstarts
          for file in maxele.63 maxwvel.63 minpr.63 maxrs.63 maxvel.63; do
             if  [ -e $FROMDIR/$file ]; then
                logMessage "Copying $FROMDIR/$file to $ADVISDIR/$ENSTORM/$file so that its values will be preserved across the hotstart." 
                cp $FROMDIR/$file $ADVISDIR/$ENSTORM/$file 2>> ${SYSLOG}
             fi
          done
       else
          logMessage "MINMAX was set to '$MINMAX' in the ASGS config file; as a result, the maxele.63 etc files will not be from the previous run to the current run. ADCIRC will start the record of max and min values anew."
       fi
       # copy existing fulldomain files if they are supposed to be appended
       for file in fort.61 fort.62 fort.63 fort.64 fort.71 fort.72 fort.73 fort.74; do
          matcharg=--${file/./}append
          if [[ $file = "fort.71" || $file = "fort.72" ]]; then
             matcharg="--fort7172append"
          elif [[ $file = "fort.73" || $file = "fort.74" ]]; then
             matcharg="--fort7374append"
          fi
          # check the output options to see if the file is being appended
          for arg in $OUTPUTOPTIONS ; do 
             if [[ $matcharg = $arg ]]; then 
                # the file is being appended; check to see if it is in netcdf
                # format, nd if so, use the netcdf name
                netCDFArg=${matcharg/append/netcdf/}      
                for outputArg in $OUTPUTOPTIONS ; do 
                   if [[ $outputArg = $netCDFArg ]]; then
                      file=${file/./}.nc
                   fi
                done
                if [ -e $FROMDIR/$file ]; then
                   logMessage "Copying $FROMDIR/$file to $ADVISDIR/$ENSTORM/$file so that it will be appended during the upcoming run." 
                   cp $FROMDIR/$file $ADVISDIR/$ENSTORM/$file 2>> ${SYSLOG}
                fi
             fi
          done
       done
       # bring in hotstart file(s)
       if [[ $HOTSTARTCOMP = fulldomain ]]; then
          if [[ $HOTSTARTFORMAT = netcdf ]]; then
             # copy netcdf file so we overwrite the one that adcprep created
             cp --remove-destination $FROMDIR/fort.67.nc $ADVISDIR/$ENSTORM/fort.68.nc >> $SYSLOG 2>&1
          else
             ln -s $FROMDIR/PE0000/fort.67 $ADVISDIR/$ENSTORM/fort.68 >> $SYSLOG 2>&1
          fi
       fi
       if [[ $HOTSTARTCOMP = subdomain || $WAVES = on ]]; then
          # copy the subdomain hotstart files over
          # subdomain hotstart files are always binary formatted          
          PE=0
          format="%04d"
          logMessage "Starting copy of subdomain hotstart files." 
          while [ $PE -lt $NCPU ]; do
             PESTRING=`printf "$format" $PE`
             if [[ $HOTSTARTCOMP = subdomain ]]; then
                cp $FROMDIR/PE${PESTRING}/fort.67 $ADVISDIR/$ENSTORM/PE${PESTRING}/fort.68 2>> ${SYSLOG}
             fi
             if [[ $WAVES = on && $HOTSWAN = on ]]; then
                 cp $FROMDIR/PE${PESTRING}/swan.67 $ADVISDIR/$ENSTORM/PE${PESTRING}/swan.68 2>> ${SYSLOG}            
             fi
             PE=`expr $PE + 1`
          done
          logMessage "Completed copy of subdomain hotstart files."
       fi 
    fi 
    # if we don't have an archive of our preprocessed files, create
    # one so that we don't have to do another prepall 
    if [[ $HAVEARCHIVE = no ]]; then
       logMessage "Creating an archive of preprocessed files and saving to ${INPUTDIR}/${PREPPED} to avoid having to run prepall again."
       FILELIST='partmesh.txt PE*/fort.14 PE*/fort.18'
       if [[ ! -z $NAFILE ]]; then
          FILELIST='partmesh.txt PE*/fort.14 PE*/fort.18 PE*/fort.13'
       fi
       tar cvzf ${INPUTDIR}/${PREPPED} ${FILELIST} 2>> ${SYSLOG}
       # check status of tar operation; if it failed, delete the file
       # it attempted to make and alert the operator
       if [[ $? != 0 ]]; then
          warn "The construction of a tar archive of the preprocessed input files has failed."
          rm ${INPUTDIR}/${PREPPED} 2>> ${SYSLOG} 2>&1
       fi
    fi 
}
#
# function to run adcprep in a platform dependent way to decompose 
# the fort.15, fort.20, or fort.88 file
prepFile()
{   JOBTYPE=$1
    NCPU=$2
    ACCOUNT=$3
    WALLTIME=$4
    if [[ $QUEUESYS = PBS ]]; then
       QSCRIPTOPTIONS="--jobtype $JOBTYPE --ncpu $NCPU --ppn $PPN --queuename $SERQUEUE --account $ACCOUNT --walltime $WALLTIME --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --qscript $INPUTDIR/$PREPCONTROLSCRIPT --enstorm ${ENSTORM} --notifyuser $NOTIFYUSER --syslog $SYSLOG"
       perl $SCRIPTDIR/$QSCRIPTGEN $QSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/adcprep.${JOBTYPE}.pbs 2>> ${SYSLOG}
       # submit adcprep job, check to make sure qsub succeeded, and if not, retry
       while [[ 1 = 1 ]];  do
          qsub $ADVISDIR/$ENSTORM/adcprep.${JOBTYPE}.pbs >> ${SYSLOG} 2>&1
          if [[ $? = 0 ]]; then
             break # qsub returned a "success" status
          else
             warn "qsub $ADVISDIR/$ENSTORM/adcprep.${JOBTYPE}.pbs failed; will retry in 60 seconds."
             sleep 60
          fi
       done
       monitorJobs $QUEUESYS ${JOBTYPE}.${ENSTORM} $WALLTIME
       logMessage "Finished adcprepping file ($JOBTYPE)."
    elif [[ $QUEUESYS = SGE ]]; then
       cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
       SERQSCRIPT=ranger.template.serial
       SERQSCRIPTOPTIONS="--jobtype $JOBTYPE --ncpu $NCPU --account $ACCOUNT --adcircdir $ADCIRCDIR --walltime $WALLTIME --advisdir $ADVISDIR --enstorm $ENSTORM --notifyuser $NOTIFYUSER --serqscript $INPUTDIR/$SERQSCRIPT"
       perl $SCRIPTDIR/ranger.serial.pl  $SERQSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/adcprep.serial.sge 2>> ${SYSLOG}
       logMessage "Submitting $ADVISDIR/$ENSTORM/adcprep.serial.sge"
       qsub $ADVISDIR/$ENSTORM/adcprep.serial.sge >> ${SYSLOG} 2>&1
       # if qsub succeeded, monitor the job, otherwise an error is indicated
       if [[ $? = 1 ]]; then
          rangerResubmit $ADVISDIR $ENSTORM adcprep.serial.sge $SYSLOG
       fi
       # check once per minute until all jobs have finished
       monitorJobs $QUEUESYS ${JOBTYPE}.${ENSTORM} $WALLTIME
       consoleMesssage "Job(s) complete."
       # prep-ing finished, get on with it
       logMessage "adcprep finished"
       consoleMessage "adcprep finished"
    else
       $ADCIRCDIR/adcprep --np $NCPU --${JOBTYPE} >> $ADVISDIR/$ENSTORM/adcprep.log 2>&1
    fi
}
#
# subroutine that calls an external script over and over until it
# pulls down a new advisory (then it returns)
downloadCycloneData() 
{   STORM=$1
    YEAR=$2
    RUNDIR=$3
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
    cd $RUNDIR 2>> ${SYSLOG}
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
       # logMessage "newAdvisoryNum is $newAdvisoryNum"
       # check to see if we have a new one, and if so, determine the
       # new advisory number correctly
       case $TRIGGER in 
       "ftp")
          if [ $START = hotstart ]; then
             if ! diff $OLDADVISDIR/$forecastFileName ./$forecastFileName > /dev/null 2>> ${SYSLOG}; then
                # forecasts from NHC ftp site do not have advisory number
                newAdvisoryNum=$[$ADVISORY + 1]
                printf "%02d" $newAdvisoryNum > $RUNDIR/advisoryNumber 2>> $SYSLOG 
                newAdvisory="true"
             fi 
          fi
          ;;
       "rss")
          # if there was a new advisory, the get_atcf.pl script
          # would have returned the advisory number in stdout
          if [ ! -z $newAdvisoryNum ]; then
             printf  $newAdvisoryNum > $RUNDIR/advisoryNumber 2>> $SYSLOG 
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
    if [ $TRIGGER = rss ]; then
       perl ${SCRIPTDIR}/nhc_advisory_bot.pl --input ${forecastFileName}.html --output $forecastFileName >> ${SYSLOG} 2>&1
    fi
}
#
# subroutine that polls an external ftp site for background meteorology data,
# converts it to OWI format (reprojecting the data if necessary), makes 
# symbolic links to it, and returns.
downloadBackgroundMet() 
{   
   RUNDIR=$1
   SCRIPTDIR=$2
   BACKSITE=$3
   BACKDIR=$4
   ENSTORM=$5
   CSDATE=$6
   HSTIME=$7
   FORECASTLENGTH=$8
   ALTNAMDIR=$9
   FORECASTCYCLE=${10}
   ARCHIVEBASE=${11}
   ARCHIVEDIR=${12}
#
#   activity_indicator "Checking remote site for background meteorology data..." &
#   pid=$!; trap "stop_activity_indicator ${pid}; exit" EXIT
#   stop_activity_indicator ${pid} 
   cd $RUNDIR 2>> ${SYSLOG}
   if [[ $ENSTORM != "nowcast" ]]; then
      cp advisoryNumber currentCycle 2>> ${SYSLOG}
   fi
   echo "0" > advisoryNumber 2>> ${SYSLOG}
   while [[ `cat advisoryNumber` -lt 2 ]]; do
      OPTIONS="--rundir $RUNDIR --backsite $BACKSITE --backdir $BACKDIR --enstorm $ENSTORM --csdate $CSDATE --hstime $HSTIME --forecastlength $FORECASTLENGTH --altnamdir $ALTNAMDIR --scriptdir $SCRIPTDIR --forecastcycle $FORECASTCYCLE --archivedruns ${ARCHIVEBASE}/${ARCHIVEDIR}"
      perl ${SCRIPTDIR}/get_nam.pl $OPTIONS 2>> ${SYSLOG} > advisoryNumber
      if [[ `cat advisoryNumber` -lt 2 ]]; then
         sleep 60
      fi
   done   
}
#
# subroutine that downloads river flux data from an external ftp site 
# and constructs a river flux boundary condition file (fort.20) to covert
# the full time period of the run
downloadRiverFluxData() 
{   
   ADVISDIR=$1
   MESHFILE=$2
   RIVERSITE=$3
   RIVERDIR=$4
   ENSTORM=$5
   CSDATE=$6
   HSTIME=$7
   SCRIPTDIR=$8
   DEFAULTFILE=$9
   USERIVERFILEONLY=${10}
#
   OPTIONS="--advisdir $ADVISDIR --meshfile $MESHFILE --riversite $RIVERSITE --riverdir $RIVERDIR --enstorm $ENSTORM --csdate $CSDATE --hstime $HSTIME --scriptdir $SCRIPTDIR --defaultfile $DEFAULTFILE"
   TRIES=0 
   SUCCESS=no
   if [[ $USERIVERFILEONLY = no ]]; then  
      while [[ $TRIES -lt 2 ]]; do
         perl ${SCRIPTDIR}/get_flux.pl $OPTIONS 2>> ${SYSLOG} 
         if [[ $? = 0 ]]; then
            logMessage "Completed construction of river flux boundary condition (fort.20 file)."
            SUCCESS=yes
            break
         else
            TRIES=$[$TRIES + 1]
            warn "Attempt $TRIES at constructing river flux boundary condition (fort.20) file has failed. After 2 attempts, the default flux boundary condition file '$DEFAULTFILE' will be used."
            sleep 60
         fi 
      done
   fi     
   if [[ $SUCCESS = no ]]; then
      warn "Using default river flux boundary condition file '$DEFAULTFILE'."
      ln -s $DEFAULTFILE ./fort.20 2>> ${SYSLOG}       
   fi
}
#
# watches for the existence of certain files that are written by the job as
# it executes and proceeds according to the status that is indicated by 
# those files
monitorJobs() 
{   QUEUESYS=$1
    ENSTORM_TEMP=$2
    WALLTIME=$3
#
    activity_indicator "Monitoring queue for run completion..." &
    pid=$!; trap "stop_activity_indicator ${pid}; exit" EXIT
    logMessage "Monitoring progress of $ENSTORM_TEMP job."
#
#   convert the expected wall clock time of the job to seconds, assuming
#   WALLTIME is in the format HH:MM:SS
    hours=${WALLTIME:0:2}   # requires leading zero! e.g., 05:00:00
    minutes=${WALLTIME:3:2} 
    seconds=${WALLTIME:6:2}
    # bash interprets numbers with leading zeroes as octal ... the 10# prefix
    # tells bash that the numbers are base 10
    limit=$((10#$hours * 3600 + 10#$minutes * 60 + 10#$seconds)) # WALLTIME in seconds
#
    if [[ $QUEUESYS = "mpiexec" ]]; then
        # do nothing, mpiexec has returned at this point
        logMessage "mpiexec has returned"
    else
        logMessage "Waiting for $ENSTORM_TEMP job to start."
        until [[ -e ${ENSTORM_TEMP}.run.start ]]; do
           sleep 1
        done
        logMessage "The $ENSTORM_TEMP job has started."   
        startTime=`date +%s`  # epoch seconds
        until [[ -e ${ENSTORM_TEMP}.run.finish || -e ${ENSTORM_TEMP}.run.error ]]; do
            sleep 1
            endTime=`date +%s`
            runTime=$(($endTime - $startTime))
            if [[ $runTime -gt $limit ]]; then
               hoursEnd=$(($limit / 3600))
               remainder=$(($limit % 3600))
               minutesEnd=$(($remainder / 60))
               secondsEnd=$(($remainder % 60))
               format="%02d:%02d:%02d"
               hms=`printf "$format" $hoursEnd $minutesEnd $secondsEnd`
               warn "The wall clock time limit is $WALLTIME but the job has been running for $hms."
               echo "The wall clock time limit is $WALLTIME but the job has been running for $hms." > ${ENSTORM_TEMP}.run.error
            fi
        done
        if [[ -e ${ENSTORM_TEMP}.run.error ]]; then
           warn "The $ENSTORM_TEMP run failed; results are not available for this ensemble member for this advisory."
           cat ${ENSTORM_TEMP}.run.error >> jobFailed         
        fi
        if [[ -e ${ENSTORM_TEMP}.run.finish ]]; then 
           logMessage "The $ENSTORM_TEMP job appears to have run to completion successfully."
        fi
    fi
    logMessage "Finished monitoring $ENSTORM_TEMP job."
    stop_activity_indicator ${pid}
}
#
# submits a job to the local queueing system
submitJob()
{  QUEUESYS=$1
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
   NUMWRITERS=${12}
   HOTSTARTCOMP=${13}
   WALLTIME=${14}
   JOBTYPE=${15}
#
   CLOPTIONS=""     # command line options
   LOCALHOTSTART=""
   if [[ $NUMWRITERS != "0" ]]; then
      CLOPTIONS="-W $NUMWRITERS"
   fi
   if [[ $HOTSTARTCOMP = subdomain ]]; then
      CLOPTIONS="${CLOPTIONS} -S"
      LOCALHOTSTART="--localhotstart"
   fi
# 
#  Load Sharing Facility (LSF); used on topsail at UNC
   if [[ $QUEUESYS = LSF ]]; then
      bsub -x -n $NCPU -q $QUEUENAME -o log.%J -e err.%J -a mvapich mpirun $ADCIRCDIR/padcirc $CLOPTION >> ${SYSLOG}
#
#  LoadLeveler (often used on IBM systems)
   elif [[ $QUEUESYS = LoadLeveler ]]; then
      perl $SCRIPTDIR/loadleveler.pl --jobtype $JOBTYPE --ncpu $NCPU --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --inputdir $INPUTDIR --enstorm $ENSTORM --notifyuser $NOTIFYUSER --numwriters $NUMWRITERS $LOCALHOTSTART > $ADVISDIR/$ENSTORM/padcirc.ll 2>> ${SYSLOG}
      llsubmit $ADVISDIR/$ENSTORM/padcirc.ll >> ${SYSLOG} 2>&1
#
#  Portable Batch System (PBS); widely used
   elif [[ $QUEUESYS = PBS ]]; then
      QSCRIPTOPTIONS="--jobtype $JOBTYPE --ncpu $NCPU --queuename $QUEUENAME --account $ACCOUNT --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --qscript $INPUTDIR/$QSCRIPT --enstorm $ENSTORM --notifyuser $NOTIFYUSER --walltime $WALLTIME --submitstring $SUBMITSTRING $LOCALHOTSTART --syslog $SYSLOG"
      if [[ $PPN -ne 0 ]]; then
         QSCRIPTOPTIONS="$QSCRIPTOPTIONS --ppn $PPN"
      fi
      if [[ $NUMWRITERS != "0" ]]; then
         QSCRIPTOPTIONS="$QSCRIPTOPTIONS --numwriters $NUMWRITERS"
      fi
      logMessage "QSCRIPTOPTIONS is $QSCRIPTOPTIONS"
      perl $SCRIPTDIR/$QSCRIPTGEN $QSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/padcirc.pbs 2>> ${SYSLOG}
      logMessage "Submitting $ADVISDIR/$ENSTORM/padcirc.pbs"
      # submit job, check to make sure qsub succeeded, and if not, retry
      while [[ 1 = 1 ]];  do
         qsub $ADVISDIR/$ENSTORM/padcirc.pbs >> ${SYSLOG} 2>&1
         if [[ $? = 0 ]]; then
            break # qsub returned a "success" status
         else
            warn "qsub $ADVISDIR/$ENSTORM/padcirc.pbs failed; will retry in 60 seconds."
            sleep 60
         fi
      done
#
#  No queueing system, just mpiexec (used on standalone computers)
   elif [[ $QUEUESYS = mpiexec ]]; then
      CPUREQUEST=$NCPU
      if [[ $NUMWRITERS != "0" ]]; then
         CPUREQUEST=`expr $NCPU + $NUMWRITERS`
      fi
      logMessage "Submitting job via $SUBMITSTRING $CPUREQUEST $ADCIRCDIR/$JOBTYPE $CLOPTION >> ${SYSLOG} 2>&1"
      $SUBMITSTRING $CPUREQUEST $ADCIRCDIR/$JOBTYPE $CLOPTION >> ${SYSLOG} 2>&1 
#
#  Sun Grid Engine (SGE); used on Sun and many Linux clusters
   elif [[ $QUEUESYS = SGE ]]; then
      QSCRIPTOPTIONS="--jobtype $JOBTYPE --ncpu $NCPU --ncpudivisor $NCPUDIVISOR --queuename $QUEUENAME --account $ACCOUNT --adcircdir $ADCIRCDIR --advisdir $ADVISDIR --qscript $INPUTDIR/$QSCRIPT --enstorm $ENSTORM --notifyuser $NOTIFYUSER --walltime $WALLTIME --submitstring $SUBMITSTRING --syslog $SYSLOG --numwriters $NUMWRITERS $LOCALHOTSTART"
      perl $SCRIPTDIR/$QSCRIPTGEN $QSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/padcirc.sge 2>> ${SYSLOG}
      logMessage "Submitting $ADVISDIR/$ENSTORM/padcirc.sge"
      qsub $ADVISDIR/$ENSTORM/padcirc.sge >> ${SYSLOG} 2>&1
      # if qsub failed, resubmit the job 5 times before giving up
      if [[ $? = 1 ]]; then
         rangerResubmit $ADVISDIR $ENSTORM padcirc.sge $SYSLOG
      fi 
   else 
      fatal "Queueing system $QUEUESYS unrecognized."
   fi
}
#
# since valid jobs are sometimes rejected on ranger, this function will
# attempt to resubmit rejected jobs 5 times before giving up
rangerResubmit()
{
   ADVISDIR=$1
   ENSTORM=$2
   SCRIPTNAME=$3
   SYSLOG=$4
#
   num_retries=0
   success=0
   while [[ $num_retries -le 5 ]]; do
      logMessage "SGE rejected the job; will resubmit after 60 seconds."
      sleep 60
      qsub $ADVISDIR/$ENSTORM/$SCRIPTNAME >> ${SYSLOG} 2>&1
      num_retries=`expr $num_retries + 1`
      logMessage "The number of retries is $num_retries."
      if [[ $? = 0 ]]; then
         success=1
         logMessage "The job was successfully resubmitted."
         break
      fi
   done
   if [[ $success = 0 ]]; then     
      date > $ADVISDIR/$ENSTORM/run.error
      msg="The job '$ADVISDIR/$ENSTORM/$SCRIPTNAME' was not accepted by SGE after it was resubmitted $num_retries times."
      warning $msg
      echo $msg >> $ADVISDIR/$ENSTORM/run.error
   fi 
}
#
# checks to see if a job has failed, and if so, copies it off to 
# another directory
handleFailedJob() 
{   
RUNDIR=$1
ADVISDIR=$2
ENSTORM=$3
NOTIFYSCRIPT=$4
HOSTNAME=$5
STORMNAME=$6
YEAR=$7
STORMDIR=$8
ADVISORY=$9
GRIDFILE=${10}
EMAILNOTIFY=${11}
POST_LIST="${12}"
# check to see that the job did not conspicuously fail
if [[ -e $ADVISDIR/${ENSTORM}/jobFailed ]]; then 
   warn "The job has failed."
   FAILDATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
   warn "Moving failed cycle to 'failed.${FAILDATETIME}'."
   mv $ADVISDIR/$ENSTORM $RUNDIR/failed.${FAILDATETIME} 2>> ${SYSLOG}
   # send an email to notify the operator that a job has failed
   $NOTIFYSCRIPT $HOSTNAME $STORM $YEAR $STORMDIR $ADVISORY $ENSTORM $GRIDFILE jobfailed $EMAILNOTIFY $SYSLOG "${POST_LIST}"
fi
}
#
# Log file will be in the directory where the asgs was executed
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
  if [[ $EMAILNOTIFY = yes || $EMAILNOTIFY = YES ]]; then
     cat ${SYSLOG} | mail -s "[ASGS] Fatal Error for PROCID ($$)" "${ASGSADMIN}"
  fi
  echo ${MSG} # send to console
  exit ${EXIT_NOT_OK} 
}
#
# log a debug message
debugMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] DEBUG: $@"
  echo ${MSG} >> ${SYSLOG} 
}
#
# send a message to the console (i.e., window where the script was started)
# (these should be rare)
consoleMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] INFO: $@"
  echo ${MSG}
}
#
# initialization subroutines for the various machines/architectures 
#
init_blueridge()
{ #<- can replace the following with a custom script
  HOSTNAME=blueridge.renci.org
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=noaccount
  SUBMITSTRING=submitstring
  SCRATCHDIR=/work/$USER
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=renci.template.pbs
  PREPCONTROLSCRIPT=renci.adcprep.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=8
}
init_kittyhawk()
{ #<- can replace the following with a custom script
  HOSTNAME=kittyhawk.renci.org
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=noaccount
  SUBMITSTRING=submitstring
  SCRATCHDIR=/work/$USER
  SSHKEY=~/.ssh/id_rsa_kittyhawk
  QSCRIPT=kittyhawk.template.pbs
  PREPCONTROLSCRIPT=kittyhawk.adcprep.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=4
}
init_sapphire()
{ #<- can replace the following with a custom script
  HOSTNAME=sapphire.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvhsp
  SUBMITSTRING="aprun"
  SCRATCHDIR=/work2/$USER
  SSHKEY=~/.ssh/id_rsa_sapphire
  QSCRIPT=erdc.template.pbs
  PREPCONTROLSCRIPT=erdc.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=erdc.adcprep.hotstart.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  ulimit -s unlimited 
  ulimit -v 2097152   # needed for NAMtoOWI.pl to avoid Out of memory error 
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
  SSHKEY=~/.ssh/id_rsa_jade
  QSCRIPT=erdc.template.pbs
  PREPCONTROLSCRIPT=erdc.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=erdc.adcprep.hotstart.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  ulimit -s unlimited 
  ulimit -v 2097152   # needed for NAMtoOWI.pl to avoid Out of memory error 
}

init_diamond()
{ #<- can replace the following with a custom script
  HOSTNAME=diamond.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvhsp
  SUBMITSTRING="mpiexec_mpt"
  SCRATCHDIR=/work/$USER
  SSHKEY=~/.ssh/id_rsa_diamond
  QSCRIPT=erdc.diamond.template.pbs
  PREPCONTROLSCRIPT=erdc.diamond.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=erdc.diamond.adcprep.hotstart.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  PPN=8 
}

init_garnet()
{ #<- can replace the following with a custom script
  HOSTNAME=garnet.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvhsp
  SUBMITSTRING="aprun"
  SCRATCHDIR=/work/$USER
  SSHKEY=~/.ssh/id_rsa_garnet
  QSCRIPT=garnet.template.pbs
  PREPCONTROLSCRIPT=garnet.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=garnet.adcprep.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  PPN=16 
  . /opt/modules/default/etc/modules.sh
  module load netcdf/4.1.1.0
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
  SCRATCHDIR=/work/cera
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
  ACCOUNT=TG-DMS100024
  SUBMITSTRING="ibrun tacc_affinity"
  SCRATCHDIR=$SCRATCH
  SSHKEY=id_rsa_ranger
  QSCRIPT=ranger.template.sge
  QSCRIPTGEN=ranger.sge.pl
  UMASK=006
  GROUP="G-81535"
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
  "kittyhawk") logMessage "Kittyhawk (RENCI) configuration found."
          init_kittyhawk
          ;;
  "blueridge") logMessage "Blueridge (RENCI) configuration found."
          init_blueridge
          ;;
  "sapphire") logMessage "Sapphire (ERDC) configuration found."
          init_sapphire
	  ;;
  "jade") logMessage "Jade (ERDC) configuration found."
          init_jade
	  ;;
  "diamond") logMessage "Diamond (ERDC) configuration found."
          init_diamond
	  ;;
  "garnet") logMessage "Garnet (ERDC) configuration found."
          init_garnet
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
  *) fatal "'$1' is not a supported environment; currently supported options: kittyhawk, blueridge, sapphire, jade, diamond, ranger, lonestar, queenbee, topsail, desktop"
     ;;
  esac
}
#####################################################################
#                 E N D  F U N C T I O N S
#####################################################################
#
#####################################################################
#               B E G I N     E X E C U T I O N
#####################################################################
#
# 
# Option Summary
#  
# -c : set location of configuration file"
# -e (environment): set the computer that the ASGS is running on" 
# -h : show help"
#
# Example:
#   bash asgs_main.sh -c /path/to/config -e topsail 
#
# mail alert
ASGSADMIN=
#
# exit statuses
EXIT_NOT_OK=1
EXIT_OK=0
#
# need to determine standard time format to be used for pasting log files
STARTDATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
SYSLOG=`pwd`/asgs-${STARTDATETIME}.$$.log
# create directories with default permissions of "775" and 
# files with the default permssion of "664"
umask 002
#
# Initialize variables accessed from config.sh
INSTANCENAME=1
BACKGROUNDMET=on
TIDEFAC=off
TROPICALCYCLONE=off
WAVES=off
VARFLUX=off
MINMAX=continuous
REINITIALIZESWAN=no
USERIVERFILEONLY=no
STORMNAME=stormname
RIVERSITE=ftp.nssl.noaa.gov
RIVERDIR=/projects/ciflow/adcirc_info
ELEVSTATIONS=null
VELSTATIONS=null
METSTATIONS=null
GRIDFILE=fort.14
GRIDNAME=fort14
OUTPUTOPTIONS=
ARCHIVEBASE=/dev/null
ARCHIVEDIR=null
FORECASTCYCLE="00,06,12,18"
TRIGGER="rss"
STARTADVISORYNUM=null
ADVISORY=null
FORECASTLENGTH=84
ALTNAMDIR=
HOTSTARTCOMP=fulldomain
HINDCASTWALLTIME="10:00:00"
ADCPREPWALLTIME="00:30:00"
NOWCASTWALLTIME="02:00:00"
FORECASTWALLTIME="05:00:00"
TIMESTEPSIZE=1.0
SWANDT=600
UMASK=002
GROUP=""
DRY=1           
DEMO=       
STORM=0        
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
SERQUEUE=
QCHECKCMD=
NCPU=
JOBTYPE=
NUMWRITERS=0
ACCOUNT=desktop
SUBMITSTRING=
INTERSTRING=
RESULTSHOST=
RESULTSPATH=
RESULTSUSERNAME=
RESULTSPROMPT=
RESULTSPASSWORD=
NOTIFYUSER=
RUNDIR=
INPUTDIR=
PERL5LIB=
HOTSTARTFORMAT=
STORMDIR=stormdir
SSHKEY=
PPN=1
ENSTORMNAMES[0]=nhcConsensus
logMessage "ASGS Start Up MSG: [PROCID] $$"
logMessage "ASGS Start Up MSG: [SYSLOG] ${SYSLOG}"
logMessage "The ADCIRC Surge/Spill Guidance System is activated."
consoleMessage "Please see ASGS log file for detailed information regarding system progress."
consoleMessage "ASGS Start Up MSG: [SYSLOG] The log file is ${SYSLOG}"

# first - look for SCRIPTDIR
while getopts "c:e:h" optname; do    #<- first getopts for SCRIPTDIR
  case $optname in
    c) CONFIG=${OPTARG}
       ;;
    e) ENV=${OPTARG} 
       ;;
    h) echoHelp 
       ;;
  esac
done

# dispatch environment
logMessage "Configuring the ASGS for the '${ENV}' platform..."
env_dispatch ${ENV} 
#
# set the file and directory permissions, which are platform dependent
logMessage "Setting permissions with the following umask: $UMASK."
umask $UMASK 2>> ${SYSLOG}
#if [[ $GROUP != "" ]]; then
#   logMessage "Setting effective group to $GROUP."
#   newgrp $GROUP 2>> ${SYSLOG}
#fi
#
# Since the config file is read after the platform configuration has been
# made, settings in the config file will override those in the plaform config.
if [ -e "${CONFIG}" ]; then 
   logMessage "Configuring the ASGS according to the file ${CONFIG}." 
    # source config file
    . ${CONFIG}
else
    fatal "${CONFIG} does not exist!"
fi
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
if [[ $TROPICALCYCLONE = on ]]; then
   checkFileExistence $ADCIRCDIR "asymmetric metadata generation executable" aswip
fi
if [[ $BACKGROUNDMET = on ]]; then
   checkFileExistence $SCRIPTDIR "NAM output reprojection executable (from lambert to geographic)" awip_lambert_interp.x
   checkFileExistence $SCRIPTDIR "GRIB2 manipulation and extraction executable" wgrib2
fi  
if [[ $WAVES = on ]]; then
   JOBTYPE=padcswan
   checkFileExistence $ADCIRCDIR "ADCIRC+SWAN parallel executable" padcswan
   checkFileExistence $INPUTDIR "SWAN initialization template file " swaninit.template
   checkFileExistence $INPUTDIR "SWAN control template file" $SWANTEMPLATE
else
   JOBTYPE=padcirc
fi
if [[ $VARFLUX = on ]]; then
   checkFileExistence $INPUTDIR "River elevation initialization file " $RIVERINIT
   checkFileExistence $INPUTDIR "River flux default file " $RIVERFLUX
fi
#
checkFileExistence $INPUTDIR "ADCIRC mesh file" $GRIDFILE
checkFileExistence $INPUTDIR "ADCIRC template fort.15 file" $CONTROLTEMPLATE
if [[ $ELEVSTATIONS != null ]]; then
   checkFileExistence $INPUTDIR "ADCIRC elevation stations file" $ELEVSTATIONS
fi
if [[ $VELSTATIONS && $VELSTATIONS != null ]]; then
   checkFileExistence $INPUTDIR "ADCIRC velocity stations file" $VELSTATIONS
fi
if [[ $METSTATIONS && $METSTATIONS != null ]]; then
   checkFileExistence $INPUTDIR "ADCIRC meteorological stations file" $METSTATIONS
fi

# fort.13 (nodal attributes) file is optional
if [[ ! -z $NAFILE ]]; then
   checkFileExistence $INPUTDIR "ADCIRC nodal attributes (fort.13) file" $NAFILE
fi
if [[ $HOTORCOLD = hotstart ]]; then
   if [[ $HOTSTARTFORMAT = netcdf ]]; then
      if [[ -d $LASTSUBDIR/hindcast ]]; then
         checkFileExistence "" "ADCIRC hotstart (fort.67.nc) file " $LASTSUBDIR/hindcast/fort.67.nc
      fi
      if [[ -d $LASTSUBDIR/nowcast ]]; then
         checkFileExistence "" "ADCIRC hotstart (fort.67.nc) file " $LASTSUBDIR/nowcast/fort.67.nc
      fi
   else
      if [[ -d $LASTSUBDIR/hindcast ]]; then
         checkFileExistence "" "ADCIRC hotstart (fort.67) file " $LASTSUBDIR/hindcast/PE0000/fort.67
      fi
      if [[ -d $LASTSUBDIR/nowcast ]]; then
         checkFileExistence "" "ADCIRC hotstart (fort.67) file " $LASTSUBDIR/nowcast/PE0000/fort.67
      fi
   fi
fi 
if [[ -e ${INPUTDIR}/${PREPPEDARCHIVE} ]]; then
   logMessage "Found archive of preprocessed input files ${INPUTDIR}/${PREPPEDARCHIVE}."
else
   warn "Could not find archive of preprocessed input files ${INPUTDIR}/${PREPPEDARCHIVE}. It will be recreated."
fi
if [[ -e ${INPUTDIR}/${HINDCASTARCHIVE} ]]; then
   logMessage "Found archive of preprocessed input files ${INPUTDIR}/${HINDCASTARCHIVE}."
else
   warn "Could not find archive of preprocessed input files ${INPUTDIR}/${HINDCASTARCHIVE}. It will be recreated."
fi
#
checkFileExistence $OUTPUTDIR "postprocessing initialization script" $INITPOST
checkFileExistence $OUTPUTDIR "postprocessing script" $POSTPROCESS
checkFileExistence $OUTPUTDIR "email notification script" $NOTIFY_SCRIPT
checkFileExistence $OUTPUTDIR "data archival script" $ARCHIVE
#
checkDirExistence ${PERL5LIB}/Date "subdirectory for the Pcalc.pm perl module"
checkFileExistence ${PERL5LIB}/Date "perl module for date calculations" Pcalc.pm
#
# initialize the directory where this instance of the ASGS will run and 
# keep all its files, info from the machine-specific initialization and
# the asgs config file (e.g., asgs_config.sh)
RUNDIR=$SCRATCHDIR/asgs$$
logMessage "The directory $RUNDIR will be used for all files associated with this execution of the ASGS."
# add the run directory to the list of alternate directories to look for 
# NAM data in
ALTNAMDIR="${ALTNAMDIR},$RUNDIR"
# set directory to get perl date calcs module from
export PERL5LIB=${SCRIPTDIR}:${PERL5LIB} #<- augment, don't write over existing
# see if the storm directory already exists in the scratch space
if [ ! -d $RUNDIR ]; then
    # -p says make the entire path tree if intermediate dirs do not exist 
    mkdir -p $RUNDIR #
else 
   fatal "The run directory $RUNDIR that the ASGS attempted to create actually already exists; it must be left over from a previous (now defunct) invokation of the ASGS. This is really more of an unfortunate coincidence than an outright error. Please restart the ASGS so it will run under a different process ID and therefore use a different run directory name."      
fi
#
# send out an email to notify users that the ASGS is ACTIVATED
${OUTPUTDIR}/${NOTIFY_SCRIPT} $HOSTNAME $STORM $YEAR $RUNDIR advisory enstorm $GRIDFILE activation $EMAILNOTIFY $SYSLOG "${ACTIVATE_LIST}" >> ${SYSLOG} 2>&1
#
OLDADVISDIR=null
CSDATE=$COLDSTARTDATE
if [ $HOTORCOLD = "" ]; then
    START=coldstart
else
    START=$HOTORCOLD
    OLDADVISDIR=$RUNDIR/$LASTSUBDIR
fi
printf "%02d" $STARTADVISORYNUM > $RUNDIR/advisoryNumber 2>> $SYSLOG 
ADVISORY=$STARTADVISORYNUM
#
###############################
#   BODY OF ASGS STARTS HERE    
###############################
if [[ $BACKGROUNDMET = on && $TROPICALCYCLONE = on ]]; then
   NWS=29
   # not ready for this yet
   fatal "Background meteorology and tropical cyclone forcing are both turned on in ${CONFIG} but simultaneous use of these two forcing types is not yet supported in ASGS."
fi
#
# If we are coldstarting, perform a hindcast ... this is necessary 
# to ramp up forcing and allow transient signals to die away before 
# performing a nowcast.
ADVISDIR=   # determined below
HSTIME=     # determined below
#
#       H I N D C A S T
#
if [[ $START = coldstart ]]; then
   logMessage "Starting hindcast."
   HOTSWAN=off
   ENSTORM=hindcast
   ADVISDIR=$RUNDIR/initialize
   mkdir -p $ADVISDIR 2>> ${SYSLOG} 
   STORMDIR=$ADVISDIR/$ENSTORM
   mkdir -p $STORMDIR 2>> ${SYSLOG} 
   HSTIME=0
   # We assume that the hindcast is only used to spin up tides or 
   # initialize rivers ... therefore no met forcing.
   NWS=0
   OLDADVISDIR=$ADVISDIR # initialize with dummy value when coldstarting
   logMessage "Coldstarting Storm '$STORM' in '$YEAR'."
   logMessage "Coldstart time is '$CSDATE'."
   logMessage "The initial hindcast duration is '$HINDCASTLENGTH' days."
   # prepare hindcast control (fort.15) file 
   CONTROLOPTIONS="--name $ENSTORM --scriptdir $SCRIPTDIR --advisdir $ADVISDIR --cst $CSDATE --endtime $HINDCASTLENGTH --dt $TIMESTEPSIZE --nws $NWS --hsformat $HOTSTARTFORMAT --advisorynum 0 --controltemplate ${INPUTDIR}/${CONTROLTEMPLATE} $OUTPUTOPTIONS"
   CONTROLOPTIONS="$CONTROLOPTIONS --elevstations ${INPUTDIR}/${ELEVSTATIONS} --velstations ${INPUTDIR}/${VELSTATIONS} --metstations ${INPUTDIR}/${METSTATIONS}"
   CONTROLOPTIONS="$CONTROLOPTIONS --gridname $GRIDNAME" # for run.properties
   logMessage "Constructing control file with the following options: $CONTROLOPTIONS."
   perl $SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS >> ${SYSLOG} 2>&1
   # don't have a meterological forcing (fort.22) file in this case
   # preprocess
   logMessage "Starting $ENSTORM preprocessing."
    echo "hostname : $HOSTNAME" >> $ADVISDIR/$ENSTORM/run.properties  
   logMessage "prep $ADVISDIR $INPUTDIR $ENSTORM $START $OLDADVISDIR $ENV $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT '$OUTPUTOPTIONS' $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE"
   prep $ADVISDIR $INPUTDIR $ENSTORM $START $OLDADVISDIR $ENV $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT "$OUTPUTOPTIONS" $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE
   # check to see that adcprep did not conspicuously fail
   handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HOSTNAME hindcast $YEAR $STORMDIR $ADVISORY $GRIDFILE $EMAILNOTIFY "${POST_LIST}"
   if [[ ! -d $ADVISDIR/$ENSTORM ]]; then
      fatal "The prep for the hindcast run has failed."
   fi
   # then submit the job
   logMessage "Submitting ADCIRC $ENSTORM job."
   cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
   JOBTYPE=padcirc  # we won't run waves during the spinup hindcast
   logMessage "submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $NOTIFYUSER $ENV $ACCOUNT $PPN $NUMWRITERS $HOTSTARTCOMP $HINDCASTWALLTIME $JOBTYPE"
   submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $NOTIFYUSER $ENV $ACCOUNT $PPN $NUMWRITERS $HOTSTARTCOMP $HINDCASTWALLTIME $JOBTYPE
   # check once per minute until all jobs have finished
   monitorJobs $QUEUESYS ${JOBTYPE}.${ENSTORM} $HINDCASTWALLTIME
   # check to see that the nowcast job did not conspicuously fail
   handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HOSTNAME hindcast $YEAR $STORMDIR $ADVISORY $GRIDFILE $EMAILNOTIFY "${POST_LIST}"
   if [[ ! -d $ADVISDIR/$ENSTORM ]]; then 
      fatal "The hindcast run has failed."
   fi
   consoleMesssage "Job(s) complete."
   # nowcast finished, get on with it
   logMessage "$ENSTORM run finished."
   cd $ADVISDIR 2>> ${SYSLOG}
   OLDADVISDIR=$ADVISDIR
   START=hotstart
   #
else 
   # start from   H O T S T A R T   file  
   logMessage "Starting nowcast from the hotstart file under '$LASTSUBDIR'."
   OLDADVISDIR=$LASTSUBDIR
fi
#
# B E G I N   N O W C A S T / F O R E C A S T   L O O P
while [ 1 -eq 1 ]; do
   # re-read configuration file to pick up any changes
   . ${CONFIG}
   FROMDIR=
   LUN=       # logical unit number; either 67 or 68
   HOTSWAN=on
   if [[ $REINITIALIZESWAN = yes ]]; then
      HOTSWAN=off
   fi
   if [[ -d $OLDADVISDIR/nowcast ]]; then
       FROMDIR=$OLDADVISDIR/nowcast
   fi
   if [[ -d $OLDADVISDIR/hindcast ]]; then
       FROMDIR=$OLDADVISDIR/hindcast
       HOTSWAN=off  # we didn't run swan in the hindcast; swan will coldstart
   fi
   checkHotstart $FROMDIR $HOTSTARTFORMAT  67
   if [[ $HOTSTARTFORMAT = netcdf ]]; then
      logMessage "hotstart format is netcdf"
      HSTIME=`$ADCIRCDIR/hstime -f ${FROMDIR}/fort.67.nc -n` 2>> ${SYSLOG}
   else
      logMessage "hotstart format is binary"
      HSTIME=`$ADCIRCDIR/hstime -f ${FROMDIR}/PE0000/fort.67` 2>> ${SYSLOG}
   fi
   logMessage "The time in the hotstart file is '$HSTIME' seconds."
   cd $RUNDIR 2>> ${SYSLOG}
    #
    # N O W C A S T
    ENSTORM=nowcast
    logMessage "Checking for new meteorological data every 60 seconds ..."
    # TROPICAL CYCLONE ONLY
    if [[ $TROPICALCYCLONE = on ]]; then
       NWS=19
       if [[ $WAVES = on ]]; then
          NWS=319
       fi
       # download wind data from ftp site every 60 seconds to see if
       # there is a new advisory
       downloadCycloneData $STORM $YEAR $RUNDIR $SCRIPTDIR $OLDADVISDIR $TRIGGER $ADVISORY $FTPSITE $RSSSITE $FDIR $HDIR
       ADVISORY=`cat advisoryNumber`
       ADVISDIR=$RUNDIR/${ADVISORY}
       STORMDIR=$ADVISDIR/$ENSTORM
       if [ ! -d $ADVISDIR ]; then 
           mkdir $ADVISDIR 2>> ${SYSLOG}
       fi
       if [ ! -d $ADVISDIR/$ENSTORM ]; then
           mkdir $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
       fi
       logMessage "$START Storm $STORM advisory $ADVISORY in $YEAR"
       consoleMessage "$START Storm $STORM advisory $ADVISORY in $YEAR"
       # move raw ATCF files into advisory directory
       mv *.fst *.dat *.xml *.html $ADVISDIR 2>> ${SYSLOG}
       #
       # prepare nowcast met (fort.22) and control (fort.15) files 
       cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
       METOPTIONS="--dir $ADVISDIR --storm $STORM --year $YEAR --name $ENSTORM --nws $NWS --hotstartseconds $HSTIME --coldstartdate $CSDATE" 
       CONTROLOPTIONS=" --scriptdir $SCRIPTDIR --metfile $ADVISDIR/$ENSTORM/fort.22 --name $ENSTORM --advisdir $ADVISDIR --dt $TIMESTEPSIZE --nws $NWS --advisorynum $ADVISORY --controltemplate ${INPUTDIR}/${CONTROLTEMPLATE} --hst $HSTIME --cst $CSDATE --hsformat $HOTSTARTFORMAT $OUTPUTOPTIONS" 
       logMessage "Generating ADCIRC Met File (fort.22) for nowcast with the following options: $METOPTIONS."
       ${SCRIPTDIR}/storm_track_gen.pl $METOPTIONS >> ${SYSLOG} 2>&1   
       STORMCLASSNAME=`cat ${ADVISDIR}/${ENSTORM}/nhcClassName`
       # find the space between the storm class (TD, TS, HU, etc) and NHC name
       ind=`expr index "$STORMCLASSNAME" ' '`
       # just use the storm's name
       STORMNAME=${STORMCLASSNAME:$ind}
       # create a new file that contains metadata
       $ADCIRCDIR/aswip >> ${SYSLOG} 2>&1
       if [ -e NWS_19_fort.22 ]; then
          mv fort.22 fort.22.orig
          cp NWS_19_fort.22 fort.22 
       fi
       createMetaDataLink $STORM $YEAR $GRIDNAME $ADVISORY $ENSTORM $ADVISDIR $HOSTNAME $HSTIME $CSDATE
       #
    fi
    # BACKGROUND METEOROLOGY 
    if [[ $BACKGROUNDMET = on ]]; then
       NWS=-12
       if [[ $WAVES = on ]]; then
          NWS=-312
       fi
       logMessage "NWS is $NWS."
       logMessage "Downloading background meteorology."
       logMessage "downloadBackgroundMet $RUNDIR $SCRIPTDIR $BACKSITE $BACKDIR $ENSTORM $CSDATE $HSTIME $FORECASTLENGTH $ALTNAMDIR $FORECASTCYCLE $ARCHIVEBASE $ARCHIVEDIR"
       downloadBackgroundMet $RUNDIR $SCRIPTDIR $BACKSITE $BACKDIR $ENSTORM $CSDATE $HSTIME $FORECASTLENGTH $ALTNAMDIR $FORECASTCYCLE $ARCHIVEBASE $ARCHIVEDIR
       ADVISORY=`cat advisoryNumber`
       ADVISDIR=$RUNDIR/${ADVISORY}
       STORMDIR=$ADVISDIR/$ENSTORM
       cd $ADVISDIR 2>> ${SYSLOG}
       logMessage "$START $ENSTORM cycle $ADVISORY."
       consoleMessage "$START $ENSTORM cycle $ADVISORY."
       # convert met files to OWI format
       NAMOPTIONS=" --ptFile ${INPUTDIR}/${PTFILE} --namFormat grib2 --namType $ENSTORM --awipGridNumber 218 --dataDir ${ADVISDIR}/${ENSTORM} --outDir ${ADVISDIR}/${ENSTORM}/ --velocityMultiplier 0.893 --scriptDir ${SCRIPTDIR}"
       logMessage "Converting NAM data to OWI format with the following options : $NAMOPTIONS"
       perl ${SCRIPTDIR}/NAMtoOWI.pl $NAMOPTIONS >> ${SYSLOG} 2>&1 
       CONTROLOPTIONS=" --advisdir $ADVISDIR --scriptdir $SCRIPTDIR --name $ENSTORM --dt $TIMESTEPSIZE --nws $NWS --controltemplate ${INPUTDIR}/${CONTROLTEMPLATE} --cst $CSDATE --hstime $HSTIME --hsformat $HOTSTARTFORMAT $OUTPUTOPTIONS"
       # create links to the OWI files
       cd $ENSTORM 2>> ${SYSLOG}
       NAM221=`ls NAM*.221`; 
       NAM222=`ls NAM*.222`;
       ln -s $NAM221 fort.221 2>> ${SYSLOG}
       ln -s $NAM222 fort.222 2>> ${SYSLOG}
    fi
    # send out an email alerting end users that a new cycle has been issued
    ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HOSTNAME $STORM $YEAR $STORMDIR $ADVISORY $ENSTORM $GRIDFILE newcycle $EMAILNOTIFY $SYSLOG "${NEW_ADVISORY_LIST}" >> ${SYSLOG} 2>&1
    # activate padcswan based on ASGS configuration
    if [[ $WAVES = on ]]; then
       CONTROLOPTIONS="${CONTROLOPTIONS} --swandt $SWANDT --swantemplate ${INPUTDIR}/${SWANTEMPLATE} --hotswan $HOTSWAN"
    fi
    CONTROLOPTIONS="${CONTROLOPTIONS} --elevstations ${INPUTDIR}/${ELEVSTATIONS} --velstations ${INPUTDIR}/${VELSTATIONS} --metstations ${INPUTDIR}/${METSTATIONS}"
    CONTROLOPTIONS="$CONTROLOPTIONS --gridname $GRIDNAME" # for run.properties
    # generate fort.15 file
    logMessage "Generating ADCIRC Control File (fort.15) for $ENSTORM with the following options: $CONTROLOPTIONS."
    perl $SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS >> ${SYSLOG} 2>&1
    # get river flux nowcast data, if configured to do so
    if [[ $VARFLUX = on ]]; then
       downloadRiverFluxData $ADVISDIR ${INPUTDIR}/${GRIDFILE} $RIVERSITE $RIVERDIR $ENSTORM $CSDATE $HSTIME $SCRIPTDIR ${INPUTDIR}/${RIVERFLUX} $USERIVERFILEONLY
    fi
    logMessage "Starting nowcast."
    consoleMessage "Starting nowcast for cycle '$ADVISORY'."
    echo "hostname : $HOSTNAME" >> $ADVISDIR/$ENSTORM/run.properties  
    # preprocess
    logMessage "Starting nowcast preprocessing."
    logMessage "prep $ADVISDIR $INPUTDIR $ENSTORM $START $OLDADVISDIR $ENV $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT '$OUTPUTOPTIONS' $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE"
    prep $ADVISDIR $INPUTDIR $ENSTORM $START $OLDADVISDIR $ENV $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT "$OUTPUTOPTIONS" $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE
    # check to see that adcprep did not conspicuously fail
    handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HOSTNAME $STORMNAME $YEAR $STORMDIR $ADVISORY $GRIDFILE $EMAILNOTIFY "${POST_LIST}"
    if [[ ! -d $ADVISDIR/$ENSTORM ]]; then 
       continue  # abandon this nowcast and wait for the next one
    fi
    JOBTYPE=padcirc
    HOTSWAN=on
    if [[ $WAVES = on ]]; then
       JOBTYPE=padcswan
    fi
    # then submit the job
    logMessage "Submitting $ENSTORM job."
    cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
    logMessage "submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $NOTIFYUSER $ENV $ACCOUNT $PPN $NUMWRITERS $HOTSTARTCOMP $NOWCASTWALLTIME $JOBTYPE"
    submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $NOTIFYUSER $ENV $ACCOUNT $PPN $NUMWRITERS $HOTSTARTCOMP $NOWCASTWALLTIME $JOBTYPE
    # check once per minute until all jobs have finished
    monitorJobs $QUEUESYS ${JOBTYPE}.${ENSTORM} $NOWCASTWALLTIME
    # check to see that the nowcast job did not conspicuously fail
    handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HOSTNAME $STORMNAME $YEAR $STORMDIR $ADVISORY $GRIDFILE $EMAILNOTIFY "${POST_LIST}"
    if [[ ! -d $ADVISDIR/$ENSTORM ]]; then
       continue # abandon this nowcast and wait for the next one
    fi
    consoleMesssage "Job(s) complete."
    # nowcast finished, get on with it
    logMessage "Nowcast run finished."
    consoleMessage "Nowcast run finished."
    cd $ADVISDIR 2>> ${SYSLOG}
    #
    # F O R E C A S T
    # 
    logMessage "Starting forecast for advisory '$ADVISORY'."
    consoleMessage "Starting forecast for advisory '$ADVISORY'."
    # source config file to pick up any configuration changes
    . ${CONFIG}
    HOTSWAN=on # doesn't do anything unless WAVES=on
    checkHotstart ${ADVISDIR}/nowcast $HOTSTARTFORMAT 67
    if [[ $HOTSTARTFORMAT = netcdf ]]; then
       HSTIME=`$ADCIRCDIR/hstime -f ${ADVISDIR}/nowcast/fort.67.nc -n` 2>> ${SYSLOG}
    else
       HSTIME=`$ADCIRCDIR/hstime -f ${ADVISDIR}/nowcast/PE0000/fort.67` 2>> ${SYSLOG}
    fi
    logMessage "The time in the hotstart file is '$HSTIME' seconds."
    let si=0
    while [ $si -lt $ENSEMBLESIZE ]; do  
       ENSTORM=${NAME[${STORMLIST[$si]}]}
       STORMDIR=$ADVISDIR/$ENSTORM
       if [ ! -d $ADVISDIR/${ENSTORM} ]; then
          mkdir $ADVISDIR/${ENSTORM} 2>> ${SYSLOG}
       fi
       cd $ADVISDIR/${ENSTORM} 2>> ${SYSLOG}
       RUNFORECAST=yes
       # TROPICAL CYCLONE ONLY
       if [[ $TROPICALCYCLONE = on ]]; then
          METOPTIONS=" --dir $ADVISDIR --storm $STORM --year $YEAR --coldstartdate $CSDATE --hotstartseconds $HSTIME --nws $NWS --name $ENSTORM --percent ${PERCENT[$si]}"
          CONTROLOPTIONS="--cst $CSDATE --scriptdir $SCRIPTDIR --advisdir $ADVISDIR --dt $TIMESTEPSIZE --nws $NWS --advisorynum $ADVISORY --controltemplate ${INPUTDIR}/${CONTROLTEMPLATE} --hst $HSTIME --metfile $ADVISDIR/${ENSTORM}/fort.22 --name $ENSTORM --hsformat $HOTSTARTFORMAT $OUTPUTOPTIONS"
          logMessage "Generating ADCIRC Met File (fort.22) for $ENSTORM with the following options: $METOPTIONS."
          ${SCRIPTDIR}/storm_track_gen.pl $METOPTIONS >> ${SYSLOG} 2>&1
          if [[ $NWS = 19 || $NWS = 319 ]]; then
             # create a new file that contains metadata and has the Rmax 
             # in it already ... potentially with Rmax changes if desired
             if [[ ! -z ${PERCENT[$si]} ]]; then
                ASWIPOPTIONS="-P ${PERCENT[$si]}"
             fi
             if [[ ! -z ${RMAX[$si]} ]]; then
                ASWIPOPTIONS="${ASWIPOPTIONS} -R ${RMAX[$si]}"
             fi
             logMessage "Running aswip fort.22 preprocessor for $ENSTORM with the following options: $ASWIPOPTIONS."
             $ADCIRCDIR/aswip $ASWIPOPTIONS >> ${SYSLOG} 2>&1
             if [ -e NWS_19_fort.22 ]; then
                mv fort.22 fort.22.orig 2>> ${SYSLOG}
                cp NWS_19_fort.22 fort.22 2>> ${SYSLOG}
             fi
          fi
          createMetaDataLink $STORM $YEAR $GRIDNAME $ADVISORY $ENSTORM $ADVISDIR $HOSTNAME $HSTIME $CSDATE
       fi
       # BACKGROUND METEOROLOGY ONLY
       if [[ $BACKGROUNDMET = on ]]; then
          logMessage "$START $ENSTORM cycle $ADVISORY."
          consoleMessage "$START $ENSTORM cycle $ADVISORY."
          # download and convert met files to OWI format
          logMessage "Downloading background meteorology."
          logMessage "downloadBackgroundMet $RUNDIR $SCRIPTDIR $BACKSITE $BACKDIR $ENSTORM $CSDATE $HSTIME $FORECASTLENGTH $ALTNAMDIR $FORECASTCYCLE $ARCHIVEBASE $ARCHIVEDIR"
          downloadBackgroundMet $RUNDIR $SCRIPTDIR $BACKSITE $BACKDIR $ENSTORM $CSDATE $HSTIME $FORECASTLENGTH $ALTNAMDIR $FORECASTCYCLE $ARCHIVEBASE $ARCHIVEDIR
          cd $ADVISDIR/${ENSTORM} 2>> ${SYSLOG}
          NAMOPTIONS=" --ptFile ${INPUTDIR}/${PTFILE} --namFormat grib2 --namType $ENSTORM --awipGridNumber 218 --dataDir ${ADVISDIR}/${ENSTORM} --outDir ${ADVISDIR}/${ENSTORM}/ --velocityMultiplier 0.893 --scriptDir ${SCRIPTDIR}"
          logMessage "Converting NAM data to OWI format with the following options : $NAMOPTIONS"
          perl ${SCRIPTDIR}/NAMtoOWI.pl $NAMOPTIONS >> ${SYSLOG} 2>&1 
          CONTROLOPTIONS=" --scriptdir $SCRIPTDIR --advisdir $ADVISDIR --name $ENSTORM --dt $TIMESTEPSIZE --nws $NWS --controltemplate ${INPUTDIR}/${CONTROLTEMPLATE} --cst $CSDATE --hstime $HSTIME --hsformat $HOTSTARTFORMAT $OUTPUTOPTIONS"
          # create links to the OWI files
          NAM221=`ls NAM*.221`; 
          NAM222=`ls NAM*.222`;
          ln -s $NAM221 fort.221 2>> ${SYSLOG}
          ln -s $NAM222 fort.222 2>> ${SYSLOG}
          if [[ ! -e $ADVISDIR/$ENSTORM/runme ]]; then
             RUNFORECAST=no
          fi
       fi
       if [[ $WAVES = on ]]; then
          CONTROLOPTIONS="${CONTROLOPTIONS} --swandt $SWANDT --swantemplate ${INPUTDIR}/${SWANTEMPLATE} --hotswan $HOTSWAN"
       fi
       CONTROLOPTIONS="${CONTROLOPTIONS} --elevstations ${INPUTDIR}/${ELEVSTATIONS} --velstations ${INPUTDIR}/${VELSTATIONS} --metstations ${INPUTDIR}/${METSTATIONS}"
       CONTROLOPTIONS="$CONTROLOPTIONS --gridname $GRIDNAME" # for run.properties
       logMessage "Generating ADCIRC Control File (fort.15) for $ENSTORM with the following options: $CONTROLOPTIONS."
       perl $SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS >> ${SYSLOG} 2>&1
       if [[ ! -d $ADVISDIR/$ENSTORM ]]; then continue; fi
       # get river flux nowcast data, if configured to do so
       if [[ $VARFLUX = on ]]; then
          downloadRiverFluxData $ADVISDIR ${INPUTDIR}/${GRIDFILE} $RIVERSITE $RIVERDIR $ENSTORM $CSDATE $HSTIME $SCRIPTDIR ${INPUTDIR}/${RIVERFLUX} $USERIVERFILEONLY
       fi
       echo "hostname : $HOSTNAME" >> $ADVISDIR/$ENSTORM/run.properties  
       if [[ $RUNFORECAST = yes ]]; then
          # preprocess
          logMessage "Starting $ENSTORM preprocessing."
          prep $ADVISDIR $INPUTDIR $ENSTORM $START $OLDADVISDIR $ENV $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT "$OUTPUTOPTIONS" $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE
          handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HOSTNAME $STORMNAME $YEAR $STORMDIR $ADVISORY $GRIDFILE $EMAILNOTIFY "${POST_LIST}"
          if [[ ! -d $ADVISDIR/$ENSTORM ]]; then
             si=$[$si + 1];
             continue # abandon this member of the forecast ensemble 
          fi
          JOBTYPE=padcirc
          if [[ $WAVES = on ]]; then
             JOBTYPE=padcswan
          fi
          # then submit the job
          logMessage "Submitting ensemble member $ENSTORM for forecast."
          consoleMessage "Submitting ensemble member $ENSTORM for forecast."
          submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $NOTIFYUSER $ENV $ACCOUNT $PPN $NUMWRITERS $HOTSTARTCOMP $FORECASTWALLTIME $JOBTYPE
          # check once per minute until job has completed
          monitorJobs $QUEUESYS ${JOBTYPE}.${ENSTORM} $FORECASTWALLTIME
          handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HOSTNAME $STORMNAME $YEAR $STORMDIR $ADVISORY $GRIDFILE $EMAILNOTIFY "${POST_LIST}"
          if [[ ! -d $ADVISDIR/$ENSTORM ]]; then
             si=$[$si + 1];
             continue  # abandon this member of the forecast ensemble  
          fi
          consoleMesssage "Job(s) complete."
          # execute post processing
          logMessage "$ENSTORM finished; postprocessing"
          # execute post processing
          ${OUTPUTDIR}/${POSTPROCESS} $CONFIG $ADVISDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $CSDATE $HSTIME $GRIDFILE $OUTPUTDIR $SYSLOG $SSHKEY >> ${SYSLOG} 2>&1
          # send out an email notifying end users that results are available
          ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HOSTNAME $STORMNAME $YEAR $STORMDIR $ADVISORY $ENSTORM $GRIDFILE results $EMAILNOTIFY $SYSLOG "${POST_LIST}" >> ${SYSLOG} 2>&1
          if [[ ! -z $POSTPROCESS2 ]]; then # creates GIS and kmz figures
             ${OUTPUTDIR}/${POSTPROCESS2} $CONFIG $ADVISDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $CSDATE $HSTIME $GRIDFILE $OUTPUTDIR $SYSLOG 2>> ${SYSLOG} 
          fi
       fi
       si=$[$si + 1];
    done
    # copy results to archive location
    logMessage "Initiating archival process, if any."
    ${OUTPUTDIR}/${ARCHIVE} $ADVISDIR $OUTPUTDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $ARCHIVEBASE $ARCHIVEDIR  2>> ${SYSLOG} & 
    logMessage "Forecast complete for advisory '$ADVISORY.'"
    consoleMessage "Forecast complete for advisory '$ADVISORY.'"
    OLDADVISDIR=$ADVISDIR
    LASTSUBDIR=""
done
