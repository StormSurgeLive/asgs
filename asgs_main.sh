#!/bin/bash

#trap read debug
#----------------------------------------------------------------
# asgs_main.sh: This is the main driver script for the ADCIRC Surge Guidance
# System (ASGS). It performs configuration tasks via config.sh, then enters a
# loop which is executed once per advisory cycle.
#----------------------------------------------------------------
# Copyright(C) 2006--2024 Jason Fleming
# Copyright(C) 2006--2007, 2019--2024 Brett Estrade
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
THIS=$(basename -- $0)
#
#####################################################################
#                B E G I N   F U N C T I O N S
#####################################################################
spinner()
{
   # $1 is the time limit in seconds to spin in seconds
   #    (0 to spin forever or until associated process exits)
   # $2 is the (optional) process ID to wait on
   #    (if both pid and time limit were provided, and
   #     time limit is exceeded before the process exits,
   #     this function returns an error code for the calling
   #     routine to interpret and deal with)
   local spin='-\|/'
   local i=0
   local j=0
   while [[ $j -le $1 ]]; do
      i=$(( (i+1) %4 ))
      printf "\b${spin:$i:1}" # to the console
      sleep 1
      # if there is a process ID, and the associated process
      # has finished, break out of the loop
      if [[ ! -z $2 ]]; then
         if ! kill -0 $2 >> /dev/null 2>&1 ; then
            return 0  # process we were waiting on has exited
         fi
         if [[ $1 -eq 0 ]]; then
            # wait indefinitely for process to end
            j=$(( (j-1) ))
         fi
      fi
      j=$(( (j+1) ))
   done
   # time limit has been reached; return success unless
   # process ID was also provided
   if [[ ! -z $2 ]]; then
      return 1  # process we are waiting on is still running
   else
      return 0  # we successfully waited for the right amount of time
   fi
}

# reads/rereads+rebuilds derived variables
# Sets default values for many different asgs parameters;
# the order of precedence is to (1) use the value from the Operator's
# configuration file, then (2) to use the value from the default
# files listed below, then (3) to use the initialized parameter
# value in this script itself (in variables_init())
readConfig()
{
   logMessage "Resetting defaults and then re-reading configuration."
   # Initialize variables accessed from ASGS config parameters to reasonable values
   source ${SCRIPTDIR}/config/config_defaults.sh
   # Initialize model parameters to appropriate values
   source ${SCRIPTDIR}/config/model_defaults.sh
   # HPC environment defaults (using the functions in platforms.sh)
   env_dispatch "$HPCENVSHORT"
   # set default output file formats and frequencies
   source ${SCRIPTDIR}/config/io_defaults.sh
   # set default values related to forcing URLs etc
   source ${SCRIPTDIR}/config/forcing_defaults.sh
   # pick up config parameters, set by the Operator, that differ from the defaults
   source ${CONFIG}
   # ensure single digit STORM numbers issued by NHC are zero-padded
   if [[ ${#STORM} -lt 2 && $STORM -lt 10 ]]; then
     STORM=$(printf "%02d" "$STORM")
   fi
   # maintain backward compatibility with old config files
   if [[ $ENSEMBLESIZE != "null" ]]; then
       SCENARIOPACKAGESIZE=$ENSEMBLESIZE
   fi
   #
   RUNARCHIVEBASE=$SCRATCHDIR
}
#
# helper subroutine to check for the existence of required files that have
# been specified in config.sh
checkFileExistence()
{ FPATH=$1
  FTYPE=$2
  FNAME=$3
  local THIS="asgs_main.sh>checkFileExistence()"
  if [[ -z $FNAME ]]; then
     fatal "$THIS: The $FTYPE was not specified in the configuration file. When it is specified, the ASGS will look for it in the path ${FPATH}."
  fi
  local success=no
  if [ $FNAME ]; then
     if [ -e "${FPATH}/${FNAME}" ]; then
        logMessage "$THIS: The $FTYPE '${FPATH}/${FNAME}' was found."
        success=yes
     else
        # If this is a mesh (fort.14), nodal attributes (fort.13), static water level correction,
        # or self attracting / earth load tide (fort.24) file, attempt to download and uncompress it.
        # If the URL starts with "scp://" then the Operator's ssh configuration must support
        # public key authentication with the host that the files will be downloaded from.
        # The URLs used here have their default values set in config/mesh_defaults.sh and these
        # URL values can be overridden in the Operator's ~/.asgsh_profile file or in the
        # configuration file for the ASGS instance.
        # If scp is to be used to download files, this function expects URLs to be in the form
        #    scp://tacc_tds3/meshes
        # where the directory ("meshes" in the above case) is assumed by scp to be relative
        # to the Operator's home directory on the remote machine unless the path starts with
        # a forward slash:
        #    scp://tacc_tds3//meshes
        # In which case it is treated as a full path.
        local URL
        case $FTYPE in
           "ADCIRC mesh file")
              URL=$MESHURL
              ;;
           "ADCIRC nodal attributes (fort.13) file")
              URL=$NODALATTRIBUTESURL
              ;;
           "ADCIRC static water level offset data file")
              URL=$OFFSETURL
              ;;
           "ADCIRC self attracting earth load tide file")
              URL=$LOADTIDEURL
              ;;
           *)
              warn "$THIS: Unrecognized file type to download: '$FTYPE'."
              URL="unknown"
              ;;
        esac
        local downloadCMD
        if [[ $URL =~ "http://" || $URL =~ "https://" ]]; then
           logMessage "$THIS: The curl version is $(curl --version)"
           downloadCMD="curl --insecure ${URL}/${FNAME}.xz --output ${FPATH}/${FNAME}.xz"
        elif [[ $URL =~ "scp://" ]]; then
           URL=${URL:6}     # remove the scp://
           URL=${URL/\//:}  # replace the / between the host and the path with a :
           downloadCMD="scp $URL/${FNAME}.xz $FPATH/${FNAME}.xz"
        else
           warn "$THIS: Unrecognized protocol in URL: '$URL'."
           downloadCMD="unknown"
        fi
        # attempt to download the file
        logMessage "$THIS: Downloading $FTYPE from ${URL}/${FNAME}.xz with the command '$downloadCMD'."
        consoleMessage "$I Downloading '${FNAME}.xz'"
        $downloadCMD 2> errmsg &
        local pid=$!
        spinner 120 $pid  # hardcode that it should not take longer than 2 minutes to download in any case
        local err=$?
        if [[ $err == 0 ]]; then
           logMessage "$THIS: Uncompressing ${FPATH}/${FNAME}.xz."
           consoleMessage "$I Uncompressing '${FNAME}.xz'."
           xz -d ${FPATH}/${FNAME}.xz 2> errmsg 2>&1 || warn "$THIS: Failed to uncompress ${FPATH}/${FNAME}.xz : `cat errmsg`." &
           pid=$!
           spinner 120 $pid
           [[ -e ${FPATH}/${FNAME} ]] && success=yes || success=no
        else
           consoleMessage "$W Failed to download ${FNAME}.xz"
           logMessage "$THIS: Failed to download $FTYPE from ${URL}/${FNAME}.xz to ${FPATH}/${FNAME}.xz: `cat errmsg`."
        fi
     fi
  fi
  if [[ $success == no ]]; then
     fatal "$THIS: The $FTYPE '${FPATH}/${FNAME}' does not exist."
  fi
}
#
# The ASGS stores a .tar.gz archive of decomposed subdomain
# fort.14 and fort.18 to avoid having to re-run adcprep --partmesh
# and --prepall for each cycle (which is time consuming) ... however
# if the fulldomain files change, then this archive will have to be
# rebuilt.
#
# compare the modification times of the input files with the archive of
# subdomain files to avoid using a stale archive
# @jasonfleming: 20180814: moved the storage of the prepped archive
# from the ASGS $SCRIPTDIR/input/meshes/[mesh] subdirectory to the
# scratch directory
checkArchiveFreshness()
{  PREPPEDARCHIVE=$1
   HINDCASTARCHIVE=$2
   GRIDFILE=$3
   CONTROLTEMPLATE=$4
   ELEVSTATIONS=$5
   VELSTATIONS=$6
   METSTATIONS=$7
   NAFILE=$8
   SCRATCHDIR=$9
   THIS="asgs_main.sh>checkArchiveFreshness()"

   logMessage "$THIS: Checking to see if the archive of preprocessed subdomain files is up to date."
   for archiveFile in $PREPPEDARCHIVE $HINDCASTARCHIVE; do
      if [ ! -e $RUNARCHIVEBASE/$archiveFile ]; then
         logMessage "$THIS: The subdomain archive file $SCRATCHDIR/$archiveFile does not exist."
         continue
      fi
      # jgfdebug:: for some meshes, $NAFILE is undefined but this case is not handled
      inputFiles=( $GRIDFILE $CONTROLTEMPLATE $ELEVSTATIONS $VELSTATIONS $METSTATIONS )
      if [[ ! -z $NAFILE && $NAFILE != "null" ]]; then
         inputFiles+=( $NAFILE )
      fi
      for inputFile in ${inputFiles[@]}; do
         if [ ! -e $INPUTDIR/$inputFile ]; then
            consoleMessage "$W The input file $INPUTDIR/$inputFile does not exist."
            continue
         fi
         # see if the archiveFile is older than inputFile
         if [ $SCRATCHDIR/$archiveFile -ot $INPUTDIR/$inputFile ]; then
            logMessage "$THIS: A change in the input files has been detected. The archive file $archiveFile is older than the last modification time of the input file ${inputFile}. The archive file is therefore stale and will be deleted. A fresh one will automatically be created the next time adcprep is run."
            rm $SCRATCHDIR/$archiveFile 2>> $SYSLOG
         fi
      done
   done
}
#
# helper subroutine to check for the existence of required directories
# that have been specified in config.sh
checkDirExistence()
{ DIR=$1
  TYPE=$2
  THIS="asgs_main.sh>checkDirExistence()"
  if [[ -z $DIR ]]; then
     fatal "$THIS: The $TYPE was not specified in the configuration file."
  fi
  if [[ -e $DIR ]] ; then
     logMessage "$THIS: The $TYPE '$DIR' was found."
  else
     fatal "$THIS: The $TYPE '$DIR' does not exist."
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
   # TODO: This function should autodetect the hotstart file format,
   # composition, and location rather than assuming it based on the
   # current ASGS configuration file.
   local THIS="asgs_main.sh>checkHotstart()"
   # set name and specific file location based on format (netcdf or binary)
   HOTSTARTFILE=$FROMDIR/fort.$LUN.nc # netcdf format is the default
   if [[ $HOTSTARTFORMAT == binary ]]; then
      HOTSTARTFILE=$FROMDIR/PE0000/fort.$LUN # could be either fulldomain or subdomain
   fi
   # check for existence of hotstart file
   if [ ! -e $HOTSTARTFILE ]; then
      fatal "$THIS: The hotstart file '$HOTSTARTFILE' was not found. The preceding simulation run must have failed to produce it."
   # if it exists, check size to be sure its nonzero
   else
      hotstartSize=`stat -c %s $HOTSTARTFILE`
      if [ $hotstartSize == "0" ]; then
         fatal "$THIS: The hotstart file '$HOTSTARTFILE' is of zero length. The preceding simulation run must have failed to produce it properly."
      else
         logMessage "$THIS: The hotstart file '$HOTSTARTFILE' was found and it contains $hotstartSize bytes."
         # check time in hotstart file to be sure it can be found and that
         # it is nonzero
         # jgf20170131: hstime reports errors to stderr so we must capture
         # that with backticks and tee to the log file
         HSTIME=''
         if [[ $HOTSTARTFORMAT == "netcdf" || $HOTSTARTFORMAT == "netcdf3" ]]; then
            HSTIME=`$ADCIRCDIR/hstime -f $HOTSTARTFILE -n 2>&1 | tee --append ${SYSLOG}`
         else
            HSTIME=`$ADCIRCDIR/hstime -f $HOTSTARTFILE 2>&1 | tee --append ${SYSLOG}`
         fi
         failureOccurred=$?
         errorOccurred=$(expr index "$HSTIME" ERROR)
         if [[ $failureOccurred != 0 || $errorOccurred != 0 ]]; then
            fatal "$THIS: The hstime utility could not read the ADCIRC time from the file '$HOTSTARTFILE'. The output from hstime was as follows: '$HSTIME'."
         else
            if float_cond '$HSTIME == 0.0'; then
               THIS="asgs_main.sh>checkHotstart()"
               fatal "$THIS: The time in the hotstart file '$HOTSTARTFILE' is zero. The preceding simulation run must have failed to produce a proper hotstart file."
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
    local THIS="asgs_main.sh>float_cond()"
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
# subroutine to run adcprep, using a pre-prepped archive of
# fort.14, fort.24 files
#
# This subroutine deals with all the possible situations that can arise
# when running adcprep (hot vs cold, remaking the archive, swan vs no swan etc)
#
# TODO: Refactor this code so that it runs one serial script that does
# all the prep types that are needed. For example, if partmesh, prep15, and
# prep20 all need to be run, this should be done in a single batch script
# rather than submitting these serial adcprep jobs sequentially.
#
prep()
{   ADVISDIR=$1  # directory containing the now/forecast runs for this cycle
    INPUTDIR=$2 # directory where grid and nodal attribute files are found
    ENSTORM=$3  # scenario name (nowcast, storm1, storm5, etc)
    START=$4    # coldstart or hotstart
    FROMDIR=$5 # directory containing files to hotstart this run from
    HPCENVSHORT=$6     # machine to run on (jade, desktop, queenbee, etc)
    NCPU=$7     # number of CPUs to request in parallel jobs
    PREPPEDARCHIVE=$8 # preprocessed fort.14 and fort.18 package
    GRIDFILE=$9 # fulldomain grid
    ACCOUNT=${10} # account to charge time to
    OUTPUTOPTIONS="${11}" # contains list of args for appending files
    HOTSTARTCOMP=${12} # fulldomain or subdomain
    WALLTIME=${13} # HH:MM:SS format
    HOTSTARTFORMAT=${14}   # "binary" or "netcdf" or "netcdf3"
    MINMAX=${15}           # "continuous" or "reset"
    HOTSWAN=${16} # "yes" or "no" to reinitialize SWAN only
    NAFILE=${17}  # full domain nodal attributes file
    #
    THIS="asgs_main.sh>prep()"
    #debugMessage "top of prep() has the following values: RUNDIR=$RUNDIR ADVISDIR=$ADVISDIR ENSTORM=$ENSTORM NOTIFYSCRIPT=${OUTPUTDIR}/${NOTIFY_SCRIPT} HPCENV=$HPCENV STORMNAME=$STORMNAME YEAR=$YEAR STORMDIR=$STORMDIR ADVISORY=$ADVISORY LASTADVISORYNUM=$LASTADVISORYNUM STATEFILE=$STATEFILE GRIDFILE=$GRIDFILE EMAILNOTIFY=$EMAILNOTIFY JOBFAILEDLIST=${JOB_FAILED_LIST} ARCHIVEBASE=$ARCHIVEBASE ARCHIVEDIR=$ARCHIVEDIR"
    echo "time.adcprep.start : ($date +'%Y-%h-%d-T%H:%M:%S%z')" >> ${STORMDIR}/run.properties
    # set the name of the archive of preprocessed input files
    PREPPED=$PREPPEDARCHIVE
    if [[ $START = coldstart ]]; then
       PREPPED=$HINDCASTARCHIVE
    fi
    # determine if there is an archive of preprocessed input files
    HAVEARCHIVE=yes
    if [[ ! -e ${SCRATCHDIR}/${PREPPED} ]]; then
       HAVEARCHIVE=no
    fi
    # create directory to run in
    if [ ! -d $ADVISDIR/$ENSTORM ]; then
       mkdir $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
    fi
    cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
    logMessage "Linking to full domain input files."
    # symbolically link mesh file (fort.14)
    if [ ! -e $ADVISDIR/$ENSTORM/fort.14 ]; then
        ln -s $INPUTDIR/$GRIDFILE $ADVISDIR/$ENSTORM/fort.14 2>> ${SYSLOG}
    fi
    # symbolically link self attraction / earth load tide file if needed
    if [[ ! -e $ADVISDIR/$ENSTORM/fort.24 && $selfAttractionEarthLoadTide != "notprovided" ]]; then
        ln -s $INPUTDIR/$selfAttractionEarthLoadTide $ADVISDIR/$ENSTORM/fort.24 2>> ${SYSLOG}
    fi
    # create the znorth file if needed
    if [[ ! -e $ADVISDIR/$ENSTORM/fort.rotm && $zNorth != "northpole" ]]; then
        echo "znorth_in_spherical_coors" > $ADVISDIR/$ENSTORM/fort.rotm 2>> ${SYSLOG}
        echo "$zNorth"                  >> $ADVISDIR/$ENSTORM/fort.rotm 2>> ${SYSLOG}
    fi
    if [ $START = coldstart ]; then
       # if we have variable river flux, link the fort.20 file
       if [[ $VARFLUX = on || $VARFLUX = default ]]; then
          # jgf20110525: For now, just copy a static file to this location
          # and adcprep it. TODO: When real time flux data become available,
          # grab those instead of relying on a static file.
          ln -s ${INPUTDIR}/${HINDCASTRIVERFLUX} ./fort.20
       fi
    else
       # hotstart
       #
       # TODO: Autodetect the format of the hotstart files to read (the
       # type of hotstart files to write is determined by the HOTSTARTCOMP
       # and HOTSTARTFORMAT parameters in io_defaults.sh).
       # The io_defaults.sh values are "fulldomain" and "netcdf", respectively.
       # Supported use cases include : (a) reading fulldomain binary hotstart
       # file from $FROMDIR or $FROMDIR/PE0000; (b) reading subdomain binary
       # hotstart files from PE* directories or from a .tar.gz archive;
       # (c) reading fulldomain netcdf hotstart files; (d) starting serial
       # run from subdomain and (e) hotstarting from subdomain binary hotstart
       # files decomposed to a different number of cores than the source run.
       # This would be best accomplished by writing/reading properties
       # to/from the run.properties file.
       #
       # copy in the swaninit file which contains the name of the swan
       # control file (conventionally named fort.26 when used with ADCIRC)
       #
       # Also need to add a check on the copying of subdomain hotstart
       # files; on certain platforms, this copying will sometimes fail
       # (one of the hotstart files will be missed). It is also possible
       # for hotstart files to be copied but the parallel job that reads
       # them will start very soon after and the filesystem will report
       # that these files are missing.
       #
       if [[ $WAVES = on ]]; then
          cp $SCRIPTDIR/input/meshes/common/swan/swaninit.template $ADVISDIR/$ENSTORM/swaninit 2>> ${SYSLOG}
       fi
       # jgfdebug: TODO: FIXME: Hardcoded the time varying weirs input file
       if [ -e $INPUTDIR/time-bonnet.in ]; then
          logMessage "$ENSTORM: $THIS: Copying $INPUTDIR/time-bonnet.in to $ADVISDIR/$ENSTORM."
          cp $INPUTDIR/time-bonnet.in $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
       fi
       logMessage "$ENSTORM: $THIS: Copying existing output files to this directory."
       if [[ $MINMAX = continuous ]]; then
          # copy max and min files so that the max values will be
          # preserved across hotstarts
          # @jasonfleming: Applying netcdf fix from @mattbilskie
          for file in maxele.63 maxwvel.63 minpr.63 maxrs.63 maxvel.63 elemaxdry.63 nodeflag.63 rising.63 tinun.63 maxele.63.nc maxinundepth.63.nc maxrs.63.nc maxvel.63.nc maxwvel.63.nc minpr.63.nc elemaxdry.63.nc nodeflag.63.nc rising.63.nc tinun.63.nc swan_*_max.*; do
             if  [ -e $FROMDIR/$file ]; then
                logMessage "$ENSTORM: $THIS: Copying $FROMDIR/$file to $ADVISDIR/$ENSTORM/$file so that its values will be preserved across the hotstart."
                cp $FROMDIR/$file $ADVISDIR/$ENSTORM/$file 2>> ${SYSLOG}
             fi
          done
       else
          logMessage "$ENSTORM: $THIS: MINMAX was set to '$MINMAX' in the ASGS config file; as a result, the maxele.63 etc files will not be from the previous run to the current run. ADCIRC will start the record of max and min values anew."
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
                   logMessage "$ENSTORM: $THIS: Copying $FROMDIR/$file to $ADVISDIR/$ENSTORM/$file so that it will be appended during the upcoming run."
                   cp $FROMDIR/$file $ADVISDIR/$ENSTORM/$file 2>> ${SYSLOG}
                fi
             fi
          done
       done
       # bring in hotstart file(s)
       if [[ $QUEUESYS = serial ]]; then
          if [[ $HOTSTARTFORMAT == netcdf || $HOTSTARTFORMAT == "netcdf3" ]]; then
             # copy netcdf file so we overwrite the one that adcprep created
             cp --remove-destination $FROMDIR/fort.67.nc $ADVISDIR/$ENSTORM/fort.68.nc >> $SYSLOG 2>&1
          else
             cp $FROMDIR/fort.67 $ADVISDIR/$ENSTORM/fort.68 >> $SYSLOG 2>&1
          fi
       fi
       # swan hotstart file
       if [[ $WAVES = on && $HOTSWAN = on ]]; then
          cp $FROMDIR/swan.67 $ADVISDIR/$ENSTORM/swan.68 >> $SYSLOG 2>&1
       fi
    fi
    #
    #
    # R E T U R N   N O W
    # I F   T H I S   I S   A   S E R I A L   R U N
    #
    # adcprep is not required if the job is to run in serial
    if [[ $QUEUESYS = "serial" ]]; then
       return
    fi
    #
    # C O N T I N U E   W I T H   A D C P R E P
    # F O R   P A R A L L E L   R U N
    #
    echo "time.adcprep.start : $(date +'%Y-%h-%d-T%H:%M:%S%z')" >> ${STORMDIR}/run.properties
    # set the name of the archive of preprocessed input files
    PREPPED=$PREPPEDARCHIVE
    if [[ $START = coldstart ]]; then
       PREPPED=$HINDCASTARCHIVE
    fi
    # determine if there is an archive of preprocessed input files
    HAVEARCHIVE=yes
    if [[ ! -e ${SCRATCHDIR}/${PREPPED} ]]; then
       HAVEARCHIVE=no
    fi
    if [[ $HAVEARCHIVE = yes ]]; then
        # copy in the files that have already been preprocessed
        logMessage "$ENSTORM: $THIS: Copying input files that have already been decomposed."
        cp ${SCRATCHDIR}/${PREPPED} . 2>> ${SYSLOG}
        gunzip -f ${PREPPED} 2>> ${SYSLOG}
        # untar the uncompressed archive
        UNCOMPRESSEDARCHIVE=${PREPPED%.gz}
        # extract the archive redirecting stdout (list of files extracted
        # by tar) to scenario.log and any error messages to both scenario.log
        # and syslog with a time stamp
        tar xvf $UNCOMPRESSEDARCHIVE >> scenario.log 2> >(awk -v this='asgs_main.sh>prep' -v level=ERROR -f $SCRIPTDIR/monitoring/timestamp.awk | tee -a ${SYSLOG})
        logMessage "$ENSTORM: $THIS: Removing $UNCOMPRESSEDARCHIVE"
        rm $UNCOMPRESSEDARCHIVE 2>> ${SYSLOG}
    fi
    #
    # this is a P A R A L L E L    C O L D S T A R T
    if [ $START = coldstart ]; then
       # now run adcprep to decompose the files
       if [[ $HAVEARCHIVE = no ]]; then
          logMessage "$ENSTORM: $THIS: Running adcprep to partition the mesh for $NCPU compute processors."
          prepFile partmesh $NCPU $ACCOUNT $WALLTIME
          THIS="asgs_main.sh>prep()"
          logMessage "$ENSTORM: $THIS: Running adcprep to prepare all files."
          prepFile prepall $NCPU $ACCOUNT $WALLTIME
          THIS="asgs_main.sh>prep()"
       else
          logMessage "$ENSTORM: $THIS: Running adcprep to prepare new fort.15 file."
          prepFile prep15 $NCPU $ACCOUNT $WALLTIME
          THIS="asgs_main.sh>prep()"
          if [[ $VARFLUX = on || $VARFLUX = default ]]; then
             logMessage "$ENSTORM: $THIS: Running adcprep to prepare new fort.20 file."
             prepFile prep20 $NCPU $ACCOUNT $WALLTIME
             THIS="asgs_main.sh>prep()"
          fi
          if [ -e $ADVISDIR/$ENSTORM/fort.13 ]; then
             logMessage "$ENSTORM: $THIS: Running adcprep to prepare new fort.13 file."
             prepFile prep13 $NCPU $ACCOUNT $WALLTIME
             THIS="asgs_main.sh>prep()"
          fi
       fi
    else
       # this is a P A R A L L E L   H O T S T A R T
       #
       # run adcprep to decompose the new files
       if [[ $HAVEARCHIVE = no ]]; then
          logMessage "$ENSTORM: $THIS: Running adcprep to partition the mesh for $NCPU compute processors."
          prepFile partmesh $NCPU $ACCOUNT $WALLTIME
          THIS="asgs_main.sh>prep()"
          logMessage "$ENSTORM: $THIS: Running adcprep to prepare all files."
          prepFile prepall $NCPU $ACCOUNT $WALLTIME
          THIS="asgs_main.sh>prep()"
       else
          logMessage "$ENSTORM: $THIS: Running adcprep to prepare new fort.15 file."
          prepFile prep15 $NCPU $ACCOUNT $WALLTIME
          THIS="asgs_main.sh>prep()"
          if [[ $VARFLUX = on || $VARFLUX = default ]]; then
             logMessage "$ENSTORM: $THIS: Running adcprep to prepare new fort.20 file."
             prepFile prep20 $NCPU $ACCOUNT $WALLTIME
             THIS="asgs_main.sh>prep()"
          fi
          if [[ -e $ADVISDIR/$ENSTORM/fort.13 ]]; then
             logMessage "$ENSTORM: $THIS: Running adcprep to prepare new fort.13 file."
             prepFile prep13 $NCPU $ACCOUNT $WALLTIME
             THIS="asgs_main.sh>prep()"
          fi
          if [[ $WAVES = on ]]; then
             PE=0
             format="%04d"
             while [[ $PE -lt $NCPU ]]; do
                PESTRING=$(printf "$format" $PE)
                ln -s $ADVISDIR/$ENSTORM/fort.26 $ADVISDIR/$ENSTORM/PE${PESTRING}/fort.26 2>> ${SYSLOG}
                PE=$(($PE + 1))
             done
          fi
       fi
       # bring in hotstart file(s)
       if [[ $HOTSTARTCOMP = fulldomain ]]; then
          if [[ $HOTSTARTFORMAT == "netcdf" || $HOTSTARTFORMAT == "netcdf3" ]]; then
             # copy netcdf file so we overwrite the one that adcprep created
             cp --remove-destination $FROMDIR/fort.67.nc $ADVISDIR/$ENSTORM/fort.68.nc >> $SYSLOG 2>&1
          else
             ln -s $FROMDIR/PE0000/fort.67 $ADVISDIR/$ENSTORM/fort.68 >> $SYSLOG 2>&1
          fi
       fi
       # jgfdebug
       if [[ $HOTSTARTCOMP = subdomain ]]; then
          logMessage "$ENSTORM: $THIS: Starting copy of subdomain hotstart files."
          # copy the subdomain hotstart files over
          # subdomain hotstart files are always binary formatted
          PE=0
          format="%04d"
          while [ $PE -lt $NCPU ]; do
             PESTRING=`printf "$format" $PE`
             if [[ $HOTSTARTCOMP = subdomain ]]; then
                cp $FROMDIR/PE${PESTRING}/fort.67 $ADVISDIR/$ENSTORM/PE${PESTRING}/fort.68 2>> ${SYSLOG}
             fi
             PE=$(($PE + 1))
          done
          logMessage "$ENSTORM: $THIS: Completed copy of subdomain hotstart files."
          # add a delay here because on certain platforms, the filesystem
          # cannot keep up with this many files being copied, and when the
          # parallel compute job starts, the filesystem may report files not
          # found. This is a stopgap until a proper sanity check on the file
          # copy process can be implemented.
          logMessage "$ENSTORM: $THIS: Pausing 30 seconds after copying subdomain hotstart files."
          consoleMessage "$I Pausing 30 seconds after copying subdomain hotstart files."
          spinner 30
       fi
       #
       #  H O T S T A R T I N G   S W A N
       #
       # Globalizing and localizing the SWAN hotstart files can take
       # a significant amount of time and must be done in serial. If the
       # number of subdomains for this run is the same as the number of
       # subdomains used in the run used as the source of SWAN hotstart files,
       # then try to use the SWAN subdomain hotstart files directly.
       if [[ $WAVES = on && $HOTSWAN = on ]]; then
          logMessage "$ENSTORM: $THIS: Preparing SWAN hotstart file."
          swanHotstartOK=no
          # if archiving of the hotstart source run has started but is not
          # complete, wait until it is complete so that we don't
          # accidentally ingest partially complete tar files or partially
          # globalized fulldomain swan hotstart files or start to copy
          # subdomain swan hotstart files that are being deleted by an
          # archiving script
          logMessage "$ENSTORM: $THIS: Detecting time that SWAN hotstart archiving process started in ${FROMDIR}."
          swanArchiveStart=`sed -n 's/[ ^]*$//;s/time.archive.start\s*:\s*//p' $FROMDIR/run.properties`
          if [[ ! -z $swanArchiveStart ]]; then
             # archiving process has started
             logMessage "$ENSTORM: $THIS: The archiving process for the hotstart source run started at $swanArchiveStart."
             waitMinutes=0 # number of minutes waiting for the archiving process to complete
             waitMinutesMax=60  # max number of minutes to wait for upstream archiving process to finish
             consoleMessage "$I Waiting for SWAN hotstart file archive to be completed."
             while [[ $waitMinutes -lt $waitMinutesMax ]]; do
                # wait until it is finished or has errored out
                logMessage "$ENSTORM: $THIS: Detecting finish or error condition for archiving SWAN hotstart files in ${FROMDIR}."
                swanArchiveFinish=`sed -n 's/[ ^]*$//;s/time.archive.finish\s*:\s*//p' $FROMDIR/run.properties`
                swanArchiveError=`sed -n 's/[ ^]*$//;s/time.archive.error\s*:\s*//p' $FROMDIR/run.properties`
                if [[ ! -z $swanArchiveFinish || ! -z $swanArchiveError ]]; then
                   logMessage "$ENSTORM: $THIS: The archiving process for the hotstart source run has finished."
                   break
                else
                   printf "."
                   spinner 60
                   printf "\b.." # progress bar
                   waitMinutes=$(($waitMinutes + 1))
                fi
             done
             if [[ $waitMinutes -ge 60 ]]; then
                logMessage "$ENSTORM: $THIS: The archiving process for the hotstart source run did not finish within $watiMinutesMax minutes. Attempting to collect SWAN hotstart files anyway."
                consoleMessage "$W Archiving for SWAN hotstart files did not complete within the time limit."
             fi
          else
             # FIXME: how to handle this situation?
             logMessage "$ENSTORM: $THIS: The SWAN hotstart archiving process has not started in ${FROMDIR}."
             consoleMessage "$W The SWAN hotstart archiving process has not started."
          fi
          logMessage "$ENSTORM: $THIS: Detecting number of subdomains for SWAN hotstart files in ${FROMDIR}."
          hotSubdomains=`sed -n 's/[ ^]*$//;s/hpc.job.padcswan.ncpu\s*:\s*//p' $FROMDIR/run.properties`
          logMessage "hotSubdomains is $hotSubdomains ; NCPU is $NCPU ; FROMDIR is $FROMDIR"
          if [[ $hotSubdomains = $NCPU ]]; then
             logMessage "$ENSTORM: $THIS: The number of subdomains is the same as hotstart source; subdomain SWAN hotstart files will be copied directly."
             # subdomain swan hotstart files
             if [[ -e $FROMDIR/PE0000/swan.67 ]]; then
                logMessage "$ENSTORM: $THIS: Starting copy of subdomain swan hotstart files."
                # copy the subdomain hotstart files over
                # subdomain hotstart files are always binary formatted
                PE=0
                format="%04d"
                while [ $PE -lt $NCPU ]; do
                   PESTRING=`printf "$format" $PE`
                   cp $FROMDIR/PE${PESTRING}/swan.67 $ADVISDIR/$ENSTORM/PE${PESTRING}/swan.68 2>> ${SYSLOG}
                   PE=$(($PE + 1))
                done
                logMessage "$ENSTORM: $THIS: Completed copy of subdomain hotstart files."
                # add a delay here because on certain platforms, the filesystem
                # cannot keep up with this many files being copied, and when the
                # parallel compute job starts, the filesystem may report files not
                # found. This is a stopgap until a proper sanity check on the file
                # copy process can be implemented.
                logMessage "$ENSTORM: $THIS: Pausing 30 seconds after copying SWAN subdomain hotstart files."
                consoleMessage "Pausing 30 seconds after copying SWAN subdomain hotstart files."
                spinner 30
                swanHotstartOK=yes
             fi
             # subdomain SWAN hotstart files in a tar archive
             if [[ $swanHotstartOK = no ]]; then
                logMessage "$ENSTORM: $THIS: Could not copy subdomain SWAN hotstart files directly."
                for suffix in tar tar.gz tar.bz2 ; do
                   logMessage "$ENSTORM: $THIS: Looking for ${FROMDIR}/swan.67.${suffix}."
                   if [[ -e $FROMDIR/swan.67.${suffix} ]]; then
                      logMessage "$ENSTORM: $THIS: Found $FROMDIR/swan.67.${suffix}."
                      cp $FROMDIR/swan.67.${suffix} ./swan.68.${suffix} 2>> $SYSLOG
                      scenarioMessage "$THIS: Untarring SWAN hotstart files:"
                      case $suffix in
                      tar)
                         tar xvf swan.68.${suffix} >> scenario.log 2> >(awk -v this='asgs_main.sh>prep' -v level=ERROR -f $SCRIPTDIR/monitoring/timestamp.awk | tee -a ${SYSLOG})
                         if [[ $? == 0 ]]; then swanHotstartOK=yes ; fi
                         ;;
                      tar.gz)
                         tar xvzf swan.68.${suffix} >> scenario.log 2> >(awk -v this='asgs_main.sh>prep' -v level=ERROR -f $SCRIPTDIR/monitoring/timestamp.awk | tee -a ${SYSLOG})
                         if [[ $? == 0 ]]; then swanHotstartOK=yes ; fi
                         ;;
                      tar.bz2)
                         tar xvjf swan.68.${suffix} >> scenario.log 2> >(awk -v this='asgs_main.sh>prep' -v level=ERROR -f $SCRIPTDIR/monitoring/timestamp.awk | tee -a ${SYSLOG})
                         if [[ $? == 0 ]]; then swanHotstartOK=yes ; fi
                         ;;
                      *)
                         warn "$ENSTORM: $THIS: SWAN hotstart file archive $FROMDIR/swan.67.${suffix} unrecognized."
                         ;;
                      esac
                      for dir in `ls -d PE*`; do
                         mv $dir/swan.67 $dir/swan.68 2>> $SYSLOG
                      done
                      rm swan.68.${suffix} 2>> $SYSLOG
                      break
                   fi
                done
             fi
             if [[ $swanHotstartOK = no ]]; then
                logMessage "$ENSTORM: $THIS: Failed to obtain subdomain SWAN hotstart files."
             fi
          else
             logMessage "$ENSTORM: $THIS: The number of subdomains is different from the hotstart source; a fulldomain SWAN hotstart file will be decomposed to the subdomains."
          fi
          #
          # if copying subdomain SWAN hotstart files did not work
          # or is not appropriate because the number of subdomains in
          # this run is different from the hotstart source, try to
          # decompose a fulldomain SWAN hotstart file
          if [[ $swanHotstartOK = no ]]; then
             logMessage "$ENSTORM: $THIS: Decomposing fulldomain SWAN hotstart file."
             # fulldomain swan hotstart file or archive of subdomain
             # swan hotstart files
             if [[ -e $FROMDIR/swan.67 ]]; then
                cp $FROMDIR/swan.67 ./swan.68 2>> $SYSLOG
             elif [[ -e $FROMDIR/swan.67.gz ]]; then
                cp $FROMDIR/swan.67.gz ./swan.68.gz 2>> $SYSLOG
                gunzip swan.68.gz 2>> $SYSLOG
             elif [[ -e $FROMDIR/swan.67.bz2 ]]; then
                cp $FROMDIR/swan.67.bz2 ./swan.68.bz2 2>> $SYSLOG
                bunzip2 swan.68.bz2 2>> $SYSLOG
             fi
             if [[ -e  swan.68 ]]; then
                logMessage "$ENSTORM: $THIS: Starting decomposition of fulldomain swan hotstart file to subdomains."
                ${ADCIRCDIR}/../swan/unhcat.exe <<EOF 2>> ${SYSLOG}
2
swan.68
F
EOF
                if [[ $? == 0 ]]; then swanHotstartOK=yes ; fi
             fi
             if [[ $swanHotstartOK = yes ]]; then
                logMessage "$ENSTORM: $THIS: Completed decomposition of fulldomain swan hotstart file."
             else
                error "$ENSTORM: $THIS: Failed to obtain any swan hotstart file."
             fi
          fi
       fi
       if [[ $WAVES = off ]]; then
          logMessage "$ENSTORM: $THIS: SWAN coupling is not active."
       fi
       if [[ $WAVES = on && $HOTSWAN = off ]]; then
          logMessage "$ENSTORM: $THIS: SWAN coupling is active but SWAN hotstart files are not available in $FROMDIR. SWAN will be cold started."
       fi
    fi
    # if we don't have an archive of our preprocessed files, create
    # one so that we don't have to do another prepall
    if [[ $HAVEARCHIVE = no ]]; then
       logMessage "$ENSTORM: $THIS: Creating an archive of preprocessed files and saving to ${SCRATCHDIR}/${PREPPED} to avoid having to run prepall again."
       FILELIST='partmesh.txt PE*/fort.14 PE*/fort.18'
       if [[ $selfAttractionEarthLoadTide != "notprovided" ]]; then
          FILELIST=$FILELIST' PE*/fort.24'
       fi
       tar czf ${INPUTDIR}/${PREPPED} ${FILELIST} 2>> ${SYSLOG}
       # check status of tar operation; if it failed, delete the file
       # it attempted to make and alert the operator
       if [[ $? != 0 ]]; then
          warn "$ENSTORM: $THIS: The construction of a tar archive of the preprocessed input files has failed."
          rm ${SCRATCHDIR}/${PREPPED} 2>> ${SYSLOG} 2>&1
       fi
    fi
    echo "time.adcprep.finish : $(date +'%Y-%h-%d-T%H:%M:%S%z')" >> ${STORMDIR}/run.properties
    debugMessage "bottom of prep() has the following values: RUNDIR=$RUNDIR ADVISDIR=$ADVISDIR ENSTORM=$ENSTORM NOTIFYSCRIPT=${OUTPUTDIR}/${NOTIFY_SCRIPT} HPCENV=$HPCENV STORMNAME=$STORMNAME YEAR=$YEAR STORMDIR=$STORMDIR ADVISORY=$ADVISORY LASTADVISORYNUM=$LASTADVISORYNUM STATEFILE=$STATEFILE GRIDFILE=$GRIDFILE EMAILNOTIFY=$EMAILNOTIFY JOBFAILEDLIST=${JOB_FAILED_LIST} ARCHIVEBASE=$ARCHIVEBASE ARCHIVEDIR=$ARCHIVEDIR"
}
#
# function to run adcprep in a platform dependent way to decompose
# the fort.15 and fort.20
#
# TODO: This should be refactored and streamlined as described in the TODO
# above the prep() function above.
prepFile()
{  JOBTYPE=$1
   NCPU=$2
   ACCOUNT=$3
   WALLTIME=$4
   THIS="asgs_main.sh>prepFile()"

   echo "hpc.job.${JOBTYPE}.for.ncpu : $NCPU" >> $ADVISDIR/$ENSTORM/run.properties
   echo "hpc.job.${JOBTYPE}.limit.walltime : $ADCPREPWALLTIME" >> $ADVISDIR/$ENSTORM/run.properties
   echo "hpc.job.${JOBTYPE}.account : $ACCOUNT" >> $ADVISDIR/$ENSTORM/run.properties
   echo "hpc.job.${JOBTYPE}.file.qscripttemplate : $QSCRIPTTEMPLATE" >> $ADVISDIR/$ENSTORM/run.properties
   echo "hpc.job.${JOBTYPE}.parallelism : serial" >> $STORMDIR/run.properties

   # adjusts $SERQUEUE, if criteria is met; othewise returns current value as the defaults;
   SERQUEUE=$(HPC_Queue_Hint "$SERQUEUE" "$HPCENV" "$QOS" "1")
   echo "hpc.job.${JOBTYPE}.serqueue : $SERQUEUE" >> $STORMDIR/run.properties

   # adjusts $_PPN, if criteria is met; othewise returns current value as the defaults;
   # $PPN is not adjusted so the original value is preserved; yet the "corrected" value
   # is written to $STORMDIR/run.properties, which is where ./qscript.pl gets the value
   # for "$ppn"
   _PPN=$(HPC_PPN_Hint "serial" "$SERQUEUE" "$HPCENV" "$QOS" "1" "1")
   echo "hpc.job.${JOBTYPE}.ppn : ${_PPN}" >> $STORMDIR/run.properties
   # adjusts $RESERVATION, if criteria is met; othewise returns current value as the defaults;
   _RESERVATION=$(HPC_Reservation_Hint "$RESERVATION" "$HPCENV" "$QOS" "1")
   echo "hpc.slurm.job.${JOBTYPE}.reservation : ${_RESERVATION}" >> $STORMDIR/run.properties
   echo "hpc.slurm.job.${JOBTYPE}.constraint : $CONSTRAINT" >> $STORMDIR/run.properties
   echo "hpc.slurm.job.${JOBTYPE}.qos : $QOS" >> $STORMDIR/run.properties
   #
   # build queue script
   qScriptRequestTemplate=$SCRIPTDIR/qscript_request_template.json
   qScriptRequest=$SCENARIODIR/qscript_request_$JOBTYPE.json
   qScriptResponse=$SCENARIODIR/qscript_response_$JOBTYPE.json
   QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
   parallelism=serial
   forncpu=$NCPU
   wind10mlayer=$createWind10mLayer
   if [[ $NWS == "0" ]]; then
      wind10mlayer="no"
   fi
   #
   # create queue script request by filling in template
   # with data needed to create queue script
   sed \
      -e "s/%jobtype%/$JOBTYPE/" \
      -e "s?%qscripttemplate%?$QSCRIPTTEMPLATE?" \
      -e "s/%parallelism%/$parallelism/" \
      -e "s/%ncpu%/$NCPU/" \
      -e "s/%forncpu%/$NCPU/" \
      -e "s/%numwriters%/$NUMWRITERS/" \
      -e "s/%joblauncher%/$JOBLAUNCHER/" \
      -e "s/%walltime%/$ADCPREPWALLTIME/" \
      -e "s/%walltimeformat%/$WALLTIMEFORMAT/" \
      -e "s/%ppn%/${_PPN}/" \
      -e "s/%queuename%/$QUEUENAME/" \
      -e "s/%serqueue%/$SERQUEUE/" \
      -e "s/%account%/$ACCOUNT/" \
      -e "s?%advisdir%?$ADVISDIR?" \
      -e "s?%scriptdir%?$SCRIPTDIR?" \
      -e "s?%adcircdir%?$ADCIRCDIR?" \
      -e "s/%wind10mlayer%/$wind10mlayer/" \
      -e "s/%scenario%/$SCENARIO/" \
      -e "s/%reservation%/${_RESERVATION}/" \
      -e "s/%constraint%/$CONSTRAINT/" \
      -e "s/%qos%/$QOS/" \
      -e "s?%syslog%?$SYSLOG?" \
      -e "s?%scenariolog%?$SCENARIOLOG?" \
      -e "s/%hotstartcomp%/$HOTSTARTCOMP/" \
      -e "s/%queuesys%/$QUEUESYS/" \
      -e "s/%hpcenvshort%/$HPCENVSHORT/" \
      -e "s/%asgsadmin%/$ASGSADMIN/" \
      -e "s/%NULLLASTUPDATER%/$THIS/" \
      -e "s/%NULLLASTUPDATETIME%/$(date +'%Y-%h-%d-T%H:%M:%S%z')/" \
      < $qScriptRequestTemplate \
      > $qScriptRequest \
    2>> $SYSLOG
   unset _PPN
   unset _RESERVATION
   # generate queue script
   $SCRIPTDIR/qscript.pl < $qScriptRequest   \
                         > $qScriptResponse 2>> $SYSLOG
   if [[ $? != 0 ]]; then
      fatal "Failed to generate queue script."
   fi
   # extract queue script name from response
   qscript=$(bashJSON.pl --key "qScriptFileName"        \
                        < $qScriptResponse 2>> $SYSLOG)
   # extract queue script from response
   bashJSON.pl --key "script" < $qScriptResponse 2>> $SYSLOG \
                              | base64 -d                    \
                              > $qscript 2>> $SYSLOG
   # check to make sure the file is there
   if [[ ! -e $qscript ]]; then
      fatal "Failed to extract queue script $qscript from $qScriptResponse."
   fi
   # update the run.properties file
   echo "hpc.job.$JOBTYPE.file.qscript : $qscript" >> run.properties
   #
   case $QUEUESYS in
   "SLURM" | "PBS" | "SGE" )
      queuesyslc=$(echo $QUEUESYS | tr '[:upper:]' '[:lower:]')
      # submit adcprep job, check to make sure queue script submission
      # succeeded, and if not, retry
      local jobSubmitInterval=60
      while [ true ];  do
         echo "time.hpc.job.${JOBTYPE}.submit : $(date +'%Y-%h-%d-T%H:%M:%S%z')" >> run.properties
         # submit job , capture stdout from sbatch and direct it
         # to scenario.log; capture stderr and send to all logs
         $SUBMITSTRING ${JOBTYPE}.${queuesyslc} 2>>$SYSLOG >jobID
         if [[ $? == 0 ]]; then
            ${SCRIPTDIR}/monitoring/captureJobID.sh $HPCENVSHORT
            echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : \"$(date +'%Y-%h-%d-T%H:%M:%S%z')\", \"jobid\" : \"$(<jobID)\", \"start\" : null, \"finish\" : null, \"error\" : null" >> ${ADVISDIR}/${ENSTORM}/jobs.status
            break # job submission command returned a "success" status
         else
            awk -v this='asgs_main.sh>prep' -v level=ERROR -f $SCRIPTDIR/monitoring/timestamp.awk jobErr | tee -a ${SYSLOG} | tee -a $CYCLELOG | tee -a scenario.log
            logMessage "$ENSTORM: $THIS: $SUBMITSTRING ${JOBTYPE}.${queuesyslc} failed; will retry in '$jobSubmitInterval' seconds."
            consoleMessage "$W Submission of ${JOBTYPE}.${queuesyslc} failed. Waiting to retry."
            echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : \"$(date +'%Y-%h-%d-T%H:%M:%S%z')\", \"jobid\" : null, \"start\" : null, \"finish\" : null, \"error\" : null, \"error.message\" : \"$(<jobErr)\"" >> ${ADVISDIR}/${ENSTORM}/jobs.status
            spinner $jobSubmitInterval

         fi
      done
      monitorJobs $QUEUESYS ${JOBTYPE} ${ENSTORM} $WALLTIME
      THIS="asgs_main.sh>prepFile()"
      logMessage "$ENSTORM: $THIS: Finished adcprepping file ($JOBTYPE)."
      ;;
   *)
      echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : \"$(date +'%Y-%h-%d-T%H:%M:%S%z')\", \"jobid\" : null, \"start\" : \"$(date +'%Y-%h-%d-T%H:%M:%S%z')\", \"finish\" : null, \"error\" : null" >> ${ADVISDIR}/${ENSTORM}/jobs.status
      # make the queue script executable and execute it
      chmod +x ./$qscript >> $ADVISDIR/$ENSTORM/scenario.log 2>&1
      ./$qscript >> $ADVISDIR/$ENSTORM/scenario.log 2>&1
      ;;
   esac
}
#
# subroutine that calls an external script over and over until it
# pulls down a new advisory from the NHC (then it returns)
#
# Also contains code to pull advisories or track file data from the
# local filesystem.
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
    STATEFILE=${12}
    #
    THIS="asgs_main.sh>downloadCycloneData()"
    APPLOGFILE=$RUNDIR/get_atcf.log
#    activity_indicator "Checking remote site for new advisory..." &
    logMessage "$THIS: Checking remote site for new advisory..." $APPLOGFILE
#    pid=$!; trap "stop_activity_indicator ${pid}; exit" EXIT
    cd $RUNDIR 2>> ${SYSLOG}
    local cycloneDataCheckInterval=60 # seconds
    newAdvisory=false
    newAdvisoryNum=null
    forecastFileName=al${STORM}${YEAR}.fst
    hindcastFileName=bal${STORM}${YEAR}.dat
    # check to see if we have a leftover forecast.properties file from
    # a previous advisory laying around here in our run directory, and if
    # so, delete it
    if [[ -e forecast.properties ]]; then
       rm forecast.properties 2>> ${SYSLOG}
    fi
    OPTIONS="--storm $STORM --year $YEAR --ftpsite $FTPSITE --fdir $FDIR --hdir $HDIR --rsssite $RSSSITE --trigger $TRIGGER --adv $ADVISORY"
    logMessage "$THIS: Options for $GET_ATCF_SCRIPT are as follows : $OPTIONS" $APPLOGFILE
    if [ "$START" = coldstart ]; then
       logMessage "$THIS: Downloading initial hindcast/forecast."
    else
       logMessage "$THIS: Checking remote site for new advisory..."
    fi

    while [ $newAdvisory = false ]; do
       if [[ $TRIGGER != "atcf" ]]; then
          appMessage "perl $GET_ATCF_SCRIPT $OPTIONS"  $APPLOGFILE
          newAdvisoryNum=$($GET_ATCF_SCRIPT $OPTIONS 2>> $SYSLOG)
       fi
       # check to see if we have a new one, and if so, determine the
       # new advisory number correctly
       case $TRIGGER in
       "atcf")
          # if the forecast is already in ATCF format, then simply copy it
          # to the run directory
          cp $HDIR/$hindcastFileName . 2>> ${SYSLOG}
          cp $FDIR/$forecastFileName . 2>> ${SYSLOG}
          linkTarget=`readlink $FDIR/$forecastFileName`
          # assume the advisory number is the first two characters in the
          # symbolic link target of the forecast file name
          newAdvisoryNum=${linkTarget:0:2}
          if [[ $newAdvisoryNum -gt $ADVISORY ]]; then
             newAdvisory="true"
          else
             newAdvisory="false"
          fi
          ;;
       "ftp")
          if [ $START = hotstart ]; then
             if ! diff $OLDADVISDIR/$forecastFileName ./$forecastFileName > /dev/null 2>> ${SYSLOG}; then
                # forecasts from NHC ftp site do not have advisory number
                newAdvisoryNum=`printf "%02d" $[$ADVISORY + 1]`
                newAdvisory="true"
             fi
          fi
          ;;
       "rss" | "rssembedded" | "auto" )
          # if there was a new advisory, the $GET_ATCF_SCRIPT script (default is get_atcf.pl)
          # would have returned the advisory number in stdout
          if [[ ! -z $newAdvisoryNum && $newAdvisoryNum != null ]]; then
             newAdvisory="true"
             if [ -e $forecastFileName ]; then
                mv $forecastFileName $forecastFileName.ftp 2>> $SYSLOG
             fi
          fi
          ;;
       *)
          fatal "$THIS: Invalid 'TRIGGER' type: '$TRIGGER'; must be ftp, rss, rssembedded, auto, or atcf."
          ;;
       esac
       if [ $START = coldstart ]; then
          if [ $TRIGGER = ftp ]; then
             newAdvisoryNum=$ADVISORY
          fi
          newAdvisory="true"
       fi
       if [[ $newAdvisory = false ]]; then
          printf "."  # progress bar
          spinner $cycloneDataCheckInterval
          printf "\b.."
       fi
    done
    logMessage "$THIS: New forecast detected." $APPLOGFILE
    cp -f $STATEFILE ${STATEFILE}.old 2>> ${SYSLOG}
    sed 's/ADVISORY=.*/ADVISORY='$newAdvisoryNum'/' $STATEFILE > ${STATEFILE}.new 2>> ${SYSLOG} 2>&1
    logMessage "$ENSTORM: $THIS: The new advisory number is ${newAdvisoryNum}." $APPLOGFILE
    cp -f ${STATEFILE}.new $STATEFILE 2>> ${SYSLOG}
    if [[ $TRIGGER = rss || $TRIGGER = rssembedded ]]; then
       perl ${SCRIPTDIR}/nhc_advisory_bot.pl --input ${forecastFileName}.html --output $forecastFileName --metadata forecast.properties >> ${SYSLOG} 2>&1
    fi
    if [[ $FTPSITE = filesystem ]]; then
       cp $HDIR/$hindcastFileName $hindcastFileName 2>> ${SYSLOG}
    fi
}
#
# subroutine that polls an external ftp site for background meteorology data
# and writes metadata to document the current state
source $SCRIPTDIR/downloadBackgroundMet.sh
#
# subroutine that polls an external ftp site for GFS data,
# subsets and downloads grib2 files with curl, and reprojects
# to latlon grid with wgrib2
source $SCRIPTDIR/downloadGFS.sh
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
   RIVERUSER=$5
   RIVERDATAPROTOCOL=$6
   ENSTORM=$7
   CSDATE=$8
   HSTIME=$9
   SCRIPTDIR=${10}
   DEFAULTFILE=${11}
   USERIVERFILEONLY=${12}

   THIS="asgs_main.sh>downloadRiverFluxData()"
   OPTIONS="--advisdir $ADVISDIR --meshfile $MESHFILE --riversite $RIVERSITE --riverdir $RIVERDIR --riveruser $RIVERUSER --riverdataprotocol $RIVERDATAPROTOCOL --enstorm $ENSTORM --csdate $CSDATE --hstime $HSTIME --scriptdir $SCRIPTDIR --defaultfile $DEFAULTFILE"
   TRIES=0
   SUCCESS=no
   if [[ $USERIVERFILEONLY = no ]]; then
      while [[ $TRIES -lt 2 ]]; do
         perl ${SCRIPTDIR}/get_flux.pl $OPTIONS 2>> ${SYSLOG}
         if [[ $? = 0 ]]; then
            logMessage "$ENSTORM: $THIS: Completed construction of river flux boundary condition (fort.20 file)."
            SUCCESS=yes
            break
         else
            TRIES=$[$TRIES + 1]
            logMessage "$ENSTORM: $THIS: Attempt $TRIES at constructing river flux boundary condition (fort.20) file has failed. After 2 attempts, the default flux boundary condition file '$DEFAULTFILE' will be used."
            consoleMessage "$W Constructing river flux boundary condition (fort.20) file has failed."
            spinner 60
         fi
      done
   fi
   if [[ $SUCCESS = no ]]; then
      error "$ENSTORM: $THIS: Using default river flux boundary condition file '$DEFAULTFILE'."
      ln -s $DEFAULTFILE ./fort.20 2>> ${SYSLOG}
   fi
}
#
# See if a task has been running (or waiting) longer than a specified time limit
checkTimeLimit()
{
   THIS="asgs_main.sh>checkTimeLimit()"
   if [[ -z "$1" || -z "$2" ]]; then
	   warn "$ENSTORM: $THIS: One or both parameters for checkTimeLimit() is empty. STARTTIME='$1', TIMELIMIT='$2'."
     return 0
   fi
   STARTTIME=$1
   TIMELIMIT=$2
   #
   # convert time limit to seconds, assuming it is in the format HH:MM:SS
   hours=${TIMELIMIT:0:2}   # requires leading zero! e.g., 05:00:00
   minutes=${TIMELIMIT:3:2}
   seconds=${TIMELIMIT:6:2}
   # bash interprets numbers with leading zeroes as octal ... the 10# prefix
   # tells bash that the numbers are base 10
   limit=$((10#$hours * 3600 + 10#$minutes * 60 + 10#$seconds)) # in seconds
   endTime=$(date +%s)
   runTime=$(($endTime - $STARTTIME))
   if [[ $runTime -gt $limit ]]; then
      hoursEnd=$(($limit / 3600))
      remainder=$(($limit % 3600))
      minutesEnd=$(($remainder / 60))
      secondsEnd=$(($remainder % 60))
      format="%02d:%02d:%02d"
      hms=$(printf "$format" $hoursEnd $minutesEnd $secondsEnd)
      warn "$ENSTORM: $THIS: The time limit is $TIMELIMIT but the total time used so far is $hms. Therefore, the time limit has been exceeded."
      return 1
   else
      return 0
   fi
}
#
# watches for the existence of certain files that are written by the job as
# it executes and proceeds according to the status that is indicated by
# those files
monitorJobs()
{  QUEUESYS=$1
   JOBTYPE=$2
   ENSTORM=$3
   WALLTIME=$4
   #
   ENSTORM_TEMP=${JOBTYPE}.${ENSTORM}
   THIS="asgs_main.sh>monitorJobs()"
   jobCheckIntervalSeconds=60
   #
   logMessage "$ENSTORM_TEMP: $THIS: Waiting for $ENSTORM_TEMP job to start."
   until [[ -e ${ENSTORM_TEMP}.run.start ]]; do
      writeScenarioFilesStatus
      if [[ $enablePostStatus == "yes" ]]; then
         postScenarioStatus
      fi
      sleep $jobCheckIntervalSeconds
   done
   logMessage "$ENSTORM_TEMP: $THIS: The $ENSTORM_TEMP job has started."
   startTime=$(date +%s)  # epoch seconds
   retries=0  # count resubmits on hatteras due to io errors
   #
   # Keep checking every $jobCheckIntervalSeconds to see if the job has
   # (a) timed out; (b) written a .finish file; or (c) written a .error file.
   # Job status is monitored via these files, which are actually
   # written by the queue script on HPC systems that use SLURM or PBS (i.e.,
   # basically all of them). One consequence of this is that if the
   # job simply disappears (e.g. is cancelled by the Operator or the
   # sysadmins), the ASGS won't notice until the wall clock time ends.
   # This behavior is actually useful for real time tweaks and fixes
   # because the Operator can cancel a job, make modifications, and then
   # resubmit it without the ASGS noticing or being disturbed.
   #
   while [[ 1 ]]; do
      writeScenarioFilesStatus
      if [[ $enablePostStatus == "yes" ]]; then
         postScenarioStatus
      fi

      # check job run status
      check=$(checkTimeLimit "$startTime" "$WALLTIME")
      if [[ "$check" -eq 1 ]]; then
         THIS="asgs_main.sh>monitorJobs()"
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
         echo "[$DATETIME] $THIS: The ${ENSTORM_TEMP} job exceeded its wall clock time limit of '$WALLTIME'." > ${ENSTORM_TEMP}.run.error  # <-OVERWRITE
         # if this job was submitted by mpiexec, then terminate it; otherwise,
         # it could run for a long time, delaying the continued execution
         # of the ASGS (the ASGS won't start the next cycle until all forecast
         # scenarios in the current cycle have completed); this also
         # prevents cpus from being tied up unnecessarily ...
         # if the job was submitted through a queueing system, then the
         # queueing system will terminate it
         case $QUEUESYS in
         "mpiexec")
            logMessage "$ENSTORM: $THIS: Detecting mpiexec subshell pid."
            local pid=`grep 'mpiexec subshell pid' ${ADVISDIR}/${ENSTORM}/run.properties | sed 's/mpiexec subshell pid.*://' | sed 's/^\s//'`
            #logMessage "Terminating the $ENSTORM_TEMP job with the command 'kill -TERM `ps --ppid $pid -o pid --no-headers'."
            # need to kill the mpiexec process, but don't know its process ID
            # ... but we do have the process ID of its parent subshell
            kill -TERM `ps --ppid $pid -o pid --no-headers` >> ${SYSLOG} 2>&1
            logMessage "$THIS: $ENSTORM_TEMP job in $PWD terminated by ASGS for exceeding expected wall clock time." >> ${ENSTORM_TEMP}.run.error
            echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : null, \"jobid\" : \"$pid\", \"start\" : null, \"finish\" : null, \"error\" : \"$DATETIME\"" >> jobs.status
            ;;
         "serial")
            local pid=`grep 'serial $JOBTYPE job subshell pid' ${ADVISDIR}/${ENSTORM}/run.properties | sed 's/serial $JOBTYPE job subshell pid.*://' | sed 's/^\s//'`
            # need to kill the serial process, but don't know its process ID
            # ... but we do have the process ID of its parent subshell
            kill -TERM `ps --ppid $pid -o pid --no-headers` >> ${SYSLOG} 2>&1
            logMessage "$THIS: $ENSTORM_TEMP job in $PWD terminated by ASGS for exceeding expected wall clock time." >> ${ENSTORM_TEMP}.run.error
            echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : null, \"jobid\" : \"$pid\", \"start\" : null, \"finish\" : null, \"error\" : \"$DATETIME\"" >> jobs.status
            ;;
         *)
            # if we are over the wall clock limit, wait until the operating
            # system has had a chance to write the job log file, or
            # until 5 minutes have passed
            echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : null, \"jobid\" : null, \"start\" : null, \"finish\" : null, \"error\" : \"$DATETIME\"" >> jobs.status
            overLimitTime=$(date +%s)
            until [[ -e ${ENSTORM_TEMP}.out ]]; do
               logMessage "$ENSTORM_TEMP: $THIS: Waiting for queueing system to write out the job log file ${ENSTORM_TEMP}.out."
               sleep 60
               nowTime=$(date +%s)
               if [[ $(($nowTime - $overLimitTime)) -gt 300 ]]; then
                  warn "$ENSTORM_TEMP: $THIS: After 5 minutes, the ${ENSTORM_TEMP}.out file did not appear. Proceeding with error recovery."
                  break
               fi
            done
            ;;
         esac
      fi
      if [[ -e ${ENSTORM_TEMP}.run.error ]]; then
         if [[ $QUEUESYS = "SLURM" ]]; then
            # check to see if there was an i/o error reading the hotstart  file,
            # and if so, resubmit the job
            $SCRIPTDIR/monitoring/hatteras/hatteras_io_error_detector.pl --outfile ${ENSTORM_TEMP}.out >> $SYSLOG 2>&1
            if [[ -e netcdf_io.error ]]; then
               retries=$(($retries + 1))
               logMessage "$ENSTORM_TEMP: $THIS: There was an i/o error reading the hotstart file. Resubmitting job."
               retriesStr=`printf "%02d" $retries`
               # store ends of subdomain logs
               tail PE*/fort.16 > fort16.log 2>> $SYSLOG 2>&1
               for file in `ls *.error *.out *.log`; do
                  mv $file ${file}.${retriesStr} 2>> $SYSLOG 2>&1
               done
               for jobtype in padcirc padcswan; do
                  if [[ -e ${jobtype}.slurm ]]; then
                     sbatch ${jobtype}.slurm >jobID 2>jobErr | tee -a scenario.log
                     DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
                     echo "\"jobtype\" : \"$jobtype\", \"submit\" : \"$DATETIME\", \"jobid\" : \"$(<jobID)\", \"start\" : null, \"finish\" : null, \"error\" : null" >> jobs.status
                     break
                  fi
               done
               continue
            else
               # there was an error under a SLURM system, but not
               # an error we can get around by resubmitting
               error "$ENSTORM_TEMP: $THIS: The $ENSTORM_TEMP run failed; results are not available for this scenario for this advisory."
               cat ${ENSTORM_TEMP}.run.error >> jobFailed
               break
            fi
         else
            # there was an error under a non-SLURM system
            error "$ENSTORM_TEMP: $THIS: The $ENSTORM_TEMP run failed; results are not available for this scenario for this advisory."
            cat ${ENSTORM_TEMP}.run.error >> jobFailed
            break
         fi
      fi
      if [[ -e ${ENSTORM_TEMP}.run.finish ]]; then
         logMessage "$ENSTORM_TEMP: $THIS: The $ENSTORM_TEMP job appears to have run to completion successfully."
         break
      fi
      sleep $jobCheckIntervalSeconds
   done
   if [[ -e ${ENSTORM_TEMP}.run.error ]]; then
      error "$ENSTORM_TEMP: $THIS: The $ENSTORM_TEMP run failed; results are not available for this scenario for this advisory."
      cat ${ENSTORM_TEMP}.run.error >> jobFailed
   fi
   if [[ -e ${ENSTORM_TEMP}.run.finish ]]; then
      logMessage "$ENSTORM_TEMP: $THIS: The $ENSTORM_TEMP job appears to have run to completion successfully."
   fi
   #
   # terminate redirect processes for centralized logging
   writeScenarioFilesStatus  # final status update for files
   if [[ $enablePostStatus == "yes" ]]; then
      postScenarioStatus
   fi
   sleep 30 # give buffers a chance to flush to the filesystem
   #
   # final messages
   logMessage "$ENSTORM_TEMP: $THIS: Finished monitoring $ENSTORM_TEMP job."
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
   HPCENVSHORT=$8
   ACCOUNT=$9
   PPN=${10}
   NUMWRITERS=${11}
   HOTSTARTCOMP=${12}
   WALLTIME=${13}
   JOBTYPE=${14}
   #
   consoleMessage "$I Submitting $ENSTORM $JOBTYPE job."
   THIS="asgs_main.sh>submitJob()"
   STORMDIR=${ADVISDIR}/${ENSTORM}
   #
   CLOPTIONS=""     # command line options
   LOCALHOTSTART=""
   if [[ $NUMWRITERS != "0" ]]; then
      CLOPTIONS="-W $NUMWRITERS"
   fi
   # record the number of requested CPUs for use in determining capacity to run another job
   if [[ $HOTSTARTCOMP = subdomain ]]; then
      CLOPTIONS="${CLOPTIONS} -S -R"
      LOCALHOTSTART="--localhotstart"
   fi
   echo "hpc.job.${JOBTYPE}.cloptions : \"$CLOPTIONS\"" >> $ADVISDIR/$ENSTORM/run.properties
   echo "hpc.job.${JOBTYPE}.localhotstart : $LOCALHOTSTART" >> $ADVISDIR/$ENSTORM/run.properties
   _CPUREQUEST=$(($NCPU + $NUMWRITERS))
   _QUEUENAME=$(HPC_Queue_Hint "$QUEUENAME" "$HPCENV" "$QOS" "$CPUREQUEST")
   _PPN=$(HPC_PPN_Hint "parallel" "$QUEUENAME" "$HPCENV" "$QOS" "$PPN" "$CPUREQUEST")
   _RESERVATION=$(HPC_Reservation_Hint "$RESERVATION" "$HPCENV" "$QOS" "$CPUREQUEST")
   #
   # build queue script
   qScriptRequestTemplate=$SCRIPTDIR/qscript_request_template.json
   qScriptRequest=$SCENARIODIR/qscript_request_$JOBTYPE.json
   qScriptResponse=$SCENARIODIR/qscript_response_$JOBTYPE.json
   QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
   if [[ $QUEUESYS == "serial" ]]; then
      parallelism=serial
   else
      parallelism=parallel
   fi
   if [[ $QUEUESYS == "mpiexec" ]]; then
      JOBLAUNCHER="mpiexec -n %totalcpu%"
   fi
   wind10mlayer=$createWind10mLayer
   if [[ $NWS == "0" ]]; then
      wind10mlayer="no"
   fi
   #
   # create queue script request by filling in template
   # with data needed to create queue script
   sed \
      -e "s/%jobtype%/$JOBTYPE/" \
      -e "s?%qscripttemplate%?$QSCRIPTTEMPLATE?" \
      -e "s/%parallelism%/$parallelism/" \
      -e "s/%ncpu%/$NCPU/" \
      -e "s/%forncpu%/$NCPU/" \
      -e "s/%numwriters%/$NUMWRITERS/" \
      -e "s/%joblauncher%/$JOBLAUNCHER/" \
      -e "s/%walltime%/$WALLTIME/" \
      -e "s/%walltimeformat%/$WALLTIMEFORMAT/" \
      -e "s/%ppn%/${_PPN}/" \
      -e "s/%queuename%/${_QUEUENAME}/" \
      -e "s/%serqueue%/$SERQUEUE/" \
      -e "s/%account%/$ACCOUNT/" \
      -e "s?%advisdir%?$ADVISDIR?" \
      -e "s?%scriptdir%?$SCRIPTDIR?" \
      -e "s?%adcircdir%?$ADCIRCDIR?" \
      -e "s/%wind10mlayer%/$wind10mlayer/" \
      -e "s/%scenario%/$SCENARIO/" \
      -e "s/%reservation%/${_RESERVATION}/" \
      -e "s/%constraint%/$CONSTRAINT/" \
      -e "s/%qos%/$QOS/" \
      -e "s?%syslog%?$SYSLOG?" \
      -e "s?%scenariolog%?$SCENARIOLOG?" \
      -e "s/%hotstartcomp%/$HOTSTARTCOMP/" \
      -e "s/%queuesys%/$QUEUESYS/" \
      -e "s/%hpcenvshort%/$HPCENVSHORT/" \
      -e "s/%asgsadmin%/$ASGSADMIN/" \
      -e "s/%NULLLASTUPDATER%/$THIS/" \
      -e "s/%NULLLASTUPDATETIME%/$(date +'%Y-%h-%d-T%H:%M:%S%z')/" \
      < $qScriptRequestTemplate \
      > $qScriptRequest \
    2>> $SYSLOG
   unset _CPUREQUEST
   unset _PPN
   unset _RESERVATION
   unset _QUEUENAME
   # generate queue script
   $SCRIPTDIR/qscript.pl < $qScriptRequest   \
                         > $qScriptResponse 2>> $SYSLOG
   if [[ $? != 0 ]]; then
      fatal "Failed to generate queue script."
   fi
   # extract queue script name from response
   qscript=$(bashJSON.pl --key "qScriptFileName"        \
                        < $qScriptResponse 2>> $SYSLOG)
   # extract queue script from response
   bashJSON.pl --key "script" < $qScriptResponse 2>> $SYSLOG \
                              | base64 -d                    \
                              > $qscript 2>> $SYSLOG
   # check to make sure the file is there
   if [[ ! -e $qscript ]]; then
      fatal "Failed to extract queue script $qscript from $qScriptResponse."
   fi
   # update the run.properties file
   echo "hpc.job.$JOBTYPE.file.qscript : $qscript" >> run.properties
   #
   # start the job in a queueing system-dependent way
   case $QUEUESYS in
   #
   #  No queueing system, just run adcirc or adcswan (used on standalone computers or cloud)
   "serial")
      if [[ -e "$ADCIRCDIR/../adcirc.bin.buildinfo.json" && ! -e "adcirc.bin.buildinfo.json" ]]; then
         cp "$ADCIRCDIR/../adcirc.bin.buildinfo.json" . 2>> $SYSLOG
         echo "adcirc.file.metadata.build : adcirc.bin.buildinfo.json" >> run.properties
      fi
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'%z`
      echo "time.${JOBTYPE}.start : $DATETIME" >> run.properties
      logMessage "$ENSTORM: $THIS: Submitting ${JOBTYPE}.${ENSTORM} job in $PWD via $ADCIRCDIR/$JOBTYPE $CLOPTIONS >> ${SYSLOG} 2>&1"
      # submit the serial job in a subshell
      (
         $ADCIRCDIR/$JOBTYPE $CLOPTIONS >> ${ADVISDIR}/${ENSTORM}/serial-adcirc.log 2>&1
         ERROVALUE=$?
         RUNSUFFIX="finish"
         DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
         if [ $ERROVALUE != 0 ] ; then
            RUNSUFFIX="error"
            echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : \"$DATETIME\", \"jobid\" : \"$PPID\", \"start\" : null, \"finish\" : null, \"error\" : \"$DATETIME\"" >> ${ADVISDIR}/${ENSTORM}/jobs.status
         else
            echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : \"$DATETIME\", \"jobid\" : \"$PPID\", \"start\" : null, \"finish\" : \"$DATETIME\", \"error\" : null" >> ${ADVISDIR}/${ENSTORM}/jobs.status
         fi
         echo "\"$RUNSUFFIX\" : \"$DATETIME\", \"jobid\" : \"$PPID\"" > ${ADVISDIR}/${ENSTORM}/${JOBTYPE}.${ENSTORM}.run.$RUNSUFFIX #<-OVERWRITE
         echo "time.${JOBTYPE}.${RUNSUFFIX} : $DATETIME" >> run.properties
         # terminate redirect processes for centralized logging
         consoleMessage "$I Job has completed, waiting for job i/o to finalize."
         sleep 30 # give buffers a chance to flush to the filesystem
      ) &
      local pid=$!
      spinner 0 $pid
      # write the process id to the run.properties file so that monitorJobs()
      # can kill the job if it exceeds the expected wall clock time
      local subshellPID=$!
      echo "serial $JOBTYPE job subshell pid : $subshellPID" >> ${ADVISDIR}/${ENSTORM}/run.properties 2>> ${SYSLOG}
      echo "\"start\" : \"$DATETIME\", \"jobid\" : \"$subshellPID\"" > ${ADVISDIR}/${ENSTORM}/${JOBTYPE}.${ENSTORM}.run.start #<-OVERWRITE
      echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : \"$DATETIME\", \"jobid\" : \"$subshellPID\", \"start\" : \"$DATETIME\", \"finish\" : null, \"error\" : null" >> ${ADVISDIR}/${ENSTORM}/jobs.status
      ;;
   #
   "SLURM" | "PBS" )
      queuesyslc=$(echo $QUEUESYS | tr '[:upper:]' '[:lower:]')
      logMessage "$ENSTORM: $THIS: Submitting $ADVISDIR/$ENSTORM/${JOBTYPE}.${queuesyslc}."
      # initialize log files so they can be centralized
      local jobSubmitInterval=60
      #
      # submit job, check to make sure qsub succeeded, and if not, retry (forever)
      while [ true ];  do
         DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
         echo "time.hpc.job.${JOBTYPE}.submit : $DATETIME" >> ${STORMDIR}/run.properties
         $SUBMITSTRING ${JOBTYPE}.${queuesyslc} 2>>$SYSLOG >jobID
         if [[ $? == 0 ]]; then
            ${SCRIPTDIR}/monitoring/captureJobID.sh $HPCENVSHORT
            echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : \"$DATETIME\", \"jobid\" : \"$(<jobID)\", \"start\" : null, \"finish\" : null, \"error\" : null" >> ${ADVISDIR}/${ENSTORM}/jobs.status
            break # job submission command returned a "success" status
         else
            logMessage "$ENSTORM: $THIS: $SUBMITSTRING $ADVISDIR/$ENSTORM/${JOBTYPE}.${queuesys} failed: $(<jobErr); ASGS will retry in 60 seconds."
            consoleMessage "$W ${JOBTYPE}.${queuesys} job submission failed. Waiting to retry."
            echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : \"$DATETIME\", \"jobid\" : null, \"start\" : null, \"finish\" : null, \"error\" : null, \"error.message\" : \"$(<jobErr)\"" >> ${ADVISDIR}/${ENSTORM}/jobs.status
            writeScenarioFilesStatus  # final status update for files
            if [[ $enablePostStatus == "yes" ]]; then
               postScenarioStatus
            fi
            spinner $jobSubmitInterval
         fi
      done
      writeScenarioFilesStatus  # final status update for files
      if [[ $enablePostStatus == "yes" ]]; then
         postScenarioStatus
      fi
      ;;
   #
   # No queueing system, just mpiexec (used on standalone computers
   # and small clusters)
   "mpiexec")
      DATETIME=
      echo "time.${JOBTYPE}.start : $(date +'%Y-%h-%d-T%H:%M:%S'%z)" >> run.properties
      CPUREQUEST=$(($NCPU + $NUMWRITERS))
      # submit the parallel job in a subshell
      echo "\"jobtype\" : \"$JOBTYPE\", \"submit\" : \"$(date +'%Y-%h-%d-T%H:%M:%S%z')\", \"jobid\" : null, \"start\" : \"$(date +'%Y-%h-%d-T%H:%M:%S%z')\", \"finish\" : null, \"error\" : null" >> ${ADVISDIR}/${ENSTORM}/jobs.status
      (
         # make the queue script executable and execute it
         chmod +x ./$qscript >> $ADVISDIR/$ENSTORM/scenario.log 2>&1
         ./$qscript >> $ADVISDIR/$ENSTORM/scenario.log 2>&1
         # terminate redirect processes for centralized logging
         sleep 3 # give buffers a chance to flush to the filesystem
      ) &
      local pid=$!
      spinner 0 $pid
      # write the process id for mpiexec to the run.properties file so that monitorJobs()
      # can kill the job if it exceeds the expected wall clock time
      echo "mpiexec subshell pid : $!" >> ${ADVISDIR}/${ENSTORM}/run.properties 2>> ${SYSLOG}
      ;;
   *)
      fatal "$ENSTORM: $THIS: Queueing system $QUEUESYS unrecognized."
      ;;
   esac
}
#
# checks to see if a job has failed, and if so, copies the whole
# scenario to another directory so it is out of the way and can be
# used in troubleshooting; it changes nothing inside the scenario direcory
handleFailedJob()
{
   RUNDIR=$1
   ADVISDIR=$2
   ENSTORM=$3
   NOTIFYSCRIPT=$4
   HPCENV=$5
   STORMNAME=$6
   YEAR=$7
   STORMDIR=$8
   ADVISORY=$9
   LASTADVISORYNUM=${10}
   STATEFILE=${11}
   GRIDFILE=${12}
   EMAILNOTIFY=${13}
   JOB_FAILED_LIST="${14}"
   ARCHIVEBASE=${15}
   ARCHIVEDIR=${16}
   THIS="asgs_main.sh>handleFailedJob()"
   debugMessage "$ENSTORM: $THIS: handleFailedJob called with the following arguments: RUNDIR=$RUNDIR ADVISDIR=$ADVISDIR ENSTORM=$ENSTORM NOTIFYSCRIPT=${OUTPUTDIR}/${NOTIFY_SCRIPT} HPCENV=$HPCENV STORMNAME=$STORMNAME YEAR=$YEAR STORMDIR=$STORMDIR ADVISORY=$ADVISORY LASTADVISORYNUM=$LASTADVISORYNUM STATEFILE=$STATEFILE GRIDFILE=$GRIDFILE EMAILNOTIFY=$EMAILNOTIFY JOBFAILEDLIST=${JOB_FAILED_LIST} ARCHIVEBASE=$ARCHIVEBASE ARCHIVEDIR=$ARCHIVEDIR"
   # check to see that the job did not conspicuously fail
   if [[ -e $ADVISDIR/${ENSTORM}/jobFailed ]]; then
      warn "$ENSTORM: $THIS: The job has failed."
      FAILDATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
      # send an email to notify the operator that a job has failed
      $NOTIFYSCRIPT $HPCENV $STORM $YEAR $STORMDIR $ADVISORY $ENSTORM $GRIDFILE jobfailed $EMAILNOTIFY $SYSLOG "${JOB_FAILED_LIST}" $ARCHIVEBASE $ARCHIVEDIR
      warn "$ENSTORM: $THIS: Moving failed cycle to 'failed.${FAILDATETIME}'."
      mv $ADVISDIR/$ENSTORM $RUNDIR/failed.${FAILDATETIME} 2>> ${SYSLOG}
      # roll back the latest advisory number if the nowcast failed
      if [[ $ENSTORM = nowcast ]]; then
         logMessage "Rolling back the advisory number in the state file $STATEFILE due to failed nowcast."
         sed 's/ADVISORY=.*/ADVISORY='$LASTADVISORYNUM'/' $STATEFILE > ${STATEFILE}.new 2>> ${SYSLOG}
         mv -f ${STATEFILE}.new $STATEFILE >> ${SYSLOG} 2>&1
      fi
   fi
}
source $SCRIPTDIR/monitoring/logging.sh
source $SCRIPTDIR/platforms.sh            # this includes source $SCRIPTDIR/monitoring/logging.sh
source $SCRIPTDIR/properties.sh           # read properties file into a hash
source $SCRIPTDIR/variables_init.sh
source $SCRIPTDIR/writeProperties.sh
source $SCRIPTDIR/manageHooks.sh          # depends on monitoring/logging.sh
source $SCRIPTDIR/generateDynamicInput.sh # generates tide_fac.out, fort.13, fort.15, fort.26

#####################################################################
#                 E N D  F U N C T I O N S
#####################################################################
#
#####################################################################
#               B E G I N     E X E C U T I O N
#####################################################################

SCENARIO="null"
#
# Option Summary
#
# -c : set location of configuration file"
# -e (environment): set the computer that the ASGS is running on"
# -h : show help"
#
# Example:
#   bash asgs_main.sh -c /path/to/config -r /path/to/rundir -e topsail
#
# mail alert
operator=$USER
#ASGSADMIN Operators should set this value in their ~/.asgsh_profile files on each platform
#
# exit statuses
EXIT_NOT_OK=1
EXIT_OK=0
#
si=-2  # storm index for forecast scenario; -1 indicates nowcast, -2 hindcast
# need to determine standard time format to be used for pasting log files
STARTDATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`

# set the value of SCRIPTDIR
SCRIPTDIR=${0%%/asgs_main.sh}  # ASGS scripts/executables

variables_init           # initialize variables
initFileStatusMonitoring # initialize variables
#
# create directories with default permissions of "775" and
# files with the default permssion of "664"
umask 002
#
while getopts "c:e:s:h" optname; do
   case $optname in
      c) CONFIG=${OPTARG}
         if [[ ! -e $CONFIG ]]; then
            echo "ERROR: $CONFIG does not exist."
            exit $EXIT_NOT_OK
         fi
         ;;
      e) HPCENVSHORT=${OPTARG}
         ;;
      s) STATEFILE=${OPTARG}
         ONESHOT=yes
         ;;
      h) echoHelp
         ;;
   esac
done
#
# determine hpc environment via function from platforms.sh
if [[ $HPCENVSHORT = "null" ]]; then
   set_hpc
fi
#
readConfig # now we have the instancename and can name the asgs log file after it
setSyslogFileName     # set the value of SYSLOG in monitoring/logging.sh
nullifyHooks          # in manageHooks.sh
#
consoleMessage "$I START_INIT $GRIDNAME $HPCENVSHORT"
executeHookScripts "START_INIT"
#
# set a trap for a signal to reread the ASGS config file
trap 'echo Received SIGUSR1. Re-reading ASGS configuration file. ; readConfig' USR1
# catch ^C for a final message?
trap 'sigint' INT
trap 'sigterm' TERM
trap 'sigexit' EXIT
#
# set the file and directory permissions, which are platform dependent
umask $UMASK
#
RUNDIR=$SCRATCHDIR/asgs$$
#
# save the value from of LASTSUBDIR from config in case
# LASTSUBDIR=null in STATEFILE due to previous failed
# initialization
ORIGLASTSUBDIR=$LASTSUBDIR
# if we are starting from cron, look for a state file
if [[ $ONESHOT = yes ]]; then
   # if it is there, read it
   if [[ -e $STATEFILE ]]; then
      logMessage "$THIS: Reading $STATEFILE for previous ASGS state."
      consoleMessage "$I Reading $STATEFILE for previous ASGS state."
      source $STATEFILE # contains RUNDIR, LASTSUBDIR, ADVISORY and SYSLOG values
      if [[ $LASTSUBDIR = null ]]; then
         HOTORCOLD=coldstart
         LASTSUBDIR=$ORIGLASTSUBDIR
      else
         HOTORCOLD=hotstart
      fi
   else
      # if the state file is not there, just start from cold
      consoleMessage "$I '$STATEFILE' was not found. Creating a new statefile."
      logMessage "$THIS: The statefile '$STATEFILE' was not found. The ASGS will start cold and create a new statefile."
      HOTORCOLD=coldstart
   fi
else
   # if we are not starting from cron, use the default statefile name,
   # and load it if it is there; if it is not there, just go by the
   # info in the config file
   STATEFILE=${SCRATCHDIR}/${INSTANCENAME}.state
   if [[ -e $STATEFILE ]]; then
      logMessage "$THIS: Reading $STATEFILE for previous ASGS state."
      consoleMessage "$I Reading '$STATEFILE'."
      source $STATEFILE # contains RUNDIR, LASTSUBDIR, ADVISORY and SYSLOG values
      if [[ $LASTSUBDIR = null ]]; then
         HOTORCOLD=coldstart
         LASTSUBDIR=$ORIGLASTSUBDIR
      else
         HOTORCOLD=hotstart
      fi
   else
      # this is an ongoing execution, and the statefile does not
      # exist yet, so create it now using info straight from the
      # ASGS config file
      echo RUNDIR=${RUNDIR} > $STATEFILE 2>> ${SYSLOG}
      echo SCRIPTDIR=${SCRIPTDIR} >> $STATEFILE 2>> ${SYSLOG}
      echo LASTSUBDIR=${LASTSUBDIR} >> $STATEFILE 2>> ${SYSLOG}
      echo SYSLOG=${SYSLOG} >> $STATEFILE 2>> ${SYSLOG}
      echo ADVISORY=${ADVISORY} >> $STATEFILE 2>> ${SYSLOG}
   fi
fi
# used to store instantaneous status files
statusDir=$RUNDIR/status  # after reading STATEFILE so we have value of RUNDIR
# see if the storm directory already exists in the scratch space
for dir in $RUNDIR $statusDir ; do
   if [ ! -d $dir ]; then
      logMessage "$THIS: Making directory '$dir'."
      mkdir -p $dir 2>> $SYSLOG
   fi
done

logMessage "$THIS: The ADCIRC Surge/Spill Guidance System is activated."
logMessage "$THIS: Please see ASGS log file for detailed information regarding system progress."
logMessage "$THIS: ASGS Start Up MSG: [SYSLOG] The log file is ${SYSLOG}"
logMessage "$THIS: ASGS Start Up MSG: [PROCID] $$"
logMessage "$THIS: ASGS Start Up MSG: [SYSLOG] ${SYSLOG}"
logMessage "$THIS: Set permissions with the following umask: $UMASK."
logMessage "$THIS: Configured the ASGS for the ${HPCENV} platform."
logMessage "$THIS: Configured the ASGS according to the file ${CONFIG}."
logMessage "$THIS: ASGS state file is ${STATEFILE}."
#
consoleMessage "$I SYSLOG: '${SYSLOG}'"
consoleMessage "$I CONFIG: '${CONFIG}'"
consoleMessage "$I Verifying that required files and directories actually exist."
#
checkDirExistence $INPUTDIR "directory for input files"
checkDirExistence $OUTPUTDIR "directory for post processing scripts"
#
if [[ $QUEUESYS = serial ]]; then
   checkFileExistence $ADCIRCDIR "ADCIRC serial executable" adcirc
else
   checkFileExistence $ADCIRCDIR "ADCIRC preprocessing executable" adcprep
   checkFileExistence $ADCIRCDIR "ADCIRC parallel executable" padcirc
fi
checkFileExistence $ADCIRCDIR "hotstart time extraction executable" hstime
checkFileExistence "$SCRIPTDIR/tides" "tide_factor executable" tide_fac.x
if [[ $TROPICALCYCLONE = on ]]; then
   checkFileExistence $ADCIRCDIR "asymmetric metadata generation executable" aswip
fi
if [[ $TIDEFAC = on ]]; then
   checkFileExistence $SCRIPTDIR/tides "tidal nodal factors and equilibrium arguments executable" tide_fac.x
fi
if [[ $BACKGROUNDMET = on ]]; then
   checkFileExistence $SCRIPTDIR "NAM output reprojection executable (from lambert to geographic)" awip_lambert_interp.x
   checkFileExistence $SCRIPTDIR "NAM output reprojection with spatial extrapolation ramp executable (from lambert to geographic)" lambertInterpRamp.x
   checkFileExistence $SCRIPTDIR/bin "GRIB2 manipulation and extraction executable" wgrib2
fi

if [[ $WAVES = on ]]; then
   JOBTYPE=padcswan
   checkDirExistence $SWANDIR "SWAN executables directory (SWANDIR)"
   checkFileExistence $SCRIPTDIR/input/meshes/common/swan "SWAN initialization template file " swaninit.template
   checkFileExistence $SCRIPTDIR/input/meshes/common/swan "SWAN control template file" $SWANTEMPLATE
   if [[ $QUEUESYS = serial ]]; then
      JOBTYPE=adcswan
      checkFileExistence $ADCIRCDIR "ADCIRC+SWAN serial executable" adcswan
   else
      checkFileExistence $SWANDIR "SWAN fulldomain hotstart file decomposition executable " unhcat.exe
      checkFileExistence $ADCIRCDIR "ADCIRC+SWAN parallel executable" padcswan
   fi
else
   JOBTYPE=padcirc
   if [[ $QUEUESYS = serial ]]; then
      JOBTYPE=adcirc
   fi
fi
if [[ $VARFLUX = on || $VARFLUX = default ]]; then
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
if [[ ! -z $NAFILE && $NAFILE != null ]]; then
   checkFileExistence $INPUTDIR "ADCIRC nodal attributes (fort.13) file" $NAFILE
fi
# unit offset file if unit offset has been specified
if [[ $STATICOFFSET != "null" ]]; then
   checkFileExistence $INPUTDIR "ADCIRC static water level offset data file" $UNITOFFSETFILE
fi
# self attraction / earth load tide forcing
if [[ $selfAttractionEarthLoadTide != "notprovided" ]]; then
   checkFileExistence $INPUTDIR "ADCIRC self attracting earth load tide file" $selfAttractionEarthLoadTide
fi
#
#  O B T A I N   I N I T I A L   H O T S T A R T   F I L E
#  F R O M   F I L E S Y S T E M   O R   U R L
#
LUN=67
hotstartBase=fort.${LUN}
hotstartSuffix=.nc
hotstartPath=${LASTSUBDIR}/nowcast # only for reading from local filesystem
hotstartURL=null
if [[ $HOTORCOLD = hotstart ]]; then
   consoleMessage "$I Acquiring hotstart file."
   # check to see if the LASTSUBDIR is actually a URL
   urlCheck=$(expr match "$LASTSUBDIR" 'http')
   if [[ $urlCheck -eq 4 ]]; then
      # always look for fort.68.nc from a URL because only a forecast
      # will be posted to a URL, and only the hotstart file that was used
      # to start the forecast will be posted ... asgs always hotstarts from
      # a fort.68 file and always writes a fort.67 file
      hotstartURL=$LASTSUBDIR
   else
      # we are reading the hotstart file from the local filesystem, determine
      # whether it is from a nowcast or hindcast
      if [[ -d $LASTSUBDIR/hindcast ]]; then
         hotstartPath=${LASTSUBDIR}/hindcast
      fi
   fi
   if [[ $HOTSTARTFORMAT = binary ]]; then
      # don't need the .nc suffix
      hotstartSuffix=
   fi
   # form the name of the hotstart file
   hotstartFile=${hotstartBase}${hotstartSuffix}
   # check existence and validity of hotstart file
   # FIXME: this does not download the SWAN hotstart file from the URL;
   # that would require the post processing of the forecast to include the
   # globalization of the SWAN hotstart file
   # FIXME: this also assumes that the hotstart format for this instance
   # is the same as the hotstart format that is being read
   if [[ $hotstartURL != "null" ]]; then
      consoleMessage "$I Downloading hotstart file."
      debugMessage "The current directory is ${PWD}."
      debugMessage "The run directory is ${RUNDIR}."
      logMessage "Downloading run.properties file associated with hotstart file from ${hotstartURL}."
      # get cold start time from the run.properties file
      curl $hotstartURL/run.properties > $RUNDIR/from.run.properties
      logMessage "$THIS: Detecting cold start date from $RUNDIR/from.run.properties."
      COLDSTARTDATE=`sed -n 's/[ ^]*$//;s/ColdStartTime\s*:\s*//p' ${RUNDIR}/from.run.properties`
      logMessage "The cold start datetime associated with the remote hotstart file is ${COLDSTARTDATE}."
      # pull down fort.68 file and save as fort.67 just because that
      # is what the rest of asgs_main.sh is expecting
      if [[ $HOTSTARTFORMAT = "binary" ]]; then
         mkdir -p $RUNDIR/PE0000 2>> $SYSLOG
         curl ${hotstartURL}/fort.68${hotstartSuffix} > ${RUNDIR}/PE0000/${hotstartFile}
         logMessage "Downloaded hotstart file fort.68$hotstartSuffix from $hotstartURL to $RUNDIR/PE0000/${hotstartFile}."
      else
         curl ${hotstartURL}/fort.68${hotstartSuffix} > ${RUNDIR}/${hotstartFile}
         logMessage "Downloaded hotstart file fort.68$hotstartSuffix from $hotstartURL to $RUNDIR/${hotstartFile}."
      fi

      logMessage "Now checking hotstart file content."
      checkHotstart $RUNDIR $HOTSTARTFORMAT 67
      # get cold start time from the run.properties file
      curl $hotstartURL/run.properties > from.run.properties
   else
      # starting from a hotstart file on the local filesystem, not from a URL
      checkDirExistence $LASTSUBDIR "local subdirectory containing hotstart file from the previous run"
      # check to make sure the COLDSTARTDATE was not set to "auto" in the
      # asgs config file (unless the run.properties file was also supplied)
      if [[ $COLDSTARTDATE = auto ]]; then
         logMessage "The COLDSTARTDATE parameter in the ASGS config file was set to 'auto' and the LASTSUBDIR parameter was set to the local filesystem directory ${LASTSUBDIR}. The COLDSTARTDATE will therefore be determined from the ColdStartTime property in the $LASTSUBDIR (nowcast or hindcast) run.properties file."
      fi
      for dir in nowcast hindcast; do
         if [[ -d $LASTSUBDIR/$dir ]]; then
            configColdStartDate=$COLDSTARTDATE
            checkFileExistence $LASTSUBDIR/$dir "run properties file" run.properties
            COLDSTARTDATE=`sed -n 's/[ ^]*$//;s/ColdStartTime\s*:\s*//p' ${LASTSUBDIR}/$dir/run.properties`
            logMessage "The cold start datetime from the run.properties file is ${COLDSTARTDATE}."
            if [[ $configColdStartDate != $COLDSTARTDATE ]]; then
               logMessage "The ASGS config file set the COLDSTARTDATE to '$configColdStartDate' but the value found from the '$LASTSUBDIR/$dir/run.properties' file was '$COLDSTARTDATE'. The value from the run.properties file will be used."
            fi 
            break
         fi
      done
      checkHotstart $hotstartPath $HOTSTARTFORMAT 67
   fi
fi
#
if [[ -e ${RUNARCHIVEBASE}/${PREPPEDARCHIVE} ]]; then
   logMessage "$THIS: Found archive of preprocessed input files ${RUNARCHIVEBASE}/${PREPPEDARCHIVE}."
else
   logMessage "$THIS: Could not find archive of preprocessed input files ${RUNARCHIVEBASE}/${PREPPEDARCHIVE}. It will be recreated."
fi
if [[ -e ${RUNARCHIVEBASE}/${HINDCASTARCHIVE} ]]; then
   logMessage "$THIS: Found archive of preprocessed input files ${RUNARCHIVEBASE}/${HINDCASTARCHIVE}."
else
   logMessage "$THIS: Could not find archive of preprocessed input files ${RUNARCHIVEBASE}/${HINDCASTARCHIVE}. It will be recreated."
fi
#
checkFileExistence $OUTPUTDIR "postprocessing initialization script" $INITPOST
for script in "${POSTPROCESS[@]}" ; do
   checkFileExistence $OUTPUTDIR "postprocessing script" $script
done
checkFileExistence $OUTPUTDIR "email notification script" $NOTIFY_SCRIPT
checkFileExistence ${SCRIPTDIR}/archive "data archival script" $ARCHIVE

#
if [[ $PERIODICFLUX != null ]]; then
   if [[ $FLUXCALCULATOR == "static" ]];
      # $PERIODICFLUX is a static file to insert into fort.15
      logMessage "$THIS: checking for static PERIODICFLUX file."
      checkFileExistence $INPUTDIR "static file for periodic flux boundary" $PERIODICFLUX
   else
      logMessage "$THIS: checking for FLUXCALCULATOR script"
      checkFileExistence $SCRIPTDIR/bin "perl script for calculating periodic flux boundary" $FLUXCALCULATOR
   fi
fi
#
# # @jasonfleming : temporarily disable until we can get this to work reliably
# on all platforms without having to build and install additional perl modules
#
#if [[ $TROPICALCYCLONE != off ]]; then
#   checkFileExistence ${PERL5LIB} "perl library to support downloading forecast/advisories from the National Hurricane Center website" Tiny.pm
#fi
#
# Check for any issues or inconsistencies in configuration parameters.
if [[ $(($NCPU + $NUMWRITERS)) -gt $NCPUCAPACITY ]]; then
   fatal "$THIS: NCPUCAPACITY must be greater than or equal to NCPU plus NUMWRITERS, however NCPUCAPACITY=$NCPUCAPACITY and NUMWRITERS=$NUMWRITERS and NCPU=$NCPU."
fi
#
# initialize the directory where this instance of the ASGS will run and
# keep all its files
logMessage "$THIS: The directory $RUNDIR will be used for all files associated with this execution of the ASGS."
consoleMessage "$I RUNDIR: '$RUNDIR'"
# add the run directory to the list of alternate directories to look for
# NAM data in
ALTNAMDIR="${ALTNAMDIR},$RUNDIR"
#
# send out an email to notify users that the ASGS is ACTIVATED
${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV $STORM $YEAR $RUNDIR advisory enstorm $GRIDFILE activation $EMAILNOTIFY $SYSLOG "${ACTIVATE_LIST}" $ARCHIVEBASE $ARCHIVEDIR >> ${SYSLOG} 2>&1
#
OLDADVISDIR=null
CSDATE=$COLDSTARTDATE
START=$HOTORCOLD
OLDADVISDIR=$LASTSUBDIR/hindcast
#

if [[ $BACKGROUNDMET = on && $TROPICALCYCLONE = on ]]; then
   NWS=29
   # not ready for this yet
   fatal "$THIS: Background meteorology and tropical cyclone forcing are both turned on in ${CONFIG} but simultaneous use of these two forcing types is not yet supported in ASGS."
fi
NOFORCING=false
# If there is no forcing from an external data source, set a flag; this
# is most often used in running test cases for ADCIRC.
if [[ $BACKGROUNDMET = off && $TIDEFAC = off && $TROPICALCYCLONE = off && $WAVES = off && $VARFLUX = off ]]; then
   NOFORCING=true
fi
#
# If we are coldstarting, perform a hindcast ... this is necessary
# to ramp up forcing and allow transient signals to die away before
# performing a nowcast.
ADVISDIR=null   # determined below
HSTIME=null     # determined below
#
# execute FINISH_INIT hooks
executeHookScripts "FINISH_INIT"
#
###############################
#   BODY OF ASGS STARTS HERE
###############################
#
#
#       S P I N U P
#
#
stage="SPINUP"  # modelling phase : SPINUP, NOWCAST, or FORECAST
CYCLE="initialize"
executeHookScripts "START_SPINUP_STAGE"
#
if [[ $START = coldstart ]]; then
   ENSTORM=hindcast
   SCENARIO=$ENSTORM
   executeHookScripts "BUILD_SPINUP"
   nullifyFilesFirstTimeUpdated  # for monitoring the first modification time of files
   logMessage "$THIS: Starting hindcast."
   HOTSWAN=off
   si=-2      # represents a hindcast
   readConfig
   THIS=asgs_main.sh
   ADVISDIR=$RUNDIR/initialize
   mkdir -p $ADVISDIR 2>> ${SYSLOG}
   CYCLEDIR=$ADVISDIR
   CYCLELOG=$ADVISDIR/cycle.log
   STORMDIR=$ADVISDIR/$ENSTORM
   mkdir -p $STORMDIR 2>> ${SYSLOG}
   SCENARIODIR=$STORMDIR
   cd $SCENARIODIR 2>> $SYSLOG
   SCENARIOLOG=$SCENARIODIR/scenario.log
   HSTIME=0
   # We assume that the hindcast is only used to spin up tides or
   # initialize rivers ... therefore no met forcing.
   NWS=0
   OLDADVISDIR=$ADVISDIR # initialize with dummy value when coldstarting
   HINDCASTLENGTH=${HINDCASTLENGTH:-30.0} # needed or --endtime swallows "--nws" in control_file_gen.pl options
   consoleMessage "$I Coldstarting."
   logMessage "$ENSTORM: $THIS: Coldstarting."
   logMessage "$ENSTORM: $THIS: Coldstart time is '$CSDATE'."
   logMessage "$ENSTORM: $THIS: The initial hindcast duration is '$HINDCASTLENGTH' days."
   writeProperties $SCENARIODIR
   writeScenarioProperties $SCENARIODIR
   writeScenarioFilesStatus $SCENARIODIR
   if [[ -e ${INPUTDIR}/$MESHPROPERTIES ]]; then
      cat ${INPUTDIR}/$MESHPROPERTIES >> $SCENARIODIR/run.properties
   else
      logMessage "$ENSTORM: $THIS: The properties file ${INPUTDIR}/$MESHPROPERTIES was not found and will not be added to the run.properties file."
   fi
   # prepare hindcast control (fort.15) file
   # calculate periodic fux data for insertion in fort.15 if necessary
   if [[ $PERIODICFLUX != null ]]; then
      FLUXOPTIONS="--gridfile ${INPUTDIR}/${GRIDFILE} --outfile $PERIODICFLUX --discharge $RIVERDISCHARGE --units $FLUXUNITS"
      logMessage "$ENSTORM: $THIS: Running $FLUXCALCULATOR with options $FLUXOPTIONS."
      perl $FLUXCALCULATOR $FLUXOPTIONS >> ${SYSLOG} 2>&1
   fi
   CONTROLOPTIONS="--name $ENSTORM --advisorynum $ADVISORY --cst $CSDATE --hsformat $HOTSTARTFORMAT --advisorynum 0"
   CONTROLOPTIONS="$CONTROLOPTIONS --elevstations ${INPUTDIR}/${ELEVSTATIONS} --velstations ${INPUTDIR}/${VELSTATIONS} --metstations ${INPUTDIR}/${METSTATIONS}"
   CONTROLOPTIONS="$CONTROLOPTIONS --gridname $GRIDNAME" # for run.properties
   CONTROLOPTIONS="$CONTROLOPTIONS --nscreen $NSCREEN"
   if [[ $NOFORCING = true ]]; then
      CONTROLOPTIONS="$_RPCONTROLOPTIONS --specifiedRunLength $HINDCASTLENGTH"
   fi
   #
   logMessage "$ENSTORM: $THIS: Constructing control file with the following options: $CONTROLOPTIONS."
   runLength=$HINDCASTLENGTH # to compute long term tidal constituents
   # uses parameters described above as well as control-parameters.yaml
   # to generate tide_fac.out, fort.13, fort.15, and fort.26
   controlExitStatus=0
   controlMsg=""
   generateDynamicInput
   THIS="asgs_main.sh"
   #
   logMessage "$ENSTORM: $THIS: Starting $ENSTORM preprocessing."
   # make sure the archive of subdomain files is up to date
   checkArchiveFreshness $PREPPEDARCHIVE $HINDCASTARCHIVE $GRIDFILE $CONTROLTEMPLATE $ELEVSTATIONS $VELSTATIONS $METSTATIONS $NAFILE $INPUTDIR
   THIS="asgs_main.sh"
   logMessage "$THIS: prep $ADVISDIR $INPUTDIR $ENSTORM $START $OLDADVISDIR $HPCENVSHORT $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT '$OUTPUTOPTIONS' $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE"
   prep $ADVISDIR $INPUTDIR $ENSTORM $START $OLDADVISDIR $HPCENVSHORT $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT "$OUTPUTOPTIONS" $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE
   THIS="asgs_main.sh"
   # check to see that adcprep did not conspicuously fail
   handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV hindcast $YEAR $STORMDIR $ADVISORY $LASTADVISORYNUM $STATEFILE $GRIDFILE $EMAILNOTIFY "${JOB_FAILED_LIST}" $ARCHIVEBASE $ARCHIVEDIR
   THIS="asgs_main.sh"
   if [[ ! -d $ADVISDIR/$ENSTORM ]]; then
      fatal "$ENSTORM: $THIS: The prep for the hindcast run has failed."
   fi
   # then submit the job
   logMessage "$ENSTORM: $THIS: Submitting ADCIRC $ENSTORM job."
   cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
   JOBTYPE=padcirc  # we won't run waves during the spinup hindcast
   if [[ $QUEUESYS = serial ]]; then
      JOBTYPE=adcirc
   fi
   writeJobResourceRequestProperties ${ADVISDIR}/${ENSTORM}
   echo "hpc.job.${JOBTYPE}.limit.walltime : $HINDCASTWALLTIME" >> $ADVISDIR/$ENSTORM/run.properties
   #
   executeHookScripts "SUBMIT_SPINUP"
   #
   logMessage "$ENSTORM: $THIS: submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $HPCENVSHORT $ACCOUNT $PPN $NUMWRITERS $HOTSTARTCOMP $HINDCASTWALLTIME $JOBTYPE"
   submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $HPCENVSHORT $ACCOUNT $PPN $NUMWRITERS $HOTSTARTCOMP $HINDCASTWALLTIME $JOBTYPE
   THIS="asgs_main.sh"
   # check once per minute until all jobs have finished
   monitorJobs $QUEUESYS ${JOBTYPE} ${ENSTORM} $HINDCASTWALLTIME
   THIS="asgs_main.sh"
   # check to see that the hindcast job did not conspicuously fail
   handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV hindcast $YEAR $STORMDIR $ADVISORY $LASTADVISORYNUM $STATEFILE $GRIDFILE $EMAILNOTIFY "${JOB_FAILED_LIST}" $ARCHIVEBASE $ARCHIVEDIR
   THIS="asgs_main.sh"
   if [[ ! -d $ADVISDIR/$ENSTORM ]]; then
      fatal "$ENSTORM: $THIS: The hindcast run has failed."
   fi

   scenarioMessage "$ENSTORM: $THIS: $ENSTORM run finished."
   #
   executeHookScripts "FINISH_SPINUP_SCENARIO"
   #
   cd $ADVISDIR 2>> ${SYSLOG}
   OLDADVISDIR=$ADVISDIR
   START=hotstart
   #
   echo RUNDIR=${RUNDIR} > $STATEFILE 2>> ${SYSLOG}
   echo SCRIPTDIR=${SCRIPTDIR} >> $STATEFILE 2>> ${SYSLOG}
   echo LASTSUBDIR=${OLDADVISDIR} >> $STATEFILE 2>> ${SYSLOG}
   echo SYSLOG=${SYSLOG} >> $STATEFILE 2>> ${SYSLOG}
   echo ADVISORY=${ADVISORY} >> $STATEFILE 2>> ${SYSLOG}

else
   # start from   H O T S T A R T   file
   #
   executeHookScripts "HOT_SPINUP"
   #
   if [[ $hotstartURL = null ]]; then
      if [[ `basename $LASTSUBDIR` = nowcast || `basename $LASTSUBDIR` = hindcast ]]; then
      logMessage "$THIS: The LASTSUBDIR path is $LASTSUBDIR but ASGS looks in this path to find either a nowcast or hindcast subdirectory. The LASTSUBDIR parameter is being reset to to remove either nowcast or hindcast from the end of it."
      LASTSUBDIR=`dirname $LASTSUBDIR`
      fi
   fi
   if [[ $LASTSUBDIR = null ]]; then
      fatal "LASTSUBDIR is set to null, but the ASGS is trying to hotstart. Is the STATEFILE $STATEFILE up to date and correct? If not, perhaps it should be deleted. Otherwise, the HOTORCOLD parameter in the ASGS config file has been set to $HOTORCOLD and yet the LASTSUBDIR parameter is still set to null."
   fi
   if [[ $hotstartURL = null ]]; then
      OLDADVISDIR=$LASTSUBDIR
   else
      OLDADVISDIR=$RUNDIR
   fi

fi
CYCLELOG=null
SCENARIOLOG=null
#
executeHookScripts "FINISH_SPINUP_STAGE"

# Use for hindcast activities to end ASGS after the hindcast has been run
if [[ "${HINDCASTONCE_AND_EXIT}" == "y" || "${HINDCASTONCE_AND_EXIT}" == "yes" ]]; then
   logMessage "$THIS: Shutting down due to HINDCASTONCE_AND_EXIT mode ..."
   echo "$THIS: Shutting down due to HINDCASTONCE_AND_EXIT mode ..."
   exit 0
fi

#
# B E G I N   N O W C A S T / F O R E C A S T   L O O P
#
while [ true ]; do
   THIS="asgs_main.sh"
   #
   stage="NOWCAST"  # modelling phase : SPINUP, NOWCAST, or FORECAST
   CYCLE="null"     # don't know the cycle until we successfully download it
   executeHookScripts "START_NOWCAST_STAGE"
   #
   ENSTORM=nowcast
   SCENARIO=$ENSTORM
   SCENARIODIR="null" # don't know the path until we have the forcing for this cycle

   si=-1
   # re-read configuration file to pick up any changes, or any config that is specific to nowcasts
   readConfig
   THIS=asgs_main.sh
   FROMDIR=null
   if [[ $hotstartURL = null ]]; then
      for dir in nowcast hindcast; do
         logMessage "$ENSTORM: $THIS: Looking for the directory $OLDADVISDIR/${dir}."
         if [[ -d $OLDADVISDIR/$dir ]]; then
            FROMDIR=$OLDADVISDIR/$dir
         fi
      done
   else
      # already downloaded the hotstart file
      FROMDIR=$RUNDIR
   fi
   # turn SWAN hotstarting on or off as appropriate
   HOTSWAN=off
   if [[ $WAVES = on && $REINITIALIZESWAN = no ]]; then
      # look for a swan hotstart file
      for swanhsfile in PE0000/swan.67 swan.67; do
         if [[ -e $FROMDIR/$swanhsfile ]]; then
            HOTSWAN=on
            logMessage "Found SWAN hotstart file $FROMDIR/${swanhsfile}."
         fi
         for swanhssuffix in tar.gz tar.bz2 gz bz2; do
            if [[ -e $FROMDIR/${swanhsfile}.${swanhssuffix} ]]; then
               HOTSWAN=on
               logMessage "Found SWAN hotstart file $FROMDIR/${swanhsfile}."
            fi
         done
      done
   fi

   checkHotstart $FROMDIR $HOTSTARTFORMAT  67

   cd $RUNDIR 2>> ${SYSLOG}
   #
   # N O W C A S T
   SCENARIO=nowcast
   ENSTORM=nowcast
   RUNNOWCAST=yes
   NOWCASTDIR=null    # directory with hotstart files to be used in forecast
   # write the properties associated with asgs configuration to the
   # run.properties file
   writeProperties $RUNDIR
   if [[ $BACKGROUNDMET != off ]]; then
      BASENWS=-12
      case $BACKGROUNDMET in
         "on"|"NAM")
            writeNAMProperties $RUNDIR
            ;;
         "GFS")
            writeGFSProperties $RUNDIR
            ;;
         *) # other values are allowed but don't have properties that need to be written in advance
            ;;
      esac
   fi
   if [[ $WAVES = on ]]; then
      writeWaveCouplingProperties $RUNDIR
   fi
   logMessage "$ENSTORM: $THIS: Checking for new meteorological data every 60 seconds ..."
   # TROPICAL CYCLONE ONLY
   if [[ $TROPICALCYCLONE == "on" ]]; then
      case $VORTEXMODEL in
         "GAHM")
            BASENWS=20
            ;;
         "ASYMMETRIC")
            BASENWS=19
            ;;
         "SYMMETRIC")
            BASENWS=8
            ;;
         *)
            fatal "$ENSTORM: $THIS: The VORTEXMODEL parameter was set to '$VORTEXMODEL' but the only supported choices SYMMETRIC, ASYMMETRIC, and GAHM."
            ;;
      esac
      NWS=$BASENWS
      if [[ $WAVES == on ]]; then
         NWS=$(($BASENWS + 300))
      fi
      # need to set NWS properly for NAM or GFS blending
      case $BACKGROUNDMET in
         "namBlend"|"gfsBlend")
            if [[ $BASENWS -gt 8 ]]; then
               NWS=$(($BASENWS + 10))  # e.g., 20 becomes 30
               if [[ $WAVES == on ]]; then
                  NWS=$(($NWS + 300))  # e.g., 30 becomes 330
               fi
               NWS=-$NWS  # indicates that the first OWI dataset starts at the hotstart time
            else
               # ADCIRC does not support blended winds with the symmetric vortex model
               fatal "$ENSTORM: $THIS: The BACKGROUNDMET parameter was set to '$BACKGROUNDMET' but this setting cannot be combined with VORTEXMODEL=$VORTEXMODEL."
            fi
            ;;
         "on"|"NAM"|"OWI")
            fatal "$ENSTORM: $THIS: The parameter settings TROPICALCYCLONE=$TROPICALCYCLONE and BACKGROUNDMET=$BACKGROUNDMET cannot be combined in an ASGS configuration."
            ;;
         "off")
            # this is the typical setting when TROPICALCYCLONE=on
            ;;
         *)
            fatal "$ENSTORM: $THIS: The BACKGROUNDMET parameter was set to '$BACKGROUNDMET' but the only supported choices when TROPICALCYCLONE=$TROPICALCYCLONE are 'off' and 'namBlend'."
            ;;
      esac
      writeTropicalCycloneProperties $RUNDIR
      #
      executeHookScripts "NOWCAST_POLLING"
      #
      # download wind data from ftp site every 60 seconds to see if
      # there is a new advisory
      downloadCycloneData $STORM $YEAR $RUNDIR $SCRIPTDIR $OLDADVISDIR $TRIGGER $ADVISORY $FTPSITE $RSSSITE $FDIR $HDIR $STATEFILE
      THIS="asgs_main.sh"
      #
      LASTADVISORYNUM=$ADVISORY
      # pull the latest advisory number from the statefile
      logMessage "$ENSTORM: $THIS: Pulling latest advisory number from the state file ${STATEFILE}."
      ADVISORY=`grep "ADVISORY" $STATEFILE | sed 's/ADVISORY.*=//' | sed 's/^\s//'` 2>> ${SYSLOG}
      CYCLE=$ADVISORY
      executeHookScripts "NOWCAST_TRIGGERED"
      consoleMessage "$I Advisory '$CYCLE'"
      ADVISDIR=$RUNDIR/${ADVISORY}
      if [ ! -d $ADVISDIR ]; then
          mkdir $ADVISDIR 2>> ${SYSLOG}
      fi
      CYCLEDIR=$ADVISDIR
      CYCLELOG=$CYCLEDIR/cycle.log
      NOWCASTDIR=$ADVISDIR/$ENSTORM
      STORMDIR=$ADVISDIR/$ENSTORM
      if [ ! -d $NOWCASTDIR ]; then
          mkdir $NOWCASTDIR 2>> ${SYSLOG}
      fi
      SCENARIODIR=$CYCLEDIR/$SCENARIO
      SCENARIOLOG=$SCENARIODIR/scenario.log
      mv $RUNDIR/run.properties $SCENARIODIR 2>> $SYSLOG
      writeScenarioProperties $SCENARIODIR
      #
      logMessage "$ENSTORM: $THIS: $START Storm $STORM advisory $ADVISORY in $YEAR"
      # move raw ATCF files into advisory directory
      mv *.fst *.dat *.xml *.html $ADVISDIR 2>> ${SYSLOG}
      #
      # prepare nowcast met (fort.22) and control (fort.15) files
      cd $NOWCASTDIR 2>> ${SYSLOG}
      #
      executeHookScripts "BUILD_NOWCAST_SCENARIO"
      nullifyFilesFirstTimeUpdated  # for monitoring the first modification time of files
      #
      METOPTIONS="--dir $ADVISDIR --storm $STORM --year $YEAR --name $ENSTORM --nws $NWS --hotstartseconds $HSTIME --coldstartdate $CSDATE $STORMTRACKOPTIONS"
      CONTROLOPTIONS=" --name $ENSTORM --advisorynum $ADVISORY --hst $HSTIME --cst $CSDATE --hsformat $HOTSTARTFORMAT"
      logMessage "$ENSTORM: $THIS: Generating ADCIRC Met File (fort.22) for nowcast with the following options: $METOPTIONS."
      ${SCRIPTDIR}/storm_track_gen.pl $METOPTIONS >> ${SYSLOG} 2>&1
      # get the storm's name (e.g. BERTHA) from the run.properties
      logMessage "$ENSTORM: $THIS: Detecting storm name in run.properties file."
      STORMNAME=$(grep "forcing.tropicalcyclone.stormname" run.properties | sed 's/forcing.tropicalcyclone.stormname.*://' | sed 's/^\s//' 2>> ${SYSLOG})
      tcEnd=$(grep "forcing.tropicalcyclone.best.time.end" run.properties | sed 's/forcing.tropicalcyclone.best.time.end.*://' | sed 's/^\s//' 2>> ${SYSLOG})
      CONTROLOPTIONS="$CONTROLOPTIONS --endtime $tcEnd"
      # create a GAHM or ASYMMETRIC fort.22 file from the existing track file
      case $VORTEXMODEL in
         "GAHM"|"ASYMMETRIC")
            # need to run aswip to pre-calculate Rmaxes
            $ADCIRCDIR/aswip -n $BASENWS >> ${SYSLOG} 2>&1
            if [[ -e NWS_${BASENWS}_fort.22 ]]; then
               mv fort.22 fort.22.orig >> ${SYSLOG} 2>&1
               case $BACKGROUNDMET in
                  "namBlend"|"gfsBlend")
                     # ADCIRC needs to read a file named fort.22 that represents
                     # the gridded NAM wind field
                     CONTROLOPTIONS=" $CONTROLOPTIONS --metfile $NOWCASTDIR/NWS_${BASENWS}_fort.22"
                     ;;
                  "off")
                     # this is the only met file ADCIRC will need to read so
                     # rename it fort.22
                     cp NWS_${BASENWS}_fort.22 fort.22 >> ${SYSLOG} 2>&1
                     CONTROLOPTIONS=" $CONTROLOPTIONS --metfile $NOWCASTDIR/fort.22"
                     ;;
                  *)
                     # should be unreachable based on param checks above
                     ;;
               esac
            else
               fatal "$ENSTORM: $THIS: '$ADCIRCDIR/aswip -n $BASENWS' failed to produce 'NWS_${BASENWS}_fort.22'."
            fi
            ;;
         "SYMMETRIC")
            # cannot blend symmetric wind fields in ADCIRC yet
            # and don't need to run aswip b/c symmetric vortex
            # uses BEST track Rmax and persists it into the forecast
            scenarioMessage "$ENSTORM: $THIS: Using symmetric vortex model, NWS=8."
            ;;
         *)
            # should be unreachable based on param checks above
            ;;
      esac
   fi
   # BACKGROUND METEOROLOGY
   case $BACKGROUNDMET in
      "on"|"NAM"|"OWI"|"GFS")
         if [[ $WAVES == "on" ]]; then
            NWS=-312
         else
            NWS=-12
         fi
         ;;
      "namBlend"|"gfsBlend")
         if [[ $TROPICALCYCLONE == "off" ]]; then
            fatal "$ENSTORM: $THIS: BACKGROUNDMET was set to '$BACKGROUNDMET' but this setting is only meaningful when TROPICALCYCLONE is set to 'on'."
         fi
         ;;
      "off")
         # don't need to set NWS
         ;;
      *)
         fatal "$ENSTORM: $THIS: BACKGROUNDMET was set to $BACKGROUNDMET but the only allowable values are 'on', 'NAM', 'OWI', 'namBlend', and 'off'."
         ;;
   esac

   case $BACKGROUNDMET in
      "namBlend")
         logMessage "$ENSTORM: $THIS: NWS is $NWS. Downloading NAM far field winds."
         downloadBackgroundMet $SCENARIODIR $RUNDIR $SCRIPTDIR $BACKSITE $BACKDIR $ENSTORM $CSDATE $HSTIME $FORECASTLENGTH $ALTNAMDIR "00,06,12,18" $ARCHIVEBASE $ARCHIVEDIR $STATEFILE
         cp $RUNDIR/get_nam_data.pl.json $SCENARIODIR 2>> $SYSLOG
         cd $SCENARIODIR 2>> ${SYSLOG}
         # convert met files to ASCII WIN/PRE format
         logMessage "$ENSTORM: $THIS: Converting NAM data to OWI format."
         boolApplyRamp=false
         if [[ $SPATIALEXTRAPOLATIONRAMP == "yes" ]]; then
            boolApplyRamp=true
         fi
         ptFilePath=${SCRIPTDIR}/input/ptFile_oneEighth.txt
         escPtFilePath=${ptFilePath////'\/'}
         escSCENARIODIR=${SCENARIODIR////'\/'}
         sed \
            -e "s/%NULLNAMWINPREDATAPATH%/$escSCENARIODIR/" \
            -e "s/%NULLNAMWINPREGRID%/$escPtFilePath/" \
            -e "s/\"%NULLNAMAWIPGRID%\"/218/" \
            -e "s/%NULLNAMRAWFORMAT%/grib2/" \
            -e "s/\"%NULLVELMULT%\"/$VELOCITYMULTIPLIER/" \
            -e "s/\"%NULLPRESSMULT%\"/0.01/" \
            -e "s/\"%NULLAPPLYRAMP%\"/$boolApplyRamp/" \
            -e "s/\"%NULLRAMPDIST%\"/$SPATIALEXTRAPOLATIONRAMPDISTANCE/" \
             < get_nam_data.pl.json \
             | $SCRIPTDIR/NAMtoOWIRamp.pl \
             > NAMtoOWIRamp.pl.json
           2>> $SYSLOG
         preFile=$(bashJSON.pl --key winPrePressureFile < NAMtoOWIRamp.pl.json)
         winFile=$(bashJSON.pl --key winPreVelocityFile < NAMtoOWIRamp.pl.json)
         WTIMINC=$(bashJSON.pl --key winPreWtimincSeconds < NAMtoOWIRamp.pl.json)
         tlimits=( $( head -n 1 $preFile | awk '{ print $(NF-1)" "($NF) }' ) )
         owiWinPre["startDateTime"]=${tlimits[0]}
         owiWinPre["endDateTime"]=${tlimits[1]}
         # determine the number of blank snaps (if any)
         # epoch seconds associated with cold start and hotstart times
         date=${tlimits[0]}
         csEpochSeconds=$(TZ=UTC date -u -d "${CSDATE:0:4}-${CSDATE:4:2}-${CSDATE:6:2} ${CSDATE:8:2}:00:00" "+%s" 2>>$SYSLOG)
         hsEpochSeconds=$((csEpochSeconds + ${HSTIME%.*}))
         owiStartEpochSeconds=$(TZ=UTC date -u -d "${date:0:4}-${date:4:2}-${date:6:2} ${date:8:2}:00:00" "+%s" 2>>$SYSLOG)
         owiWinPre["NWBS"]=$(echo "scale=0; ($owiStartEpochSeconds - $hsEpochSeconds)/$WTIMINC" | bc)
         cp $RUNDIR/get_nam_data.pl.* $SCENARIODIR 2>> $SYSLOG
         # copy log data to scenario.log
         for file in lambert_diag.out reproject.log ; do
            if [[ -e $ADVISDIR/$file ]]; then
               scenarioMessage "$ENSTORM: $THIS: $file is as follows:"
               cat $ADVISDIR/$file >> $SCENARIOLOG
            fi
         done
         # create links to the OWI files
         ln -s $(basename $preFile) fort.221 2>> ${SYSLOG}
         ln -s $(basename $winFile) fort.222 2>> ${SYSLOG}
         fort22="owi_fort.22"
         echo "${owiWinPre["NWSET"]} ! NWSET" > $fort22
         echo "${owiWinPre["NWBS"]} ! NWBS"  >> $fort22
         echo "${owiWinPre["DWM"]} ! DWM"    >> $fort22
         ;;
      "on"|"NAM")
         logMessage "$ENSTORM: $THIS: NWS is $NWS. Downloading background meteorology."
         #
         executeHookScripts "NOWCAST_POLLING"
         #
         downloadBackgroundMet $SCENARIODIR $RUNDIR $SCRIPTDIR $BACKSITE $BACKDIR $ENSTORM $CSDATE $HSTIME $FORECASTLENGTH $ALTNAMDIR $FORECASTCYCLE $ARCHIVEBASE $ARCHIVEDIR $STATEFILE
         THIS="asgs_main.sh"
         #
         LASTADVISORYNUM=$ADVISORY
         logMessage "$ENSTORM: $THIS: Detecting the ADVISORY from the state file ${STATEFILE}."
         ADVISORY=`grep ADVISORY $STATEFILE | sed 's/ADVISORY.*=//' | sed 's/^\s//'` 2>> ${SYSLOG}
         echo "forcing.nwp.year : ${ADVISORY:0:4}" >> $RUNDIR/run.properties
         #
         executeHookScripts "NOWCAST_TRIGGERED" # now that we know the advisory number
         consoleMessage "$I NAM cycle '$ADVISORY'"
         ADVISDIR=$RUNDIR/${ADVISORY}
         CYCLEDIR=$ADVISDIR
         CYCLELOG=$CYCLEDIR/cycle.log
         NOWCASTDIR=$ADVISDIR/$ENSTORM
         SCENARIODIR=$CYCLEDIR/$SCENARIO
         SCENARIOLOG=$SCENARIODIR/scenario.log
         mkdir -p $SCENARIODIR 2>> $SYSLOG
         mv $RUNDIR/run.properties $SCENARIODIR 2>> $SYSLOG
         cp $RUNDIR/get_nam_data.pl.json $SCENARIODIR 2>> $SYSLOG
         writeScenarioProperties $SCENARIODIR
         cd $SCENARIODIR 2>> ${SYSLOG}
         logMessage "$ENSTORM: $THIS: $START $ENSTORM cycle $ADVISORY."
         # convert met files to OWI format
         #
         executeHookScripts "BUILD_NOWCAST_SCENARIO"
         #
         logMessage "$ENSTORM: $THIS: Converting NAM data to OWI format."
         boolApplyRamp=false
         if [[ $SPATIALEXTRAPOLATIONRAMP == "yes" ]]; then
            boolApplyRamp=true
         fi
         ptFilePath=${SCRIPTDIR}/input/ptFile_oneEighth.txt
         escPtFilePath=${ptFilePath////'\/'}
         escSCENARIODIR=${SCENARIODIR////'\/'}
         sed \
            -e "s/%NULLNAMWINPREDATAPATH%/$escSCENARIODIR/" \
            -e "s/%NULLNAMWINPREGRID%/$escPtFilePath/" \
            -e "s/\"%NULLNAMAWIPGRID%\"/218/" \
            -e "s/%NULLNAMRAWFORMAT%/grib2/" \
            -e "s/\"%NULLVELMULT%\"/$VELOCITYMULTIPLIER/" \
            -e "s/\"%NULLPRESSMULT%\"/0.01/" \
            -e "s/\"%NULLAPPLYRAMP%\"/$boolApplyRamp/" \
            -e "s/\"%NULLRAMPDIST%\"/$SPATIALEXTRAPOLATIONRAMPDISTANCE/" \
             < get_nam_data.pl.json \
             | $SCRIPTDIR/NAMtoOWIRamp.pl \
             > NAMtoOWIRamp.pl.json
           2>> $SYSLOG
         preFile=$(bashJSON.pl --key winPrePressureFile < NAMtoOWIRamp.pl.json)
         winFile=$(bashJSON.pl --key winPreVelocityFile < NAMtoOWIRamp.pl.json)
         WTIMINC=$(bashJSON.pl --key winPreWtimincSeconds < NAMtoOWIRamp.pl.json)
         tlimits=( $( head -n 1 $preFile | awk '{ print $(NF-1)" "($NF) }' ) )
         owiWinPre["startDateTime"]=${tlimits[0]}
         owiWinPre["endDateTime"]=${tlimits[1]}
         # determine the number of blank snaps (if any)
         # epoch seconds associated with cold start and hotstart times
         date=${tlimits[0]}
         csEpochSeconds=$(TZ=UTC date -u -d "${CSDATE:0:4}-${CSDATE:4:2}-${CSDATE:6:2} ${CSDATE:8:2}:00:00" "+%s" 2>>$SYSLOG)
         hsEpochSeconds=$((csEpochSeconds + ${HSTIME%.*}))
         owiStartEpochSeconds=$(TZ=UTC date -u -d "${date:0:4}-${date:4:2}-${date:6:2} ${date:8:2}:00:00" "+%s" 2>>$SYSLOG)
         owiWinPre["NWBS"]=$(echo "scale=0; ($owiStartEpochSeconds - $hsEpochSeconds)/$WTIMINC" | bc)
         cp $RUNDIR/get_nam_data.pl.* $SCENARIODIR 2>> $SYSLOG
         # copy log data to scenario.log
         for file in lambert_diag.out reproject.log ; do
            if [[ -e $ADVISDIR/$file ]]; then
               scenarioMessage "$ENSTORM: $THIS: $file is as follows:"
               cat $ADVISDIR/$file >> $SCENARIOLOG
            fi
         done
         # create links to the OWI files
         ln -s $(basename $preFile) fort.221 2>> ${SYSLOG}
         ln -s $(basename $winFile) fort.222 2>> ${SYSLOG}
         fort22="fort.22"
         echo "${owiWinPre["NWSET"]} ! NWSET" > $fort22
         echo "${owiWinPre["NWBS"]} ! NWBS"  >> $fort22
         echo "${owiWinPre["DWM"]} ! DWM"    >> $fort22
         STORMDIR=$NOWCASTDIR
         CONTROLOPTIONS="$CONTROLOPTIONS --advisorynum $ADVISORY --name $ENSTORM --cst $CSDATE --hstime $HSTIME --hsformat $HOTSTARTFORMAT"
         ;;
      "gfsBlend")
         logMessage "$ENSTORM: $THIS: NWS is $NWS. Downloading GFS meteorological data for blending."
         #
         # Detect latest GFS data, subset, download, reproject, reformat
         # to Oceanweather WIN/PRE format, and make symbolic links
         downloadGFS $SCENARIODIR $RUNDIR $SCRIPTDIR $GFSBACKSITE $GFSBACKDIR $ENSTORM $CSDATE $HSTIME $GFSFORECASTLENGTH $ALTNAMDIR "00,06,12,18" $ARCHIVEBASE $ARCHIVEDIR $STATEFILE
         cd $SCENARIODIR 2>> $SYSLOG
         ;;
      "GFS")
         logMessage "$ENSTORM: $THIS: NWS is $NWS. Downloading background meteorology."
         #
         executeHookScripts "NOWCAST_POLLING"
         #
         # Detect latest GFS data, subset, download, reproject, reformat
         # to Oceanweather WIN/PRE format, and make symbolic links
         downloadGFS $SCENARIODIR $RUNDIR $SCRIPTDIR $GFSBACKSITE $GFSBACKDIR $ENSTORM $CSDATE $HSTIME $GFSFORECASTLENGTH $ALTNAMDIR $FORECASTCYCLE $ARCHIVEBASE $ARCHIVEDIR $STATEFILE
         THIS="asgs_main.sh"
         #
         LASTADVISORYNUM=$ADVISORY
         logMessage "$ENSTORM: $THIS: Detecting the ADVISORY from the state file ${STATEFILE}."
         ADVISORY=`grep ADVISORY $STATEFILE | sed 's/ADVISORY.*=//' | sed 's/^\s//'` 2>> ${SYSLOG}
         echo "forcing.nwp.year : ${ADVISORY:0:4}" >> $RUNDIR/run.properties
         #
         executeHookScripts "NOWCAST_TRIGGERED" # now that we know the advisory number
         consoleMessage "$I GFS cycle '$ADVISORY'"

         writeScenarioProperties $SCENARIODIR
         cd $SCENARIODIR 2>> $SYSLOG

         logMessage "$ENSTORM: $THIS: $START $ENSTORM cycle $ADVISORY."
         #
         executeHookScripts "BUILD_NOWCAST_SCENARIO"
         #
         STORMDIR=$NOWCASTDIR
         CONTROLOPTIONS="--advisorynum $ADVISORY --name $ENSTORM --cst $CSDATE --hstime $HSTIME --hsformat $HOTSTARTFORMAT"
         ;;

      "OWI")
         # this is a hack to enable running pre-existing OWI files for hindcast
         #
         # hard code the file location and assume the names of the files have
         # been prepended with the datetime as follows: 2017110100
         #
         # fort.22 is a symbolic link to the actual file with datatime filename
         if [[ -e ${HDIR}/fort.22 ]]; then
            linkTarget=`readlink ${HDIR}/fort.22`
            newAdvisoryNum=${linkTarget:0:10}
            # record the advisory number to the statefile
            cp -f $STATEFILE ${STATEFILE}.old 2>> ${SYSLOG}
            logMessage "$ENSTORM: $THIS: Recording the advisory number $newAdvisoryNum to the state file ${STATEFILE}."
            sed 's/ADVISORY=.*/ADVISORY='$newAdvisoryNum'/' $STATEFILE > ${STATEFILE}.new
            cp -f ${STATEFILE}.new $STATEFILE >> ${SYSLOG} 2>&1
            LASTADVISORYNUM=$ADVISORY
            ADVISORY=$newAdvisoryNum
         else
            fatal "${HDIR}/fort.22 file was not found."
         fi
         ADVISDIR=$RUNDIR/${ADVISORY}
         NOWCASTDIR=$ADVISDIR/$ENSTORM
         CYCLEDIR=$ADVISDIR
         CYCLELOG=$CYCLEDIR/cycle.log
         SCENARIODIR=$CYCLEDIR/$SCENARIO
         SCENARIOLOG=$SCENARIODIR/$SCENARIO
         mkdir -p $NOWCASTDIR 2>> ${SYSLOG}
         mv $RUNDIR/run.properties $SCENARIODIR 2>> $SYSLOG
         cd $ADVISDIR 2>> ${SYSLOG}
         # write the properties associated with asgs configuration to the
         # run.properties file
         writeScenarioProperties $NOWCASTDIR
         logMessage "$ENSTORM: $THIS: $START $ENSTORM cycle $ADVISORY."
         # create links to the OWI files, assuming they already have the
         # adcirc 221, 222, etc file name extensions
         cd $ENSTORM 2>> ${SYSLOG}
         owiFiles=`ls ${HDIR}/${ADVISORY}*.22*`
         for file in $owiFiles; do
            ext=${file##*.}
            if [[ $ext = 22 ]]; then
               cp $file fort.${ext} 2>> ${SYSLOG} # copy fort.22
            else
               ln -s $file fort.${ext} 2>> ${SYSLOG} # symbolically link data
            fi
         done
         CONTROLOPTIONS="$CONTROLOPTIONS --advisorynum $ADVISORY --name $ENSTORM --cst $CSDATE --hstime $HSTIME --hsformat $HOTSTARTFORMAT"
         ;;
      "off")
         # don't need to download any data
         # FIXME: writeProperties?
         ;;
      *) # should be unreachable
         ;;
   esac

   # send out an email alerting end users that a new cycle has been issued
   cycleStartTime=`date +%s`  # epoch seconds
   ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV $STORM $YEAR $NOWCASTDIR $ADVISORY $ENSTORM $GRIDFILE newcycle $EMAILNOTIFY $SYSLOG "${NEW_ADVISORY_LIST}" $ARCHIVEBASE $ARCHIVEDIR >> ${SYSLOG} 2>&1
   if [[ -e ${INPUTDIR}/$MESHPROPERTIES ]]; then
      cat ${INPUTDIR}/$MESHPROPERTIES >> $ADVISDIR/$ENSTORM/run.properties
   else
      logMessage "$ENSTORM: $THIS: The properties file ${INPUTDIR}/$MESHPROPERTIES was not found and will not be added to the run.properties file."
   fi
   # if there is no forcing from an external data source, set control options
   if [[ $NOFORCING = true ]]; then
      logMessage "NOFORCING is $NOFORCING"
            # pull the latest advisory number from the statefile
      ADVISORY=99999
      ADVISDIR=$RUNDIR/${ADVISORY}
      CYCLEDIR=$ADVISDIR
      CYCLELOG=$CYCLEDIR/cycle.log
      NOWCASTDIR=$ADVISDIR/$ENSTORM
      SCENARIODIR=$NOWCASTDIR
      SCENARIOLOG=$SCENARIODIR/scenario.log
      if [ ! -d $NOWCASTDIR ]; then
          mkdir -p $NOWCASTDIR 2>> ${SYSLOG}
      fi
      mv $RUNDIR/run.properties $NOWCASTDIR 2>> run.properties
      writeScenarioProperties $NOWCASTDIR
      CONTROLOPTIONS="--advisorynum $ADVISORY"
      CONTROLOPTIONS="$CONTROLOPTIONS --specifiedRunLength $NOWCASTDAYS"
      CONTROLOPTIONS="$CONTROLOPTIONS --name $ENSTORM --cst $CSDATE --hstime $HSTIME --hsformat $HOTSTARTFORMAT"
      logMessage "CONTROLOPTIONS is $CONTROLOPTIONS"
   fi
   # activate padcswan based on ASGS configuration
   if [[ $WAVES = on ]]; then
      CONTROLOPTIONS="${CONTROLOPTIONS} --swantemplate ${SCRIPTDIR}/input/meshes/common/swan/${SWANTEMPLATE} --hotswan $HOTSWAN"
   fi
   CONTROLOPTIONS="${CONTROLOPTIONS} --elevstations ${INPUTDIR}/${ELEVSTATIONS} --velstations ${INPUTDIR}/${VELSTATIONS} --metstations ${INPUTDIR}/${METSTATIONS}"
   CONTROLOPTIONS="$CONTROLOPTIONS --gridname $GRIDNAME" # for run.properties
   CONTROLOPTIONS="$CONTROLOPTIONS --nscreen $NSCREEN"
   # generate fort.15 file
   runLength=$(echo "scale=2; ($HSTIME)/86400" | bc)
   # uses parameters described above as well as control-parameters.yaml
   # to generate tide_fac.out, fort.13, fort.15, and fort.26
   controlExitStatus=0
   controlMsg=""
   generateDynamicInput
   THIS="asgs_main.sh"
   if [[ $controlExitStatus -ne 0 ]]; then
      handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV $STORMNAME $YEAR $STORMDIR $ADVISORY $LASTADVISORYNUM $STATEFILE $GRIDFILE $EMAILNOTIFY "${JOB_FAILED_LIST}" $ARCHIVEBASE $ARCHIVEDIR
      continue  # abandon this nowcast and wait for the next one
   fi
   # load properties
   declare -A properties
   loadProperties ${SCENARIO}.run-control.properties
   if [[ ${properties['RunEndTime']} != ${properties['RunStartTime']} ]]; then
      logMessage "$ENSTORM: $THIS: Starting nowcast for cycle '$ADVISORY'."

      # get river flux nowcast data, if configured to do so
      if [[ $VARFLUX = on ]]; then
         downloadRiverFluxData $ADVISDIR ${INPUTDIR}/${GRIDFILE} $RIVERSITE $RIVERDIR $RIVERUSER $RIVERDATAPROTOCOL $ENSTORM $CSDATE $HSTIME $SCRIPTDIR ${INPUTDIR}/${RIVERFLUX} $USERIVERFILEONLY
         THIS="asgs_main.sh"
      fi
      if [[ $VARFLUX = default ]]; then
         ln -s ${INPUTDIR}/${RIVERFLUX} ./fort.20 2>> ${SYSLOG}
      fi
      # preprocess
      checkArchiveFreshness $PREPPEDARCHIVE $HINDCASTARCHIVE $GRIDFILE $CONTROLTEMPLATE $ELEVSTATIONS $VELSTATIONS $METSTATIONS $NAFILE $INPUTDIR
      THIS="asgs_main.sh"
      logMessage "$ENSTORM: $THIS: Nowcast preprocessing."
      #logMessage "$ENSTORM: $THIS: prep $ADVISDIR $INPUTDIR $ENSTORM $START $OLDADVISDIR $HPCENVSHORT $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT '$OUTPUTOPTIONS' $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE"
      logMessage "$ENSTORM: $THIS: prep $ADVISDIR $INPUTDIR $ENSTORM $START $FROMDIR $HPCENVSHORT $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT '$OUTPUTOPTIONS' $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE"
      prep $ADVISDIR $INPUTDIR $ENSTORM $START $FROMDIR $HPCENVSHORT $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT "$OUTPUTOPTIONS" $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE
      THIS="asgs_main.sh"
      # check to see that adcprep did not conspicuously fail
      handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV $STORMNAME $YEAR $STORMDIR $ADVISORY $LASTADVISORYNUM $STATEFILE $GRIDFILE $EMAILNOTIFY "${JOB_FAILED_LIST}" $ARCHIVEBASE $ARCHIVEDIR
      THIS="asgs_main.sh"
      # if handleFailedJob has detected a problem, it will rename the
      # nowcast directory; therefore, the non-existence of the nowcast
      # directory is evidence that something has gone wrong in prep
      if [[ ! -d $NOWCASTDIR ]]; then
         continue  # abandon this nowcast and wait for the next one
      fi

      JOBTYPE=padcirc
      if [[ $QUEUESYS = "serial" ]]; then
         JOBTYPE=adcirc
      fi
      HOTSWAN=on
      if [[ $WAVES = on ]]; then
         JOBTYPE=padcswan
         if [[ $QUEUESYS = "serial" ]]; then
            JOBTYPE=adcswan
         fi
      fi
      echo "hpc.job.${JOBTYPE}.limit.walltime : $NOWCASTWALLTIME" >> $ADVISDIR/$ENSTORM/run.properties
      # then submit the job
      logMessage "$ENSTORM: $THIS: Submitting $ENSTORM job."

      cd $ADVISDIR/$ENSTORM 2>> ${SYSLOG}
      #
      executeHookScripts "SUBMIT_NOWCAST_SCENARIO"
      consoleMessage "$I $(head fort.15 | awk 'NR==2 { print $0 }')"
      #
      logMessage "$ENSTORM: $THIS: submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $HPCENVSHORT $ACCOUNT $PPN $NUMWRITERS $HOTSTARTCOMP $NOWCASTWALLTIME $JOBTYPE"
      writeJobResourceRequestProperties ${ADVISDIR}/${ENSTORM}

      submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $HPCENVSHORT $ACCOUNT $PPN $NUMWRITERS $HOTSTARTCOMP $NOWCASTWALLTIME $JOBTYPE
      THIS="asgs_main.sh"
      # check once per minute until all jobs have finished
      monitorJobs $QUEUESYS ${JOBTYPE} ${ENSTORM} $NOWCASTWALLTIME
      THIS="asgs_main.sh"
      # check to see that the nowcast job did not conspicuously fail
      handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV $STORMNAME $YEAR $STORMDIR $ADVISORY $LASTADVISORYNUM $STATEFILE $GRIDFILE $EMAILNOTIFY "${JOB_FAILED_LIST}" $ARCHIVEBASE $ARCHIVEDIR
      THIS="asgs_main.sh"
      if [[ ! -d $NOWCASTDIR ]]; then
         continue # abandon this nowcast and wait for the next one
      fi

      # nowcast finished, get on with it
      logMessage "$ENSTORM: $THIS: Nowcast run finished."

      # archive nowcast
      logMessage "$ENSTORM: $THIS: Initiating nowcast archival process, if any."

      # log the start time of the archiving script so that downstream processes
      # (e.g., a subsequent nowcast or forecast run) know that this process is
      # underway and that the final tar and/or globalized swan hotstart files
      # are still under construction
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
      echo "time.archive.start : $DATETIME" >> ${STORMDIR}/run.properties
      ${SCRIPTDIR}/archive/${ARCHIVE} $CONFIG $ADVISDIR $STORM $YEAR $ADVISORY $HPCENVSHORT $ENSTORM $CSDATE $HSTIME $GRIDFILE $OUTPUTDIR $SYSLOG $SSHKEY >> ${SYSLOG} 2>&1
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
      echo "time.archive.finish : $DATETIME" >> ${STORMDIR}/run.properties
      THIS="asgs_main.sh"
      logMessage "$ENSTORM: $THIS: Nowcast complete for advisory '$ADVISORY.'"
      #
      executeHookScripts "FINISH_NOWCAST_SCENARIO"
      cd $ADVISDIR 2>> ${SYSLOG}
   else
      # we didn't run the nowcast, because our latest nowcast data end
      # at the same time as the previous nowcast data, so we can just use
      # the prior cycle's nowcast hotstart file
      logMessage "$ENSTORM: $THIS: The nowcast data end at the same time as the hindcast/nowcast data from the previous cycle. As a result, a nowcast will not be run on this cycle; this cycle's forecast(s) will be hotstarted from the hindcast/nowcast of the previous cycle."
      logMessage "$ENSTORM: $THIS: Skipping the submission of the nowcast job and proceeding directly to the forecast(s)."
      NOWCASTDIR=$FROMDIR
   fi

   # write the ASGS state file
   if [[ $hotstartURL != "null" ]]; then
      hotstartURL=null
   fi
   LUN=67  # asgs always tells adcirc to read a 68 file and write a 67 file
   logMessage "Detecting LASTSUBDIR from NOWCASTDIR ${NOWCASTDIR}."
   LASTSUBDIR=`echo $NOWCASTDIR | sed 's/\/nowcast//g ; s/\/hindcast//g'`
   logMessage "RUNDIR is $RUNDIR STATEFILE is $STATEFILE SYSLOG is $SYSLOG" #jgfdebug
   echo RUNDIR=${RUNDIR} > $STATEFILE 2>> ${SYSLOG}
   echo SCRIPTDIR=${SCRIPTDIR} >> $STATEFILE 2>> ${SYSLOG}
   echo LASTSUBDIR=${LASTSUBDIR} >> $STATEFILE 2>> ${SYSLOG}
   echo SYSLOG=${SYSLOG} >> $STATEFILE 2>> ${SYSLOG}
   echo ADVISORY=${ADVISORY} >> $STATEFILE 2>> ${SYSLOG}
   SCENARIOLOG=null
   #
   executeHookScripts "FINISH_NOWCAST_STAGE"
   #
   #  F O R E C A S T
   #
   stage="FORECAST"  # modelling phase : SPINUP, NOWCAST, or FORECAST
   #
   executeHookScripts "START_FORECAST_STAGE"
   #
   ENSTORM="forecast"
   logMessage "$ENSTORM: $THIS: Starting forecast scenarios for advisory '$ADVISORY'."

   checkHotstart $NOWCASTDIR $HOTSTARTFORMAT 67
   THIS="asgs_main.sh"
   if [[ $HOTSTARTFORMAT == "netcdf" || $HOTSTARTFORMAT == "netcdf3" ]]; then
      HSTIME=`$ADCIRCDIR/hstime -f ${NOWCASTDIR}/fort.67.nc -n` 2>> ${SYSLOG}
   else
      HSTIME=`$ADCIRCDIR/hstime -f ${NOWCASTDIR}/PE0000/fort.67` 2>> ${SYSLOG}
   fi
   logMessage "$ENSTORM: $THIS: The time in the hotstart file is '$HSTIME' seconds."
   si=0
   while [ $si -lt $SCENARIOPACKAGESIZE ]; do
      # source config file to pick up any configuration changes, or any
      # config that is specific to forecasts, and set up the current
      # scenario
      ENSTORM=forecast
      # grab the config specified by the operator
      readConfig
      SCENARIO=$ENSTORM
      executeHookScripts "INITIALIZE_FORECAST_SCENARIO" # now that we know the name of the scenario
      consoleMessage "$I Scenario '$SCENARIO'"
      nullifyFilesFirstTimeUpdated  # for monitoring the first modification time of files
      THIS=asgs_main.sh
      # write the properties associated with asgs configuration to the
      # run.properties file
      writeProperties $RUNDIR 2>> $SYSLOG
      if [[ $TROPICALCYCLONE = on ]]; then
         writeTropicalCycloneProperties $RUNDIR
      fi
      if [[ $BACKGROUNDMET != off ]]; then
         if [[ $BACKSITE == "filesystem" ]]; then
            logMessage "$ENSTORM: $THIS: BACKGROUNDMET is set to '$BACKGROUNDMET' but BACKSITE is set to '$BACKSITE'."
            logMessage "$ENSTORM: $THIS: The ASGS does not support the construction of forecast scenarios from gridded meteorology stored on the local filesystem."
            si=$[$si + 1]
            if [[ $si -ge $SCENARIOPACKAGESIZE ]]; then
               logMessage "$ENSTORM: $THIS: This is the last forecast scenario for this cycle. Proceeding to the next nowcast cycle."
            else
               logMessage "$ENSTORM: $THIS: Skipping this forecast scenario and proceeding to the next one."
            fi
            continue
         fi
         case $BACKGROUNDMET in
            "on"|"NAM")
               writeNAMProperties $RUNDIR
               echo "forcing.nwp.year : ${ADVISORY:0:4}" >> $RUNDIR/run.properties
	       ;;
            "GFS")
               writeGFSProperties $RUNDIR
               echo "forcing.nwp.year : ${ADVISORY:0:4}" >> $RUNDIR/run.properties
               ;;
            *) # other values are allowed but don't have properties that need to be written in advance
               ;;
         esac
      fi
      if [[ $WAVES = on ]]; then
         writeWaveCouplingProperties $RUNDIR
      fi
      JOBTYPE=padcirc
      if [[ $QUEUESYS = "serial" ]]; then
         JOBTYPE=adcirc
      fi
      HOTSWAN=on
      if [[ $WAVES = on ]]; then
         JOBTYPE=padcswan
         if [[ $QUEUESYS = "serial" ]]; then
            JOBTYPE=adcswan
         fi
      fi
      # Check for a misconfiguration where the Operator has set the
      # number of CPUs and number of writers greater than the total
      # number of CPUs that will ever be available.
      if [[ $(($NCPU + $NUMWRITERS)) -gt $NCPUCAPACITY ]]; then
         error "$ENSTORM: $THIS: The requested number of CPUs for $ENSTORM is set to $NCPU and the number of writer processors has been set to $NUMWRITERS but the total number of requested CPUs exceeds the NCPUCAPACITY parameter value of ${NCPUCAPACITY}; therefore this scenario will never be able to execute. This scenario is being abandoned."
         # increment the scenario package counter
         si=$[$si + 1]
         continue
      fi
      subDirs=`find ${ADVISDIR} -maxdepth 1 -type d -print`
      #debugMessage "subDirs is $subDirs" # jgfdebug
      if [[ ! -z $subDirs ]]; then  # see if we have any scenario directories
         #
         executeHookScripts "CAPACITY_WAIT"
         #
         # continuously loop to see if conditions are right to submit the next job
         while [ true ]; do
            # check to see if the deadline has passed for submitting
            # forecast jobs for this cycle.
            if [[ $forecastSelection = "latest" ]]; then
               check=$(checkTimeLimit "$cycleStartTime" "$CYCLETIMELIMIT")
               if [[ "$check" -eq 1 ]]; then
                  THIS="asgs_main.sh"
                  DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
                  warn "[${DATETIME}] $ENSTORM: $THIS: The deadline for submitting jobs ($CYCLETIMELIMIT) has passed for this cycle."
                  break 2
               fi
            fi
            # total up the number of cpus currently engaged and compare with capacity
            cpusEngaged=0
            for ensembleMemDir in $subDirs; do
               # ignore the nowcast and the advisory directory itself
               if [[ $ensembleMemDir = $ADVISDIR || $ensembleMemDir = "./nowcast" || $ensembleMemDir = "." || $ensembleMemDir = "$ADVISDIR/nowcast" ]]; then
                  #debugMessage "ensembleMemDir $ensembleMemDir is the same as ADVISDIR $ADVISDIR" #jgfdebug
                  continue
               fi
               # parse the run.properties to see what the cpu request is for this job
               # ... if the was never submitted, there won't be a cpurequest property
               cpuRequest=$(${SCRIPTDIR}/metadata.pl --all                \
                  < ${ensembleMemDir}/run.properties 2>> $SYSLOG          \
                  | bashJSON.pl --key cpurequest 2>> $SYSLOG)
               if [[ -z $cpuRequest ]]; then
                  continue   # this job was never submitted, so doesn't count; go to the next directory
               fi
               # parse out the name of the scenario
               ensembleMemName=`basename $ensembleMemDir`
               runType=`awk '$1 == "Model" { print tolower($3) }' ${ensembleMemDir}/run.properties`
               # look to see if the job ended (either success or failure)
               if [[ ! -e $ensembleMemDir/${runType}.${ensembleMemName}.run.finish && ! -e $ensembleMemDir/${runType}.${ensembleMemName}.run.error ]]; then
                  # job is still going, add its cpus to the total that are currently engaged
                  cpusEngaged=$(($cpusEngaged + $cpuRequest))
               fi
            done
            debugMessage "$ENSTORM: $THIS: The next scenario ('$ENSTORM') requires $NCPU compute cores and $NUMWRITERS dedicated writer cores. The number of CPUs currently engaged is $cpusEngaged. The max number of cores that can be engaged is $NCPUCAPACITY."
            if [[ $(($NCPU + $NUMWRITERS + $cpusEngaged)) -le $NCPUCAPACITY ]]; then
               #debugMessage "Sufficient capacity exists to run the next job."
               break      # we now have the spare capacity to run this scenario
            else
               logMessage "$ENSTORM: $THIS: Insufficient capacity to submit the next job. Sleeping for 1 minute."
               spinner 60   # not enough cores available; sleep for a minute, then recheck/recalculate
            fi
         done
      fi

      THIS="asgs_main.sh"
      #
      executeHookScripts "BUILD_FORECAST_SCENARIO"

      #
      # turn SWAN hotstarting on or off as appropriate
      HOTSWAN=off
      if [[ $WAVES = on && $REINITIALIZESWAN = no ]]; then
         # look for a swan hotstart file
         for swanhsfile in PE0000/swan.67 swan.67; do
            if [[ -e $FROMDIR/$swanhsfile ]]; then
               HOTSWAN=on
               logMessage "Found SWAN hotstart file $FROMDIR/${swanhsfile}."
               break
            fi
            for swanhssuffix in tar.gz tar.bz2 gz bz2; do
               if [[ -e $FROMDIR/${swanhsfile}.${swanhssuffix} ]]; then
                  HOTSWAN=on
                  logMessage "Found SWAN hotstart file $FROMDIR/${swanhsfile}."
                  break
               fi
            done
         done
      fi
      # make the scenario directory and move the run.properties file into it
      STORMDIR=$ADVISDIR/$ENSTORM
      if [ ! -d $STORMDIR ]; then
         mkdir $STORMDIR 2>> ${SYSLOG}
      fi
      cd $STORMDIR 2>> ${SYSLOG}
      SCENARIODIR=$STORMDIR
      SCENARIOLOG=$SCENARIODIR/scenario.log
      mv $RUNDIR/run.properties $SCENARIODIR 2>> $SYSLOG
      writeScenarioProperties $SCENARIODIR 2>> $SYSLOG

      echo "forecast.scenario.number : $si" >> ${SCENARIODIR}/run.properties
      RUNFORECAST=yes
      # TROPICAL CYCLONE ONLY
      if [[ $TROPICALCYCLONE = on ]]; then
         BASENWS=20
         if [[ $VORTEXMODEL = ASYMMETRIC ]]; then
            BASENWS=19
         fi
         if [[ $VORTEXMODEL = SYMMETRIC ]]; then
            BASENWS=8
         fi
         NWS=$BASENWS
         if [[ $WAVES = on ]]; then
            NWS=$(($BASENWS + 300))
         fi
         METOPTIONS=" --dir $ADVISDIR --storm $STORM --year $YEAR --coldstartdate $CSDATE --hotstartseconds $HSTIME --nws $NWS --name $ENSTORM $STORMTRACKOPTIONS"
         if [[ ${PERCENT} != default ]]; then
            echo "modified : y" >> run.properties 2>> ${SYSLOG}
            echo "track_modified : fort.22" >> run.properties 2>> ${SYSLOG}
            if [[ ! $ENSTORM =~ maxWindSpeedOnly ]]; then
               METOPTIONS="$METOPTIONS --percent ${PERCENT}"
            fi
         else
            echo "modified : n" >> run.properties 2>> ${SYSLOG}
            echo "track_modified : n" >> run.properties 2>> ${SYSLOG}
         fi
         writeTropicalCycloneForecastProperties $STORMDIR
         CONTROLOPTIONS="--cst $CSDATE --advisorynum $ADVISORY --hst $HSTIME --metfile ${STORMDIR}/fort.22 --name $ENSTORM --hsformat $HOTSTARTFORMAT"
         logMessage "$ENSTORM: $THIS: Generating ADCIRC Met File (fort.22) for $ENSTORM with the following options: $METOPTIONS."
         ${SCRIPTDIR}/storm_track_gen.pl $METOPTIONS >> ${SYSLOG} 2>&1
         tcEnd=$(grep "forcing.tropicalcyclone.best.time.end" run.properties | sed 's/forcing.tropicalcyclone.best.time.end.*://' | sed 's/^\s//' 2>> ${SYSLOG})
         CONTROLOPTIONS="$CONTROLOPTIONS --endtime $tcEnd"
         if [[ $BASENWS = 19 || $BASENWS = 20 ]]; then
            # create a new file that contains metadata and has the Rmax
            # in it already ... potentially with Rmax changes if desired
            ASWIPOPTIONS=""
            if [[ ${PERCENT} != default && $ENSTORM =~ maxWindSpeedOnly ]]; then
               ASWIPOPTIONS="-X ${PERCENT}"
               echo "variation maxWindSpeedOnly : ${PERCENT}" >> run.properties 2>> ${SYSLOG}
            fi
            if [[ ${PERCENT} != default && ${RMAX} = scaled ]]; then
               ASWIPOPTIONS="-P ${PERCENT}"
               echo "variation rMax : ${PERCENT}" >> run.properties 2>> ${SYSLOG}
            fi
            if [[ ${RMAX} != default && ${RMAX} != scaled ]]; then
               ASWIPOPTIONS="${ASWIPOPTIONS} -R ${RMAX}"
               echo "variation constant rMax : ${PERCENT}" >> run.properties 2>> ${SYSLOG}
               echo "modified : y" >> run.properties 2>> ${SYSLOG}
            fi
            logMessage "$ENSTORM: $THIS: Running aswip fort.22 preprocessor for $ENSTORM with the following options: $ASWIPOPTIONS."
            $ADCIRCDIR/aswip -n $BASENWS $ASWIPOPTIONS >> ${SYSLOG} 2>&1
            if [[ -e NWS_${BASENWS}_fort.22 ]]; then
               mv fort.22 fort.22.orig 2>> ${SYSLOG}
               cp NWS_${BASENWS}_fort.22 fort.22 2>> ${SYSLOG}
            fi
         fi
      fi
      # BACKGROUND METEOROLOGY ONLY
      if [[ $BACKGROUNDMET == "on" || $BACKGROUNDMET == "NAM" || $BACKGROUNDMET == "GFS" ]]; then
         NWS=-12
         if [[ $WAVES == "on" ]]; then
            NWS=-312
         fi
         logMessage "$ENSTORM: $THIS: $START $ENSTORM cycle $ADVISORY."
         # determine whether a forecast was specified for this cycle
         forecastCyclesArray=( ${FORECASTCYCLE//,/ } )
         cycleHour=${ADVISORY:8:2}
         logMessage "$ENSTORM: $THIS: The specified FORECASTCYCLE includes '$FORECASTCYCLE' and this cycleHour is '$cycleHour'."
         runme=0
         for fc in ${forecastCyclesArray[@]} ; do
            if [[ $cycleHour -eq $fc ]]; then
               runme=1
               break
            fi
         done
         if [[ $runme -eq 0 ]]; then
            # increment the scenario package counter
            si=$((si + 1))
            if [[ $si -ge $SCENARIOPACKAGESIZE ]]; then
               logMessage "${ADVISORY}.${SCENARIO}: $THIS: This forecast will not be run. Moving on to seeking the next nowcast."
            else
               logMessage "${ADVISORY}.${SCENARIO}: $THIS: This forecast will not be run. Moving on to the next forecast scenario."
            fi
            continue # no reason to continue processing this NAM forecast scenario at this point if it will not be run
         fi
         # download and convert met files to OWI format
         logMessage "$ENSTORM: $THIS: Downloading background meteorology."
         if [[ $BACKGROUNDMET == "on" || $BACKGROUNDMET == "NAM" ]]; then
            logMessage "$ENSTORM: $THIS: downloadBackgroundMet $SCENARIODIR $RUNDIR $SCRIPTDIR $BACKSITE $BACKDIR $ENSTORM $CSDATE $HSTIME $FORECASTLENGTH $ALTNAMDIR $FORECASTCYCLE $ARCHIVEBASE $ARCHIVEDIR $STATEFILE"
            downloadBackgroundMet $SCENARIODIR $RUNDIR $SCRIPTDIR $BACKSITE $BACKDIR $ENSTORM $CSDATE $HSTIME $FORECASTLENGTH $ALTNAMDIR $FORECASTCYCLE $ARCHIVEBASE $ARCHIVEDIR $STATEFILE
         else
            # GFS
            downloadGFS $SCENARIODIR $RUNDIR $SCRIPTDIR $GFSBACKSITE $GFSBACKDIR $ENSTORM $CSDATE $HSTIME $GFSFORECASTLENGTH $ALTNAMDIR $FORECASTCYCLE $ARCHIVEBASE $ARCHIVEDIR $STATEFILE
         fi
         THIS="asgs_main.sh"
         cd $SCENARIODIR 2>> ${SYSLOG}
         if [[ $BACKGROUNDMET == "on" || $BACKGROUNDMET == "NAM" ]]; then
            boolApplyRamp=false
            if [[ $SPATIALEXTRAPOLATIONRAMP == "yes" ]]; then
               boolApplyRamp=true
            fi
            ptFilePath=${SCRIPTDIR}/input/ptFile_oneEighth.txt
            escPtFilePath=${ptFilePath////'\/'}
            escSCENARIODIR=${SCENARIODIR////'\/'}
            cp $RUNDIR/get_nam_data.pl.* $SCENARIODIR 2>> $SYSLOG
            sed \
               -e "s/%NULLNAMWINPREDATAPATH%/$escSCENARIODIR/" \
               -e "s/%NULLNAMWINPREGRID%/$escPtFilePath/" \
               -e "s/\"%NULLNAMAWIPGRID%\"/218/" \
               -e "s/%NULLNAMRAWFORMAT%/grib2/" \
               -e "s/\"%NULLVELMULT%\"/$VELOCITYMULTIPLIER/" \
               -e "s/\"%NULLPRESSMULT%\"/0.01/" \
               -e "s/\"%NULLAPPLYRAMP%\"/$boolApplyRamp/" \
               -e "s/\"%NULLRAMPDIST%\"/$SPATIALEXTRAPOLATIONRAMPDISTANCE/" \
               < get_nam_data.pl.json \
               | $SCRIPTDIR/NAMtoOWIRamp.pl \
               > NAMtoOWIRamp.pl.json
            2>> $SYSLOG
            preFile=$(bashJSON.pl --key winPrePressureFile < NAMtoOWIRamp.pl.json 2>> $SYSLOG)
            winFile=$(bashJSON.pl --key winPreVelocityFile < NAMtoOWIRamp.pl.json 2>> $SYSLOG)
            WTIMINC=$(bashJSON.pl --key winPreWtimincSeconds < NAMtoOWIRamp.pl.json)
            tlimits=( $( head -n 1 $preFile | awk '{ print $(NF-1)" "($NF) }' ) )
            owiWinPre["startDateTime"]=${tlimits[0]}
            owiWinPre["endDateTime"]=${tlimits[1]}
            # determine the number of blank snaps (if any)
            # epoch seconds associated with cold start and hotstart times
            date=${tlimits[0]}
            csEpochSeconds=$(TZ=UTC date -u -d "${CSDATE:0:4}-${CSDATE:4:2}-${CSDATE:6:2} ${CSDATE:8:2}:00:00" "+%s" 2>>$SYSLOG)
            hsEpochSeconds=$((csEpochSeconds + ${HSTIME%.*}))
            owiStartEpochSeconds=$(TZ=UTC date -u -d "${date:0:4}-${date:4:2}-${date:6:2} ${date:8:2}:00:00" "+%s" 2>>$SYSLOG)
            owiWinPre["NWBS"]=$(echo "scale=0; ($owiStartEpochSeconds - $hsEpochSeconds)/$WTIMINC" | bc)
            # copy log data to scenario.log
            for file in lambert_diag.out reproject.log ; do
               if [[ -e $ADVISDIR/$file ]]; then
                  scenarioMessage "$ENSTORM: $THIS: $file is as follows:"
                  cat $ADVISDIR/$file >> $SCENARIOLOG 2>> $SYSLOG
               fi
            done
            # create links to the OWI files
            ln -s $(basename $preFile) fort.221 2>> ${SYSLOG}
            ln -s $(basename $winFile) fort.222 2>> ${SYSLOG}
            fort22="fort.22"
            echo "${owiWinPre["NWSET"]} ! NWSET" > $fort22
            echo "${owiWinPre["NWBS"]} ! NWBS"  >> $fort22
            echo "${owiWinPre["DWM"]} ! DWM"    >> $fort22
         fi
         CONTROLOPTIONS=" --advisorynum $ADVISORY --name $ENSTORM --cst $CSDATE --hstime $HSTIME --hsformat $HOTSTARTFORMAT"
      fi
      # if there is no forcing from an external data source, set control options
      if [[ $NOFORCING = true ]]; then
         CONTROLOPTIONS="--advisorynum $ADVISORY"
         CONTROLOPTIONS="${CONTROLOPTIONS} --specifiedRunLength $FORECASTDAYS"
         CONTROLOPTIONS="${CONTROLOPTIONS} --name $ENSTORM --cst $CSDATE --hstime $HSTIME --hsformat $HOTSTARTFORMAT"
      fi
      if [[ $WAVES = on ]]; then
         CONTROLOPTIONS="${CONTROLOPTIONS} --swantemplate ${SCRIPTDIR}/input/meshes/common/swan/${SWANTEMPLATE} --hotswan $HOTSWAN"
      fi
      CONTROLOPTIONS="${CONTROLOPTIONS} --elevstations ${INPUTDIR}/${ELEVSTATIONS} --velstations ${INPUTDIR}/${VELSTATIONS} --metstations ${INPUTDIR}/${METSTATIONS}"
      CONTROLOPTIONS="$CONTROLOPTIONS --gridname $GRIDNAME" # for run.properties
      CONTROLOPTIONS="$CONTROLOPTIONS --nscreen $NSCREEN"
      runLength=$(echo "scale=2; ($HSTIME)/86400" | bc)
      # uses parameters described above as well as control-parameters.yaml
      # to generate tide_fac.out, fort.13, fort.15, and fort.26
      controlExitStatus=0
      controlMsg=""
      generateDynamicInput
      THIS="asgs_main.sh"
      if [[ $controlExitStatus -ne 0 ]]; then
         handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV $STORMNAME $YEAR $STORMDIR $ADVISORY $LASTADVISORYNUM $STATEFILE $GRIDFILE $EMAILNOTIFY "${JOB_FAILED_LIST}" $ARCHIVEBASE $ARCHIVEDIR
      fi
      THIS="asgs_main.sh"
      #
      # if the $STORMDIR is not there, it is probably because handleFailedJob has
      # moved it ... should we increment the scenario counter and just go on?
      # ... just using "continue" as below will have the effect of retrying this
      # forecast scenario
      if [[ ! -d $STORMDIR ]]; then
         # increment the scenario package counter
         si=$((si + 1))
         if [[ $si -ge $SCENARIOPACKAGESIZE ]]; then
            logMessage "${ADVISORY}.${SCENARIO}: $THIS: This forecast will not be run. Moving on to seeking the next nowcast."
         else
            logMessage "${ADVISORY}.${SCENARIO}: $THIS: This forecast will not be run. Moving on to the next forecast scenario."
         fi
         continue # no reason to continue processing this NAM forecast scenario at this point if it will not be run
      fi
      # get river flux nowcast data, if configured to do so
      if [[ $VARFLUX = on ]]; then
         downloadRiverFluxData $ADVISDIR ${INPUTDIR}/${GRIDFILE} $RIVERSITE $RIVERDIR $RIVERUSER $RIVERDATAPROTOCOL $ENSTORM $CSDATE $HSTIME $SCRIPTDIR ${INPUTDIR}/${RIVERFLUX} $USERIVERFILEONLY
         THIS="asgs_main.sh"
      fi
      if [[ $VARFLUX = default ]]; then
         ln -s ${INPUTDIR}/${RIVERFLUX} ./fort.20 2>> ${SYSLOG}
      fi
      if [[ -e ${INPUTDIR}/$MESHPROPERTIES ]]; then
         cat ${INPUTDIR}/$MESHPROPERTIES >> $SCENARIODIR/run.properties
      else
         logMessage "$ENSTORM: $THIS: The properties file ${INPUTDIR}/$MESHPROPERTIES was not found and will not be added to the run.properties file."
      fi
      # recording the scenario number may come in handy for load
      # balancing the postprocessing, particularly for CERA
      # copy log data to scenario.log
      for file in lambert_diag.out reproject.log ; do
         if [[ -e $ADVISDIR/$file ]]; then
            scenarioMessage "$ENSTORM: $THIS: $file is as follows:"
            cat $ADVISDIR/$file >> $SCENARIODIR/scenario.log
         fi
      done
      writeJobResourceRequestProperties $SCENARIODIR 2>> $SYSLOG
      # set up post processing for the forecast, including initiation
      # of real time post processing
      ${OUTPUTDIR}/${INITPOST} $CONFIG $ADVISDIR $STORM $YEAR $ADVISORY $HPCENVSHORT $ENSTORM $CSDATE $HSTIME $GRIDFILE $OUTPUTDIR $SYSLOG $SSHKEY >> ${SYSLOG} 2>&1
      # preprocess
      checkArchiveFreshness $PREPPEDARCHIVE $HINDCASTARCHIVE $GRIDFILE $CONTROLTEMPLATE $ELEVSTATIONS $VELSTATIONS $METSTATIONS $NAFILE $INPUTDIR
      THIS="asgs_main.sh"
      logMessage "$ENSTORM: $THIS: Starting $ENSTORM preprocessing with the following command: prep $ADVISDIR $INPUTDIR $ENSTORM $START $NOWCASTDIR $HPCENVSHORT $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT "$OUTPUTOPTIONS" $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE"
      prep $ADVISDIR $INPUTDIR $ENSTORM $START $NOWCASTDIR $HPCENVSHORT $NCPU $PREPPEDARCHIVE $GRIDFILE $ACCOUNT "$OUTPUTOPTIONS" $HOTSTARTCOMP $ADCPREPWALLTIME $HOTSTARTFORMAT $MINMAX $HOTSWAN $NAFILE
      THIS="asgs_main.sh"
      handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV $STORMNAME $YEAR $STORMDIR $ADVISORY $LASTADVISORYNUM $STATEFILE $GRIDFILE $EMAILNOTIFY "${JOB_FAILED_LIST}" $ARCHIVEBASE $ARCHIVEDIR
      THIS="asgs_main.sh"
      # if the prep task was successful, the scenario directory will still be there
      if [[ ! -d $STORMDIR ]]; then
         # increment the scenario package counter
         si=$((si + 1))
         if [[ $si -ge $SCENARIOPACKAGESIZE ]]; then
            logMessage "${ADVISORY}.${SCENARIO}: $THIS: This forecast will not be run. Moving on to seeking the next nowcast."
         else
            logMessage "${ADVISORY}.${SCENARIO}: $THIS: This forecast will not be run. Moving on to the next forecast scenario."
         fi
         continue # no reason to continue processing this NAM forecast scenario at this point if it will not be run
      fi
      JOBTYPE=padcirc
      if [[ $QUEUESYS = "serial" ]]; then
         JOBTYPE=adcirc
      fi
      if [[ $WAVES = on ]]; then
         JOBTYPE=padcswan
         if [[ $QUEUESYS = "serial" ]]; then
            JOBTYPE=adcswan
         fi
      fi
      # then submit the job
      logMessage "$ENSTORM: $THIS: Submitting scenario package member ${ENSTORM}."
      writeJobResourceRequestProperties $SCENARIODIR

      echo "hpc.job.${JOBTYPE}.limit.walltime : $FORECASTWALLTIME" >> $ADVISDIR/$ENSTORM/run.properties
      #
      executeHookScripts "SUBMIT_FORECAST_SCENARIO"
      consoleMessage "$I $(head fort.15 | awk 'NR==2 { print $0 }')"
      submitJob $QUEUESYS $NCPU $ADCIRCDIR $ADVISDIR $SCRIPTDIR $INPUTDIR $ENSTORM $HPCENVSHORT $ACCOUNT $PPN $NUMWRITERS $HOTSTARTCOMP $FORECASTWALLTIME $JOBTYPE
      THIS="asgs_main.sh"
      # monitor for completion and post process in a subshell running
      # in the background ... this allows us to go on to the
      # next scenario
      (
         monitorJobs $QUEUESYS ${JOBTYPE} ${ENSTORM} $FORECASTWALLTIME
         THIS="asgs_main.sh"
         handleFailedJob $RUNDIR $ADVISDIR $ENSTORM ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV $STORMNAME $YEAR $STORMDIR $ADVISORY $LASTADVISORYNUM $STATEFILE $GRIDFILE $EMAILNOTIFY "${JOB_FAILED_LIST}" $ARCHIVEBASE $ARCHIVEDIR
         THIS="asgs_main.sh"
         # only attempt post processing if this scenario
         # ended successfully
         if [[ -d $STORMDIR ]]; then
            logMessage "$ENSTORM: $THIS: The $ENSTORM job ended successfully. Starting postprocessing."
            DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
            echo "time.post.start : $DATETIME" >> ${STORMDIR}/run.properties
            scriptIndex=0
            for script in "${POSTPROCESS[@]}" ; do
               logMessage "$SCENARIO: $THIS: Executing POSTPROCESS hook ${OUTPUTDIR}/$script."
               com="${OUTPUTDIR}/$script $CONFIG $ADVISDIR $STORM $YEAR $ADVISORY $HPCENV $ENSTORM $CSDATE $HSTIME $GRIDFILE $OUTPUTDIR $SYSLOG $SSHKEY >> ${SYSLOG} 2>&1"
               $com
            done
            DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
            echo "time.post.finish : $DATETIME" >> ${STORMDIR}/run.properties
            # notify analysts that new results are available
            ${OUTPUTDIR}/${NOTIFY_SCRIPT} $HPCENV $STORM $YEAR $STORMDIR $ADVISORY $ENSTORM $GRIDFILE results $EMAILNOTIFY $SYSLOG "${POST_LIST}" $ARCHIVEBASE $ARCHIVEDIR >> ${SYSLOG} 2>&1
            # archive the files for this scenario
            logMessage "$ENSTORM: $THIS: Initiating archival process, if any."
            ${SCRIPTDIR}/archive/${ARCHIVE} $CONFIG $ADVISDIR $STORM $YEAR $ADVISORY $HPCENVSHORT $ENSTORM $CSDATE $HSTIME $GRIDFILE $OUTPUTDIR $SYSLOG $SSHKEY >> ${SYSLOG} 2>&1
            DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
            echo "time.archive.finish : $DATETIME" >> ${STORMDIR}/run.properties
         fi
      ) &
      si=$((si + 1))
   done
   #
   SCENARIOLOG=null
   THIS="asgs_main.sh"
   # allow all scenarios and associated post processing to complete
   logMessage "$ENSTORM: $THIS: Finished with forecast scenarios for cycle '$ADVISORY'."
   #
   consoleMessage "$I Finished with forecast scenarios for cycle '$ADVISORY'."
   executeHookScripts "FINISH_FORECAST_STAGE"
   #
   LASTSUBDIR=null # don't need this any longer
   # if we ran the nowcast on this cycle, then this cycle's nowcast becomes
   # the basis for the next cycle; on the other hand, if we used a previous
   # nowcast as the basis for this cycle, then that previous nowcast will
   # also have to be the basis for the next cycle
   if [[ $RUNNOWCAST = yes ]]; then
      OLDADVISDIR=$ADVISDIR
   fi
   if [[ $ONESHOT = yes || $NOFORCING = true || $TRIGGER = auto ]]; then
      exit $OK  # exit because the ASGS will be started again later
   fi
   CYCLELOG=null
done

