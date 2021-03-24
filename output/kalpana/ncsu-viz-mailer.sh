#!/bin/bash

logMessage()
{
  dateTime=`date +'%Y-%m-%d %H:%M:%S'`
  message="${dateTime} $@"
  echo ${message} >> ${sysLog}
}

findFiles()
{
  workingDateTime=$1
# This is where the startDateTime is used
  logMessage "Searching for NCFS results newer than ${workingDateTime}."
  found0="false"
  until [[ $found0 == "true" ]]; do
    found1="false"
    until [[ $found1 == "true" ]]; do
      found4="false"
      until [[ $found4 == "true" ]]; do
        wget -N -q http://tds.renci.org:8080/thredds/fileServer/2019/catalog.tree.tc
        if [[ ! $? == 0 ]]; then
          logMessage "Unable to download the catalog.tree.tc from the server; retrying now."
          sleep 10
        else
          fileSize=$(stat -c%s "catalog.tree.tc")
          if [[ $fileSize == "0" ]]; then
            logMessage "The catalog.tree.tc has nothing in it; retrying now."
            sleep 10
          else
            logMessage "The catalog.tree.tc was downloaded successfully."
            found4="true"
          fi
        fi
      done
      sed -e '1,3d' catalog.tree.tc > junk
      mv junk catalog.tree.tc
      storm=()
      advisory=()
      mesh=()
      machine=()
      instance=()
      ensemble=()
      createDate=()
      counter="0"
      while IFS='$' read f0 f1 f2 f3 f4 f5 f6; do
        # Remove leading and trailing blanks from these strings.
        f0=`echo "${f0}" | sed 's/^[ \t]*//;s/[ \t]*$//'`
        f1=`echo "${f1}" | sed 's/^[ \t]*//;s/[ \t]*$//'`
        f2=`echo "${f2}" | sed 's/^[ \t]*//;s/[ \t]*$//'`
        f3=`echo "${f3}" | sed 's/^[ \t]*//;s/[ \t]*$//'`
        f4=`echo "${f4}" | sed 's/^[ \t]*//;s/[ \t]*$//'`
        f5=`echo "${f5}" | sed 's/^[ \t]*//;s/[ \t]*$//'`
        f6=`echo "${f6}" | sed 's/^[ \t]*//;s/[ \t]*$//'`
        # Save strings into arrays.
        storm[counter]=${f0}
        advisory[counter]=${f1}
        mesh[counter]=${f2}
        machine[counter]=${f3}
        instance[counter]=${f4}
        ensemble[counter]=${f5}
        createDate[counter]=${f6}
        if [[ $counter == "0" ]]; then
          tempDate=${createDate[counter]}
          tempDate=`date -d "${tempDate}" +'%Y%m%d%H%M%S'`
          if [[ $tempDate -gt $workingDateTime ]]; then
            found2="true"
          else
            found2="false"
            break
          fi
        fi
        counter=`expr ${counter} + 1`
      done < catalog.tree.tc
      if [[ $found2 == "true" ]]; then
        counter=${#createDate[@]}
        logMessage "The catalog.tree.tc contains ${counter} listings."
        logMessage "Searching listings for new results."
        counter=`expr ${counter} - 1`
        found3="false"
        until [[ $counter == "-1" || $found3 == "true" ]]; do
          tempDate=${createDate[counter]}
          tempDate=`date -d "${tempDate}" +'%Y%m%d%H%M%S'`
          if [[ $tempDate -gt $workingDateTime ]]; then
            tempStorm=${storm[counter]}
	    tempAdvisory=${advisory[counter]}
            tempMesh=${mesh[counter]}
            tempInstance=${instance[counter]}
            tempEnsemble=${ensemble[counter]}
            tempMachine=${machine[counter]}            
	    duplicate="false"
	    if [[ $tempEnsemble == "nhcConsensus" ]]; then
	      tempEnsembleName="nhcForecast"
	    else
	      tempEnsembleName=$tempEnsemble
	    fi
# Avoids duplicates. Searches through .hist file to find previous runs with same storm, advisory, mesh, and ensemble.
# Sometimes identical runs are submitted to different clusters.
            if [[ -f rgrow_pll.hist ]]; then
              while IFS= read -r line; do
	        if [[ "${tempStorm} ${tempAdvisory} ${tempMesh} ${tempEnsembleName}" == "$line" ]]; then
                  duplicate="true"
		  break
		fi
	      done <"rgrow_pll.hist"
	    fi
# This line is important, ignores results in archivee that don't match these patterns, like name of mesh or hindcasts for previous storms like matthew.
# hsofs mesh can be added in here if desired, but weights file would be necessary, and datum conversion from MSL to NAVD88 would be necessary.
            if [[ ( $duplicate == "false" ) && ( ${tempMesh} == *nc_inundation_v9.99_w_rivers* || ${tempMesh} == *PlaceholderForHSOFS* ) && ( ${tempStorm} != *matthew* ) ]]; then
              baseURL=http://tds.renci.org:8080/thredds/fileServer/2019
              directory=dorian/${advisory[counter]}/${mesh[counter]}/${machine[counter]}/${instance[counter]}/${ensemble[counter]}
              wget -N -q ${baseURL}/${directory}/maxele.63.nc
              found1="true"
              found3="true"
            else
              counter=`expr ${counter} - 1`
            fi
          else
            counter=`expr ${counter} - 1`
          fi
        done
      fi
      if [[ $found1 == "true" ]]; then
        logMessage "A new advisory was found with an archiving date of ${tempDate}."
      else
        logMessage "No new advisories were found."
        sleep 120
      fi
    done
    found0="true"
    if [[ $found0 == "true" ]]; then
      currentDateTime=${tempDate}
      currentAdvisory=${advisory[counter]}
      currentMesh=${mesh[counter]}
      currentStorm=${storm[counter]}
      currentEnsemble=${ensemble[counter]}
# This line has been added for naming in Python scripts
# For now it is hardcoded for NC. Not hard to adapt code to accept swan_HS_max.63 files, but not sure if using enhanced res script on wave heights is appropriate.
      currentRegion="north_carolina"
      currentFiletype="maxele"
      if [[ $currentEnsemble == "nhcConsensus" ]]; then
        currentEnsemble="nhcForecast"
      fi
      logMessage "Finished identifying the next set of files."
    else
      logMessage "There was an error in identifying the next set of files."
    fi
  done
}

doJob()
{
  subtractionKey=$1
  # Remove files from any previous advisory, so there is no confusion.
  rm *-enhanced.zip rgrow_pll.started rgrow_pll.finished
  datumConv="no"
  # For NC9 mesh, there is a need to simplify the mesh name used throughout because GRASS won't allow use of special characters other than underscores in map names
  if [[ ${currentMesh} == "nc_inundation_v9.99_w_rivers" ]]; then
    meshAbv="nc9rivers"
  else
    meshAbv=${currentMesh}
  fi
  echo "${currentRegion} ${currentStorm} ${currentAdvisory} ${meshAbv} ${currentEnsemble} ${currentFiletype} ${subtractionKey} ${datumConv}" > rgrow_pll.inp
  echo "${currentStorm} ${currentAdvisory} ${currentMesh} ${currentEnsemble}" >> rgrow_pll.hist
  # Submit the job.
  bsub < kalpana.csh
  logMessage "The visualization was submitted as a job to the queue."
  # Monitor to see when it has started.
  found5="false"
  until [[ $found5 == "true" ]]; do
    if [[ ! -f rgrow_pll.started ]]; then
      sleep 10
    else
      logMessage "The viz job has started successfully."
      SECONDS=0
      found5="true"
    fi
  done
  # Monitor to see when it has finished.
  found6="false"
  until [[ $found6 == "true" ]]; do
    if [[ ! -f rgrow_pll.finished ]]; then
      sleep 10
    else
      logMessage "The viz job has finished successfully."
# creates zip file to be sent in email
      zip -r ${currentStorm}-${currentAdvisory}-${currentMesh}-${currentEnsemble}-${currentFiletype}-enhanced.zip ${currentStorm}_${currentAdvisory}_${meshAbv}_${currentEnsemble}_${currentFiletype}_enhanced
# removes some temporary files
      rm -fr GRASS_LOCATION *_enhanced kalpana_out GRASS_LOCATION_wgs84
      duration=$SECONDS
      minutes=$(($duration / 60))
      seconds=$(($duration % 60))
      found6="true"
    fi
  done
}

mailProducts()
{
  addresses=$1
  bccAddresses=$2
  subtractionKey=$3
cat <<END > notify.txt
Mesh: ${currentMesh}
Storm: ${currentStorm}
Advisory: ${currentAdvisory}
Ensemble: ${currentEnsemble}

The North Carolina Forecast System (NCFS) has produced guidance for advisory ${currentAdvisory} / ${currentEnsemble} on the ${currentMesh} mesh.  This guidance includes predictions of maximum values for water levels in feet above NAVD88. The guidance is attached to this email in a GIS shapefile. The enhanced resolution post-processing and visualization of the maximum water levels took $minutes minutes and $seconds seconds to complete, ${subtractionKey} removal of overpredicted water levels.

For more information, including visualizations of maxima and time series for other parameters, please visit the CERA Web site for North Carolina: http://nc-cera.renci.org/

END
  cat notify.txt | mail -A gmail -s "[ncfs-notify] NCFS results for ${currentStorm} advisory ${currentAdvisory} on the ${currentMesh} mesh" -a ${currentStorm}-${currentAdvisory}-${currentMesh}-${currentEnsemble}-${currentFiletype}-enhanced.zip -b "${bccAddresses}" "${addresses}"
  logMessage "Sent mail with visualization products to these addresses: ${addresses}."
}

  if [ ! -e "logs" ]; then
    mkdir "logs"
  fi
  logDateTime=`date +'%Y%m%d%H%M%S'`
  sysLog=`pwd`/logs/ncsu-viz-${logDateTime}.$$.log
  echo "Messages are being written to the file: ${sysLog}."
  while getopts ":c:" optname; do
    case $optname in
      c) config=${OPTARG}
         ;;
      \?)
         logMessage "FATAL ERROR: Invalid option: -${OPTARG}"
         exit 1
         ;;
    esac
  done
  logMessage "Starting the NCFS Vector Plotting System."
  if [ -e "${config}" ]; then
    logMessage "Configuring the system according to the file ${config}."
  else
    logMessage "FATAL ERROR: The configuration file ${config} does not exist!"
    exit 1
  fi
  counter=0
  until [[ 1 == 0 ]]; do
    . ${config}
    logMessage "The configuration file ${config} was read successfully."
    if [[ ${counter} == "0" ]]; then
      currentDateTime=${startDateTime}
      logMessage "Starting with the specified date/time of ${currentDateTime}."
    else
      if [[ ${restart} == "yes" ]]; then
        currentDateTime=${startDateTime}
        logMessage "Restarting with the specified date/time of ${currentDateTime}."
      else
        currentDateTime=${currentDateTime}
        logMessage "Continuing with the working date/time of ${currentDateTime}."
      fi
    fi
    findFiles $currentDateTime
# A loop can be created to run doJob twice, once without subtraction and then again 'with' subtraction. The job will usually take longer with subtraction.
    subtractionKey="without"
    doJob $subtractionKey
    mailProducts $addressList $bccAddressList $subtractionKey
    counter=`expr ${counter} + 1`
  done
  logMessage "Finishing the NCFS Vector Plotting System." 
 
 
