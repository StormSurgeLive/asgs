#!/bin/bash
#-----------------------------------------------------------------------
# createMaxCSV.sh : Create a point shape file in CSV format.
#-----------------------------------------------------------------------
# Copyright(C) 2017--2022 Jason Fleming
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
#-----------------------------------------------------------------------
# This script assumes it is executed within an ASGS shell process
# and has access to all the normal environmental variables
# ($SCRIPTDIR, $PATH, etc)
#-----------------------------------------------------------------------
#
sigint() {
   trap - SIGTERM && kill -- -$$
   exit 0
}
sigterm() {
   trap - SIGTERM && kill -- -$$
   exit 0
}
sigexit() {
   trap - SIGTERM && kill -- -$$
   exit 0
}
trap 'sigint' INT
trap 'sigterm' TERM
trap 'sigexit' EXIT
#
#
THIS=$(basename -- $0)
numProcesses=${1:-1}
# Count command line arguments; use them if provided or use
# run.properties if not.
declare -A properties
SCENARIODIR=$PWD
RUNPROPERTIES=$SCENARIODIR/run.properties
context="auto"  # run by ASGS in production when a scenario completes
if [[ $# -gt 1 ]]; then
   context="manual" # run by the Operator (or other process), not the ASGS
   RUNPROPERTIES=$2
   SYSLOG=createMaxCSV.log
   CYCLELOG=$SYSLOG
   SCENARIOLOG=$SYSLOG
else
   SYSLOG=${properties['monitoring.logging.file.syslog']}
   CYCLELOG=${properties['monitoring.logging.file.cyclelog']}
   SCENARIOLOG=${properties['monitoring.logging.file.scenariolog']}
fi
# this script can be called with just one command line option: the
# full path to the run.properties file
echo "Loading properties."
# load run.properties file into associative array
source $SCRIPTDIR/properties.sh
loadProperties $RUNPROPERTIES
echo "Finished loading properties."
# now set variables that would otherwise be set by command line arguments
CYCLE=${properties['advisory']}
SCENARIO=${properties['scenario']}
GRIDFILE=${properties['adcirc.file.input.gridfile']}
GRIDNAME=${properties['adcirc.gridname']}
BACKGROUNDMET=${properties['forcing.backgroundmet']}
TROPICALCYCLONE=${properties['forcing.tropicalcyclone']}
if [[ $TROPICALCYCLONE != "off" ]]; then
   STORM=${properties['forcing.tropicalcyclone.stormnumber']}
   YEAR=${properties['forcing.tropicalcyclone.year']}
else
   STORM="null"
   YEAR=${CYCLE:0:4}
fi
#
#-----------------------------------------------------------------------
#     C R E A T E   M A X   C S V
#------------------------------------------------------------------------
# form the csv file name, e.g.: jose2017adv44HSOFSnhcConsensusMax.csv

if [[ $TROPICALCYCLONE != off ]]; then
   STORMNAME=${properties['forcing.tropicalcyclone.stormname']}
   # make the storm name lower case
   STORMNAMELC=$(echo $STORMNAME | tr '[:upper:]' '[:lower:]') 2>> ${SYSLOG}
   csvFileName="${STORMNAMELC}${YEAR}adv${CYCLE}${GRIDNAME}${SCENARIO}Max.csv"
else
   if [[ $BACKGROUNDMET == "on" || $BACKGROUNDMET == "nam" ]]; then
      STORMNAMELC=nam
   fi
   if [[ $BACKGROUNDMET == "GFS" ]]; then
      STORMNAMELC=gfs
   fi
   csvFileName="${STORMNAMELC}adv${CYCLE}${GRIDNAME}${SCENARIO}Max.csv"
fi
# these files all cover the full domain and summarize some
# aspect of the numberical results over the course of the run
summaryFiles=( maxele.63.nc )          # peak water surface elevation
summaryFiles+=( maxvel.63.nc )         # peak water current speed
summaryFiles+=( maxwvel.63.nc )        # peak wind speed
summaryFiles+=( minpr.63.nc )          # nadir of barometric pressure
summaryFiles+=( maxrs.63.nc )          # peak wave radiation stress
summaryFiles+=( initiallydry.63.nc )   # dry ground at cold start
summaryFiles+=( everdried.63.nc )      # locations that have ever become dried
summaryFiles+=( endrisinginun.63.nc )  # water rising when run ends
summaryFiles+=( inundationtime.63.nc ) # length of time of inundation
summaryFiles+=( maxinundepth.63.nc )   # peak inundation depth
summaryFiles+=( swan_HS_max.63.nc )    # peak significant wave height
summaryFiles+=( swan_TPS_max.63.nc )   # peak wave period
summaryFiles+=( swan_DIR_max.63.nc )   # wave direction at time of peak significant wave height
#
timeSeriesFiles+=( fort.63 )           # full domain time varying water surface elevation
#
# create the metadata header
echo '#' $(ncdump -h maxele.63.nc | grep agrid | sed -e 's/\t\t//' -e 's/://' -e 's/;//' -e 's/=/:/' 2>>$SCENARIOLOG) > header.csv
echo '#' $(ncdump -h maxele.63.nc | grep rundes | sed -e 's/\t\t//' -e 's/://' -e 's/;//' -e 's/=/:/' | cut -d ! -f 1 2>>$SCENARIOLOG) '"' >> header.csv
echo '#' $(ncdump -h maxele.63.nc | grep runid | sed -e 's/\t\t//' -e 's/://' -e 's/;//' -e 's/=/:/' | cut -d ! -f 1 2>>$SCENARIOLOG) '"' >> header.csv
# mesh bathy/topo : FIXME : get bathytopo directly from the netcdf file
if [[ ! -f $GRIDFILE && ! -f $INPUTDIR/$GRIDFILE && ! -f "fort.14" ]]; then
   echo "ERROR: Mesh file '$GRIDFILE' specified in run.properties file was not found in $INPUTDIR or '.'; nor was 'fort.14'." | tee $SCENARIOLOG
   exit 1
fi
fort14=$GRIDFILE
if [[ ! -f $GRIDFILE ]]; then
   if [[ -f $INPUTDIR/$GRIDFILE ]]; then
      fort14=$INPUTDIR/$GRIDFILE
   else
      fort14="fort.14"
   fi
fi
awk 'NR==2 { np=$2 } NR>2 && NR<=np+2 { print $2","$3","$4 }' $fort14 > xyd.txt 2>> $SCENARIOLOG
#
# create metadata for summary column definitions and column units and
# convert the netcdf files to ascii and then extract just the data values
# from the resulting ascii files
columnDefinitions="# longitude,latitude,depth,maxele"
columnUnits="# degrees east,degrees north,m below datum,m above datum"
windFile=wind10m.maxwvel.txt
waveFile=swan_HS_max.txt
netcdf2adcirc.x --datafile maxele.63.nc 2>> $SCENARIOLOG
awk 'NR>3 { print $2 }' maxele.63 > maxele.txt
if [[ -e wind10m.maxwvel.63.nc  ]]; then
   columnDefinitions="${columnDefinitions},wind10m.maxwvel"
   columnUnits="${columnUnits},m/s at 10m above ground"
   netcdf2adcirc.x --datafile wind10m.maxwvel.63.nc 2>> $SYSLOG
   mv maxwvel.63 wind10m.maxwvel.63 2>> $SYSLOG
   awk 'NR>3 { print $2 }' wind10m.maxwvel.63 > wind10m.maxwvel.txt
elif [[ -e maxwvel.63.nc  ]]; then
   columnDefinitions="${columnDefinitions},maxwvel"
   columnUnits="${columnUnits},m/s at ground level"
   netcdf2adcirc.x --datafile maxwvel.63.nc 2>> $SYSLOG
   awk 'NR>3 { print $2 }' maxwvel.63 > maxwvel.txt
   windFile=maxwvel.txt
else
   echo "WARNING: Wind file not found."
   unset windFile
fi
if [[ -e swan_HS_max.63.nc  ]]; then
   columnDefinitions="${columnDefinitions},swan_HS_max"
   columnUnits="${columnUnits},m above msl"
   netcdf2adcirc.x --datafile swan_HS_max.63.nc 2>> $SYSLOG
   awk 'NR>3 { print $2 }' swan_HS_max.63 > swan_HS_max.txt
else
   echo "WARNING: Wave file not found."
   unset waveFile
fi
# place all columns in one file with a comma as the delimiter
paste -d "," xyd.txt maxele.txt $windFile $waveFile > max_data.csv
# add header to data
echo $columnDefinitions >> header.csv
echo $columnUnits >> header.csv
cat header.csv max_data.csv > $csvFileName 2>> $SYSLOG
gzip -f $csvFileName 2>> $SYSLOG &
echo "Maximum Values Point CSV File Name : ${csvFileName}.gz" >> run.properties
echo "Maximum Values Point CSV File Format : gzipped ascii csv" >> run.properties
#
# create metadata for time series column definitions and column units and
# convert the netcdf files to ascii and then extract just the data values
# from the resulting ascii files
if [[ -s fort.63.nc ]]; then
   columnDefinitions="# longitude,latitude,depth,timeVaryingWaterSurfaceElevation"
   columnUnits="# degrees east,degrees north,m below datum,m above datum"
   base_date=$(ncdump -h fort.63.nc | grep base_date | cut -d = -f 2 | tr -d '";')
   csEpochSeconds=$(TZ=UTC date -u -d "$base_date" "+%s" 2>>$SYSLOG)
   dataSetSecondsList=( $(ncks --json -v 'time' fort.63.nc | grep data | tr -d '"data:[,]') )
   netcdf2adcirc.x --split --datafile fort.63.nc 2>> $SCENARIOLOG
   splitPid=$!
   numDataSets=$(ncdump -h fort.63.nc | grep currently | grep -Eo [0-9]+)
   wetonlyCsvFileName="${STORMNAMELC}adv${CYCLE}${GRIDNAME}${SCENARIO}wetonly.csv"
   for file in $(ls ??????_fort.63); do
      count=${file:0:6}
      awk 'NR>3 && $2<-99998 { print "0" } NR>3 && $2>-99998 { print "1" }' $file | resultScope.x --meshfile LA_v20a-WithUpperAtch_chk.grd --datafile $file --datafiletype fort.63 --datafileformat ascii --resultfileformat ascii --resultshape wetonly
      nnodes=$(awk 'NR==2 { print $2 }' "LA_v20a-WithUpperAtch_chk.grd_wetonly-sub.14")
      nele=$(awk 'NR==2 { print $1 }' "LA_v20a-WithUpperAtch_chk.grd_wetonly-sub.14")
      awk -v n=$nnodes -v e=$nele 'NR>=(n+3) && NR<(n+3+e) { print $3","$4","$5 }' LA_v20a-WithUpperAtch_chk.grd_wetonly-sub.14 > tin_connectivity_1_$file.csv
      (xz -f tin_connectivity_1_$file.csv &)
      awk -v n=$nnodes -v e=$nele 'NR>=(n+3) && NR<(n+3+e) { print $3-1","$4-1","$5-1 }' LA_v20a-WithUpperAtch_chk.grd_wetonly-sub.14 > tin_connectivity_0_$file.csv
      (xz -f tin_connectivity_0_$file.csv &)
      awk 'NR==2 { np=$2 } NR>2 && NR<=np+2 { print $2","$3","$4 }' LA_v20a-WithUpperAtch_chk.grd_wetonly-sub.14 > xyd.txt 2>> $SCENARIOLOG
      splitCsvFileName=${wetonlyCsvFileName%.*}_$count.csv
      echo $columnDefinitions >> $splitCsvFileName
      awk 'NR>3 { print $2 }' "sub-wetonly_$file" | paste -d "," xyd.txt - >> $splitCsvFileName
      (xz -f $splitCsvFileName &)
   done
   exit
   #echo '#' $(ncdump -h fort.63.nc | grep agrid | tr -d ':;\t\t' | sed -e 's/=/:/' 2>>$SCENARIOLOG) > staticHeader.csv
   #echo '#' $(ncdump -h fort.63.nc | grep rundes | tr -d ':;\t\t' | sed -e 's/=/:/' | cut -d ! -f 1 2>>$SCENARIOLOG) >> staticHeader.csv
   #echo '#' $(ncdump -h fort.63.nc | grep runid | tr -d ':;\t\t' \
   #   | sed '/nowcast/!s/run/forward model guidance/'            \
   #   | sed -e 's/=/:/' -e 's/nowcast/model retrospective/'      \
   #   | cut -d ! -f 1 2>>$SCENARIOLOG) '"' >> staticHeader.csv
   if [[ $numProcesses == "auto" ]]; then
      numProcesses=$numDataSets
      echo "Setting numProcesses to '$numProcesses'."
   fi
   c=1
   while [[ $c -lt $numDataSets ]] ; do
      for file in $(ls ??????_fort.63); do
         count=${file:0:6}
         splitCsvFileName=${csvFileName%.*}_$count.csv
         if [[ ! -f $splitCsvFileName.xz && ! -f $file.lock ]]; then
            if [[ $(( $(date '+%s') - $(stat -c %Y $file) )) -gt 10 && $(ls *.lock 2> /dev/null | wc -l) -lt $numProcesses ]]; then
               echo $(date +'%Y-%h-%d-T%H:%M:%S%z') > $file.lock 2>> $SYSLOG
               echo $count
               c=$(( c + 1 ))
               (
                  cp staticHeader.csv $splitCsvFileName 2>> $SYSLOG
                  d=$(( 10#$count - 1 ))
                  dataSetSeconds=${dataSetSecondsList[$d]}
                  dataSetEpochSeconds=$(( $csEpochSeconds + $dataSetSeconds ))
                  dataSetDateTime="$(TZ=UTC date -u -d "1970-01-01 UTC $dataSetEpochSeconds seconds" +"%Y-%m-%d %H:%M:%S %Z")"
                  #echo "# date : \"$dataSetDateTime\"" >> $splitCsvFileName
                  #echo "# dataSetCount : \"$(( 10#$count )) of $numDataSets\"" >> $splitCsvFileName
                  echo $columnDefinitions >> $splitCsvFileName
                  #echo $columnUnits >> $splitCsvFileName
                  # place all columns in one file with a comma as the delimiter
                  awk 'NR>3 { print $2 }' $file | paste -d "," xyd.txt - >> $splitCsvFileName
                  xz -f $splitCsvFileName 2>> $SYSLOG
                  echo rm $file.lock
                  rm $file $file.lock 2>> $SYSLOG
               ) &
            fi
         fi
      done
      sleep 10 # sleep before going to look for more files
   done
fi
echo "Waiting for background jobs to finish ..."
wait
