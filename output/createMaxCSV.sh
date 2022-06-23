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
THIS=$(basename -- $0)
# Count command line arguments; use them if provided or use
# run.properties if not.
declare -A properties
SCENARIODIR=$PWD
RUNPROPERTIES=$SCENARIODIR/run.properties
context="auto"  # run by ASGS in production when a scenario completes
if [[ $# -eq 1 ]]; then
   context="manual" # run by the Operator (or other process), not the ASGS
   RUNPROPERTIES=$1
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

#
# create the metadata header
echo '#' $(ncdump -h maxele.63.nc | grep agrid | sed -e 's/\t\t//' -e 's/://' -e 's/;//' -e 's/=/:/' 2>>$SCENARIOLOG) > header.csv
echo '#' $(ncdump -h maxele.63.nc | grep rundes | sed -e 's/\t\t//' -e 's/://' -e 's/;//' -e 's/=/:/' | cut -d ! -f 1 2>>$SCENARIOLOG) '"' >> header.csv
echo '#' $(ncdump -h maxele.63.nc | grep runid | sed -e 's/\t\t//' -e 's/://' -e 's/;//' -e 's/=/:/' | cut -d ! -f 1 2>>$SCENARIOLOG) '"' >> header.csv
# mesh bathy/topo
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
# create metadata for column definitions and column units and
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
gzip $csvFileName 2>> $SYSLOG
echo "Maximum Values Point CSV File Name : ${csvFileName}.gz" >> run.properties
echo "Maximum Values Point CSV File Format : gzipped ascii csv" >> run.properties