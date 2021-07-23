#!/bin/bash
#-----------------------------------------------------------------------
# createMaxCSV.sh : Create a point shape file in CSV format.
#-----------------------------------------------------------------------
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
#-----------------------------------------------------------------------
#
THIS=$(basename -- $0)
# Count command line arguments; use them if provided or use 
# run.properties if not.
declare -A properties
SCENARIODIR=$PWD
RUNPROPERTIES=$SCENARIODIR/run.properties
if [[ $# -eq 1 ]]; then
   RUNPROPERTIES=$1
fi
# this script can be called with just one command line option: the
# full path to the run.properties file
echo "Loading properties."
# get loadProperties function
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' $RUNPROPERTIES`
source $SCRIPTDIR/properties.sh
# load run.properties file into associative array
loadProperties $RUNPROPERTIES
echo "Finished loading properties."
# now set variables that would otherwise be set by command line arguments
CONFIG=${properties['config.file']}
CYCLEDIR=${properties['path.advisdir']}
CYCLE=${properties['advisory']}
HPCENV=${properties['hpc.hpcenv']}
SCENARIO=${properties['scenario']}
CSDATE=${properties['adcirc.time.coldstartdate']}
HSTIME=${properties['InitialHotStartTime']}
GRIDFILE=${properties['adcirc.file.input.gridfile']}
OUTPUTDIR=${properties['path.outputdir']}
SYSLOG=${properties['monitoring.logging.file.syslog']}
SSHKEY=${properties['post.file.sshkey']}
HPCENVSHORT=${properties['hpc.hpcenvshort']}
HPCENV=${properties['hpc.hpcenv']}
TROPICALCYCLONE=${properties['forcing.tropicalcyclone']}
if [[ $TROPICALCYCLONE != "off" ]]; then
   STORM=${properties['forcing.tropicalcyclone.stormnumber']}
   YEAR=${properties['forcing.tropicalcyclone.year']}
else
   STORM="null"
   YEAR=${CYCLE:0:4}
fi      
#
SCENARIODIR=${CYCLEDIR}/${SCENARIO}       # shorthand
CYCLELOG=${properties['monitoring.logging.file.cyclelog']}
SCENARIOLOG=${properties['monitoring.logging.file.scenariolog']}
source ${SCRIPTDIR}/monitoring/logging.sh
source ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${HPCENVSHORT}
THIS=output/createMaxCSV.sh
allMessage "$SCENARIO: $THIS: Starting post processing."
scenarioMessage "$THIS: SCENARIO=$SCENARIO ; SCENARIODIR=$SCENARIODIR"
cd ${SCENARIODIR} 2>&1 > errmsg || warn "cycle $CYCLE: $SCENARIO: $THIS: Could not change directory to $SCENARIODIR: `cat $errmsg`"
#
#-----------------------------------------------------------------------
#     C R E A T E   M A X   C S V  
#------------------------------------------------------------------------
# form the csv file name, e.g.: jose2017adv44HSOFSnhcConsensusMax.csv
STORMNAMELC=nam
if [[ $TROPICALCYCLONE != off ]]; then
   STORMNAME=${properties['forcing.tropicalcyclone.stormname']}
   # make the storm name lower case
   STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'` 2>> ${SYSLOG}
fi
csvFileName="${STORMNAMELC}${YEAR}adv${CYCLE}${GRIDNAME}${SCENARIO}Max.csv"
#
# create the metadata header
echo '#' `ncdump -h maxele.63.nc | grep agrid | sed -e 's/\t\t//' -e 's/://' -e 's/;//' -e 's/=/:/'` > header.csv
echo '#' `ncdump -h maxele.63.nc | grep rundes | sed -e 's/\t\t//' -e 's/://' -e 's/;//' -e 's/=/:/' | cut -d ! -f 1` '"' >> header.csv
echo '#' `ncdump -h maxele.63.nc | grep runid | sed -e 's/\t\t//' -e 's/://' -e 's/;//' -e 's/=/:/' | cut -d ! -f 1` '"' >> header.csv
# mesh bathy/topo
awk 'NR==2 { np=$2 } NR>2 && NR<=np+2 { print $2","$3","$4 }' fort.14 > xyd.txt
# 
# create metadata for column definitions and column units and 
# convert the netcdf files to ascii and then extract just the data values
# from the resulting ascii files
columnDefinitions="# longitude,latitude,depth,maxele"
columnUnits="# degrees east,degrees north,m below datum,m above datum"
windFile=wind10m.maxwvel.txt
waveFile=swan_HS_max.txt
${OUTPUTDIR}/netcdf2adcirc.x --datafile maxele.63.nc 2>> $SYSLOG
awk 'NR>3 { print $2 }' maxele.63 > maxele.txt
if [[ -e wind10m.maxwvel.nc  ]]; then
   columnDefinitions="${columnDefinitions},wind10m.maxwvel"
   columnUnits="${columnUnits},m/s at 10m above ground" 
   ${OUTPUTDIR}/netcdf2adcirc.x --datafile wind10m.maxwvel.63.nc 2>> $SYSLOG
   mv maxwvel.63 wind10m.maxwvel.63 2>> $SYSLOG
   awk 'NR>3 { print $2 }' wind10m.maxwvel.63 > wind10m.maxwvel.txt
else 
   columnDefinitions="${columnDefinitions},maxwvel" 
   columnUnits="${columnUnits},m/s at ground level" 
   ${OUTPUTDIR}/netcdf2adcirc.x --datafile maxwvel.63.nc 2>> $SYSLOG
   awk 'NR>3 { print $2 }' maxwvel.63 > maxwvel.txt
   windFile=maxwvel.txt
fi
if [[ -e swan_HS_max.nc  ]]; then
   columnDefinitions="${columnDefinitions},swan_HS_max"
   columnUnits="${columnUnits},m above msl" 
   ${OUTPUTDIR}/netcdf2adcirc.x --datafile swan_HS_max.63.nc 2>> $SYSLOG
   awk 'NR>3 { print $2 }' swan_HS_max.63 > swan_HS_max.txt
else
   waveFile=''
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
