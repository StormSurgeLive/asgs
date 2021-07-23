#!/bin/bash
#-----------------------------------------------------------------------
# make_max_csv.sh : Produce an ascii csv file containing mesh vertices
# and max values.
#-----------------------------------------------------------------------
# Copyright(C) 2017 Jason Fleming
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
CONFIG=$1
ADVISDIR=$2
STORM=$3
YEAR=$4
ADVISORY=$5
HOSTNAME=$6
ENSTORM=$7
CSDATE=$8
HSTIME=$9
GRIDFILE=${10}
OUTPUTDIR=${11}
SYSLOG=${12}
#
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR} 2>> ${SYSLOG}
THIS=$(basename -- $0)
# get the forecast ensemble member number 
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
# grab all config info
si=$ENMEMNUM
. ${CONFIG}
# Bring in logging functions
. ${SCRIPTDIR}/monitoring/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
# form the csv file name, e.g.: jose2017adv44HSOFSnhcConsensusMax.csv
STORMNAMELC=nam
if [[ $TROPICALCYCLONE = on ]]; then
   STORMNAME=`grep "stormname" ${STORMDIR}/run.properties | sed 's/stormname.*://' | sed 's/^\s//g' | tail -n 1` 2>> ${SYSLOG}
   # make the storm name lower case
   STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'` 2>> ${SYSLOG}
fi
csvFileName="${STORMNAMELC}${YEAR}adv${ADVISORY}${GRIDNAME}${ENSTORM}Max.csv"
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
