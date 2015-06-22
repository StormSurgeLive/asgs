#!/bin/bash
############################################################################
# ut-post2015.sh : Post processing for Texas. 
############################################################################
#
# Copyright(C) 2015 Jason Fleming
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
#
############################################################################
#
# Sample execution:
# ~/asgs/2014stable/output/ut-post2015.sh ~/asgs/2014stable/config/asgs_config_nam_stampede_ec95d.sh /scratch/00976/jgflemin/asgs26137/2015061500 91L 2015 2015061500 stampede.tacc.utexas.edu namforecast 2015051300 2851200 texas2008_r35h.grd ~/asgs/2014stable/output syslog.log nullsshkey
#
#
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
SSHKEY=${13}
#
#
# get the forecast ensemble member number for use in picking up any bespoke
# configuration for this ensemble member in the configuration files
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR}
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
si=$ENMEMNUM
#
. ${CONFIG} # grab all static config info
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
# write the target area to the run.properties file for the CERA
echo "asgs : ng" >> run.properties 2>> ${SYSLOG}
echo "enstorm : $ENSTORM" >> run.properties 2>> ${SYSLOG}
#
# grab storm class and name from file
if [[ $BACKGROUNDMET = on ]]; then
   # the NAM cycle time is the last two digits of the "advisory"
   namcyclehour=${ADVISORY:8:2}
   STORMNAME="NAM ${namcyclehour}Z"
fi
if [[ $TROPICALCYCLONE = on ]]; then 
   STORMCLASSNAME=`cat nhcClassName 2>>${SYSLOG}` 
   # find the space between the storm class (TD, TS, HU, etc) and the NHC name
   ind=`expr index "$STORMCLASSNAME" ' '`
   # just use the storm's name 
   STORMNAME=${STORMCLASSNAME:$ind}
fi
#
# record the sea_surface_height_above_geoid nodal attribute to the
# run.properties file
isUsed=`grep -c sea_surface_height_above_geoid fort.15 2>>${SYSLOG}`
if [[ $isUsed = 0 ]]; then
   # this nodal attribute is not being used; report this to run.properties file
   echo "sea_surface_height_above_geoid : null" >> run.properties 2>>${SYSLOG}
else
   # get the line number where the start of this nodal attribute is specified
   # in the header of the fort.13 (nodal attributes) file
   linenum=`grep --line-number --max-count 1 sea_surface_height_above_geoid fort.13 2>> ${SYSLOG} | awk 'BEGIN { FS=":" } { print $1 }' 2>> ${SYSLOG}`
   # get the actual default value, which is specified three lines after the
   # the name of the nodal attribute in the fort.13 header
   datumOffsetDefaultValueLine=`expr $linenum + 3 2>> ${SYSLOG}`
   datumOffsetDefaultValue=`awk -v linenum=$datumOffsetDefaultValueLine 'NR==linenum { print $0 }' fort.13 2>>${SYSLOG}`
   echo "sea_surface_height_above_geoid : $datumOffsetDefaultValue" >> run.properties 2>> ${SYSLOG}
fi
#
#  R E F O R M A T T I N G
#
# transpose elevation output file so that we can graph it with gnuplot
#
# jgf20150616: Convert fort.61 and fort.72 files from netcdf to adcirc (ascii)
# format so they can be processed with station_transpose.pl.
STATIONELEVATION=${STORMDIR}/fort.61
if [[ -e $STATIONELEVATION || -e ${STATIONELEVATION}.nc ]]; then
   if [[ -e $STATIONELEVATION.nc ]]; then
      ${OUTPUTDIR}/netcdf2adcirc.x --datafile ${STATIONELEVATION}.nc 2>> ${SYSLOG}
   fi
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.61 --format space --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
   # now create csv files that can easily be imported into excel
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.61 --format comma --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
   # rename csv files to something more intuitive
   mv ${ADVISDIR}/${ENSTORM}/fort.61_transpose.csv ${ADVISDIR}/${ENSTORM}/${STORMNAME}.${ADVISORY}.station.elevation.csv 2>> ${SYSLOG}
fi
# transpose wind velocity output file so that we can graph it with gnuplot
STATIONVELOCITY=${STORMDIR}/fort.72
if [[ -e $STATIONVELOCITY || -e ${STATIONVELOCITY}.nc ]]; then
   if [[ -e $STATIONVELOCITY.nc ]]; then
      ${OUTPUTDIR}/netcdf2adcirc.x --datafile ${STATIONVELOCITY}.nc 2>> ${SYSLOG}
   fi
   # transpose wind velocity output file so that we can graph it with gnuplot
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.72 --format space --vectorOutput magnitude --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.72 --format comma --vectorOutput magnitude --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
   #
   mv ${ADVISDIR}/${ENSTORM}/fort.72_transpose.csv ${ADVISDIR}/${ENSTORM}/${STORMNAME}.${ADVISORY}.station.windspeed.csv 2>> ${SYSLOG}
fi 
#
# G N U P L O T   F O R   L I N E   G R A P H S
# 
# switch to plots directory
InitialDirectory=`pwd`
mkdir ${ADVISDIR}/${ENSTORM}/plots 2>> ${SYSLOG}
mv *transpose*.txt *.csv ${ADVISDIR}/$ENSTORM/plots 2>> ${SYSLOG}
cd ${ADVISDIR}/$ENSTORM/plots 2>> ${SYSLOG}
# plot elevation data with gnuplot
perl ${OUTPUTDIR}/autoplot.pl --filetoplot fort.61_transpose.txt --plotType elevation --plotdir ${ADVISDIR}/${ENSTORM}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname "$STORMNAME" --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88 2>> ${SYSLOG}
# plot wind speed data with gnuplot 
perl ${OUTPUTDIR}/autoplot.pl --filetoplot fort.72_transpose.txt --plotType windvelocity --plotdir ${ADVISDIR}/${ENSTORM}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname "$STORMNAME" --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88 2>> ${SYSLOG}
#
# We will need to call the 'convert' program, and the path is 
# not configurable there, so let's see if we can get the program to work
# by adding the imagemagick path to the path before calling that program
#export PATH=/share/home/00675/jdietri1/ImageMagick-6.7.6-9/bin/convert:$PATH
# jgf20150616: stampede has convert.
for plotfile in `ls *.gp`; do
   gnuplot $plotfile 2>> ${SYSLOG}
done
for plotfile in `ls *.ps`; do
   pngname=${plotfile%.ps}.png
   convert -rotate 90 $plotfile $pngname 2>> ${SYSLOG}
done
plotarchive=${ADVISORY}.plots.zip
if [[ $TROPICALCYCLONE = on ]]; then
   plotarchive=${YEAR}.${STORM}.${plotarchive} 2>> ${SYSLOG}
fi
#
# extract netcdf data to ascii for use with FigureGen
cd ${STORMDIR} 2>> ${SYSLOG}
for file in maxele.63 swan_HS_max.63 maxwvel.63; do
   if [[ -e ${file}.nc ]]; then
      ${OUTPUTDIR}/netcdf2adcirc.x --datafile ${file}.nc 2>> ${SYSLOG}
   fi
done
#
# FigureGen commands for spatial plots.
#
module load intel/14.0.1.106 2>> ${SYSLOG} 2>&1
module load gmt/5.1.1 2>> ${SYSLOG} 2>&1
mkdir ${ADVISDIR}/${ENSTORM}/FG49 2>> ${SYSLOG}
cd ${ADVISDIR}/${ENSTORM}/FG49 2>> ${SYSLOG}
cp ../fort.14 . 2>> ${SYSLOG}
cp ../maxele.63 . 2>> ${SYSLOG}
cp ../swan_HS_max.63 . 2>> ${SYSLOG}
cp ../maxwvel.63 . 2>> ${SYSLOG}
cp ${OUTPUTDIR}/FG49/* . 2>> ${SYSLOG}
tropcyc=0
if [[ $TROPICALCYCLONE = on ]]; then
   tropcyc=1
fi
perl FG49.pl --template FG49_MaxEle_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxEle_LATEX.inp 2>> ${SYSLOG} 
perl FG49.pl --template FG49_MaxEle_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxEle_LATEX_KMZ.inp 2>> ${SYSLOG} 
./FigureGen49_Serial.exe -I ./FG49_MaxEle_LATEX.inp -V 49 2>> ${SYSLOG} 2>&1
./FigureGen49_Serial.exe -I ./FG49_MaxEle_LATEX_KMZ.inp -V 49 2>> ${SYSLOG} 2>&1
perl FG49.pl --template FG49_MaxHS_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxHS_LATEX.inp 2>> ${SYSLOG}
perl FG49.pl --template FG49_MaxHS_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxHS_LATEX_KMZ.inp 2>> ${SYSLOG}
./FigureGen49_Serial.exe -I ./FG49_MaxHS_LATEX.inp -V 49 2>> ${SYSLOG} 2>&1
./FigureGen49_Serial.exe -I ./FG49_MaxHS_LATEX_KMZ.inp -V 49 2>> ${SYSLOG} 2>&1
perl FG49.pl --template FG49_MaxWind_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxWind_LATEX.inp 2>> ${SYSLOG}
perl FG49.pl --template FG49_MaxWind_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxWind_LATEX_KMZ.inp 2>> ${SYSLOG}
./FigureGen49_Serial.exe -I ./FG49_MaxWind_LATEX.inp -V 49 2>> ${SYSLOG} 2>&1
./FigureGen49_Serial.exe -I ./FG49_MaxWind_LATEX_KMZ.inp -V 49 2>> ${SYSLOG} 2>&1
perl FG49.pl --template FG49_MaxEle_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxEle_FL.inp 2>> ${SYSLOG}
perl FG49.pl --template FG49_MaxEle_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxEle_FL_KMZ.inp 2>> ${SYSLOG}
./FigureGen49_Serial.exe -I ./FG49_MaxEle_FL.inp -V 49 2>> ${SYSLOG} 2>&1
./FigureGen49_Serial.exe -I ./FG49_MaxEle_FL_KMZ.inp -V 49 2>> ${SYSLOG} 2>&1
perl FG49.pl --template FG49_MaxHS_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxHS_FL.inp 2>> ${SYSLOG}
perl FG49.pl --template FG49_MaxHS_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxHS_FL_KMZ.inp 2>> ${SYSLOG}
./FigureGen49_Serial.exe -I ./FG49_MaxHS_FL.inp -V 49 2>> ${SYSLOG} 2>&1
./FigureGen49_Serial.exe -I ./FG49_MaxHS_FL_KMZ.inp -V 49 2>> ${SYSLOG} 2>&1
perl FG49.pl --template FG49_MaxWind_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxWind_FL.inp 2>> ${SYSLOG}
perl FG49.pl --template FG49_MaxWind_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxWind_FL_KMZ.inp 2>> ${SYSLOG}
./FigureGen49_Serial.exe -I ./FG49_MaxWind_FL.inp -V 49 2>> ${SYSLOG} 2>&1
./FigureGen49_Serial.exe -I ./FG49_MaxWind_FL_KMZ.inp -V 49 2>> ${SYSLOG} 2>&1
#
# tar up the plots and the csv files
cd $InitialDirectory 2>> ${SYSLOG} 2>&1
zip ${ADVISDIR}/${ENSTORM}/WGRFC.${plotarchive} plots/*.png plots/*.csv ./fort.61 ./fort.72 2>> ${SYSLOG} 2>&1
zip ${ADVISDIR}/${ENSTORM}/UT-CSR.${plotarchive} ./fort.14 ./fort.22 ./maxele.63 ./elemaxdry.63 ./nodeflag.63 ./rising.63 ./tinun.63 FG49/*LATEX_0001.tif FG49/*LATEX_KMZ.kmz 2>> ${SYSLOG} 2>&1
zip ${ADVISDIR}/${ENSTORM}/Miami-NWS.${plotarchive} FG49/ASGS.txt FG49/*FL_0001.tif FG49/*FL_KMZ.kmz 2>> ${SYSLOG} 2>&1

