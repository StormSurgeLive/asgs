#!/bin/bash
############################################################################
# ut-post.sh : Post processing for Texas. 
############################################################################
# Copyright(C) 2008--2015 Jason Fleming
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
. ${CONFIG} # grab all static config info
#
# write the target area to the run.properties file for the CERA
# web app
echo "asgs : la" >> run.properties
#
# grab storm class and name from file
if [[ $BACKGROUNDMET = on ]]; then
   # the NAM cycle time is the last two digits of the "advisory"
   namcyclehour=${ADVISORY:8:2}
   STORMNAME="NAM ${namcyclehour}Z"
fi
if [[ $TROPICALCYCLONE = on ]]; then 
   STORMCLASSNAME=`cat nhcClassName` 
   # find the space between the storm class (TD, TS, HU, etc) and the NHC name
   ind=`expr index "$STORMCLASSNAME" ' '`
   # just use the storm's name 
   STORMNAME=${STORMCLASSNAME:$ind}
fi
#
#  R E F O R M A T T I N G
#
# transpose elevation output file so that we can graph it with gnuplot
perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.61 --format space --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english
# transpose wind velocity output file so that we can graph it with gnuplot
perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.72 --format space --vectorOutput magnitude --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english
# now create csv files that can easily be imported into excel
perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.61 --format comma --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english
perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.72 --format comma --vectorOutput magnitude --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english
#
# rename csv files to something more intuitive
mv ${ADVISDIR}/${ENSTORM}/fort.61_transpose.csv ${ADVISDIR}/${ENSTORM}/${STORMNAME}.${ADVISORY}.station.elevation.csv
mv ${ADVISDIR}/${ENSTORM}/fort.72_transpose.csv ${ADVISDIR}/${ENSTORM}/${STORMNAME}.${ADVISORY}.station.windspeed.csv
#
# G N U P L O T   F O R   L I N E   G R A P H S
# 
# switch to plots directory
InitialDirectory=`pwd`;
mkdir ${ADVISDIR}/${ENSTORM}/plots
mv *.txt *.csv ${ADVISDIR}/$ENSTORM/plots
cd ${ADVISDIR}/$ENSTORM/plots
# plot elevation data with gnuplot
perl ${OUTPUTDIR}/autoplot.pl --filetoplot fort.61_transpose.txt --plotType elevation --plotdir ${ADVISDIR}/${ENSTORM}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname "$STORMNAME" --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88
# plot wind speed data with gnuplot 
perl ${OUTPUTDIR}/autoplot.pl --filetoplot fort.72_transpose.txt --plotType windvelocity --plotdir ${ADVISDIR}/${ENSTORM}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname "$STORMNAME" --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88
#
# We will need to call the 'convert' program, and the path is 
# not configurable there, so let's see if we can get the program to work
# by adding the imagemagick path to the path before calling that program
export PATH=/share/home/00675/jdietri1/ImageMagick-6.7.6-9/bin/convert:$PATH
for plotfile in `ls *.gp`; do
   gnuplot $plotfile
done
for plotfile in `ls *.ps`; do
   pngname=${plotfile%.ps}.png
   convert -rotate 90 $plotfile $pngname
done
plotarchive=${ADVISORY}.plots.zip
if [[ $TROPICALCYCLONE = on ]]; then
   plotarchive=${YEAR}.${STORM}.${plotarchive}
fi
#
# FigureGen commands for spatial plots.
cd $InitialDirectory
mkdir ${ADVISDIR}/${ENSTORM}/FG49
cd ${ADVISDIR}/${ENSTORM}/FG49
cp ../fort.14 .
cp ../maxele.63 .
cp ../swan_HS_max.63 .
cp ../maxwvel.63 .
cp ${OUTPUTDIR}/FG49/* .
tropcyc=0
if [[ $TROPICALCYCLONE = on ]]; then
   tropcyc=1
fi
perl FG49.pl --template FG49_MaxEle_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxEle_LATEX.inp
perl FG49.pl --template FG49_MaxEle_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxEle_LATEX_KMZ.inp
./FigureGen49_Serial.exe -I ./FG49_MaxEle_LATEX.inp -V 49
./FigureGen49_Serial.exe -I ./FG49_MaxEle_LATEX_KMZ.inp -V 49
perl FG49.pl --template FG49_MaxHS_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxHS_LATEX.inp
perl FG49.pl --template FG49_MaxHS_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxHS_LATEX_KMZ.inp
./FigureGen49_Serial.exe -I ./FG49_MaxHS_LATEX.inp -V 49
./FigureGen49_Serial.exe -I ./FG49_MaxHS_LATEX_KMZ.inp -V 49
perl FG49.pl --template FG49_MaxWind_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxWind_LATEX.inp
perl FG49.pl --template FG49_MaxWind_LATEX.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxWind_LATEX_KMZ.inp
./FigureGen49_Serial.exe -I ./FG49_MaxWind_LATEX.inp -V 49
./FigureGen49_Serial.exe -I ./FG49_MaxWind_LATEX_KMZ.inp -V 49
perl FG49.pl --template FG49_MaxEle_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxEle_FL.inp
perl FG49.pl --template FG49_MaxEle_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxEle_FL_KMZ.inp
./FigureGen49_Serial.exe -I ./FG49_MaxEle_FL.inp -V 49
./FigureGen49_Serial.exe -I ./FG49_MaxEle_FL_KMZ.inp -V 49
perl FG49.pl --template FG49_MaxHS_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxHS_FL.inp
perl FG49.pl --template FG49_MaxHS_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxHS_FL_KMZ.inp
./FigureGen49_Serial.exe -I ./FG49_MaxHS_FL.inp -V 49
./FigureGen49_Serial.exe -I ./FG49_MaxHS_FL_KMZ.inp -V 49
perl FG49.pl --template FG49_MaxWind_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 0 > FG49_MaxWind_FL.inp
perl FG49.pl --template FG49_MaxWind_FL.template --tropcyc ${tropcyc} --year ${YEAR} --storm ${STORM} --advisory ${ADVISORY} --kmzflag 1 > FG49_MaxWind_FL_KMZ.inp
./FigureGen49_Serial.exe -I ./FG49_MaxWind_FL.inp -V 49
./FigureGen49_Serial.exe -I ./FG49_MaxWind_FL_KMZ.inp -V 49
#
# tar up the plots and the csv files
cd $InitialDirectory
zip ${ADVISDIR}/${ENSTORM}/WGRFC.${plotarchive} plots/*.png plots/*.csv ./fort.61 ./fort.72
zip ${ADVISDIR}/${ENSTORM}/UT-CSR.${plotarchive} ./fort.14 ./fort.22 ./maxele.63 ./elemaxdry.63 ./nodeflag.63 ./rising.63 ./tinun.63 FG49/*LATEX_0001.tif FG49/*LATEX_KMZ.kmz
zip ${ADVISDIR}/${ENSTORM}/Miami-NWS.${plotarchive} FG49/ASGS.txt FG49/*FL_0001.tif FG49/*FL_KMZ.kmz

