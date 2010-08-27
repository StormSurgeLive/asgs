#!/bin/bash
#
# Copyright(C) 2008, 2009 Jason Fleming
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
# grab storm class and name from file
if [[ $BACKGROUNDMET = on ]]; then
   STORMNAME="NAM $ADVISORY $ENSTORM"
fi
if [[ $TROPICALCYCLONE = on ]]; then 
   STORMNAME=`cat nhcClassName` 
   STORMNAME=${STORMNAME}" "${YEAR}" "$ENSTORM
fi
# transpose elevation output file so that we can graph it with gnuplot
perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.61 --format space --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english
# transpose wind velocity output file so that we can graph it with gnuplot
perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.72 --format space --vectorOutput magnitude --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english
# now create csv files that can easily be imported into excel
perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.61 --format comma --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english
perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.72 --format comma --vectorOutput magnitude --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english
#
# rename csv files to something more intuitive
mv fort.61_transpose.csv ${STORMNAME}.${ADVISORY}.station.elevation.csv
mv fort.72_transpose.csv ${STORMNAME}.${ADVISORY}.station.windspeed.csv
# 
# switch to plots directory
initialDirectory=`pwd`;
mkdir ${ADVISDIR}/${ENSTORM}/plots
mv *.txt *.csv ${ADVISDIR}/$ENSTORM/plots
cd ${ADVISDIR}/$ENSTORM/plots
# plot elevation data with gnuplot
perl ${OUTPUTDIR}/autoplot.pl --filetoplot fort.61_transpose.txt --plotType elevation --plotdir ${ADVISDIR}/${ENSTORM}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname "$STORMNAME" --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88
# plot wind speed data with gnuplot 
perl ${OUTPUTDIR}/autoplot.pl --filetoplot fort.72_transpose.txt --plotType windvelocity --plotdir ${ADVISDIR}/${ENSTORM}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname "$STORMNAME" --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88
#
# FigGen32.exe (called below) calls the 'convert' program, and the path is 
# not configurable there, so let's see if we can get the program to work
# by adding the imagemagick path to the path before calling that program
# Also, the 'convert' program is called below.
export PATH=$PATH:$IMAGEMAGICKPATH 
for plotfile in `ls *.gp`; do
   gnuplot $plotfile
done
for plotfile in `ls *.ps`; do
   pngname=${plotfile%.ps}.png
   convert -rotate 90 $plotfile $pngname
done
tar cvzf ${ADVISDIR}/${ENSTORM}/${YEAR}${STORM}.${ADVISORY}.plots.tar.gz *.png *.csv
cd $initialDirectory
#
#  now create the Google Earth, jpg, and GIS output files
${OUTPUTDIR}/POSTPROC_KMZGIS/POST_SCRIPT_Corps.sh $ADVISDIR $OUTPUTDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $GRIDFILE $GISCONFIG $CLIPCOAST
#
# grab the names of the output files
GISKMZJPG=`ls *KMZ_GIS.tar.gz`
PLOTS=`ls *plots.tar.gz`
#
# now create the index.html file to go with the output
perl ${OUTPUTDIR}/corps_index.pl --advisory $ADVISORY --templatefile ${OUTPUTDIR}/corps_index_template.html --giskmzjpgarchive $GISKMZJPG --plotsarchive $PLOTS > index.html
#
# now copy plots and visualizations to the website
ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "mkdir -p ${WEBPATH}/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY}"
scp -i $SSHKEY index.html ${WEBUSER}@${WEBHOST}:${WEBPATH}/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY}
scp -i $SSHKEY $GISKMZJPG ${WEBUSER}@${WEBHOST}:${WEBPATH}/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY}
scp -i $SSHKEY $PLOTS ${WEBUSER}@${WEBHOST}:${WEBPATH}/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY}
ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "chmod -R 755 ${WEBPATH}/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY}"
