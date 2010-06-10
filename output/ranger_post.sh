#!/bin/bash
#----------------------------------------------------------------
# Copyright(C) 2008, 2009, 2010 Jason Fleming
# Copyright(C) 2009, 2010 Robert Weaver
#----------------------------------------------------------------
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
. ${CONFIG} # grab all static config info
#
# grab storm class and name from file
STORMNAME=`cat nhcClassName` 
STORMNAME=${STORMNAME}" "${YEAR}
# transpose elevation output file so that we can graph it with gnuplot
if [[ -e ${ADVISDIR}/${ENSTORM}/fort.61 ]]; then
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.61 --format space --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
   # now create csv files that can easily be imported into excel
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.61 --format comma --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
fi
if [[ -e ${ADVISDIR}/${ENSTORM}/fort.72 ]] then   
   # transpose wind velocity output file so that we can graph it with gnuplot
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.72 --format space --vectorOutput magnitude --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.72 --format comma --vectorOutput magnitude --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
fi
# 
# switch to plots directory
initialDirectory=`pwd`; 
mkdir ${ADVISDIR}/${ENSTORM}/plots 2>> ${SYSLOG}
mv *.txt *.csv ${ADVISDIR}/$ENSTORM/plots 2>> ${SYSLOG}
cd ${ADVISDIR}/$ENSTORM/plots 2>> ${SYSLOG}
# plot elevation data with gnuplot
if [[ -e ${ADVISDIR}/${ENSTORM}/fort.61 ]]; then
   perl ${OUTPUTDIR}/autoplot.pl --filetoplot fort.61_transpose.txt --plotType elevation --plotdir ${ADVISDIR}/${ENSTORM}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname "$STORMNAME" --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88
fi
if [[ -e ${ADVISDIR}/${ENSTORM}/fort.72 ]] then
   # plot wind speed data with gnuplot 
   perl ${OUTPUTDIR}/autoplot.pl --filetoplot fort.72_transpose.txt --plotType windvelocity --plotdir ${ADVISDIR}/${ENSTORM}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname "$STORMNAME" --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88
fi
for plotfile in `ls *.gp`; do
   gnuplot $plotfile
done
for plotfile in `ls *.ps`; do
   pngname=${plotfile%.ps}.png
   ${IMAGEMAGICKPATH}/convert -rotate 90 $plotfile $pngname
done
tar cvzf ${ADVISDIR}/${ENSTORM}/${YEAR}${STORM}.${ADVISORY}.plots.tar.gz *.png *.csv 2>> ${SYSLOG}
cd $initialDirectory
#
# FigGen32.exe (called below) calls the 'convert' program, and the path is 
# not configurable there, so let's see if we can get the program to work
# by adding the imagemagick path to the path before calling that program
export PATH=$PATH:$IMAGEMAGICKPATH 
#
#  now create the Google Earth, jpg, and GIS output files
${OUTPUTDIR}/POSTPROC_KMZGIS/POST_SCRIPT.sh $CONFIG $ADVISDIR $OUTPUTDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $GRIDFILE 

# -----------------------------------
# RANGER SPECIFIC
# ----------------------------------

#  now copy the files to the appropriate locations
OUTPUTPREFIX1=${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-KMZ_GIS
OUTPUTPREFIX2=${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-plots
mv ${ADVISDIR}/${ENSTORM}/${YEAR}${STORM}.${ADVISORY}.plots.tar.gz ${ADVISDIR}/${ENSTORM}/$OUTPUTPREFIX2.tar.gz 2>> ${SYSLOG}
#
DESTDIR=$DATAPOSTDIR/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}
if [[ ! -d $DESTDIR ]]; then
   mkdir $DESTDIR 2>> ${SYSLOG}
   chmod 766 $DESTDIR 2>> ${SYSLOG}
fi
#
cp $OUTPUTPREFIX1.tar.gz $DESTDIR 2>> ${SYSLOG}
cp $OUTPUTPREFIX2.tar.gz $DESTDIR 2>> ${SYSLOG}
cp fort.22  $DESTDIR 2>> ${SYSLOG}
cp fort.22.meta  $DESTDIR 2>> ${SYSLOG}
cp $ADVISDIR/al${STORM}${YEAR}.fst $DESTDIR 2>> ${SYSLOG}
cp $ADVISDIR/bal${STORM}${YEAR}.dat  $DESTDIR 2>> ${SYSLOG}
cp fort.61 $DESTDIR 2>> ${SYSLOG}
cp maxele.63 $DESTDIR 2>> ${SYSLOG}
cp maxwvel.63 $DESTDIR 2>> ${SYSLOG}

#    tar -xzf $DATAPOSTDIR/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}/$OUTPUTPREFIX1.tar.gz -C $DATAPOSTDIR/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}/
#    mv $DATAPOSTDIR/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}/${OUTPUTPREFIX1}_files/* $DATAPOSTDIR/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}/
#    rmdir $DATAPOSTDIR/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}/${OUTPUTPREFIX1}_files
#
chmod 644  $DATAPOSTDIR/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}/*
#
${OUTPUTDIR}/scp_files_ranger.exp ${OUTPUTDIR} $ADVISDIR $RESULTSHOST $RESULTSPATH $RESULTSPROMPT $RESULTSUSER $RESULTSPASSWORD $HOSTNAME $ENSTORM $OUTPUTPREFIX1 $OUTPUTPREFIX2 > scp.log &
#
sleep 60
#
# now create a timeseries animation KMZ
#  cd $POSTPROC_DIR/TimeseriesKMZ/
#
#    ln -fs $INPUTFILE ./
#    ln -fs $ADVISDIR/$ENSTORM/fort.14 ./
#    ln -fs $ADVISDIR/$ENSTORM/fort.63 ./
#    ln -fs $ADVISDIR/$ENSTORM/fort.74 ./
#
#  perl make_JPG.pl --outputdir $POSTPROC_DIR --gmthome $GMTHOME --storm ${STORM} --year ${YEAR} --adv $ADVISORY --n $NORTH --s $SOUTH --e $EAST --w $WEST --outputprefix $OUTPUTPREFIX
#
#     ${OUTPUTDIR}/POSTPROC_KMZGIS/TimeseriesKMZ/

