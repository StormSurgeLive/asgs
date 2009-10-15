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
   COLDSTARTDATE=$8
   HSTIME=$9
   GRIDFILE=$10
   OUTPUTDIR=$11
   #
   . ${CONFIG} # 
   #
   # 
   mkdir ${ADVISDIR}/${ENSTORM}/plots
   # transpose elevation output file so that we can graph it with gnuplot
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.61 --format space --coldstartdate ${COLDSTARTDATE} --gmtoffset -5 --timezone CDT --units english
   # transpose wind velocity output file so that we can graph it with gnuplot
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.72 --format space --vectorOutput magnitude --coldstartdate ${COLDSTARTDATE} --gmtoffset -5 --timezone CDT --units english
   # plot elevation data with gnuplot
   perl ${OUTPUTDIR}/autoplot.pl --filetoplot fort.61_transpose.txt --plotType elevation --plotdir ${ADVISDIR}/${ENSTORM}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname ${YEAR}${STORM} --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88
   # plot wind speed data with gnuplot 
   perl ${OUTPUTDIR}/autoplot.pl --filetoplot fort.72_transpose.txt --plotType windvelocity --plotdir ${ADVISDIR}/${ENSTORM}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname ${YEAR}${STORM} --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88
   initialDirectory=`pwd`;
   cd ${ADVISDIR}/$ENSTORM/plots
   for plotfile in `ls *.gp`; do
      gnuplot $plotfile
   done
   for plotfile in `ls *.ps`; do
      pngname=${plotfile%.ps}.png
      convert $plotfile $pngname
   done
   # now create csv files that can easily be imported into excel
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.61 --format comma --coldstartdate ${COLDSTARTDATE} --gmtoffset -5 --timezone CDT --units english
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${ADVISDIR}/${ENSTORM}/fort.15 --stationfile ${ADVISDIR}/${ENSTORM}/fort.72 --format comma --vectorOutput magnitude --coldstartdate ${COLDSTARTDATE} --gmtoffset -5 --timezone CDT --units english
   # 
   tar cvzf ${ADVISDIR}/${ENSTORM}/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-plots.tar.gz *.png *.csv
   cd $initialDirectory
#
# ---------------------------------------------------------
#  now run POST_SCRIPT for KMZ GIS and JPGs
# --------------------------------------------------------
      ${OUTPUTDIR}/POSTPROC_KMZGIS/POST_SCRIPT.sh  $ADVISDIR $OUTPUTDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $GRIDFILE 
#
# now copy the files to the appropriate locations
      OUTPUTPREFIX1=${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-KMZ_GIS
      OUTPUTPREFIX2=${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-plots
#
    ${OUTPUTDIR}/scp_files_ranger.exp ${OUTPUTDIR} $ADVISDIR $RESULTSHOST $RESULTSPATH $RESULTSPROMPT $RESULTSUSER $RESULTSPASSWORD $HOSTNAME $ENSTORM $OUTPUTPREFIX1 $OUTPUTPREFIX2 > scp.log &

    sleep 60


    
