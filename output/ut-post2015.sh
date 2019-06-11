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
. ${SCRIPTDIR}/monitoring/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
# write the target area to the run.properties file for the CERA
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
# tar up the plots and the csv files
cd $InitialDirectory 2>> ${SYSLOG} 2>&1
#
#--------------------------------------------------------------------------
#              I N U N D A T I O N    M  A S K  
#--------------------------------------------------------------------------
# When presenting inundation data on Google Maps, the ADCIRC extent of
# initially dry area is often landward of the Google Maps shoreline, 
# resulting in the erroneous depiction of non-inundated land areas 
# seaward of inundated areas. The inundationMask program expands the 
# inundation area presented on Google Maps to cover the full land area
# as depicted by Google Maps. 
# 
if [ -e ${STORMDIR}/initiallydry.63.nc ]; then
   if [ -e ${OUTPUTDIR}/inundationMask.x ]; then
      ${OUTPUTDIR}/inundationMask.x --filename initiallydry.63.nc --netcdf4 --numpasses 2 2>> ${SYSLOG} 2>&1
      ERROVALUE=$?
      if [ $ERROVALUE == 0 ]; then
         echo "Inundation Mask File Name : inundationmask.63.nc" >> run.properties
         echo "Inundation Mask Format : netcdf" >> run.properties
      else
         error "Failed to create inundationmask.63.nc file."
      fi
   else
      error "The initiallydry.63.nc file was found in $STORMDIR but the inundationMask.x executable was not found in ${OUTPUTDIR}."
   fi
fi
#
#-----------------------------------------------------------------------
#         O P E N  D A P    P U B L I C A T I O N 
#-----------------------------------------------------------------------
logMessage "Creating list of files to post to opendap."
FILES=(`ls *.nc ../al*.fst ../bal*.dat run.properties`)
#
# For each opendap server in the list in ASGS config file.
for server in ${TDS[*]}; do
   logMessage "Posting to $server opendap with opendap_post.sh using the following command: ${OUTPUTDIR}/opendap_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server \"${FILES[*]}\" $OPENDAPNOTIFY"
   ${OUTPUTDIR}/opendap_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server "${FILES[*]}" $OPENDAPNOTIFY >> ${SYSLOG} 2>&1
done

