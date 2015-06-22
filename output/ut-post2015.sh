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
#  O P E N  D A P    P U B L I C A T I O N 
#
STORMNAMEPATH=null
DOWNLOADPREFIX="http://opendap.renci.org:1935/thredds/fileServer"
CATALOGPREFIX="http://opendap.renci.org:1935/thredds/catalog"
if [[ $BACKGROUNDMET = on ]]; then
   # for NAM, the "advisory number" is actually the cycle time 
   STORMNAMEPATH=tc/nam
fi
if [[ $TROPICALCYCLONE = on ]]; then
   STORMNAME=`grep -m 1 "stormname" ${STORMDIR}/run.properties | sed 's/stormname.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
   STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
   STORMNAMEPATH=tc/$STORMNAMELC
fi
OPENDAPSUFFIX=$ADVISORY/$GRIDNAME/$HOSTNAME/$INSTANCENAME/$ENSTORM
# put the opendap download url in the run.properties file for CERA to find
downloadURL=$DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX
echo "downloadurl : $downloadURL" >> run.properties
# now actually make the directory (OPENDAPBASEDIR is specified in CONFIG)
OPENDAPDIR=$OPENDAPBASEDIR/$STORMNAMEPATH/$OPENDAPSUFFIX
#
logMessage "Transferring files to $OPENDAPDIR on $OPENDAPHOST as user $OPENDAPUSER with the ssh key in $SSHKEY."
debugMessage "The actual transfer has been commented out, pending the activation of a suitable thredds server."
#ssh $OPENDAPHOST -l $OPENDAPUSER -i $SSHKEY "mkdir -p $OPENDAPDIR" 2>> $SYSLOG
for file in `ls *.nc *.xmf ${ADVISDIR}/al*.fst ${ADVISDIR}/bal*.dat fort.15 fort.22 run.properties`; do
   chmod +r $file 2>> $SYSLOG
   logMessage "Transferring $file."
   debugMessage "(not actually transferring the file)"
   #scp -i $SSHKEY $file ${OPENDAPUSER}@${OPENDAPHOST}:${OPENDAPDIR} 2>> $SYSLOG
   #ssh $OPENDAPHOST -l $OPENDAPUSER -i $SSHKEY "chmod +r $OPENDAPDIR/$file"
done
#
COMMA_SEP_LIST="jason.g.fleming@gmail.com" #,asgs.cera.lsu@gmail.com"
runStartTime=`grep RunStartTime run.properties | sed 's/RunStartTime.*://' | sed 's/\s//g'`
subject="ADCIRC NCFS POSTED for $runStartTime"
if [[ $TROPICALCYCLONE = on ]]; then
   subject=${subject}" (TROPICAL CYCLONE)"
fi
subject="${subject} $CERASERVER"
subject="${subject} $HOSTNAME.$INSTANCENAME $ENMEMNUM"
cat <<END > ${STORMDIR}/cera_results_notify.txt 

The ADCIRC NCFS solutions for $ADVISORY have been posted to $CATALOGPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX

The run.properties file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
   
or wget the file with the following command

wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
END
echo "INFO: ut-post2015.sh: Sending 'results available' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/cera_results_notify.txt | mail -s "$subject" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1

