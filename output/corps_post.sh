#!/bin/bash
#------------------------------------------------------------------------
# Copyright(C) 2008--2013 Jason Fleming
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
#------------------------------------------------------------------------
# Example of invocation:
# ~/asgs/trunk/output/corps_post.sh ~/asgs/config/asgs_config_phil_garnet_hsdrrs2014.sh /lustre/work1/jgflemin/asgs19368/38 99 2008 38 garnet.erdc.hpc.mil nhcConsensus 2008080800  2743200.000000000 HSDRRS2014_MRGO_leveeupdate_fixSTC_MX.grd  ~/asgs/trunk/output /u/jgflemin/asgs/log/asgs-2014-Apr-23-T05:33:41.19368.log ~/.ssh/id_rsa.pub
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
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR}
# get the forecast ensemble member number for use in CERA load balancing
# as well as picking up any bespoke configuration for this ensemble
# member in the configuration files
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
si=$ENMEMNUM
#
# grab all config info
. ${CONFIG} 
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
export PERL5LIB=${PERL5LIB}:${SCRIPTDIR}/PERL
#-----------------------------------------------------------------------
# O P E N  D A P    P U B L I C A T I O N 
#-----------------------------------------------------------------------
logMessage "Creating list of files to post to opendap."
FILES=(`ls *.nc ${ADVISDIR}/al*.fst ${ADVISDIR}/bal*.dat fort.15 fort.22 run.properties`)
#
# For each opendap server in the list in ASGS config file.
primaryCount=0
for server in ${TDS[*]}; do
   logMessage "Posting to $server opendap with opendap_post.sh using the following command: ${OUTPUTDIR}/opendap_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server \"${FILES[*]}\" $OPENDAPNOTIFY"
   ${OUTPUTDIR}/opendap_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server "${FILES[*]}" $OPENDAPNOTIFY >> ${SYSLOG} 2>&1
   # add downloadurl_backup propert(ies) to 

   # add downloadurl_backup propert(ies) to the properties file that refer to previously 
   # posted results
   backupCount=0
   for backup in ${TDS[*]}; do
      # don't list the same server as primary and backup and don't list
      # a server as a backup if nothing has been posted there yet
      if [[ $backupCount -ge $primaryCount ]]; then
         break
      fi
      # opendap_post.sh writes each primary download URL to the file
      # downloadurl.txt as it posts the results. Use these to populate
      # the downloadurl_backupX properties.
      # add +1 b/c the 0th backup url is on 1st line of the file
      backupURL=`sed -n $(($backupCount+1))p downloadurl.log`
      OPENDAPDIR=`sed -n $(($backupCount+1))p opendapdir.log`
      propertyName="downloadurl_backup"$(($backupServer+1))
      # need to grab the SSHPORT from the configuration
      env_dispatch $backup
      if [[ $SSHPORT != "null" ]]; then
         ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "echo $propertyName : $backupURL >> $OPENDAPDIR/run.properties"
      else
         ssh $OPENDAPHOST -l $OPENDAPUSER "echo $propertyName : $backupURL >> $OPENDAPDIR/run.properties"
      fi
      backupCount=$(($backupCount+1))
   done
   primaryCount=$((primaryCount+1))
done
#
#
export PATH=$PATH:$IMAGEMAGICKBINPATH # if ImageMagick is in nonstd location
#
# we expect the ASGS config file to tell us how many cera servers there
# are with CERASERVERNUM and assume they are consecutively named 
# cera1, cera2, etc. We alternate the forecast ensemble members evenly 
# among them
#CERASERVERNUM=`expr $ENMEMNUM % $NUMCERASERVERS + 1`
#CERASERVER=cera$CERASERVERNUM
#echo "ceraServer : $CERASERVER" >> run.properties
#
# write the intended audience to the run.properties file for CERA
echo "intendedAudience : $INTENDEDAUDIENCE" >> run.properties
#
# write the target area to the run.properties file for the CERA
# web app
#echo "asgs : ng" >> run.properties 2>> $SYSLOG
echo "enstorm : $ENSTORM" >> run.properties 2>> $SYSLOG
#
# record the sea_surface_height_above_geoid nodal attribute to the
# run.properties file
#isUsed=`grep -c sea_surface_height_above_geoid fort.15`
#if [[ $isUsed = 0 ]]; then
#   # this nodal attribute is not being used; report this to run.properties file
#   echo "sea_surface_height_above_geoid : null" >> run.properties
#else
#   # get the line number where the start of this nodal attribute is specified
#   # in the header of the fort.13 (nodal attributes) file
#   linenum=`grep --line-number --max-count 1 sea_surface_height_above_geoid fort.13 | awk 'BEGIN { FS=":" } { print $1 }'`
#   # get the actual default value, which is specified three lines after the
#   # the name of the nodal attribute in the fort.13 header
#   datumOffsetDefaultValueLine=`expr $linenum + 3`
#   datumOffsetDefaultValue=`awk -v linenum=$datumOffsetDefaultValueLine 'NR==linenum { print $0 }' fort.13`
#   echo "sea_surface_height_above_geoid : $datumOffsetDefaultValue" >> run.properties
#fi
#
#  R E F O R M A T T I N G
#
#
# add CPP projection to netcdf files 
# generate XDMF xml files 
#for file in `ls *.nc`; do
#   # don't try to write XDMF xml files for station files or hotstart files
#   if [[ $file = fort.61.nc || $file = fort.71.nc || $file = fort.72.nc || $file = fort.67.nc || $file = fort.68.nc ]]; then
#      continue
#   fi
#   logMessage "Adding CPP coordinates to $file."
#   ${OUTPUTDIR}/generateCPP.x --datafile $file --cpp $SLAM0 $SFEA0 2>> $SYSLOG
#   logMessage "Generating XDMF xml file to accompany $file."
#   ${OUTPUTDIR}/generateXDMF.x --use-cpp --datafile $file 2>> $SYSLOG
#done

# record the sea_surface_height_above_geoid nodal attribute to the
# run.properties file
isUsed=`grep -c sea_surface_height_above_geoid fort.15`
if [[ $isUsed = 0 ]]; then
   # this nodal attribute is not being used; report this to run.properties file
   echo "sea_surface_height_above_geoid : null" >> run.properties
else
   # get the line number where the start of this nodal attribute is specified
   # in the header of the fort.13 (nodal attributes) file
   linenum=`grep --line-number --max-count 1 sea_surface_height_above_geoid fort.13 | awk 'BEGIN { FS=":" } { print $1 }'`
   # get the actual default value, which is specified three lines after the
   # the name of the nodal attribute in the fort.13 header
   datumOffsetDefaultValueLine=`expr $linenum + 3`
   datumOffsetDefaultValue=`awk -v linenum=$datumOffsetDefaultValueLine 'NR==linenum { print $0 }' fort.13`
   echo "sea_surface_height_above_geoid : $datumOffsetDefaultValue" >> run.properties
fi



#
# G N U P L O T   F O R   L I N E   G R A P H S
# 
# transpose elevation output file so that we can graph it with gnuplot
STATIONELEVATION=${STORMDIR}/fort.61
if [[ -e $STATIONELEVATION || -e ${STATIONELEVATION}.nc ]]; then
   if [[ -e $STATIONELEVATION.nc ]]; then
      ${OUTPUTDIR}/netcdf2adcirc.x --datafile ${STATIONELEVATION}.nc 2>> ${SYSLOG}
   fi
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${STORMDIR}/fort.15 --stationfile ${STATIONELEVATION} --format space --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
   # now create csv files that can easily be imported into excel
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose elevation --controlfile ${STORMDIR}/fort.15 --stationfile ${STATIONELEVATION} --format comma --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
   # rename csv files to something more intuitive
   mv ${STORMDIR}/fort.61_transpose.csv ${STORMDIR}/${STORMNAME}.${ADVISORY}.hydrographs.csv 2>> ${SYSLOG} 2>&1 
fi
# transpose wind velocity output file so that we can graph it with gnuplot
STATIONVELOCITY=${STORMDIR}/fort.72
if [[ -e $STATIONVELOCITY || -e ${STATIONVELOCITY}.nc ]]; then
   if [[ -e $STATIONVELOCITY.nc ]]; then
      ${OUTPUTDIR}/netcdf2adcirc.x --datafile ${STATIONVELOCITY}.nc 2>> ${SYSLOG}
   fi
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${STORMDIR}/fort.15 --stationfile ${STATIONVELOCITY} --format space --vectorOutput magnitude --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
   # now create csv files that can easily be imported into excel
   perl ${OUTPUTDIR}/station_transpose.pl --filetotranspose windvelocity --controlfile ${STORMDIR}/fort.15 --stationfile ${STATIONVELOCITY} --format comma --vectorOutput magnitude --coldstartdate $CSDATE --gmtoffset -5 --timezone CDT --units english 2>> ${SYSLOG}
   # rename csv files to something more intuitive
   mv ${ADVISDIR}/${ENSTORM}/fort.72_transpose.csv ${ADVISDIR}/${ENSTORM}/${STORMNAME}.${ADVISORY}.station.windspeed.csv 2>> ${SYSLOG} 2>&1
fi
# switch to plots directory
if [[ -e ${STORMDIR}/fort.61_transpose.txt || -e ${STORMDIR}/fort.72_transpose.txt ]]; then
   initialDirectory=`pwd`;
   mkdir ${STORMDIR}/plots 2>> ${SYSLOG}
   mv *.txt *.csv ${STORMDIR}/plots 2>> ${SYSLOG}
   cd ${STORMDIR}/plots
   # generate gnuplot scripts for elevation data
   if [[ -e ${STORMDIR}/plots/fort.61_transpose.txt ]]; then
      logMessage "Generating gnuplot script for $ENSTORM hydrographs."
      perl ${OUTPUTDIR}/autoplot.pl --filetoplot ${STORMDIR}/plots/fort.61_transpose.txt --plotType elevation --plotdir ${STORMDIR}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname "$STORMNAME" --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88
   fi
   # plot wind speed data with gnuplot 
   if [[ -e ${STORMDIR}/plots/fort.72_transpose.txt ]]; then
      logMessage "Generating gnuplot script for $ENSTORM wind speed stations."
      perl ${OUTPUTDIR}/autoplot.pl --filetoplot ${STORMDIR}/plots/fort.72_transpose.txt --plotType windvelocity --plotdir ${STORMDIR}/plots --outputdir ${OUTPUTDIR} --timezone CDT --units english --stormname "$STORMNAME" --enstorm $ENSTORM --advisory $ADVISORY --datum NAVD88
   fi
   # execute gnuplot scripts to actually generate the plots
   for plotfile in `ls *.gp`; do
      gnuplot $plotfile 2>> ${SYSLOG}
   done
   # convert them all to png
   for plotfile in `ls *.ps`; do
      pngname=${plotfile%.ps}.png
      convert -rotate 90 $plotfile $pngname 2>> ${SYSLOG}
   done
   plotarchive=${ADVISORY}.plots.tar.gz
   if [[ $TROPICALCYCLONE = on ]]; then
      plotarchive=${YEAR}${STORM}.${plotarchive}
   fi
   # tar up the plots and the csv files
   # also include the maxele.63 file and the original fort.61 and fort.72
   # as requested by Max Agnew and David Ramirez at the New Orleans District
   tar cvzf ${STORMDIR}/${plotarchive} *.png *.csv ../maxele.63 ../fort.61 ../fort.72
   cd $initialDirectory 2>> ${SYSLOG}
fi
#
#  G I S     K M Z      J P G 
#
# name of bounding box for contour plots (see config_simple_gmt_pp.sh
# for choices)
#BOX=LA
# FigureGen executable to use for making JPG files (assumed to be located
# in $OUTPUTDIR/POSTPROC_KMZGIS/FigGen/
#FIGUREGENEXECUTABLE=FigureGen32_prompt_inp.x
# The full path and name for the FigureGen template file.
#FIGUREGENTEMPLATE=$OUTPUTDIR/POSTPROC_KMZGIS/FigGen/FG_asgs.inp.orig
#
#  now create the Google Earth (kmz), jpg, and GIS contour plots
#${OUTPUTDIR}/POSTPROC_KMZGIS/POST_SCRIPT_Corps.sh $ADVISDIR $OUTPUTDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $GRIDFILE $CONFIG $BOX $FIGUREGENEXECUTABLE $FIGUREGENTEMPLATE
#
#  P U B L I C A T I O N
#
# grab the names of the output files
#GISKMZJPG=`ls *KMZ_GIS.tar.gz`
#PLOTS=`ls *plots.tar.gz`
#
# now create the index.html file to go with the output
#perl ${OUTPUTDIR}/corps_index.pl --stormname $STORMNAME --advisory $ADVISORY --templatefile ${OUTPUTDIR}/corps_index_template.html --giskmzjpgarchive $GISKMZJPG --plotsarchive $PLOTS > index.html
#
# now copy plots and visualizations to the website, based on the forcing
# (i.e., NAM or NHC tropical cyclone), machine on which they were run, the 
# grid name, and the advisory 
#if [[ $BACKGROUNDMET = on ]]; then
#   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "mkdir -p ${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/$ADVISORY"
#   scp -i $SSHKEY index.html ${WEBUSER}@${WEBHOST}:${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/$ADVISORY
#   scp -i $SSHKEY $GISKMZJPG ${WEBUSER}@${WEBHOST}:${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/$ADVISORY
#   scp -i $SSHKEY $PLOTS ${WEBUSER}@${WEBHOST}:${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/$ADVISORY
#   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "chmod -R 755 ${WEBPATH}/NAM"
#fi
#if [[ $TROPICALCYCLONE = on ]]; then 
#   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "mkdir -p ${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}"
#   scp -i $SSHKEY index.html ${WEBUSER}@${WEBHOST}:${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}
#   scp -i $SSHKEY $GISKMZJPG ${WEBUSER}@${WEBHOST}:${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}
#   scp -i $SSHKEY $PLOTS ${WEBUSER}@${WEBHOST}:${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}
#   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "chmod -R 755 ${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}"
#fi
