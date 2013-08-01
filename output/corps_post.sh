#!/bin/bash
#
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
export PATH=$PATH:$IMAGEMAGICKBINPATH # if ImageMagick is in nonstd location
#
# we expect the ASGS config file to tell us how many cera servers there
# are with CERASERVERNUM and assume they are consecutively named 
# cera1, cera2, etc. We alternate the forecast ensemble members evenly 
# among them
CERASERVERNUM=`expr $ENMEMNUM % $NUMCERASERVERS + 1`
CERASERVER=cera$CERASERVERNUM
echo "ceraServer : $CERASERVER" >> run.properties
#
# write the intended audience to the run.properties file for CERA
echo "intendedAudience : $INTENDEDAUDIENCE" >> run.properties
#
# write the target area to the run.properties file for the CERA
# web app
echo "asgs : ng" >> run.properties 2>> $SYSLOG
echo "enstorm : $ENSTORM" >> run.properties 2>> $SYSLOG
#
# grab storm class and name from file
STORMNAME=null
if [[ $BACKGROUNDMET = on ]]; then
   # the NAM cycle time is the last two digits of the "advisory"
   namcyclehour=${ADVISORY:8:2}
   STORMNAME="NAM${namcyclehour}Z"
fi
if [[ $TROPICALCYCLONE = on ]]; then 
   STORMNAME=`grep "storm name" ${STORMDIR}/run.properties | sed 's/storm name.*://' | sed 's/\s//g'` 2>> ${SYSLOG}
fi
#
#  R E F O R M A T T I N G
#
#
# add CPP projection to netcdf files 
# generate XDMF xml files 
for file in `ls *.nc`; do
   # don't try to write XDMF xml files for station files or hotstart files
   if [[ $file = fort.61.nc || $file = fort.71.nc || $file = fort.72.nc || $file = fort.67.nc || $file = fort.68.nc ]]; then
      continue
   fi
   logMessage "Adding CPP coordinates to $file."
   ${OUTPUTDIR}/generateCPP.x --datafile $file --cpp $SLAM0 $SFEA0 2>> $SYSLOG
   logMessage "Generating XDMF xml file to accompany $file."
   ${OUTPUTDIR}/generateXDMF.x --use-cpp --datafile $file 2>> $SYSLOG
done
#
#  O P E N  D A P    P U B L I C A T I O N 
#
STORMNAMEPATH=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
# make opendap directory
OPENDAPTYPE=tc
if [[ $BACKGROUNDMET = on ]]; then
   OPENDAPTYPE=nam
   STORMNAMEPATH=nam218
fi
OPENDAPDIR=$OPENDAPBASEDIR/$OPENDAPTYPE/$STORMNAMEPATH/$ADVISORY/$GRIDNAME/$HOSTNAME/$INSTANCENAME/$ENSTORM
logMessage "Transferring files to $OPENDAPDIR on $OPENDAPHOST as user $OPENDAPUSER with the ssh key in $SSHKEY."
ssh $OPENDAPHOST -l $OPENDAPUSER -i $SSHKEY "mkdir -p $OPENDAPDIR" 2>> $SYSLOG
for file in `ls *.nc *.xmf fort.15 fort.22 run.properties`; do 
   chmod +r $file 2>> $SYSLOG
   logMessage "Transferring $file."
   scp -i $SSHKEY $file ${OPENDAPUSER}@${OPENDAPHOST}:${OPENDAPDIR} 2>> $SYSLOG
   ssh $OPENDAPHOST -l $OPENDAPUSER -i $SSHKEY "chmod +r $OPENDAPDIR/$file"
done
#
# make symbolic links to directory structure that CERA web app uses
$openDAPDirectory = "$OPENDAPBASEDIR/blueridge.renci.org:2/$model/$ADCIRCgrid/$windtag/$year/$mon/$mday/$cycle"
# RunStartTime : 2012080812
if [[ $ENSTORM = nhcConsensus || $ENSTORM = namforecast ]]; then
runStartTime=`grep RunStartTime run.properties | sed 's/RunStartTime.*://' | sed 's/\s//g'` 
year=${runStartTime:0:4} 
month=${runStartTime:4:2} 
day=${runStartTime:6:2} 
currentCycle=`grep currentcycle run.properties | sed 's/currentcycle.*://' | sed 's/\s//g'`
model=`grep ^Model run.properties | sed 's/Model.*://' | sed 's/\s//g'`
mesh=`grep mesh run.properties | sed 's/mesh.*://' | sed 's/\s//g'`
windtag=`grep WindModel run.properties | sed 's/WindModel.*://' | sed 's/\s//g'`
# CERA web app is hard coded to look for blueridge.renci.org:2
openDapDirectory=$OPENDAPBASEDIR/blueridge.renci.org:2/$model/$mesh/$windtag/$year/$month/$day/$currentCycle/$ENSTORM
ssh $OPENDAPHOST -l $OPENDAPUSER -i $SSHKEY "mkdir -p $openDapDirectory" 2>> $SYSLOG 
for file in `ls *.nc *.xmf fort.15 fort.22 run.properties`; do
   logMessage "Making symbolic link on OpenDAP server for $file."
   ssh $OPENDAPHOST -l $OPENDAPUSER -i $SSHKEY "ln -s $OPENDAPDIR/$file $openDapDirectory/$file"
done
scp -i $SSHKEY ${ADVISDIR}/al*.fst ${OPENDAPUSER}@${OPENDAPHOST}:${openDapDirectory} 2>> $SYSLOG
scp -i $SSHKEY ${ADVISDIR}/bal*.dat ${OPENDAPUSER}@${OPENDAPHOST}:${openDapDirectory} 2>> $SYSLOG
if [[ $INSTANCENAME = nodcorps ]]; then
   if [[ $ENSTORM != nhcConGarratt && $ENSTORM != veerLeft ]]; then
      COMMA_SEP_LIST="jason.fleming@seahorsecoastal.com,asgs.cera.lsu@gmail.com"
      subject="ADCIRC NCFS POSTED for $runStartTime $ENSTORM"
      if [[ $TROPICALCYCLONE = on ]]; then
         subject=${subject}" (TROPICAL CYCLONE)"
      fi
      openDAPPrefix="http://opendap.renci.org:1935/thredds/catalog/blueridge.renci.org:2/$model/$mesh"
      httpPathName="http://opendap.renci.org:1935/thredds/fileServer/blueridge.renci.org:2/$model/$mesh"
      path_suffix="$windtag/$year/$month/$day/$currentCycle/$ENSTORM"
      cat <<END > ${STORMDIR}/cera_results_notify.txt 

The ADCIRC NCFS solutions for $runStartDate have been posted to $openDAPPrefix/$path_suffix

The run.properties file is : $httpPathName/$path_suffix/run.properties
  
or wget the file with the following command

wget  $httpPathName/$path_suffix/run.properties
END

      echo "INFO: corps_post.sh: Sending 'results available' email to the following addresses: $COMMA_SEP_LIST."
      cat ${STORMDIR}/cera_results_notify.txt | mail -s "$subject" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
   fi
fi
fi
# Convert max elevation file from netcdf to ascii if necessary
if [[ -e ${STORMDIR}/maxele.63.nc ]]; then
   logMessage "Converting maxele.63.nc from netcdf to ascii."
   ${OUTPUTDIR}/netcdf2adcirc.x --datafile ${STORMDIR}/maxele.63.nc 2>> ${SYSLOG}
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
BOX=LA
# FigureGen executable to use for making JPG files (assumed to be located
# in $OUTPUTDIR/POSTPROC_KMZGIS/FigGen/
FIGUREGENEXECUTABLE=FigureGen32_prompt_inp.x
# The full path and name for the FigureGen template file.
FIGUREGENTEMPLATE=$OUTPUTDIR/POSTPROC_KMZGIS/FigGen/FG_asgs.inp.orig
#
#  now create the Google Earth (kmz), jpg, and GIS contour plots
${OUTPUTDIR}/POSTPROC_KMZGIS/POST_SCRIPT_Corps.sh $ADVISDIR $OUTPUTDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $GRIDFILE $CONFIG $BOX $FIGUREGENEXECUTABLE $FIGUREGENTEMPLATE
#
#  P U B L I C A T I O N
#
# grab the names of the output files
GISKMZJPG=`ls *KMZ_GIS.tar.gz`
PLOTS=`ls *plots.tar.gz`
#
# now create the index.html file to go with the output
perl ${OUTPUTDIR}/corps_index.pl --stormname $STORMNAME --advisory $ADVISORY --templatefile ${OUTPUTDIR}/corps_index_template.html --giskmzjpgarchive $GISKMZJPG --plotsarchive $PLOTS > index.html
#
# now copy plots and visualizations to the website, based on the forcing
# (i.e., NAM or NHC tropical cyclone), machine on which they were run, the 
# grid name, and the advisory 
if [[ $BACKGROUNDMET = on ]]; then
   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "mkdir -p ${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/$ADVISORY"
   scp -i $SSHKEY index.html ${WEBUSER}@${WEBHOST}:${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/$ADVISORY
   scp -i $SSHKEY $GISKMZJPG ${WEBUSER}@${WEBHOST}:${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/$ADVISORY
   scp -i $SSHKEY $PLOTS ${WEBUSER}@${WEBHOST}:${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/$ADVISORY
   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "chmod -R 755 ${WEBPATH}/NAM"
fi
if [[ $TROPICALCYCLONE = on ]]; then 
   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "mkdir -p ${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}"
   scp -i $SSHKEY index.html ${WEBUSER}@${WEBHOST}:${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}
   scp -i $SSHKEY $GISKMZJPG ${WEBUSER}@${WEBHOST}:${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}
   scp -i $SSHKEY $PLOTS ${WEBUSER}@${WEBHOST}:${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}
   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "chmod -R 755 ${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}"
fi
