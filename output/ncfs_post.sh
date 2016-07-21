#!/bin/bash
#-----------------------------------------------------------------------
# ncfs_post.sh : Post processing for North Carolina.
#-----------------------------------------------------------------------
# Copyright(C) 2011--2016 Jason Fleming
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
cd ${STORMDIR} 2>> ${SYSLOG}
# get the forecast ensemble member number 
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
# grab all config info
si=$ENMEMNUM
. ${CONFIG}
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
# we expect the ASGS config file to tell us how many cera servers there
# are with CERASERVERNUM and assume they are consecutively named 
# cera1, cera2, etc. We alternate the forecast ensemble members evenly 
# among them
CERASERVERNUM=`expr $ENMEMNUM % $NUMCERASERVERS + 1`
CERASERVER=cera$CERASERVERNUM
echo "ceraServer : $CERASERVER" >> run.properties
#
# write the target area to the run.properties file for the CERA web app
if [[ $GRIDNAME = nc6b ]]; then
   echo "asgs : nc" >> run.properties
fi
if [[ $GRIDNAME = "FEMA_R2_norivers_gcs_mNAVD.grd" ]]; then 
   echo "asgs : nynj" >> run.properties
   #echo "remark : Winter Storm Jonas" >> run.properties
fi
if [[ $GRIDNAME = "FEMA_R3" ]]; then 
   echo "asgs : delmarva" >> run.properties
   #echo "remark : Winter Storm Jonas" >> run.properties
fi
#
# write the intended audience to the run.properties file for CERA
echo "intendedAudience : $INTENDEDAUDIENCE" >> run.properties
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
   logMessage "Creating an inundationmask.63.nc file from the initiallydry.63.nc file."
   if [ -e ${OUTPUTDIR}/inundationMask.x ]; then
      ${OUTPUTDIR}/inundationMask.x --filename initiallydry.63.nc --netcdf4 --numpasses 2 2>> ${SYSLOG} 2>&1
      ERROVALUE=$?
      if [ $ERROVALUE == 0 ]; then
         echo "Inundation Mask File Name : inundationmask.63.nc" >> run.properties
         echo "Inundation Mask Format : netcdf" >> run.properties
      else
         error "Failed to create inundationMask.63.nc file."
      fi
   else
      error "The initiallydry.63.nc file was found in $STORMDIR but the inundationMask.x executable was not found in ${OUTPUTDIR}."
   fi
else
   logMessage "The initiallydry.63.nc file was not found, so an inundationmask.63.nc file will not be created."
fi
#
# generate XDMF xml files 
#for file in `ls *.nc`; do
#   ${OUTPUTDIR}/generateXDMF.x --datafile $file 2>> $SYSLOG
#done
#
#--------------------------------------------------------------------------
#             K A L P A N A    K M Z   A N D   G I S 
#--------------------------------------------------------------------------
# Create Google Earth images and shapefiles of water surface elevation 
# and significant wave height using Kalpana.
#--------------------------------------------------------------------------
#
# First, load the GDAL system module, since it is used by the fiona python
# module in Kalpana.
module load gdal/1.11.1_gcc
#
# Now enter the python environment created for Kalpana using virtualenv.
source /projects/ncfs/apps/kalpana/env/bin/activate
#
# Grab the name of the storm from the run.properties file if this is 
# a tropical cyclone; otherwise set the storm name to NAM.
STORMNAMELC=nam
if [[ $TROPICALCYCLONE = on ]]; then
   STORMNAME=`grep "stormname" ${STORMDIR}/run.properties | sed 's/stormname.*://' | sed 's/^\s//g' | tail -n 1` 2>> ${SYSLOG}
   # make the storm name lower case
   STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'` 2>> ${SYSLOG}
fi
#
# Format/construct the name of the storm as Kalpana expects to receive it.
KALPANANAME=asgs.${STORMNAMELC}.${ADVISORY}.${ENSTORM}.${GRIDNAME}.${INSTANCENAME}
#
# Link in the palette file(s) that Kalpana expects to find in the 
# local directory.
ln -s ${OUTPUTDIR}/water-level.pal ${STORMDIR} 2>> ${SYSLOG}
ln -s ${OUTPUTDIR}/wavht.pal ${STORMDIR} 2>> ${SYSLOG}
#
# Link in the logo bar that Kalpana expects to find in the local directory
# for the top of the Google Earth visualization. 
ln -s ${OUTPUTDIR}/kalpana_logo.png ${STORMDIR}/logo.png 2>> ${SYSLOG}
#
# Call the script to generate the input files for Kalpana. Then call
# Kalpana to generate the product, then package up the result.
if [[ -e maxele.63.nc ]]; then
   # maxele kmz
   perl ${OUTPUTDIR}/kalpana_input.pl --template ${OUTPUTDIR}/kalpana_input.template --name $KALPANANAME --filechoice 2 --shape B --vchoice Y --domain Y --l '36 33.5 -60 -100' --lonlatbuffer 0 > input-kml.maxele 2>> kalpana.log
   python ${OUTPUTDIR}/kalpana.py < input-kml.maxele 2>> kalpana.log
   zip Maximum-Water-Levels.kmz Maximum-Water-Levels.kml Colorbar-water-levels.png logo.png 2>> ${SYSLOG}
   rm Maximum-Water-Levels.kml 2>> ${SYSLOG}
   # maxele shapefile
   perl ${OUTPUTDIR}/kalpana_input.pl --template ${OUTPUTDIR}/kalpana_input.template --name $KALPANANAME --filechoice 2 --shape B --vchoice X --domain N > input-shp.maxele 2>> kalpana.log
   python ${OUTPUTDIR}/kalpana.py < input-shp.maxele 2>> kalpana.log
   zip -r Maximum-Water-Levels-gis.zip water-level 2>> ${SYSLOG}
   rm -rf water-level 2>> ${SYSLOG}
fi
#
# Maximum significant wave height
if [[ -e swan_HS_max.63.nc ]]; then
   # swan hs max kmz
   perl ${OUTPUTDIR}/kalpana_input.pl --template ${OUTPUTDIR}/kalpana_input.template --name $KALPANANAME --filechoice 4 --shape B --vchoice Y --domain Y --l '36 33.5 -60 -100' --lonlatbuffer 0 > input-kml.maxhs 2>> kalpana.log
   python ${OUTPUTDIR}/kalpana.py < input-kml.maxhs 2>> kalpana.log
   zip Maximum-Wave-Heights.kmz Maximum-Wave-Heights.kml Colorbar-wave-heights.png logo.png 2>> ${SYSLOG}
   rm Maximum-Wave-Heights.kml 2>> ${SYSLOG}
   # swan hs max shapefile
   perl ${OUTPUTDIR}/kalpana_input.pl --template ${OUTPUTDIR}/kalpana_input.template --name $KALPANANAME --filechoice 4 --shape B --vchoice X --domain N > input-shp.maxhs 2>> kalpana.log
   python ${OUTPUTDIR}/kalpana.py < input-shp.maxhs 2>> kalpana.log
   zip -r Maximum-Wave-Heights-gis.zip wave-height 2>> ${SYSLOG}
   rm -rf wave-height 2>> ${SYSLOG}
fi
#
# Maximum wave periods
if [[ -e swan_TPS_max.63.nc ]]; then
   # swan tps max kmz
   perl ${OUTPUTDIR}/kalpana_input.pl --template ${OUTPUTDIR}/kalpana_input.template --name $KALPANANAME --filechoice 8 --shape B --vchoice Y --domain Y --l '36 33.5 -60 -100' --lonlatbuffer 0 > input-kml.maxTPS 2>> kalpana.log
   python ${OUTPUTDIR}/kalpana.py < input-kml.maxTPS 2>> kalpana.log
   zip Maximum-Wave-Period.kmz Maximum-Wave-Period.kml Colorbar-wave-periods.png logo.png 2>> ${SYSLOG}
   rm Maximum-Wave-Period.kml 2>> ${SYSLOG}
   # maxele shapefile
   perl ${OUTPUTDIR}/kalpana_input.pl --template ${OUTPUTDIR}/kalpana_input.template --name $KALPANANAME --filechoice 8 --shape B --vchoice X --domain N > input-shp.maxTPS 2>> kalpana.log
   python ${OUTPUTDIR}/kalpana.py < input-shp.maxTPS 2>> kalpana.log
   zip -r Maximum-Wave-Periods-gis.zip wave-period 2>> ${SYSLOG}
   rm -rf wave-period 2>> ${SYSLOG}
fi
#
# Unload the GDAL module since we no longer need it.
module unload gdal/1.11.1_gcc

#--------------------------------------------------------------------------
#          O P E N D A P   P U B L I C A T I O N
#--------------------------------------------------------------------------
#
# construct the opendap directory path where the results will be posted
#
STORMNAMEPATH=null
DOWNLOADPREFIX="http://opendap.renci.org:1935/thredds/fileServer"
CATALOGPREFIX="http://opendap.renci.org:1935/thredds/catalog"
if [[ $BACKGROUNDMET = on ]]; then
   # for NAM, the "advisory number" is actually the cycle time 
   STORMNAMEPATH=tc/nam
fi
currentDir=NCFS_CURRENT_DAILY
if [[ $TROPICALCYCLONE = on ]]; then
   STORMNAME=`grep "stormname" ${STORMDIR}/run.properties | sed 's/stormname.*://' | sed 's/^\s//g' | tail -n 1` 2>> ${SYSLOG}
   STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'`
   STORMNAMEPATH=tc/$STORMNAMELC
   currentDir=NCFS_CURRENT_TROPICAL
fi
OPENDAPSUFFIX=$ADVISORY/$GRIDNAME/$HOSTNAME/$INSTANCENAME/$ENSTORM
# put the opendap download url in the run.properties file for CERA to find
downloadURL=$DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX
echo "downloadurl : $downloadURL" >> run.properties
# now actually make the directory (OPENDAPBASEDIR is specified in CONFIG)
OPENDAPDIR=$OPENDAPBASEDIR/$STORMNAMEPATH/$OPENDAPSUFFIX
mkdir -p $OPENDAPDIR 2>> ${SYSLOG}
# make symbolic links from the opendap dir to the important files for the run
cd $OPENDAPDIR 2>> ${SYSLOG}
for file in `ls ${STORMDIR}/*.nc ${STORMDIR}/run.properties`; do  
   if [ -e $file ]; then
      ln -s $file . 2>> ${SYSLOG}
   fi
done
#ln -s ${ADVISDIR}/${ENSTORM}/*.xmf . 2>> ${SYSLOG}
#ln -s ${ADVISDIR}/${ENSTORM}/*.kmz . 2>> ${SYSLOG}
#
# Link to input files to document how the run was performed.
ln -s ${ADVISDIR}/${ENSTORM}/fort.14 .  2>> ${SYSLOG}
ln -s ${ADVISDIR}/${ENSTORM}/fort.15 . 2>> ${SYSLOG}
for file in fort.13 fort.22 fort.26 fort.221 fort.222 ; do 
   if [ -e ${ADVISDIR}/${ENSTORM}/$file ]; then
      ln -s ${ADVISDIR}/${ENSTORM}/$file . 2>> ${SYSLOG}
   fi
done
#
# Link to the tropical cyclone forecast/advisory and tc best track
# file, if available. 
for file in al*.fst bal*.dat ; do 
   if [ -e ${ADVISDIR}/$file ]; then
      ln -s ${ADVISDIR}/$file . 2>> ${SYSLOG}
   fi
done
#
# Link to the shapefile zip and the kmz files if present.
for file in `ls ${ADVISDIR}/${ENSTORM}/*.zip 2>> ${SYSLOG}`; do 
   ln -s $file . 2>> ${SYSLOG}
done
for file in `ls ${ADVISDIR}/${ENSTORM}/*.kmz 2>> ${SYSLOG}`; do 
   ln -s $file . 2>> ${SYSLOG}
done
#
# 20150826: Make symbolic links to a single location on the opendap server
# to reflect the "latest" results. There are actually two locations, one for 
# daily results, and one for tropical cyclone results. 
if [[ $ENSTORM = namforecast || $ENSTORM = nhcConsensus ]]; then
   currentResultsPath=/projects/ncfs/opendap/data/$currentDir
   cd $currentResultsPath 2>> ${SYSLOG}
   # get rid of the old symbolic links
   rm -rf * 2>> ${SYSLOG}
   # make new symbolic links
   for file in $STORMDIR/fort.*.nc $STORMDIR/swan*.nc $STORMDIR/max*.nc $STORMDIR/min*.nc $STORMDIR/run.properties $STORMDIR/fort.14 $STORMDIR/fort.15 $STORMDIR/fort.13 $STORMDIR/fort.22 $STORMDIR/fort.26 $STORMDIR/fort.221 $STORMDIR/fort.222 $ADVISDIR/al*.fst $ADVISDIR/bal*.dat $STORMDIR/*.zip $STORMDIR/*.kmz ; do 
      if [ -e $file ]; then
         ln -s $file . 2>> ${SYSLOG}
      else
         logMessage "The directory does not have ${file}."
      fi
   done
fi
#
# Copy the latest run.properties file to a consistent location in opendap
cp run.properties /projects/ncfs/opendap/data/NCFS_CURRENT/run.properties.${HOSTNAME}.${INSTANCENAME} 2>> ${SYSLOG}
#
# send an email to CERA web application to notify it that results are ready
COMMA_SEP_LIST="jason.g.fleming@gmail.com,nc.cera.renci2@gmail.com"
#COMMA_SEP_LIST="jason.fleming@seahorsecoastal.com"
runStartTime=`grep RunStartTime run.properties | sed 's/RunStartTime.*://' | sed 's/\s//g'`
subject="ADCIRC NCFS $runStartTime $HOSTNAME.$INSTANCENAME $ENMEMNUM"
if [[ $TROPICALCYCLONE = on ]]; then
   subject=${subject}" (TC)"
fi
#subject="${subject} $CERASERVER"
echo "INFO: ncfs_post.sh: The cera_results_notify.txt email subject line is '$subject'." >> ${SYSLOG}
cat <<END > ${STORMDIR}/cera_results_notify.txt 

The ADCIRC NCFS solutions for $ADVISORY have been posted to $CATALOGPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX

The run.properties file is : $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
   
or wget the file with the following command

wget $DOWNLOADPREFIX/$STORMNAMEPATH/$OPENDAPSUFFIX/run.properties
END
#
echo "INFO: ncfs_post.sh: Sending 'results available' email to the following addresses: $COMMA_SEP_LIST." >> $SYSLOG
cat ${STORMDIR}/cera_results_notify.txt | mail -s "$subject" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
