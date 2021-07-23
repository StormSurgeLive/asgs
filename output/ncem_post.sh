#!/bin/bash
#-----------------------------------------------------------------------
# ncfs_post.sh : Post processing for North Carolina.
#-----------------------------------------------------------------------
# Copyright(C) 2011--2017 Jason Fleming
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
# example invokation:
# grep -i cold run.properties
# grep -i hot run.properties
# bash ~/asgs/2014stable/output/ncfs_post.sh ~/asgs/2014stable/config/2017/asgs_config_nam_swan_river_hatteras_nc9.99wrivers.sh /projects/ncfs/data/asgs10124/2017073100 99 2017 2017073100 hatteras.renci.org namforecast 2017012400 16243200.0 fort.14 ~/asgs/2014stable/output stuff.log ~/.ssh/id_rsa.pub
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
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR} 2>> ${SYSLOG}
THIS=$(basename -- $0)
# get the forecast ensemble member number 
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
# grab all config info
si=$ENMEMNUM
. ${CONFIG}
# Bring in logging functions
. ${SCRIPTDIR}/monitoring/logging.sh
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
   logMessage "$ENSTORM: $THIS: Creating an inundationmask.63.nc file from the initiallydry.63.nc file."
   if [ -e ${OUTPUTDIR}/inundationMask.x ]; then
      ${OUTPUTDIR}/inundationMask.x --filename initiallydry.63.nc --netcdf4 --numpasses 2 2>> ${SYSLOG} 2>&1
      ERROVALUE=$?
      if [ $ERROVALUE == 0 ]; then
         echo "Inundation Mask File Name : inundationmask.63.nc" >> run.properties
         echo "Inundation Mask Format : netcdf" >> run.properties
      else
         error "$ENSTORM: $THIS: Failed to create inundationMask.63.nc file."
      fi
   else
      error "$ENSTORM: $THIS: The initiallydry.63.nc file was found in $STORMDIR but the inundationMask.x executable was not found in ${OUTPUTDIR}."
   fi
else
   logMessage "$ENSTORM: $THIS: The initiallydry.63.nc file was not found, so an inundationmask.63.nc file will not be created."
fi
#
#-----------------------------------------------------------------------
#          G E N E R A T E   C E R A   C O N T O U R S     
#-----------------------------------------------------------------------


POST2CERA=off
if [[ ${POST2CERA} == "on" ]] ; then
# list the files and time steps that should be contoured
#FILES="maxele.63.nc maxwvel.63.nc swan_HS_max.63.nc swan_TPS_max.63.nc fort.74.nc fort.63.nc swan_HS.63.nc swan_TPS.63.nc"
#
# @jasonfleming: removed wind velocity at ground level (fort.74.nc) because 
# it is not the default for display and it takes a long time (30+ minutes)
FILES="maxele.63.nc swan_HS_max.63.nc swan_TPS_max.63.nc fort.63.nc swan_HS.63.nc swan_TPS.63.nc"
# pass these to the cera_contour_post.sh script
${OUTPUTDIR}/cera_contour_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG "$FILES"
# wait until the cera contours are finished, or at least until the 
# total wait time has elapsed 
CERACONTOURWAIT=5 # minutes
ceraContoursFinished=no
minutesElapsed=0
while [[ $ceraContoursFinished = no && $minutesElapsed -lt $CERACONTOURWAIT ]]; do
   contoursHeld=`ls ./cera_contour/*.held 2>> /dev/null | wc -l`
   logMessage "$ENSTORM: $THIS: There are $contoursHeld .held files for the CERA contours."
   contoursStart=`ls ./cera_contour/*.start 2>> /dev/null | wc -l`
   logMessage "$ENSTORM: $THIS: There are $contoursStart .start files for the CERA contours."
   contoursFinish=`ls ./cera_contour/*.finish 2>> /dev/null | wc -l`
   logMessage "$ENSTORM: $THIS: There are $contoursFinish .finish files for the CERA contours."
   if [[ $contoursStart -eq 0 && $contoursHeld -eq 0 ]]; then
      ceraContoursFinished=yes
   fi
   sleep 60
   minutesElapsed=`expr $minutesElapsed + 1`
done
#
# the cera contour wait time has passed, determine which contour sets are
# are complete and available 
layersFinished=""
for layer in wvel elev hsign tps maxelev maxwvel maxhsign maxtps ; do
   if [[ -d $layer ]]; then
      ln -s $PWD/$layer ./CERA/$layer 2>> $SYSLOG
   fi
   contoursHeld=`ls ./cera_contour/${layer}*.held 2>> /dev/null | wc -l`
   contoursStart=`ls ./cera_contour/${layer}*.start 2>> /dev/null | wc -l`
   contoursFinish=`ls ./cera_contour/${layer}*.finish 2>> /dev/null | wc -l`
   if [[ $contoursHeld -eq 0 && $contoursStart -eq 0 ]]; then
      if [[ $contoursFinish -gt 0 ]]; then
         logMessage "$ENSTORM: $THIS: The contours for the $layer layer are finished."
         case $layer in
         "wvel")
            echo "Wind Velocity Contour Tar File Path : CERA/wvel" >> run.properties

            layersFinished="$layersFinished $layer"
            ;;
         "elev")
            echo "Water Surface Elevation Contour Tar File Path : CERA/elev" >> run.properties
            layersFinished="$layersFinished $layer"
            ;;
         "hsign")
            echo "Significant Wave Height Contour Tar File Path : CERA/hsign" >> run.properties
            layersFinished="$layersFinished $layer"
            ;;
         "tps")
            echo "Peak Wave Period Contour Tar File Path : CERA/tps" >> run.properties
            layersFinished="$layersFinished $layer"
            ;;
         "maxelev")
            echo "Maximum Water Surface Elevation Contour Tar File Path : CERA/maxelevshp" >> run.properties
            layersFinished="$layersFinished maxelevshp"
            ;;
         "maxwvel")
            echo "Maximum Wind Speed Tar File Path : CERA/maxwvelshp" >> run.properties
            layersFinished="$layersFinished maxwvelshp"
            ;;
         "maxhsign")
            echo "Maximum Significant Wave Height Contour Tar File Path : CERA/maxhsignshp" >> run.properties
            layersFinished="$layersFinished maxhsignshp"
            ;;
         "maxtps")
            echo "Maximum Peak Wave Period Contour Tar File Path : CERA/maxtpsshp" >> run.properties
            layersFinished="$layersFinished maxtpsshp"
            ;;
         *)
            error "Files named $file are not supported."
            break
         esac
      else
         logMessage "$ENSTORM: $THIS: The contours for the $layer layer were never started."
      fi
   else
      logMessage "$ENSTORM: $THIS: The contours for the $layer layer have not finished."
   fi
done
#
#-----------------------------------------------------------------------
#          I N C L U S I O N   O F   10 M   W I N D S
#-----------------------------------------------------------------------
# If winds at 10m (i.e., wind velocities that do not include the effect
# of land interaction from nodal attributes line directional wind roughness
# and canopy coefficient) were produced by another ensemble member, 
# then include these winds in the post processing
wind10mFound=no
wind10mContoursFinished=no
dirWind10m=$ADVISDIR/${ENSTORM}Wind10m
if [[ -d $dirWind10m ]]; then
   logMessage "Corresponding 10m wind ensemble member was found."
   wind10mFound=yes
   # determine whether the CERA contours are complete for the 10m wind
   # ensemble member 
   wind10mContoursHeld=`ls $dirWind10m/cera_contour/*.held 2>> /dev/null | wc -l`
   logMessage "$ENSTORM: $THIS: There are $wind10mContoursHeld .held files for the CERA contours for the 10m winds."
   wind10mContoursStart=`ls $dirWind10m/cera_contour/*.start 2>> /dev/null | wc -l`
   logMessage "$ENSTORM: $THIS: There are $wind10mContoursStart .start files for the CERA contours for the 10m winds."
   wind10mContoursFinish=`ls $dirWind10m/cera_contour/*.finish 2>> /dev/null | wc -l`
   logMessage "$ENSTORM: $THIS: There are $wind10mContoursFinish .finish files for the CERA contours for the 10m winds."
   if [[ $wind10mContoursHeld -eq 0 && $wind10mContoursStart -eq 0 && $wind10mContoursFinish -ne 0 ]]; then
      wind10mContoursFinished=yes
   fi
   for file in fort.72.nc fort.74.nc maxwvel.63.nc ; do
      if [[ -e $dirWind10m/$file ]]; then
         logMessage "$ENSTORM: $THIS: Found $dirWind10m/${file}."
         ln -s $dirWind10m/${file} ./wind10m.${file}
         # update the run.properties file
         case $file in
         "fort.72.nc")
            echo "Wind Velocity 10m Stations File Name : wind10m.fort.72.nc" >> run.properties
            echo "Wind Velocity 10m Stations Format : netcdf" >> run.properties
            ;;
         "fort.74.nc")
            echo "Wind Velocity 10m File Name : wind10m.fort.74.nc" >> run.properties
            echo "Wind Velocity 10m Format : netcdf" >> run.properties
            # if the CERA contours are available, link to them
            if [[ -d $dirWind10m/wvel ]]; then
               ln -s $dirWind10m/wvel ./CERA/wind10m.wvel 2>> $SYSLOG
               # notify downstream processors via run.properties
               if [[ $wind10mContoursFinished = yes ]]; then
                  echo "Wind Velocity 10m Contour Tar File Path : CERA/wind10m.wvel" >> run.properties
                  layersFinished="$layersFinished wind10m.wvel"
               fi
            fi
            ;;
         "maxwvel.63.nc")
            echo "Maximum Wind Speed 10m File Name : wind10m.maxwvel.63.nc" >> run.properties
            echo "Maximum Wind Speed 10m Format : netcdf" >> run.properties
            if [[ -d $dirWind10m/CERA/maxwvelshp ]]; then
               ln -s $dirWind10m/CERA/maxwvelshp ./CERA/wind10m.maxwvelshp 2>> $SYSLOG
               # notify downstream processors via run.properties
               if [[ $wind10mContoursFinished = yes ]]; then
                  echo "Maximum Wind Velocity 10m Contour Tar File Path : CERA/wind10m.maxwvelshp" >> run.properties
                  layersFinished="$layersFinished wind10m.maxwvelshp"
               fi
            fi
            ;;
         *)
            warn "$ENSTORM: $THIS: The file $file was not recognized."
         ;;
         esac
      else
         logMessage "$ENSTORM: $THIS: The file $dirWind10m/${file} was not found."
      fi
   done
else
   logMessage "$ENSTORM: $THIS: Corresponding 10m wind ensemble member was not found."
fi
#
#-----------------------------------------------------------------------
#      T A R   U P   C E R A   C O N T O U R S      
#-----------------------------------------------------------------------
# form list of directories that should go into the tar file
shapeDirs=""
for layer in $layersFinished; do
   shapeDirs="$shapeDirs CERA/$layer"
done
tar cvhf CERA.tar $shapeDirs > CERAtar.log 2>> $SYSLOG
# if the CERA.tar file was created successfully, add a property for it
# to the run.properties file
ERROVALUE=$?  # capture exit status
if [[ $ERROVALUE == 0 ]] ; then
   # @jasonfleming debug: don't post CERA.tar file 20170915
   #logMessage "$ENSTORM: $THIS: Created CERA tar file correctly."
   #echo "Contour Tar File : CERA.tar" >> run.properties
else
   error "$ENSTORM: $THIS: Could not create CERA tar file."
fi
#
# generate XDMF xml files 
#for file in `ls *.nc`; do
#   ${OUTPUTDIR}/generateXDMF.x --datafile $file 2>> $SYSLOG
#done
#--------------------------------------------------------------------------
#        N C F S   _   C U R R E N T   P U B L I C A T I O N
#--------------------------------------------------------------------------
#
# construct the opendap directory path where the results will be posted
#
currentDir=NCFS_CURRENT_DAILY
if [[ $TROPICALCYCLONE = on ]]; then
   currentDir=NCFS_CURRENT_TROPICAL
fi
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
         logMessage "$ENSTORM: $THIS: The directory does not have ${file}."
      fi
   done
fi
# Copy the latest run.properties file to a consistent location in opendap
cp run.properties /projects/ncfs/opendap/data/NCFS_CURRENT/run.properties.${HOSTNAME}.${INSTANCENAME} 2>> ${SYSLOG}
# switch back to the directory where the results were produced 
cd $STORMDIR 2>> ${SYSLOG}
#
#-------------------------------------------------------------------
#               C E R A   F I L E   P R I O R I T Y
#-------------------------------------------------------------------
# @jasonfleming: Hack in a notification email once the bare minimum files
# needed by CERA have been posted. 
#
#FILES=(`ls *.nc al${STORM}${YEAR}.fst bal${STORM}${YEAR}.dat fort.15 fort.22 CERA.tar run.properties 2>> /dev/null`)
logMessage "$ENSTORM: $THIS: Creating list of files to post to opendap."
if [[ -e ../al${STORM}${YEAR}.fst ]]; then
   cp ../al${STORM}${YEAR}.fst . 2>> $SYSLOG
fi
if [[ -e ../bal${STORM}${YEAR}.dat ]]; then
   cp ../bal${STORM}${YEAR}.dat . 2>> $SYSLOG
fi
ceraNonPriorityFiles=( `ls endrisinginun.63.nc everdried.63.nc fort.64.nc fort.68.nc fort.71.nc fort.72.nc fort.73.nc initiallydry.63.nc inundationtime.63.nc maxinundepth.63.nc maxrs.63.nc maxvel.63.nc minpr.63.nc rads.64.nc swan_DIR.63.nc swan_DIR_max.63.nc swan_TMM10.63.nc swan_TMM10_max.63.nc` )
ceraPriorityFiles=(`ls run.properties maxele.63.nc fort.63.nc fort.61.nc fort.15 fort.22`)
if [[ $ceraContoursAvailable = yes ]]; then
   ceraPriorityFiles=( ${ceraPriorityFiles[*]} "CERA.tar" )
fi
if [[ $TROPICALCYCLONE = on ]]; then
   ceraPriorityFiles=( ${ceraPriorityFiles[*]} `ls al${STORM}${YEAR}.fst bal${STORM}${YEAR}.dat` )
fi
if [[ $WAVES = on ]]; then
   ceraPriorityFiles=( ${ceraPriorityFiles[*]} `ls swan_HS_max.63.nc swan_TPS_max.63.nc swan_HS.63.nc swan_TPS.63.nc` )
fi
dirWind10m=$ADVISDIR/${ENSTORM}Wind10m
if [[ -d $dirWind10m ]]; then
   ceraPriorityFiles=( ${ceraPriorityFiles[*]} `ls wind10m.maxwvel.63.nc wind10m.fort.74.nc` )
   ceraNonPriorityFiles=( ${ceraNonPriorityFiles[*]} `ls maxwvel.63.nc fort.74.nc` )
else
   ceraPriorityFiles=( ${ceraPriorityFiles[*]} `ls maxwvel.63.nc fort.74.nc` )
fi
FILES=( ${ceraPriorityFiles[*]} "sendNotification" ${ceraNonPriorityFiles[*]} )

fi # end of "POST2CERA block
#
#-----------------------------------------------------------------------
#         O P E N  D A P    P U B L I C A T I O N 
#-----------------------------------------------------------------------
#
OPENDAPDIR=""
#
# For each opendap server in the list in ASGS config file.
primaryCount=0
CURRENT_EVENT="POST"
CURRENT_EVENT="RUNN"
RMQMessage "INFO" "$CURRENT_EVENT" "$THIS>$ENSTORM" "$CURRENT_STATE" "Posting to $server opendap, $ADVISORY $HOSTNAME $ENSTORM $HSTIME"
for server in ${TDS[*]}; do
   logMessage "$ENSTORM: $THIS: Posting to $server opendap using the following command: ${OUTPUTDIR}/opendap_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server \"${FILES[*]}\" $OPENDAPNOTIFY"
   ${OUTPUTDIR}/opendap_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server "${FILES[*]}" $OPENDAPNOTIFY >> ${SYSLOG} 2>&1
done
CURRENT_STATE="CMPL"
RMQMessage "INFO" "$CURRENT_EVENT" "$THIS>$ENSTORM" "$CURRENT_STATE" "Finished Posting to $server opendap"
#
#--------------------------------------------------------------------------
#             K A L P A N A    K M Z   A N D   G I S 
#--------------------------------------------------------------------------
# Create Google Earth images and shapefiles of water surface elevation 
# and significant wave height using Kalpana.
#--------------------------------------------------------------------------
#
KALPANA="off"
if [[ ${KALPANA} == "on" ]]; then
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
#
# Post links to shapefiles
OPENDAPDIR=`head -n 1 ${STORMDIR}/opendapdir.log`
cd $OPENDAPDIR 2>> ${SYSLOG}
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
# to reflect the "latest" shapefile and kmz results. There are actually two
# locations, one for daily results, and one for tropical cyclone results. 
if [[ $ENSTORM = namforecast || $ENSTORM = nhcConsensus ]]; then
   currentResultsPath=/projects/ncfs/opendap/data/$currentDir
   cd $currentResultsPath 2>> ${SYSLOG}
   # make new symbolic links
   for file in $STORMDIR/*.zip $STORMDIR/*.kmz ; do 
      if [ -e $file ]; then
         ln -s $file . 2>> ${SYSLOG}
      else
         logMessage "$ENSTORM: $THIS: The directory does not have ${file}."
      fi
   done
fi
fi  # end of KALPANA
