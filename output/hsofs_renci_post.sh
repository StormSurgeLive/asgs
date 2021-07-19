#!/bin/bash
#-----------------------------------------------------------------------
# hsofs_renci_post.sh : Post processing for HSOFS mesh with unreduced 
# winds and CERA contours at RENCI.
#-----------------------------------------------------------------------
# Copyright(C) 2017 Jason Fleming
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
# bash ~/asgs/2014stable/output/hsofs_renci_post.sh ~/asgs/2014stable/config/2017/asgs_config_natehsofsNG60cm_hatteras_hsofsx.sh /projects/ncfs/data/asgs19488/15 16 2017 15 hatteras.renci.org nhcConsensus 2017071600 7257600.0 /home/ncfs/asgs/2014stable/input/meshes/hsofs/hsofs.14 ~/asgs/2014stable/output stuff.log ~/.ssh/id_rsa.pub
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
#
# write the intended audience to the run.properties file for CERA
echo "intendedAudience : $INTENDEDAUDIENCE" >> run.properties
#
#-----------------------------------------------------------------------
#          G E N E R A T E   C E R A   C O N T O U R S     
#-----------------------------------------------------------------------
# list the files and time steps that should be contoured
#FILES="maxele.63.nc maxwvel.63.nc swan_HS_max.63.nc swan_TPS_max.63.nc fort.74.nc fort.63.nc swan_HS.63.nc swan_TPS.63.nc"
#
# @jasonfleming: removed fort.74.nc (wind velocity at ground level) because 
# it is not the default display and because it takes a long time (30+ minutes)
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
   #
   # determine whether the CERA contours are complete for the 10m wind
   # ensemble member 
   wind10mContoursHeld=`ls $dirWind10m/cera_contour/*.held 2>> /dev/null | wc -l` # count the number of tasks that haven't started (.held files)
   logMessage "hsofs_renci_post.sh: There are $wind10mContoursHeld .held files for the CERA contours for the 10m winds."
   wind10mContoursStart=`ls $dirWind10m/cera_contour/*.start 2>> /dev/null | wc -l` # count the number of number of tasks that have started but not completed (.start files)
   logMessage "hsofs_renci_post.sh: There are $wind10mContoursStart .start files for the CERA contours for the 10m winds."
   wind10mContoursFinish=`ls $dirWind10m/cera_contour/*.finish 2>> /dev/null | wc -l` # count the number of tasks that have finished (.finish files)
   logMessage "hsofs_renci_post.sh: There are $wind10mContoursFinish .finish files for the CERA contours for the 10m winds."
   if [[ $wind10mContoursHeld -eq 0 && $wind10mContoursStart -eq 0 && $wind10mContoursFinish -ne 0 ]]; then
      wind10mContoursFinished=yes
   fi
   for file in fort.72.nc fort.74.nc maxwvel.63.nc ; do
      if [[ -e $dirWind10m/$file ]]; then
         logMessage "Found $dirWind10m/${file}."
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
#ceraContoursAvailable=no
#shapeDirs=""
#for layer in $layersFinished; do
#   shapeDirs="$shapeDirs CERA/$layer"
#done
# the h parameter tells tar to dereference symbolic links
#tar cvhf CERA.tar $shapeDirs > CERAtar.log 2>> $SYSLOG
# if the CERA.tar file was created successfully, add a property for it
# to the run.properties file
#ERROVALUE=$?  # capture exit status
#if [[ $ERROVALUE == 0 ]] ; then
#   logMessage "$ENSTORM: $THIS: Created CERA tar file correctly."
   #@jasonfleming debug 20170915: don't post CERA tar file
   #echo "Contour Tar File : CERA.tar" >> run.properties
   #ceraContoursAvailable=yes
#else
#   error "$ENSTORM: $THIS: Could not create CERA tar file."
#fi
#------------------------------------------------------------------------
# accumulate min/max
#previousAdvisory=`expr $ADVISORY - 1`
#for file in maxele.63.nc maxinundepth.63.nc maxrs.63.nc maxvel.63.nc maxwvel.63.nc swan_HS_max.63.nc swan_TPS_max.63.nc ; do
#   if [[ -e $file ]]; then
#      ${OUTPUTDIR}/collectMinMax.x --source ../../$previousAdvisory/nowcast/$file --destination $file
#   fi
#done
# 
# produce csv file of maximum data
#${OUTPUTDIR}/make_max_csv.sh $CONFIG $ADVISDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $CSDATE $HSTIME $GRIDFILE $OUTPUTDIR $SYSLOG >> ${SYSLOG} 2>&1
# get name of csv file to add it to the list of files to post to opendap
#csvFileName=`grep "Maximum Values Point CSV File Name" ${STORMDIR}/run.properties | sed 's/Maximum Values Point CSV File Name.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
#-----------------------------------------------------------------------
#         O P E N  D A P    P U B L I C A T I O N 
#-----------------------------------------------------------------------
# jgf20170726: The following files are required by CERA, so they will
# be posted first.
# run.properties maxele.63.nc wind10m.maxwvel.63.nc swan_HS_max.63.nc swan_TPS_max.63.nc fort.63 wind10m.fort.74.nc swan_HS.63.nc swan_TPS.63.nc fort.61.nc fort.15 al*.fst bal*.fst fort.22
logMessage "Creating list of files to post to opendap."
if [[ -e ../al${STORM}${YEAR}.fst ]]; then
   cp ../al${STORM}${YEAR}.fst . 2>> $SYSLOG
fi
if [[ -e ../bal${STORM}${YEAR}.dat ]]; then
   cp ../bal${STORM}${YEAR}.dat . 2>> $SYSLOG
fi
#
#-------------------------------------------------------------------
#               C E R A   F I L E   P R I O R I T Y
#-------------------------------------------------------------------
# @jasonfleming: Hack in a notification email once the bare minimum files
# needed by CERA have been posted. 
#FILES=(`ls *.nc al${STORM}${YEAR}.fst bal${STORM}${YEAR}.dat fort.15 fort.22 CERA.tar run.properties 2>> /dev/null`)
ceraNonPriorityFiles=( `ls endrisinginun.63.nc everdried.63.nc fort.64.nc fort.68.nc fort.71.nc fort.72.nc fort.73.nc initiallydry.63.nc inundationtime.63.nc maxinundepth.63.nc maxrs.63.nc maxvel.63.nc minpr.63.nc rads.64.nc swan_DIR.63.nc swan_DIR_max.63.nc swan_TMM10.63.nc swan_TMM10_max.63.nc $csvFileName` )
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
#
# For each opendap server in the list in ASGS config file.
primaryCount=0
for server in ${TDS[*]}; do
   logMessage "Posting to $server opendap with opendap_post.sh using the following command: ${OUTPUTDIR}/opendap_post_from_JF_master_nam.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server \"${FILES[*]}\" $OPENDAPNOTIFY"
   ${OUTPUTDIR}/opendap_post_from_JF_master_nam.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server "${FILES[*]}" $OPENDAPNOTIFY >> ${SYSLOG} 2>&1
done
