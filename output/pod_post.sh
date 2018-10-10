#!/bin/bash
#-----------------------------------------------------------------------
# blanton_post.sh : Post processing for North Carolina.
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
# bash ~/asgs/2014stable/output/ncfs_post.sh ~/asgs/2014stable/config/2017/asgs_config_nam_swan_river_hatteras_nc9.99wrivers.sh /projects/ncfs/data/asgs10124/2017073100 99 2017 2017073100 \
# hatteras.renci.org namforecast 2017012400 16243200.0 fort.14 ~/asgs/2014stable/output stuff.log ~/.ssh/id_rsa.pub
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
THIS=blanton_post.sh
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
#
# write the intended audience to the run.properties file for CERA
echo "intendedAudience : $INTENDEDAUDIENCE" >> run.properties

#-----------------------------------------------------------------------
#         O P E N  D A P    P U B L I C A T I O N 
#-----------------------------------------------------------------------
#
OPENDAPDIR=""
#
# For each opendap server in the list in ASGS config file.
primaryCount=0
CURRENT_EVENT="POST"
CURRENT_STATE="RUNN"
FILES="run.properties fort.22 fort.61.nc maxvel.63.nc maxele.63.nc fort.15 fort.73.nc fort.74.nc fort.63.nc fort.64.nc maxwvel.63.nc"
#FILES="swan_DIR.63.nc swan_DIR_max.63.nc swan_HS.63.nc swan_HS_max.63.nc swan_TMM10.63.nc swan_TMM10_max.63.nc swan_TPS.63.nc swan_TPS_max.63.nc"

for server in ${TDS[*]}; do
   RMQMessage "INFO" "$CURRENT_EVENT" "$THIS" "$CURRENT_STATE" "Posting to $server opendap, $ADVISORY $HOSTNAME $ENSTORM $HSTIME"
   logMessage "$ENSTORM: $THIS: Posting to $server opendap using the following command: ${OUTPUTDIR}/opendap_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server \"${FILES[*]}\" $OPENDAPNOTIFY"
   ${OUTPUTDIR}/opendap_post.sh $CONFIG $ADVISDIR $ADVISORY $HOSTNAME $ENSTORM $HSTIME $SYSLOG $server "${FILES[*]}" $OPENDAPNOTIFY # >> ${SYSLOG} 2>&1
   #RMQMessage "INFO" "$CURRENT_EVENT" "$THIS>$ENSTORM" "$CURRENT_STATE" "$com"
   #$com
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
      # Establish the method of posting results for service via opendap
      OPENDAPPOSTMETHOD=scp
      for hpc in ${COPYABLEHOSTS[*]}; do
         if [[ $hpc = $TARGET ]]; then
            OPENDAPPOSTMETHOD=copy
         fi
      done
      for hpc in ${LINKABLEHOSTS[*]}; do
         if [[ $hpc = $TARGET ]]; then
            OPENDAPPOSTMETHOD=link
         fi
      done
      case $OPENDAPPOSTMETHOD in
      "scp")
         if [[ $SSHPORT != "null" ]]; then
            ssh $OPENDAPHOST -l $OPENDAPUSER -p $SSHPORT "echo $propertyName : $backupURL >> $OPENDAPDIR/run.properties"
         else
            ssh $OPENDAPHOST -l $OPENDAPUSER "echo $propertyName : $backupURL >> $OPENDAPDIR/run.properties"
        fi
        ;;
      "copy"|"link")
         echo $propertyName : $backupURL >> $OPENDAPDIR/run.properties 2>> $SYSLOG
         ;;
      *)
         warn "$ENSTORM: $THIS: OPeNDAP post method unrecogrnized."
         ;;
      esac
      backupCount=$(($backupCount+1))
   done
   primaryCount=$((primaryCount+1))
done
CURRENT_STATE="CMPL"
RMQMessage "INFO" "$CURRENT_EVENT" "$THIS" "$CURRENT_STATE" "Finished Posting to $server opendap"
