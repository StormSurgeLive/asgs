#!/bin/bash
#----------------------------------------------------------------
# swanHotstart.sh: Obtains and prepares SWAN hotstart files.
#----------------------------------------------------------------
# Copyright(C) 2019 Jason Fleming
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
#----------------------------------------------------------------
#
# Case A: SWAN subdomain hotstart files are available from the
#         previous run on and the number of subdomains is the 
#         same between the two runs.
#         --> copy the subdomain swan.67 files to the current run
#
# Case B: An archive of SWAN subdomain hotstart files exists
#         from the previous run and the number of subdomains 
#         is the same between the two runs.
#         --> check to make sure the archiving process has finished,
#             waiting up to 10 minutes if it has not; then copy
#             the archive and unpack it in the current directory;
#             leave the SWAN hotstart archive in place so it can
#             be posted to opendap or used again later
#
# Case C: A fulldomain SWAN hotstart file exists from the previous
#         run and either the number of subdomains is different
#         between the two runs or Cases A and B are not viable for
#         some reason. 
#         --> check to make sure the composition process is
#             finished if it was running locally; wait up to 10
#             minutes if it has not; decompress and decompose
#             the fulldomain SWAN hotstart file to subdomains 
#             as a batch job; retain and recompress the SWAN 
#             fulldomain hotstart file (as a background job)
#             so that it can be posted to opendap. 
# 
# Case D: Cases A-C are not viable for some reason.
#         --> Set SWAN to cold start.
#
prepareSWANHotstart() {
#
#  H O T S T A R T I N G   S W A N 
# 
# Globalizing and localizing the SWAN hotstart files can take
# a significant amount of time and must be done in serial. If the
# number of subdomains for this run is the same as the number of 
# subdomains used in the run used as the source of SWAN hotstart files, 
# then try to use the SWAN subdomain hotstart files directly. 
logMessage "$ENSTORM: $THIS: Preparing SWAN hotstart file."
swanHotstartOK=no
# See if the previous run used the same number of subdomains as
# the current run.
sameSubdomains=yes
declare -A fromProps # properties of the run we are hotstarting from
propsName=run.properties
if [[ $hotstartURL != null ]]; then
   propsName=from.run.properties
fi
loadProperties $SCENARIODIR 
loadAltProperties $FROMDIR/$propsName $fromProps
if [[ ${properties[]}  
          # if archiving of the hotstart source run has started but is not
          # complete, wait until it is complete so that we don't 
          # accidentally ingest partially complete tar files or partially
          # globalized fulldomain swan hotstart files or start to copy 
          # subdomain swan hotstart files that are being deleted by an
          # archiving script
          logMessage "$ENSTORM: $THIS: Detecting time that SWAN hotstart archiving process started in ${FROMDIR}."
          swanArchiveStart=`sed -n 's/[ ^]*$//;s/time.archive.start\s*:\s*//p' $FROMDIR/run.properties`
          if [[ ! -z $swanArchiveStart ]]; then 
             # archiving process has started
             logMessage "$ENSTORM: $THIS: The archiving process for the hotstart source run started at $swanArchiveStart."
             waitMinutes=0 # number of minutes waiting for the archiving process to complete
             waitMinutesMax=60  # max number of minutes to wait for upstream archiving process to finish
             while [[ $waitMinutes -lt $waitMinutesMax ]]; do
                # wait until it is finished or has errored out
                logMessage "$ENSTORM: $THIS: Detecting finish or error condition for archiving SWAN hotstart files in ${FROMDIR}."
                swanArchiveFinish=`sed -n 's/[ ^]*$//;s/time.archive.finish\s*:\s*//p' $FROMDIR/run.properties`
                swanArchiveError=`sed -n 's/[ ^]*$//;s/time.archive.error\s*:\s*//p' $FROMDIR/run.properties`
                if [[ ! -z $swanArchiveFinish || ! -z $swanArchiveError ]]; then
                   logMessage "$ENSTORM: $THIS: The archiving process for the hotstart source run has finished."
                   break
                else
                   sleep 60
                   waitMinutes=`expr $waitMinutes + 1`
                fi
             done
             if [[ $waitMinutes -ge 60 ]]; then
                warn "$ENSTORM: $THIS: The archiving process for the hotstart source run did not finish within $watiMinutesMax minutes. Attempting to collect SWAN hotstart files anyway."            
             fi
          else
             # FIXME: how to handle this situation?
             warn "$ENSTORM: $THIS: The SWAN hotstart archiving process has not started in ${FROMDIR}."
          fi
          logMessage "$ENSTORM: $THIS: Detecting number of subdomains for SWAN hotstart files in ${FROMDIR}."
          hotSubdomains=`sed -n 's/[ ^]*$//;s/hpc.job.padcswan.ncpu\s*:\s*//p' $FROMDIR/run.properties`
          logMessage "hotSubdomains is $hotSubdomains ; NCPU is $NCPU ; FROMDIR is $FROMDIR"
          if [[ $hotSubdomains = $NCPU ]]; then
             logMessage "$ENSTORM: $THIS: The number of subdomains is the same as hotstart source; subdomain SWAN hotstart files will be copied directly."
             # subdomain swan hotstart files
             if [[ -e $FROMDIR/PE0000/swan.67 ]]; then
                logMessage "$ENSTORM: $THIS: Starting copy of subdomain swan hotstart files."
                # copy the subdomain hotstart files over
                # subdomain hotstart files are always binary formatted
                PE=0
                format="%04d"
                while [ $PE -lt $NCPU ]; do
                   PESTRING=`printf "$format" $PE`
                   cp $FROMDIR/PE${PESTRING}/swan.67 $SCENARIODIR/PE${PESTRING}/swan.68 2>> ${SYSLOG}
                   PE=`expr $PE + 1`
                done
                logMessage "$ENSTORM: $THIS: Completed copy of subdomain hotstart files."
                swanHotstartOK=yes
             fi
             # subdomain SWAN hotstart files in a tar archive
             if [[ $swanHotstartOK = no ]]; then
                logMessage "$ENSTORM: $THIS: Could not copy subdomain SWAN hotstart files directly."
                for suffix in tar tar.gz tar.bz2 ; do 
                   logMessage "$ENSTORM: $THIS: Looking for ${FROMDIR}/swan.67.${suffix}."
                   if [[ -e $FROMDIR/swan.67.${suffix} ]]; then
                      logMessage "$ENSTORM: $THIS: Found $FROMDIR/swan.67.${suffix}."
                      cp $FROMDIR/swan.67.${suffix} ./swan.68.${suffix} 2>> $SYSLOG
                      scenarioMessage "$THIS: Untarring SWAN hotstart files:"
                      case $suffix in
                      tar)
                         tar xvf swan.68.${suffix} >> scenario.log 2> >(awk -v this='asgs_main.sh>prep' -v level=ERROR -f $SCRIPTDIR/monitoring/timestamp.awk | tee -a ${SYSLOG})
                         if [[ $? == 0 ]]; then swanHotstartOK=yes ; fi
                         ;;
                      tar.gz)
                         tar xvzf swan.68.${suffix} >> scenario.log 2> >(awk -v this='asgs_main.sh>prep' -v level=ERROR -f $SCRIPTDIR/monitoring/timestamp.awk | tee -a ${SYSLOG})
                         if [[ $? == 0 ]]; then swanHotstartOK=yes ; fi
                         ;;
                      tar.bz2)
                         tar xvjf swan.68.${suffix} >> scenario.log 2> >(awk -v this='asgs_main.sh>prep' -v level=ERROR -f $SCRIPTDIR/monitoring/timestamp.awk | tee -a ${SYSLOG})
                         if [[ $? == 0 ]]; then swanHotstartOK=yes ; fi                         
                         ;;
                      *)
                         warn "$ENSTORM: $THIS: SWAN hotstart file archive $FROMDIR/swan.67.${suffix} unrecognized."
                         ;;
                      esac
                      for dir in `ls -d PE*`; do
                         mv $dir/swan.67 $dir/swan.68 2>> $SYSLOG
                      done
                      rm swan.68.${suffix} 2>> $SYSLOG
                      break
                   fi
                done
             fi
             if [[ $swanHotstartOK = no ]]; then
                logMessage "$ENSTORM: $THIS: Failed to obtain subdomain SWAN hotstart files."
             fi
          else
             logMessage "$ENSTORM: $THIS: The number of subdomains is different from the hotstart source; a fulldomain SWAN hotstart file will be decomposed to the subdomains."
          fi
          # 
          # if copying subdomain SWAN hotstart files did not work
          # or is not appropriate because the number of subdomains in
          # this run is different from the hotstart source, try to 
          # decompose a fulldomain SWAN hotstart file
          if [[ $swanHotstartOK = no ]]; then
             logMessage "$ENSTORM: $THIS: Decomposing fulldomain SWAN hotstart file."
             # fulldomain swan hotstart file or archive of subdomain
             # swan hotstart files
             if [[ -e $FROMDIR/swan.67 ]]; then
                cp $FROMDIR/swan.67 ./swan.68 2>> $SYSLOG
             elif [[ -e $FROMDIR/swan.67.gz ]]; then
                cp $FROMDIR/swan.67.gz ./swan.68.gz 2>> $SYSLOG
                gunzip swan.68.gz 2>> $SYSLOG
             elif [[ -e $FROMDIR/swan.67.bz2 ]]; then
                cp $FROMDIR/swan.67.bz2 ./swan.68.bz2 2>> $SYSLOG
                bunzip2 swan.68.bz2 2>> $SYSLOG
             fi
             if [[ -e  swan.68 ]]; then
                logMessage "$ENSTORM: $THIS: Starting decomposition of fulldomain swan hotstart file to subdomains."
                ${ADCIRCDIR}/../swan/unhcat.exe <<EOF 2>> ${SYSLOG}
2
swan.68
F
EOF
                if [[ $? == 0 ]]; then swanHotstartOK=yes ; fi
             fi
             if [[ $swanHotstartOK = yes ]]; then
                logMessage "$ENSTORM: $THIS: Completed decomposition of fulldomain swan hotstart file."
             else
                error "$ENSTORM: $THIS: Failed to obtain any swan hotstart file."              
             fi
          fi             
       fi
       if [[ $WAVES = on && $HOTSWAN = off ]]; then
          logMessage "$ENSTORM: $THIS: SWAN coupling is active but SWAN hotstart files are not available in $FROMDIR. SWAN will be cold started."
       fi

}
