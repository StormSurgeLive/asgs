#!/bin/bash
#------------------------------------------------------------------------
# opendap_post.sh : Makes results available to thredds data server.
#------------------------------------------------------------------------
# Copyright(C) 2015--2019 Jason Fleming
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
#
# Input example,
#CONFIG=/work/$USER/asgs/asgs-configs/cera-daily/scp-only-test.sh
#ADVISDIR=/work/$USER/asgs764213/2022033018
#STORM=22
#YEAR=2020
#ADVISORY=2022033018
#HPCENV=rostam.cct.lsu.edu
#ENSTORM=namforecast
#CSDATE=2022022800
#HSTIME=2656800.00000000
#GRIDFILE=ec_95d.grd
#OUTPUT=/work/$USER/asgs-dev-opendap/output
#SYSLOG=/work/$USER/log/scp-only-test.asgs-2022-Mar-30-T17:52:47-0500.764213.log
#echo "$1, $2, $3, $4, $5, $6, $7, $8, $9, ${10}, ${11}, ${12}" >> $SCRIPTDIR/scp_files.out

CONFIG=$1
ADVISDIR=$2
STORM=$3
YEAR=$4
ADVISORY=$5
HPCENV=$6
ENSTORM=$7
CSDATE=$8
HSTIME=$9
GRIDFILE=${10}
OUTPUTDIR=${11}
SYSLOG=${12}

source $SCRIPTDIR/properties.sh
source $SCRIPTDIR/monitoring/logging.sh
source $SCRIPTDIR/platforms.sh

THIS=$(basename -- $0)
EXIT_SUCCESS=0
EXIT_ERROR=1

pingWait()
{
  local host=$1
  local count=${2:-1}

  echo "Ping check on $host ..."
  while [ 1 ]; do
    ping -q -n -c $count $host 2>&1 > /dev/null
    local ERR=$?
    if [ $ERR -eq $EXIT_SUCCESS ]; then
      break
    else
      echo "Ping check to $host failed. Will try again in 5 seconds ..."
      sleep 5
    fi
  done
}

# get to right place
SCENARIODIR=${ADVISDIR}/${ENSTORM}

RUNPROPERTIES=$SCENARIODIR/run.properties
declare -A properties
loadProperties $RUNPROPERTIES
GRIDNAME=${properties["adcirc.gridname"]}
ASGSADMIN=${properties["notification.email.asgsadmin"]}
INSTANCENAME=${properties["instancename"]}
TROPICALCYCLONE=${properties["forcing.tropicalcyclone"]}
BACKGROUNDMET=${properties["forcing.backgroundmet"]}

if [ $BACKGROUNDMET == "on" ]; then
  YEAR=$(date +%Y)
fi

declare -a SERVERS
serverList=${properties['post.opendap.tds']}
IFS=' ' read -r -a SERVERS <<< "$serverList"  # FIXME: contains "(" and ")" (don't use eval)
if [[ ${#SERVERS[@]} -eq 0 ]]; then
   warn "cycle $CYCLE: $SCENARIO: $THIS: No opendap servers in $RUNPROPERTIES."
   exit
fi

declare -a FILES
fileList=${properties["post.opendap.files"]} # array of files to post to opendap
IFS=' ' read -r -a FILES <<< "$fileList"  # FIXME: contains "(" and ")" (don't use eval)
if [[ ${#FILES[@]} -eq 0 ]]; then
   warn "cycle $CYCLE: $SCENARIO: $THIS: No files to post to opendap servers in $RUNPROPERTIES."
   exit
fi

for server in ${SERVERS[*]}; do
   if [[ $server = "(" || $server = ")" ]]; then continue; fi
   echo "Sending files to '$server'"

   # load TDS server propers into ENV
   writeTDSProperties $server
   #echo "THREDDSHOST           '$THREDDSHOST'"
   #echo "OPENDAPBASEDIR        '$OPENDAPBASEDIR'"
   #echo "OPENDAPHOST           '$OPENDAPHOST'"
   #echo "OPENDTPORT            '$OPENDAPPORT'"
   #echo "OPENDAPPROTOCOL       '$OPENDAPPROTOCOL'"
   #echo "OPENDAPCATALOGPREFIX  '$CATALOGPREFIX'"
   #echo "OPENDAPDOWNLOADPREFIX '$DOWNLOADPREFIX'"

   # e.g., /data/opendap/2022/nam/2022033012/LAv20a/qbc.loni.org/LAv20a_nam_akheir_10kcms/nowcast
   BASEDIR=$OPENDAPBASEDIR/$YEAR
   TARGDIR=$BASEDIR/$ADVISORY/$GRIDNAME/$HPCENV/$INSTANCENAME/$ENSTORM

   # blocking ping check to avoid ssh "retry"
   pingWait $THREDDSHOST 1

   # ssh command
   ssh $server <<SSH_MKDIRS
     # NOTE: this chunk of text gets executed on \$server,
     # but unless the dollar sign is escaped with a left
     # slash; the value is interpolated on the ASGS machine
     if [ ! -d "$BASEDIR" ]; then
       mkdir -p $BASEDIR &&\
         echo "$BASEDIR" created OK
     else
       echo EXISTS: "$BASEDIR"
     fi
     cd "$BASEDIR"
     if [ ! -d "$TARGDIR" ]; then
       mkdir -p $TARGDIR &&\
         echo created OK: "$TARGDIR"
     else
       echo EXISTS: "$TARGDIR"
     fi
     # NOTE: this chunk of text gets executed on \$server,
     # but unless the dollar sign is escaped with a left
     # slash; the value is interpolated on the ASGS machine
SSH_MKDIRS

   # ensure we're in the directory that contains the files
   # that were just produced
   cd $SCENARIODIR

   # iterate over all files that we care about (in $FILES),
   # and, if they exist, scp them over to $server into the
   # $TARGDIR that was created above

   for file in ${FILES[*]}; do
     if [[ $file = "(" || $file = ")" ]]; then continue; fi
     if [ -e $file ]; then
       scp "${file}" "${server}:${TARGDIR}"
     fi
   done

   # clean up variables that were loaded by virtue of the function,
   # writeTDSProperties, defined in $SCRIPTDIR/platforms.sh
   unset THREDDSHOST
   unset OPENDAPHOST
   unset OPENDAPPORT
   unset OPENDAPPROTOCOL
   unset CATALOGPREFIX
   unset DOWNLOADPREFIX
   unset OPENDAPBASEDIR
done
