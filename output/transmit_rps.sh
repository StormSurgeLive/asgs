#!/bin/bash
#------------------------------------------------------------------------
# transmit_rps.sh: transmits run.properties to run properties database.
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
THIS=$(basename -- $0)
#
declare -A properties
SCENARIODIR=$PWD
RUNPROPERTIES=$SCENARIODIR/run.properties

if [ -e "$RUNPROPERTIES" ] ; then
        echo "Found run.properties file"
else
        echo "run.properties not found file"
        exit 1
fi

if [[ $# -eq 1 ]]; then
   ppid=$1
else
   # get the grandparent pid, which is the ASGS pid 
   # this is needed for bookkeeping in the rp database
   ppid=`ps -o ppid $$ | sed '1d'|  sed -r 's/^ *//g'`
   ppid=`ps -o ppid $ppid | sed '1d'|  sed -r 's/^ *//g'`
fi

# this script can be called with just one command line option: the
# full path to the run.properties file

# get loadProperties function
SCRIPTDIR=`sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' $RUNPROPERTIES`

source $SCRIPTDIR/properties.sh

# load run.properties file into associative array
loadProperties $RUNPROPERTIES
source $SCRIPTDIR/monitoring/logging.sh
source $SCRIPTDIR/platforms.sh

export SYSLOG=${properties['monitoring.logging.file.syslog']}
export INSTANCENAME=${properties['instancename']}

if [[ ${RMQMessaging_Enable} == "on" ]]; then
  export RMQMessaging_Script_RP=${properties['monitoring.rmqmessaging.scriptrp']}
  export RMQMessaging_LocationName=${properties['monitoring.rmqmessaging.locationname']}
  export RMQMessaging_Transmit=${properties['monitoring.rmqmessaging.transmit']}
fi

# RMQMessageRunProp is in monitoring/logging.sh, 'RMQMessaging_enable'
# is checked in this function
RMQMessageRunProp "$SCENARIODIR" "$ppid"

if [ $? == 0 ]; then
        date > rps.transmit.succeeded
else
        date > rps.transmit.failed
fi

