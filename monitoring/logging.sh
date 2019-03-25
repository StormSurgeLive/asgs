#!/bin/bash
#----------------------------------------------------------------
#
# logging.sh: This file contains functions required for logging.
# It is sourced by asgs_main.sh and any other shell script that 
# requires logging capabilities. 
#
#----------------------------------------------------------------
# Copyright(C) 2012--2019 Jason Fleming
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
# Log file will be in the directory where the asgs was executed

sigint() {
  allMessage "Received Ctrl-C from console.  Shutting ASGS down...'"
  RMQMessage "EXIT" "EXIT" "asgs_main.sh>sigint()" "EXIT" "Received Ctrl-C from console.  Shutting ASGS down ..." 
  allMessage "Received Ctrl-C from console.  Shutting ASGS instance $INSTANCENAME down. See ya!"
  exit 0
}

RMQMessageStartup() 
{ 
  if [[ ${RMQMessaging_Enable} == "off" ]] ; then return; fi
  DATETIME=`date --utc +'%Y-%h-%d-T%H:%M:%S'`
  FILE2SEND=$1
  ${RMQMessaging_Python} ${RMQMessaging_StartupScript} \
         --Uid $$ \
         --LocationName ${RMQMessaging_LocationName} \
         --ClusterName ${RMQMessaging_ClusterName} \
         --Message "$FILE2SEND"  \
         --InstanceName $INSTANCENAME \
         --Transmit ${RMQMessaging_Transmit} >> $SYSLOG 2>&1
}

RMQMessage()  # MTYPE EVENT PROCESS STATE MSG PCTCOM
{ 
  if [[ ${RMQMessaging_Enable} == "off" ]] ; then return; fi

  DATETIME=`date --utc +'%Y-%h-%d-T%H:%M:%S'`
  MTYPE=$1
  EVENT=$2
  PROCESS=$3
  STATE=$4
  MSG=$5
  #MSG="RMQ-$MTYPE : $EVENT : $STATE : ${DATETIME} : $MSG"
  PCTCOM=0
  if [ "$#" -eq 6 ] ; then PCTCOM=$6 ; fi

  re='^[0-9]+([.][0-9]+)?$' 
  if ! [[ $PCTCOM =~ $re ]] ; then
     warn "PCTCOM ($PCTCOM) not a number in RMQMessage.  Not sending message." 
  else
     printf "RMQ : %s : %s : %4s : %4s : %21s : %4s : %5.1f : %s : %s\n" ${INSTANCENAME} ${ADVISORY} ${MTYPE} ${EVENT} ${DATETIME} ${STATE} ${PCTCOM} ${PROCESS}  "$5" >> $SYSLOG 2>&1

     # Send message to RabbitMQ queue.  The queue parameters are in the asgs_msgr.py code
     ${RMQMessaging_Python} ${RMQMessaging_Script} \
         --Uid $$ \
         --LocationName ${RMQMessaging_LocationName} \
         --ClusterName ${RMQMessaging_ClusterName} \
         --StormNumber $STORM \
         --StormName $STORMNAME \
         --AdvisoryNumber $ADVISORY \
         --Message "$MSG"  \
         --EventType $EVENT \
         --Process $PROCESS \
         --PctComplete $PCTCOM \
         --State $STATE \
         --RunParams $RMQRunParams \
         --InstanceName $INSTANCENAME \
         --Transmit ${RMQMessaging_Transmit} >> ${SYSLOG} 2>&1
   fi
}

logMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] INFO: $@"
  echo ${MSG} >> ${SYSLOG}
}

#
# send a message to the console (i.e., window where the script was started)
# (these should be rare)
consoleMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] INFO: $@"
  echo ${MSG}
}
#
# send a message to console as well as to the log file
allMessage()
{
   consoleMessage $@
   logMessage $@
}
#
# log a warning message, execution continues
warn()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] WARNING: $@"
  echo ${MSG} >> ${SYSLOG}
  #echo ${MSG}  # send to console
}
#
# log an error message, notify Operator 
error()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] ERROR: $@"
  echo ${MSG} >> ${SYSLOG}
  echo ${MSG}  # send to console
  # email the operator
  if [[ $EMAILNOTIFY = yes || $EMAILNOTIFY = YES ]]; then
     echo $MSG | mail -s "[ASGS] Attn: Error for $INSTANCENAME" "${ASGSADMIN}"
  fi 
}
#
# log an error message, execution halts
fatal()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] FATAL ERROR: $@"
  echo ${MSG} >> ${SYSLOG}
  if [[ $EMAILNOTIFY = yes || $EMAILNOTIFY = YES ]]; then
     cat ${SYSLOG} | mail -s "[ASGS] Fatal Error for PROCID ($$)" "${ASGSADMIN}"
  fi
  echo ${MSG} # send to console
  exit ${EXIT_NOT_OK}
}
#
# log a debug message
debugMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'`
  MSG="[${DATETIME}] DEBUG: $@"
  echo ${MSG} >> ${SYSLOG}
}
