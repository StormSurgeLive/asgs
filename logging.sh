#!/bin/bash
#----------------------------------------------------------------
#
# logging.sh: This file contains functions required for logging.
# It is sourced by asgs_main.sh and any other shell script that 
# requires logging capabilities. 
#
#----------------------------------------------------------------
# Copyright(C) 2012 Jason Fleming
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
logMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] INFO: $@"
  echo ${MSG} >> ${SYSLOG}
}
#
# send a message to the console (i.e., window where the script was started)
# (these should be rare)
consoleMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
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
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] WARNING: $@"
  echo ${MSG} >> ${SYSLOG}
  echo ${MSG}  # send to console
}
#
# log an error message, notify Operator 
error()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
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
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
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
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] DEBUG: $@"
  echo ${MSG} >> ${SYSLOG}
}

