#!/bin/bash
#-------------------------------------------------------------------
# ut-nam-notify.sh: Send emails for NAM forced model runs for Texas.
#-------------------------------------------------------------------
# Copyright(C) 2008-2015 Jason Fleming
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
#--------------------------------------------------------------------
#
logMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] INFO: $@"
  echo ${MSG} >> ${SYSLOG}
}

HOSTNAME=$1
STORM=$2
YEAR=$3
STORMDIR=$4
ADVISORY=$5
ENSTORM=$6
GRIDFILE=$7
PHASE=$8    
EMAILNOTIFY=$9
SYSLOG=${10}
ADDRESS_LIST=${11}
ARCHIVEBASE=${12}
ARCHIVEDIR=${13}
#
# simply return if we are not supposed to send out emails
if [[ $EMAILNOTIFY != yes && $EMAILNOTIFY != YES ]]; then
   exit
fi
#
# simply return if there are no email addresses to send email to
if [[ $ADDRESS_LIST = null ]]; then
   exit
fi
#
COMMA_SEP_LIST=${ADDRESS_LIST// /,}
case $PHASE in
#
#               A C T I V A T I O N
#
"activation") 
#
cat <<END > $STORMDIR/activate.txt 2>> ${SYSLOG}
This is an automated message from the ADCIRC Surge Guidance System (ASGS) running on ${HOSTNAME}.

This message is to let you know that the ASGS has been ACTIVATED using NAM forcing on the $GRIDFILE mesh.  

END
    logMessage "Sending activation email to the following addresses: $COMMA_SEP_LIST."
    cat $STORMDIR/activate.txt | mail -s "ASGS Activated on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
#
#              N E W  C Y C L E 
#
"newcycle")
#
cat <<END > $STORMDIR/new_advisory.txt 2>> ${SYSLOG}
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

The supercomputer $HOSTNAME has detected a new NAM cycle (number $ADVISORY) from the National Centers for Environmental Prediction (NCEP). The cycle data has been downloaded. Calculations are about to begin on the $GRIDFILE mesh. 

END
    logMessage "Sending 'new advisory detected' email to the following addresses: $COMMA_SEP_LIST."
     cat $STORMDIR/new_advisory.txt | mail -s "$STORMNAME advisory $ADVISORY detected by ASGS on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1

;;
#
#              R E S U L T S 
#
"results")
#
cat <<END > ${STORMDIR}/post_notify.txt 
This is an automated message from the ADCIRC Surge Guidance System (ASGS) running on ${HOSTNAME}.

The supercomputer $HOSTNAME has produced results for NAM cycle $ADVISORY on the $GRIDFILE mesh. 

END
#
logMessage "Sending 'results notification' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/post_notify.txt | mail -s "ASGS results available for $STORMNAME advisory $ADVISORY from $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
#
#              J O B   F A I L E D 
#
"jobfailed")
#
cat <<END > ${STORMDIR}/job_failed_notify.txt 
This is an automated message from the ADCIRC Surge Guidance System (ASGS) running on ${HOSTNAME}.

A job running on the supercomputer $HOSTNAME has failed when running NAM cycle $ADVISORY on the $GRIDFILE mesh. 

END
#
logMessage "Sending 'job failed' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/job_failed_notify.txt | mail -s "ASGS job $STORMNAME $ADVISORY failed on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
*)
logMessage "ERROR: ut-nam-notify.sh: The notification type was specified as '$PHASE', which is not recognized. Email was not sent."
;;
esac
