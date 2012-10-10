#!/bin/bash
#
# Copyright(C) 2008, 2009, 2010 Jason Fleming
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
#
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
STORMCLASSNAME=`cat nhcClassName`
# find the space between the storm class (TD, TS, HU, etc) and the NHC name
ind=`expr index "$STORMCLASSNAME" ' '`
# just use the storm's name 
STORMNAME=${STORMCLASSNAME:$ind}
COMMA_SEP_LIST=${ADDRESS_LIST// /,}
case $PHASE in
#
#               A C T I V A T I O N
#
"activation") 
#
cat <<END > $STORMDIR/activate.txt 2>> ${SYSLOG}
This is an automated message from the ADCIRC Surge Guidance System (ASGS) running on ${HOSTNAME}.

This message is to let you know that the ASGS has been ACTIVATED for $STORMCLASSNAME ${YEAR} on the $GRIDFILE grd.  The ASGS was activated on $HOSTNAME because the storm is forecast to impact the Texas Coast.

The supercomputer $HOSTNAME has downloaded a hurricane forecast from the National Hurricane Center, and is performing calculations with SWAN+ADCIRC to predict the storm surge and wind speed along the complete Texas Coast as well as Galveston Bay.

You will receive an email from the ASGS on $HOSTNAME as soon as the results of this guidance become available. You will also continue to receive storm surge guidance for each forecast that the NHC issues for this storm, until the storm has passed through Texas.

END
    echo "Sending activation email to the following addresses: $COMMA_SEP_LIST."
    cat $STORMDIR/activate.txt | mail -s "ASGS Activated on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
#
#              N E W  C Y C L E 
#
"newcycle")
#
cat <<END > $STORMDIR/new_advisory.txt 2>> ${SYSLOG}
This is an automated message from the ADCIRC Surge Guidance System (ASGS) running on ${HOSTNAME}.

The supercomputer $HOSTNAME has detected a new advisory (number $ADVISORY) from the National Hurricane Center for $STORMCLASSNAME ${YEAR}. This advisory has been downloaded; SWAN+ADCIRC wave and storm surge calculations are about to begin on the $GRIDFILE mesh. 

You will receive another email from the ASGS on $HOSTNAME as soon as the resulting storm surge guidance becomes available.

You may also receive emails notifying you of the detection of advisory $ADVISORY for storm $STORM from ASGS instances that are running on supercomputers OTHER THAN ${HOSTNAME}. The other instances are running for redundancy purposes.  

END
    echo "Sending 'new advisory detected' email to the following addresses: $COMMA_SEP_LIST."
     cat $STORMDIR/new_advisory.txt | mail -s "$STORMNAME advisory $ADVISORY detected by ASGS on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1

;;
#
#              R E S U L T S 
#
"results")
#
cat <<END > ${STORMDIR}/post_notify.txt 
This is an automated message from the ADCIRC Surge Guidance System (ASGS) running on ${HOSTNAME}.

The supercomputer $HOSTNAME has produced SWAN+ADCIRC results for storm surge guidance for advisory $ADVISORY for $STORMCLASSNAME $YEAR on the $GRIDFILE mesh. 

The results have been posted on Corral:
$ARCHIVEBASE/$ARCHIVEDIR/$ADVISORY

The ASGS on $HOSTNAME is now waiting for the National Hurricane Center to issue the next advisory. 

END
#
echo "Sending 'results notification' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/post_notify.txt | mail -s "ASGS results available for $STORMNAME advisory $ADVISORY from $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
#
#              J O B   F A I L E D 
#
"jobfailed")
#
cat <<END > ${STORMDIR}/job_failed_notify.txt 
This is an automated message from the ADCIRC Surge Guidance System (ASGS) running on ${HOSTNAME}.

A job running on the supercomputer $HOSTNAME has failed when running storm surge guidance for advisory $ADVISORY for $STORMCLASSNAME $YEAR on the $GRIDFILE mesh. 

END
#
echo "Sending 'job failed' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/job_failed_notify.txt | mail -s "ASGS job $STORMNAME $ADVISORY failed on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
*)
echo "ERROR: corps_cyclone_notify.sh: The notification type was specified as '$PHASE', which is not recognized. Email was not sent."
;;
esac
