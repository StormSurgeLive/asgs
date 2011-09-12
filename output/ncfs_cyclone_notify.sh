#!/bin/bash
#
# Copyright(C) 2008, 2009, 2010, 2011 Jason Fleming
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
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

This message is to let you know that the ASGS has been ACTIVATED for storm
number $STORM of ${YEAR} on the $GRIDFILE mesh. 

You will receive an email from the ASGS on $HOSTNAME
as soon as the results of this guidance become available.

END
    echo "INFO: ncfs_cyclone_notify.sh: Sending activation email to the following addresses: $COMMA_SEP_LIST."
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

The supercomputer $HOSTNAME has detected a new advisory
(number $ADVISORY) from the National Hurricane Center for
$STORMCLASSNAME ${YEAR}. This advisory has been downloaded; ADCIRC
storm surge calculations are about to begin on the $GRIDFILE 
grid. 

You will receive another email from the ASGS on $HOSTNAME
as soon as the resulting storm surge guidance becomes available.


END
    echo "INFO: ncfs_cyclone_notify.sh: Sending 'new advisory detected' email to the following addresses: $COMMA_SEP_LIST."
     cat $STORMDIR/new_advisory.txt | mail -s "ASGS: $STORMNAME advisory $ADVISOORY detected by ASGS on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1

;;
#
#              R E S U L T S 
#
"results")
#
cat <<END > ${STORMDIR}/post_notify.txt 
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

The supercomputer $HOSTNAME has produced ADCIRC results for 
storm surge guidance for advisory $ADVISORY for $STORMCLASSNAME $YEAR
on the $GRIDFILE mesh. 

The ASGS on $HOSTNAME is now waiting for the National Hurricane Center to
issue the next advisory. 

END
#
echo "INFO: ncfs_cyclone_notify.sh: Sending 'results notification' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/post_notify.txt | mail -s "ASGS results available for $STORM advisory $ADVISORY from $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
#
#               J O B   F A I L E D  
#
"jobfailed")
#
cat <<END > ${STORMDIR}/jobfailed_notify.txt 
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

The supercomputer $HOSTNAME has experienced job failure for 
advisory $ADVISORY for $STORMCLASSNAME  $YEAR on the $GRIDFILE mesh. 

The ASGS on $HOSTNAME is now waiting for the National Hurricane Center to
issue the next advisory. 

END
#
echo "INFO: ncfs_cyclone_notify.sh: Sending 'job failed' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/jobfailed_notify.txt | mail -s "ASGS job failure for $STORMNAME advisory $ADVISORY on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
*)
echo "ERROR: ncfs_cyclone_notify.sh: The notification type was specified as '$PHASE', which is not recognized. Email was not sent."
;;
esac
