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
#
# simply return if we are not supposed to send out emails
if [[ $EMAILNOTIFY != yes && $EMAILNOTIFY != YES ]]; then
   exit
fi
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
number $STORM of ${YEAR} on the $GRIDFILE grd. 

The ASGS was activated on $HOSTNAME because 
the storm is forecast to pass within 270 nautical miles of southern Louisiana
at some point in its track. The supercomputer $HOSTNAME 
has downloaded a hurricane forecast from the National Hurricane Center, 
and is performing calculations with ADCIRC to provide storm surge guidance 
for the Louisiana coast and the greater New Orleans metro area.

You will receive an email from the ASGS on $HOSTNAME
as soon as the results of this guidance become available. You will also 
continue to receive storm surge guidance for each forecast that the NHC
issues for this storm, until the storm has passed to the north of the
northern Gulf Coast.

YOU MAY ALSO RECEIVE activation emails from ASGS instances that are running on
supercomputers OTHER THAN ${HOSTNAME}. Those instances 
are completely independent of this one, and will produce results that are
identical to the results produced on ${HOSTNAME}. The other instances 
are running for redundancy purposes.

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

The supercomputer $HOSTNAME has detected a new advisory
(number $ADVISORY) from the National Hurricane Center for storm number
$STORM of ${YEAR}. This advisory has been downloaded; ADCIRC
storm surge calculations are about to begin on the $GRIDFILE 
grid. 

You will receive another email from the ASGS on $HOSTNAME
as soon as the resulting storm surge guidance becomes available.

You may also receive emails notifying you of the detection of 
advisory $ADVISORY for storm $STORM from ASGS instances 
that are running on supercomputers OTHER THAN ${HOSTNAME}. 
The other instances are running for redundancy purposes.  

END
    logMessage "Sending 'new advisory detected' email to the following addresses: $COMMA_SEP_LIST."
     cat $STORMDIR/new_advisory.txt | mail -s "advisory detected by ASGS on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1

;;
#
#              R E S U L T S 
#
"results")
#
cat <<END > ${STORMDIR}/${ADVISORY}/post_notify.txt 
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

The supercomputer $HOSTNAME has produced ADCIRC results for 
storm surge guidance for advisory $ADVISORY for $STORM of $YEAR
on the $GRIDFILE grid. 

These results can be found at the following web site:

http://www.seahorsecoastal.com/ASGS/$STORMYEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_$ADVISORY

The results consist of two archive files; the first contains hydrographs and 
wind speed plots at selected locations as well as the raw data that were
used to produce the plots, and the second contains contour plots of maximum 
surge elevation in GIS, Google Earth, and JPG format.

The ASGS on $HOSTNAME is now waiting for the National Hurricane Center to
issue the next advisory. 

END
#
logMessage "Sending 'results notification' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/${ADVISORY}/post_notify.txt | mail -s "ASGS results available for $STORM advisory $ADVISORY on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
*)
logMessage "ERROR: corps_cyclone_notify.sh: The PHASE was specified as '$PHASE', which is not recognized. Email was not sent."
;;
esac
