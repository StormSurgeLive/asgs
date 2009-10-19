#!/bin/bash
#
# Copyright(C) 2008, 2009 Jason Fleming
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
activation_email()
{ HOSTNAME=$1
  STORM=$2
  YEAR=$3
  STORMDIR=$4
  ACTIVATE_LIST=$5

COMMA_SEP_LIST=${ACTIVATE_LIST// /,}

cat <<END > $STORMDIR/activate.txt 2>> ${SYSLOG}
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

This message is to let you know that the ASGS has been ACTIVATED for storm
number $STORM of ${YEAR}. 

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
}

new_advisory_email()
{ HOSTNAME=$1
  STORM=$2
  YEAR=$3
  ADVISORY=$4
  NEW_ADVISORY_LIST=$5

  # replace spaces in mailing list with commas
  COMMA_SEP_LIST=${NEW_ADVISORY_LIST// /,}

cat <<END > $STORMDIR/new_advisory.txt 2>> ${SYSLOG}
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

The supercomputer $HOSTNAME has detected a new advisory
(number $ADVISORY) from the National Hurricane Center for storm number $STORM 
of ${YEAR}. This advisory has been downloaded; ADCIRC
storm surge calculations are about to begin. 

You will receive another email from the ASGS on $HOSTNAME
as soon as the resulting storm surge guidance becomes available.

YOU MAY ALSO RECEIVE emails notifying you of the detection of 
advisory $ADVISORY for storm $STORM from ASGS instances 
that are running on supercomputers OTHER THAN ${HOSTNAME}. 
The other instances are running for redundancy purposes.  

END
    logMessage "Sending 'new advisory detected' email to the following addresses: $COMMA_SEP_LIST."
     cat $STORMDIR/new_advisory.txt | mail -s "advisory detected by ASGS on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1

}
 
post_email()
{ ASGSADVISORYDIR=$1
STORM=$2
YEAR=$3
ADVISORY=$4
HOSTNAME=$5
ENSTORM=$6
POST_LIST=$7

cat <<END > $ASGSADVISORYDIR/post_notify.txt 
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

The supercomputer $HOSTNAME has produced ADCIRC results for 
storm surge guidance for advisory $ADVISORY for storm $STORM of $YEAR. 

These results consist of the following file sets:

$POSTADVISORYDIR/${ENSTORM}/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-plots.tar.gz
$POSTADVISORYDIR/$ENSTORM/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-KMZ_GIS.tgz

The hydrographs, wind speed plots, and data files (in csv format) are in the
first file set, while contour plots in JPG, KMZ (Google Earth), and GIS
formats are in the second file set. 

$RESULTSFTPADDRESS
${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-KMZ_GIS.tar.gz
${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-plots.tar.gz

END
# replace spaces in mailing list with commas
COMMA_SEP_LIST=${POST_LIST// /,}
#
logMessage "Sending 'results notification' email to the following addresses: $COMMA_SEP_LIST."
cat $ASGSADVISORYDIR/post_notify.txt | mail -s "ASGS results available for storm $STORM advisory $ADVISORY on $HOSTNAME" $POST_LIST
}



