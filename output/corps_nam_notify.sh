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

This message is to let you know that the ASGS has been ACTIVATED and is using
the North American Mesoscale model for meteorological forcing.

You will continue to receive email from the ASGS on $HOSTNAME
as the results of this guidance become available.

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

The supercomputer $HOSTNAME has detected a new NAM cycle
(number $ADVISORY) from the National Centers for Environmental
Prediction (NCEP). The forcing data have been downloaded; ADCIRC surge
calculations are about to begin. 

You will receive another email from the ASGS on $HOSTNAME
as soon as the resulting storm surge guidance becomes available.

END
    logMessage "Sending 'new cycle detected' email to the following addresses: $COMMA_SEP_LIST."
     cat $STORMDIR/new_advisory.txt | mail -s "new cycle detected by ASGS on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1

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
surge guidance for NAM cycle $ADVISORY. 

These results can be found at the following web site:

http://www.seahorsecoastal.com/ASGS/NAM/$HOSTNAME/$ADVISORY

The results consist of two archive files; the first contains hydrographs and 
wind speed plots at selected locations as well as the raw data that were
used to produce the plots, and the second contains contour plots of maximum 
surge elevation in GIS, Google Earth, and JPG format.

The ASGS on $HOSTNAME is now waiting for the National Centers for 
Environmental Prediction (NCEP) to issue the next cycle. 

END
# replace spaces in mailing list with commas
COMMA_SEP_LIST=${POST_LIST// /,}
#
logMessage "Sending 'results notification' email to the following addresses: $COMMA_SEP_LIST."
cat $ASGSADVISORYDIR/post_notify.txt | mail -s "ASGS results available for cycle $ADVISORY on $HOSTNAME" $POST_LIST
}



