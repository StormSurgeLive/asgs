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
# load asgs operator email address
ASGSADMIN=`grep "notification.email.asgsadmin" ${STORMDIR}/run.properties | sed 's/notification.email.asgsadmin.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
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
the storm is forecast to pass within 270 nautical miles of Lake Pontchartrain
at some point in its track. The supercomputer $HOSTNAME has downloaded 
a hurricane forecast from the National Hurricane Center, and is performing
calculations with ADCIRC to predict the storm surge and wind speed in 
Lake Pontchartrain at the gated structures at the London Ave., Orleans Ave., 
and 17th St. canals. These predictions will extend five days into the future.

You will receive an email from the ASGS on $HOSTNAME as soon 
as the results of these predictions become available. You will also continue to
receive storm surge and wind speed predictions for each forecast that the NHC
issues for this storm, until the storm has passed to the north of the
northern Gulf Coast.

YOU MAY ALSO RECEIVE activation emails from ASGS instances that are running on
supercomputers OTHER THAN ${HOSTNAME}. Those instances 
are completely independent of this one, and will produce results that are
identical to the results produced on ${HOSTNAME}. The other instances 
are running for redundancy purposes.

END
    cat $STORMDIR/activate.txt | mail  -S "replyto=$ASGSADMIN" -s "ASGS Activated on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG}
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

The supercomputer $HOSTNAME has detected a new advisory from
the National Hurricane Center for storm number $STORM 
dated $ADVISORY (GMT). This advisory has been downloaded; ADCIRC
storm surge calculations are about to begin. 

You will receive another email from the ASGS on $HOSTNAME
as soon as the results of these predictions become available.

YOU MAY ALSO RECEIVE emails notifying you of the detection of 
the advisory $ADVISORY for storm $STORM from ASGS instances 
that are running on supercomputers OTHER THAN ${HOSTNAME}. 
The other instances are running for redundancy purposes.  

END
     cat $STORMDIR/new_advisory.txt | mail  -S "replyto=$ASGSADMIN" -s "advisory detected by ASGS on $HOSTNAME" "$COMMA_SEP_LIST" 2>> ${SYSLOG}

}
 
