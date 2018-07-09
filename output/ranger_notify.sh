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
cat <<END > $STORMDIR/activate.txt 2>> ${SYSLOG}
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

This message is to let you know that the ASGS has been ACTIVATED for storm
number $STORM of ${YEAR}. 

The ASGS was activated on $HOSTNAME because 
the storm is forecast to impact the Gulf Coast.

The supercomputer $HOSTNAME has downloaded 
a hurricane forecast from the National Hurricane Center, and is performing
calculations with ADCIRC to predict the storm surge and wind speed along
the complete Texas Coast as well as Galveston Bay.

You will receive an email from the ASGS on $HOSTNAME as soon 
as the results of these predictions become available. You will also continue to
receive storm surge and wind speed predictions for each forecast that the NHC
issues for this storm.

YOU MAY ALSO RECEIVE activation emails from ASGS instances that are running on
supercomputers OTHER THAN ${HOSTNAME}. Those instances 
are completely independent of this one, and will produce results that are
identical to the results produced on ${HOSTNAME}. The other instances 
are running for redundancy purposes.

END
    cat $STORMDIR/activate.txt | mail -s "ASGS Activated on $HOSTNAME" $ACTIVATE_LIST 2>> ${SYSLOG}
}

new_advisory_email()
{ HOSTNAME=$1
  STORM=$2
  YEAR=$3
  ADVISORY=$4
  NEW_ADVISORY_LIST=$5

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
     cat $STORMDIR/new_advisory.txt | mail -s "advisory detected by ASGS on $HOSTNAME" $NEW_ADVISORY_LIST 2>> ${SYSLOG}

}

post_init_email()
{ ASGSADVISORYDIR=$1
STORM=$2
YEAR=$3
ADVISORY=$4
HOSTNAME=$5
POST_INIT_LIST=$6
#
POSTDIR=/scratch/01053/rweaver
#
cat <<END > $ASGSADVISORYDIR/post_init_notify.txt 
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

This message is to let you know that the ASGS has started a new advisory
and has initialized the following directory for its output results:

$POSTDIR/${STORM}${YEAR}/${ADVISORY}

When the results for this advisory are ready, they will be copied to
that directory by the ASGS and another email will be sent as notification.
END
#
cat $ASGSADVISORYDIR/post_init_notify.txt | mail -s "ASGS init directory for storm $STORM advisory $ADVISORY on $HOSTNAME" $POST_INIT_LIST
}

post_email() 
{ ASGSADVISORYDIR=$1
STORM=$2
YEAR=$3
ADVISORY=$4
HOSTNAME=$5
ENSTORM=$6
POST_LIST=$7
RESULTSFTPADDRESS=$8

#
POSTDIR=/corral/hurricane/asgs_output/
#POSTDIR=/scratch/01053/rweaver
#
POSTADVISORYDIR=$POSTDIR/${STORM}${YEAR}/${ADVISORY}
metalink=`ls ${YEAR}${STORM}${ADVISORY}?_w???o???v????r???`
cat <<END > $ASGSADVISORYDIR/post_notify.txt 
$POSTADVISORYDIR/al${STORM}${YEAR}.fst 
$POSTADVISORYDIR/bal${STORM}${YEAR}.dat 
$POSTADVISORYDIR/$ENSTORM/fort.22
$POSTADVISORYDIR/$ENSTORM/fort.22.meta
$POSTADVISORYDIR/$ENSTORM/fort.61
$POSTADVISORYDIR/$ENSTORM/maxele.63
$POSTADVISORYDIR/$ENSTORM/maxwvel.63
$POSTADVISORYDIR/${ENSTORM}/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-plots.tar.gz
$POSTADVISORYDIR/$ENSTORM/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-KMZ_GIS.tgz

The JPG, KMZ, GIS and hydrographs can also be found on the following ftp site:
$RESULTSFTPADDRESS
${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-KMZ_GIS.tar.gz
${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-plots.tar.gz

END
#
cat $ASGSADVISORYDIR/post_notify.txt | mail -s "ASGS results available for storm $STORM advisory $ADVISORY on $HOSTNAME" $POST_LIST
}

post2_email()
{ ASGSADVISORYDIR=$1
STORM=$2
YEAR=$3
ADVISORY=$4
HOSTNAME=$5
ENSTORM=$6
POST_LIST=$7
RESULTSFTPADDRESS=$8
#
POSTDIR=/corral/hurricane/asgs_output/
#POSTDIR=/scratch/01053/rweaver
#
POSTADVISORYDIR=$POSTDIR/${STORM}${YEAR}/${ADVISORY}
metalink=`ls ${YEAR}${STORM}${ADVISORY}?_w???o???v????r???`
cat <<END > $ASGSADVISORYDIR/post_notify.txt
$POSTADVISORYDIR/al${STORM}${YEAR}.fst
$POSTADVISORYDIR/bal${STORM}${YEAR}.dat
$POSTADVISORYDIR/$ENSTORM/fort.22
$POSTADVISORYDIR/$ENSTORM/fort.22.meta
$POSTADVISORYDIR/$ENSTORM/fort.61
$POSTADVISORYDIR/$ENSTORM/maxele.63
$POSTADVISORYDIR/$ENSTORM/maxwvel.63
$POSTADVISORYDIR/${ENSTORM}/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-plots.tar.gz
$POSTADVISORYDIR/$ENSTORM/${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}-KMZ_GIS.tgz

END
#
#
cat $ASGSADVISORYDIR/post_notify.txt | mail -s "ASGS GIS, KMZ and JPG results available for storm $STORM advisory $ADVISORY on $HOSTNAME" $POST_LIST 
#
}


