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
#
# simply return if there are no email addresses to send email to
if [[ $ADDRESS_LIST = null ]]; then
   exit
fi
#
STORMCLASSNAME=`cat nhcClassName`
# find the space between the storm class (TD, TS, HU, etc) and the NHC name
ind=`expr index "$STORMCLASSNAME" ' '`
# just use the storm's name 
STORMNAME=${STORMCLASSNAME:$ind}
COMMA_SEP_LIST=${ADDRESS_LIST// /,}
# load asgs operator email address
ASGSADMIN=`grep "notification.email.asgsadmin" ${STORMDIR}/run.properties | sed 's/notification.email.asgsadmin.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
case $PHASE in
################################################################################
#               A C T I V A T I O N
################################################################################
"activation") 
#
cat <<END > ${STORMDIR}/activate.txt 2>> ${SYSLOG}
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

This message is to let you know that the ASGS has been ACTIVATED for storm
number $STORM of ${YEAR}. 

The ASGS was activated on $HOSTNAME because 
the storm is forecast to pass within 270 nautical miles of the Louisiana
Gulf Coast at some point in its track. 

The supercomputer $HOSTNAME has downloaded 
a hurricane forecast from the National Hurricane Center, and is performing
calculations with ADCIRC to predict the storm surge and wind speed along
the complete Southern Louisiana Coast as well as greater New Orleans.

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
    echo "INFO: cera_notify.sh: Sending activation email to the following addresses; $COMMA_SEP_LIST."
    cat ${STORMDIR}/activate.txt | asgs-sendmail --subject "ASGS Activated on $HOSTNAME" --to "$COMMA_SEP_LIST" 2>> ${SYSLOG}
;;
################################################################################
#               N E W  C Y C L E
################################################################################
"newcycle")
#
cat <<END > ${STORMDIR}/new_advisory.txt 2>> ${SYSLOG}
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
     echo "INFO: cera_notify.sh: Sending activation email to the following addresses; $COMMA_SEP_LIST."
     cat ${STORMDIR}/new_advisory.txt | asgs-sendmail --subject "advisory detected by ASGS on $HOSTNAME" --to "$NEW_ADVISORY_LIST" 2>> ${SYSLOG}
;;
################################################################################
#              RESULTS 
################################################################################
"results")
#

runStartTime=`grep RunStartTime run.properties | sed 's/RunStartTime.*://' | sed 's/\s//g'` 

cat <<END > ${STORMDIR}/post_notify.txt 

The ADCIRC solutions for $runStartTime have been posted to
${HOSTNAME}:${STORMDIR}
The run.properties file is:
${HOSTNAME}:${STORMDIR}/run.properties
END

#Carola, do you need the following information anymore?

#This is an automated message from the ADCIRC Surge Guidance System (ASGS)
#running on ${HOSTNAME}.

#The supercomputer $HOSTNAME has produced ADCIRC results for 
#storm surge guidance for advisory $ADVISORY for $STORMCLASSNAME $YEAR
#on the $GRIDFILE mesh. 

#The ASGS on $HOSTNAME is now waiting for the National Hurricane Center to
#issue the next advisory. 

#ADCIRC output is located at:

#END
#if [ -f ${STORMDIR}/fort.63.gz ]; then
#echo ${STORMDIR}/fort.63.gz >> post_notify.txt
#fi 
#if [ -f ${STORMDIR}/fort.61.nc ]; then
#echo ${STORMDIR}/fort.61.nc >> post_notify.txt
#fi 

#nld  get info for the subject line from the run.properties file
# assume it was already written there by the POSTSCRIPT (e.g. cera_post.sh)
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
CERASERVER=`grep "ceraServer" ${STORMDIR}/run.properties | sed 's/ceraServer.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
INSTANCENAME=`grep "instance" ${STORMDIR}/run.properties | sed 's/instance.*://' | sed 's/^\s//'` 2>> ${SYSLOG}


#subject="ADCIRC POSTED for $runStartTime"
subject="ADCIRC NG $runStartTime $HOSTNAME.$INSTANCENAME $ENMEMNUM"
if [[ $TROPICALCYCLONE = on ]]; then
   subject=${subject}" (TC)"
fi
#subject="${subject} $CERASERVER"
#subject="${subject} $HOSTNAME.$INSTANCENAME $ENMEMNUM"
#
echo "INFO: cera_notify.sh: Sending 'results notification' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/post_notify.txt | asgs-sendmail --subject "$subject" --to "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
################################################################################
#               J O B   F A I L E D  
################################################################################
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
echo "INFO: cera_notify.sh: Sending 'job failed' email to the following addresses: $COMMA_SEP_LIST."
cat ${STORMDIR}/jobfailed_notify.txt | asgs-sendmail --subject "ASGS job failure for $STORMNAME advisory $ADVISORY on $HOSTNAME" --to "$COMMA_SEP_LIST" 2>> ${SYSLOG} 2>&1
;;
#
*)
echo "ERROR: cera__notify.sh: The notification type was specified as '$PHASE', which is not recognized. Email was not sent."
;;
esac

#post_email() 
#{ ASGSADVISORYDIR=$1
#STORM=$2
#YEAR=$3
#ADVISORY=$4
#HOSTNAME=$5
#ENSTORM=$6
#POST_LIST=$7
#
#POSTDIR=/work/cera
#
#POSTADVISORYDIR=$POSTDIR/${STORM}${YEAR}/${ADVISORY}
#metalink=`ls ${YEAR}${STORM}${ADVISORY}?_w???o???v????r???`
#cat <<END > $ASGSADVISORYDIR/post_notify.txt 
#$POSTADVISORYDIR/$ENSTORM/al${STORM}${YEAR}.fst 
#$POSTADVISORYDIR/$ENSTORM/bal${STORM}${YEAR}.dat 
#$POSTADVISORYDIR/$ENSTORM/$metalink
#$POSTADVISORYDIR/$ENSTORM/fort.22
#$POSTADVISORYDIR/$ENSTORM/fort.22.meta
#$POSTADVISORYDIR/$ENSTORM/fort.61
#$POSTADVISORYDIR/$ENSTORM/maxele.63
#$POSTADVISORYDIR/$ENSTORM/maxwvel.63
#$POSTADVISORYDIR/$ENSTORM/fort.cera.22
#END
#
#cat $ASGSADVISORYDIR/post_notify.txt | asgs-sendmail --subject "ASGS results available for storm $STORM advisory $ADVISORY on $HOSTNAME" --to "$POST_LIST"
#}
