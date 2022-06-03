#!/bin/bash
#set -x
set -e
set -u


SCRIPTDIR=${SCRIPTDIR-/home/bblanton/asgs} # should be already defined and running in asgsh shell environment
RMQMessaging_Transmit="on"
RMQMessaging_Script="${SCRIPTDIR}/monitoring/sendTestMessage.py"
RMQMessaging_LocationName="PSC"
RMQMessaging_ClusterName="Bridges2"
INSTANCENAME="testinstancename"
Message="test from ${RMQMessaging_LocationName} on ${RMQMessaging_ClusterName}"
#Message=`cat  current.sh | sed '/^#/d'`

# script should be executable
${RMQMessaging_Script} \
        --Transmit ${RMQMessaging_Transmit} \
        --InstanceName ${INSTANCENAME} \
        --ClusterName ${RMQMessaging_ClusterName} \
        --LocationName ${RMQMessaging_LocationName} \
        --Message "${Message}" 

if [ "$?" -ne 0 ] ; then
    echo "send failed."
fi

