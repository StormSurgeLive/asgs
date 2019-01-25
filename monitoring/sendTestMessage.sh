#!/bin/bash
#set -x
set -e
set -u


SCRIPTDIR="/home/bblanton/asgs"
RMQMessaging_Transmit="on"
RMQMessaging_Script="${SCRIPTDIR}/sendTestMessage.py"
RMQMessaging_Python="${SCRIPTDIR}/asgspy/bin/python"
RMQMessaging_LocationName="Penguin"
RMQMessaging_ClusterName=`hostname`
INSTANCENAME="testinstancename"
#Message="test from ${RMQMessaging_LocationName} on ${RMQMessaging_ClusterName}"
Message=`cat  current.sh | sed '/^#/d'`

${RMQMessaging_Python} ${RMQMessaging_Script} \
        --Transmit ${RMQMessaging_Transmit} \
        --InstanceName ${INSTANCENAME} \
        --ClusterName ${RMQMessaging_ClusterName} \
        --LocationName ${RMQMessaging_LocationName} \
        --Message "${Message}"

