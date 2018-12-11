#!/bin/bash
#set -x
set -e
set -u

# set a trap for a signal to reread the ASGS config file
trap 'echo Received SIGUSR1. Re-reading ASGS configuration file. ; . $CONFIG' USR1
# catch ^C for a final message
trap 'sigint' INT

SCRIPTDIR=/home/bblanton/GitHub/renci-unc/asgs       # ASGS scripts/executables  

# Bring in logging functions
echo Loading log functions
. ${SCRIPTDIR}/logging.sh

# RMQ Messaging

RMQMessaging="on"
RMQMessaging_Transmit="off"
RMQMessaging_Script="${SCRIPTDIR}/asgs-msgr.py"
#RMQMessaging_NcoHome="/home/bblanton/"
RMQMessaging_Python="/projects/storm_surge/anaconda/bin/python"
RMQMessaging_LocationName="RENCI"
RMQMessaging_ClusterName="Hatteras"

# RMQMessaging config
# this verifies that messages can be constructed.  It is possible
# that asgs-msgr.sh will set RMQMessaging to "off", in which case
# calls to RMQMessage will return without doing anything
#if [[ $RMQMessaging == "on" ]] ; then
   #. ${SCRIPTDIR}/asgs-msgr.sh
#fi

# set a RunParams string for messaging
RMQRunParams="ec_95d:EnsSize=1"
STORM=13
STORMNAME="Isabel"
ADVISORY=7
INSTANCENAME="testinstancename"
RMQMessage "INFO" "test" "xyz"  "RUNN" "fakemsg" "99.9"



echo Sleeping...
echo " "
sleep 99
