#!/bin/bash

#set -x
#set -e
#set -u

#export PATH=$PATH:${ADCIRCDIR}:${RMQMessaging_NcoHome}/bin

echo "RMQMessaging:Validating Message Service..."

RMQMessaging_Python=$(which python); # should be using python provided by ASGS environment

if [[ ! -e ${RMQMessaging_Python} ]] ; then 
	echo "Specified Python (${RMQMessaging_Python}) not found. Turning off RMQMessaging."
	RMQMessaging_Enable="off"
	return 1
fi
echo "RMQMessaging:Python=$RMQMessaging_Python"
echo "RMQMessaging:Script=$RMQMessaging_Script"
echo "RMQMessaging:Script_RP=$RMQMessaging_Script_RP"

if [[ ! -e ${RMQMessaging_Script} ]] ; then 
	echo "Messaging script not found. Turning off RMQMessaging."
	RMQMessaging_Enable="off"
	return 1
fi
if [[ ! -e ${RMQMessaging_Script_RP} ]] ; then 
	echo "Messaging_RP script not found. Turning off RMQMessaging."
	RMQMessaging_Enable="off"
	return 1
fi

#declare -a python_mod_list=("pika" "datetime" "getopt" "yaml" "json" "sys") 
declare -a python_mod_list=("pika" "datetime" "getopt" "json") 
for m in "${python_mod_list[@]}"
do 
	printf "   Checking for $m ..."
	${RMQMessaging_Python} -c "import $m" 2> /dev/null
	if [ "$?" -ne 0 ] ; then 
		echo "Needed python module $m not found. Turning off RMQMessaging."
		RMQMessaging_Enable="off"
		return 1
	else
		printf "   Found it.\n"
	fi
done

#if [ -d "$RMQMessaging_NcoHome" ] ; then

#FortCheckCommand="/home/ncfs-dev//2014stable/FortCheck_nco.sh"

echo " "
