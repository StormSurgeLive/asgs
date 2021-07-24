#!/bin/bash

THIS=$(basename -- $0)

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

declare -a python_mod_list=("pika" "datetime" "getopt" "json") 
for m in "${python_mod_list[@]}"
do 
	${RMQMessaging_Python} -c "import $m" 2> /dev/null
	if [ "$?" -ne 0 ] ; then 
		echo "Needed python module $m not found. Turning off RMQMessaging."
		RMQMessaging_Enable="off"
		return 1
	fi
done

echo
