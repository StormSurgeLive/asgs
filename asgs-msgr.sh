#!/bin/bash
# variables and paths for RabbitMQ messaging independent of the asgs/config details.  
# Other messaging variables are set in the config/machine file


export PATH=$PATH:${ADCIRCDIR}:${RMQMessaging_NcoHome}/bin

echo "Validating Message Service..."

if [[ ! -e ${RMQMessaging_Python} ]] ; then 
	echo "Specified Python (${RMQMessaging_Python}) not found. Turning off RMQMessaging."
	RMQMessaging="off"
	return 1
fi
echo $RMQMessaging_Python

if [[ ! -e ${RMQMessaging_Script} ]] ; then 
	echo "Messaging script not found. Turning off RMQMessaging."
	RMQMessaging="off"
	return 1
fi
echo $RMQMessaging_Script

declare -a python_mod_list=("pika" "datetime" "getopt" "yaml" "json" "sys") 
for m in "${python_mod_list[@]}"
do 
	printf "   Checking for $m ..."
	${MSGR_PYTHON} -c "import $m" 2> /dev/null
	if [ "$?" -eq 1 ] ; then 
		echo "\nNeeded python module $m not found. Turning off RMQMessaging."
		RMQMessaging="off"
		return 1
	else
		printf "   Found it.\n"
	fi
done

