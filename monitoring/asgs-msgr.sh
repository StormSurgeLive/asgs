#!/bin/bash

THIS=$(basename -- $0)

echo "RMQMessaging:Validating Message Service..."

RMQMessaging_Python=python3 ; # should be using python provided by ASGS environment

if [[ ! -e $(command -v ${RMQMessaging_Python}) ]] ; then 
	echo "Specified Python (${RMQMessaging_Python}) not found. Turning off RMQMessaging."
	RMQMessaging_Enable="off"
	return 1
fi
RMQMessaging_Script=${RMQMessaging_Script:-$SCRIPTDIR/monitoring/asgs-msgr.py}
RMQMessaging_Script_RP=${RMQMessaging_Script_RP:-$SCRIPTDIR/monitoring/rp2json.py}
RMQMessaging_StartupScript=${RMQMessaging_StartupScript:-${SCRIPTDIR}/monitoring/asgs-msgr_startup.py}
namedot=${HPCENVSHORT}.
RMQMessaging_LocationName=${RMQMessaging_LocationName:-${HPCENV#$namedot}}
RMQMessaging_ClusterName=${RMQMessaging_ClusterName:-$HPCENVSHORT}
unset namedot

echo "RMQMessaging:Python=$RMQMessaging_Python"
echo "RMQMessaging:Script=$RMQMessaging_Script"
echo "RMQMessaging:Script_RP=$RMQMessaging_Script_RP"
echo "RMQMessaging:StartipScript=$RMQMessaging_StartupScript"
echo "RMQMessaging:LocationName=$RMQMessaging_LocationName"
echo "RMQMessaging:ClusterName=$RMQMessaging_ClusterName"

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
