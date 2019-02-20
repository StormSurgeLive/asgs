#!/bin/bash
#----------------------------------------------------------------
# asgs-msgr.sh: Check to make sure all components needed for 
# RMQMessaging are actually available.
#----------------------------------------------------------------
# Copyright(C) 2018 Brian Blanton
# Copyright(C) 2019 Jason Fleming
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
#----------------------------------------------------------------
#set -x
#set -e
#set -u
#export PATH=$PATH:${ADCIRCDIR}:${RMQMessaging_NcoHome}/bin
#
THIS="monitoring/asgs-msgr.sh"
logMessage "$THIS: Validating Message Service..."
#
if [[ ! -e ${RMQMessaging_Python} ]] ; then 
	warn "$THIS: Specified Python RMQMessaging_Python=${RMQMessaging_Python} in ASGS configuration file not found. Turning off RMQMessaging."
	RMQMessaging_Enable="off"
	return 1
fi
logMessage "$THIS: RMQMessaging_Python=$RMQMessaging_Python"
#
if [[ ! -e ${RMQMessaging_Script} ]] ; then 
	warn "$THIS: RMQMessaging_Script $RMQMessaging_Script not found. Turning off RMQMessaging."
	RMQMessaging_Enable="off"
	return 1
fi
logMessage "$THIS: RMQMessaging_Script=$RMQMessaging_Script"
#
#declare -a python_mod_list=("pika" "datetime" "getopt" "yaml" "json" "sys") 
logMessage "$THIS: Checking for python module dependencies."
declare -a python_mod_list=("pika" "datetime" "getopt" "json") 
for m in "${python_mod_list[@]}"
do 
	logMessage "$THIS: Checking for python module dependency $m ..."
	${RMQMessaging_Python} -c "import $m" 2> /dev/null
	if [ "$?" -ne 0 ] ; then 
		warn "$THIS: Needed python module $m not found. Turning off RMQMessaging."
		RMQMessaging_Enable="off"
		return 1
	else
		logMessage "... found ${m}."
	fi
done
logMessage "$THIS: All python module dependencies are satisfied."
