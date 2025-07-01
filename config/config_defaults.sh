#!/bin/bash
#----------------------------------------------------------------
#
# config_defaults.sh: This script provides the default
# configuration parameters for the ASGS.
#
#----------------------------------------------------------------
# Copyright(C) 2014--2024 Jason Fleming
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
#
THIS=$(basename -- $0)
# platform
QUEUESYS=null
QUEUENAME=null
SERQUEUE=null
QCHECKCMD=null
SUBMITSTRING=null
RESERVATION=null # for SLURM
CONSTRAINT=null # for SLURM
QOS=null

# resources
NCPU=null
NUMWRITERS=0
ACCOUNT=desktop
NCPUCAPACITY=2  # total number of CPUs available to run jobs

# general
JOBTYPE=null
STORMDIR=stormdir
SSHKEY=null
PPN=1
HOTSWAN=off
ENSTORM=hindcast
CYCLETIMELIMIT="05:00:00"
# Operators should set the value of this parameter
# to their email address in their ~/.asgsh_profile files
# on each platform
ASGSADMIN=null
ASGSADMIN_ID=null

# post
INITPOST=( null_post.sh )
#
#  H O O K S
#
# set the list of scripts for each hook to the empty string ;
# this will prevent the same script from being re-added to a hook
# whenever the ASGS config file containing the "addScriptTo_*" functions
# is re-read
for k in ${allHooks[@]} ; do
    hooksScripts[$k]=""
done