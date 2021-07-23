#!/bin/bash
#------------------------------------------------------------------------
# queenbee_archive.sh : Archiving results on queenbee. 
#------------------------------------------------------------------------
# Copyright(C) 2017 Jason Fleming
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
#------------------------------------------------------------------------
# sample invokation:
# bash ~/asgs/2014stable/archive/queenbee_archive.sh ~/asgs/2014stable/config/2017/asgs_config_nam_swan_queenbee_hsofs.sh /work/jgflemin/asgs15547/2017101512 99 2017 2017101512 queenbee.loni.org nowcast 2017091500 2613600.0 fort.14 ~/asgs/2014stable/output syslog.log ~/.ssh/id_rsa.pub
#
CONFIG=$1
ADVISDIR=$2
STORM=$3
YEAR=$4
ADVISORY=$5
HOSTNAME=$6
ENSTORM=$7
CSDATE=$8
HSTIME=$9
GRIDFILE=${10}
OUTPUTDIR=${11}
SYSLOG=${12}
SSHKEY=${13}
#
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR}
THIS=$(basename -- $0)
#
# grab all config info
. ${CONFIG} 
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
# the pedir_removal.sh script recomposes a fulldomain swan hotstart file
# from the subdomain swan hotstart files
${SCRIPTDIR}/archive/enstorm_pedir_removal.sh -e queenbee -h ${ADCIRCDIR}/../swan 2>> $SYSLOG
