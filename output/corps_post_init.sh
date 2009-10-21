#!/bin/bash
#
# post_init.sh
#   script to initialize any post processing and/or results display
#
# Copyright(C) 2008, 2009 Jason Fleming
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
#
 ADVISDIR=$1 
 STORM=$2
 YEAR=$3
 ADVISORY=$4
 HOSTNAME=$5
 SSHKEY=$6
 CONFIG=$7
 SYSLOG=$8
#
  echo "Reading configuration data from ${CONFIG}"
#
 . ${CONFIG}
#
 ssh $WEBHOST -l $WEBUSER -i $SSHKEY "mkdir -p $WEBPATH/$HOSTNAME/$STORM$YEAR" >> $SYSLOG 2>&1
 scp -i $SSHKEY $OUTPUTDIR/placeholder.html ${WEBUSER}@${WEBHOST}:${WEBPATH}/$HOSTNAME/$STORM$YEAR/index.html  
 ssh $WEBHOST -l $WEBUSER -i $SSHKEY "chmod -R 755 ${WEBPATH}/$HOSTNAME/$STORM$YEAR/index.html" >> ${SYSLOG} 2>&1
#    $SCRIPTDIR/scp_files.exp $SCRIPTDIR $ADVISDIR $RESULTSHOST $RESULTSPATH $RESULTSPROMPT $RESULTSUSERNAME $RESULTSPASSWORD $HOSTNAME $STORM $YEAR 2>> ${SYSLOG}


