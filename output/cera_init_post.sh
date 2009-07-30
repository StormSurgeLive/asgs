#!/bin/bash
#
# cera_init_post.sh 
# 
# Creates the directory where output will be copied from the ASGS
# Workflow for post processing.
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
ASGSADVISORYDIR=$1
STORM=$2
YEAR=$3
ADVISORY=$4
NOTIFY=$5
HOSTNAME=$6
#
POSTDIR=/work/kloffler
#
mkdir -p $POSTDIR/${STORM}${YEAR}/${ADVISORY}

cat <<END > $ASGSADVISORYDIR/post_init_notify.txt 
This is an automated message from the ADCIRC Surge Guidance System (ASGS)
running on ${HOSTNAME}.

This message is to let you know that the ASGS has started a new advisory
and has initialized the following directory for its output results:

$POSTDIR/${STORM}${YEAR}/${ADVISORY}

When the results for this advisory are ready, they will be copied to
that directory by the ASGS and another email will be sent as notification.
END
#
cat $ASGSADVISORYDIR/post_init_notify.txt | mail -s "ASGS init directory for storm $STORM advisory $ADVISORY on $HOSTNAME" $NOTIFY
