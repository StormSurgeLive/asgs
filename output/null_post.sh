#!/bin/bash
#
# null_post.sh 
# 
# Copies output from the ASGS to /dev/null.
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
CONFIG=$1
ASGSADVISORYDIR=$2
STORM=$3
YEAR=$4
ADVISORY=$5
HOSTNAME=$6
ENSTORM=$7
COLDSTARTDATE=$8
HSTIME=$9
#
POSTDIR=/dev/null
#
#POSTADVISORYDIR=$POSTDIR/${STORM}${YEAR}/${ADVISORY}
cp $ASGSADVISORYDIR/al${STORM}${YEAR}.fst $POSTDIR
cp $ASGSADVISORYDIR/bal${STORM}${YEAR}.dat $POSTDIR
metalink=`ls ${YEAR}${STORM}${ADVISORY}?_w???o???v????r???`
cp $metalink $POSTDIR
cp $ASGSADVISORYDIR/$ENSTORM/fort.22 $POSTDIR
cp $ASGSADVISORYDIR/$ENSTORM/fort.22.meta $POSTDIR
cp $ASGSADVISORYDIR/$ENSTORM/fort.61 $POSTDIR
cp $ASGSADVISORYDIR/$ENSTORM/maxele.63 $POSTDIR
cp $ASGSADVISORYDIR/$ENSTORM/maxwvel.63 $POSTDIR
