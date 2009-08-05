#!/bin/bash
#
# cera_post.sh 
# 
# Copies output from the ASGS for post processing.
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
HOSTNAME=$5
ENSTORM=$6
#
POSTDIR=/work/cera
umask 002
#
POSTADVISORYDIR=$POSTDIR/${STORM}${YEAR}/${ADVISORY}
cp $ASGSADVISORYDIR/al${STORM}${YEAR}.fst $POSTADVISORYDIR
cp $ASGSADVISORYDIR/bal${STORM}${YEAR}.dat $POSTADVISORYDIR
mkdir -p $POSTADVISORYDIR/$ENSTORM
cp $ASGSADVISORYDIR/al${STORM}${YEAR}.fst $POSTADVISORYDIR/$ENSTORM
cp $ASGSADVISORYDIR/bal${STORM}${YEAR}.dat $POSTADVISORYDIR/$ENSTORM
metalink=`ls ${YEAR}${STORM}${ADVISORY}?_w???o???v????r???`
cp $metalink $POSTADVISORYDIR/$ENSTORM
cp $ASGSADVISORYDIR/$ENSTORM/fort.22 $POSTADVISORYDIR/$ENSTORM
cp $ASGSADVISORYDIR/$ENSTORM/fort.22.meta $POSTADVISORYDIR/$ENSTORM
cp $ASGSADVISORYDIR/$ENSTORM/fort.61 $POSTADVISORYDIR/$ENSTORM
cp $ASGSADVISORYDIR/$ENSTORM/maxele.63 $POSTADVISORYDIR/$ENSTORM
cp $ASGSADVISORYDIR/$ENSTORM/maxwvel.63 $POSTADVISORYDIR/$ENSTORM
