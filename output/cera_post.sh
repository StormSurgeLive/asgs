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
CONFIG=$1
ASGSADVISORYDIR=$2
STORM=$3
YEAR=$4
ADVISORY=$5
HOSTNAME=$6
ENSTORM=$7
CSDATE=$8
HSTIME=$9
#
# grab all static configuration data
. ${CONFIG}
POSTDIR=/work/cera
umask 002
# create a fort.22.cera, formatted for NWS8 (easiest for plotting)
mv fort.22 fort.22.temp
${SCRIPTDIR}/storm_track_gen.pl --dir $ASGSADVISORYDIR --storm $STORM --year $YEAR --coldstartdate $CSDATE --hotstartseconds $HSTIME --nws 8 --name $ENSTORM 
mv fort.22 fort.cera.22
mv fort.22.temp fort.22
POSTADVISORYDIR=$POSTDIR/${STORM}${YEAR}/${ADVISORY}
#cp $ASGSADVISORYDIR/fort.22.cera $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/al${STORM}${YEAR}.fst $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/bal${STORM}${YEAR}.dat $POSTADVISORYDIR
#mkdir -p $POSTADVISORYDIR/$ENSTORM
cp $POSTADVISORYDIR/al${STORM}${YEAR}.fst $POSTADVISORYDIR/$ENSTORM
cp $POSTADVISORYDIR/bal${STORM}${YEAR}.dat $POSTADVISORYDIR/$ENSTORM

#metalink=`ls ${YEAR}${STORM}${ADVISORY}?_w???o???v????r???`
#cp $metalink $POSTADVISORYDIR   #$ENSTORM
#cp $ASGSADVISORYDIR/$ENSTORM/fort.22 $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/fort.22.meta $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/fort.61 $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/maxele.63 $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/maxwvel.64 $POSTADVISORYDIR
