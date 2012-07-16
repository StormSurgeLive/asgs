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
#
# grab all static configuration data
. ${CONFIG}
umask 002

cd $ADVISDIR/$ENSTORM
# create a fort.22.cera, formatted for NWS8 (easiest for plotting)
#mv fort.22 fort.22.temp
#${SCRIPTDIR}/storm_track_gen.pl --dir $ADVISDIR --storm $STORM --year $YEAR --coldstartdate $CSDATE --hotstartseconds $HSTIME --nws 8 --name $ENSTORM 
#mv fort.22 fort.cera.22
#mv fort.22.temp fort.22
#POSTADVISORYDIR=$POSTDIR/${STORM}${YEAR}/${ADVISORY}
#cp $ASGSADVISORYDIR/fort.22.cera $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/al${STORM}${YEAR}.fst $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/bal${STORM}${YEAR}.dat $POSTADVISORYDIR
#mkdir -p $POSTADVISORYDIR/$ENSTORM
cp $ADVISDIR/al${STORM}${YEAR}.fst .
cp $ADVISDIR/bal${STORM}${YEAR}.dat .

if [ -f fort.63 ]; then
  echo "INFO: cera_post.sh: gzipping fort.63"
  gzip fort.63
fi
if [ -f fort.74 ]; then
  echo "INFO: cera_post.sh: gzipping fort.74"
   gzip fort.74
fi
if [ -f fort.64 ]; then
  echo "INFO: cera_post.sh: gzipping fort.64"
   gzip fort.64
fi
if [ -f fort.73 ]; then
  echo "INFO: cera_post.sh: gzipping fort.73"
   gzip fort.73
fi


#metalink=`ls ${YEAR}${STORM}${ADVISORY}?_w???o???v????r???`
#cp $metalink $POSTADVISORYDIR   #$ENSTORM
#cp $ASGSADVISORYDIR/$ENSTORM/fort.22 $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/fort.22.meta $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/fort.61 $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/maxele.63 $POSTADVISORYDIR
#cp $ASGSADVISORYDIR/$ENSTORM/maxwvel.64 $POSTADVISORYDIR
