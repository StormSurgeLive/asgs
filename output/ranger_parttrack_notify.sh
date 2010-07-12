#!/bin/bash
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
particle_tracking_email()
{ 
ADVISORYDIR=$1
STORM=$2
YEAR=$3
ADVISORY=$4
HOSTNAME=$5
ENSTORM=$6
OUTPUTPREFIX=$7
POST_LIST=$8

#
PTDIR=$ADVISORYDIR/TACKING
POSTDIR=/corral/hurricane/asgs_output
ANIMPOSTDIR=/corral/hurricane/asgs_output/movies

cat <<END > $ADVISORYDIR/post_notify.txt 
ASGS Oil Spill results available for $KIND $STORM advisory $ADVISORY 
and initial particle location ${PARTICLEFILE} on $HOSTNAME

The final output products (animations/movies) are located on Corral:

$ANIMPOSTDIR/$OUTPUTPREFIX.gif
$ANIMPOSTDIR/$OUTPUTPREFIX.avi
$ANIMPOSTDIR/$OUTPUTPREFIX.mp4

The particle position file can be found on Corral:

$ADVISDIR/$OUTPUTPREFIX.pth

Along with all of the individual still images and all of the processing files and 
programs found in :

$ADVISDIR/
$PTDIR/
$ADVISDIR/MONTAGE


END
#
cat $ADVISORYDIR/post_notify.txt | mail -s "ASGS Oil Spill results available for $KIND $STORM advisory $ADVISORY  Initial Particle location ${PARTICLEFILE} on $HOSTNAME" $POST_LIST
}

