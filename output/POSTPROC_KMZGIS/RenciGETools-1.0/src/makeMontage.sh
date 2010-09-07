#!/bin/bash
#
# Copyright (c) 2008  Renaissance Computing Institute. All rights reserved.
#
# This software is open source. See the bottom of this file for the license.
#
# Renaissance Computing Institute,
# (A Joint Institute between the University of North Carolina at Chapel Hill,
# North Carolina State University, and Duke University)
# http://www.renci.org
#----------- EXEC Config File -------------------------------------
#----------- Sets SYS DIRS, GMT variables, etc ... ----------------


if [ $# -lt 5 ] ; then
    echo Must have 4 Command-Line args
    echo Usage: $0 \<prefix\> \<level\> \<target\> \<box\>
    echo where \<prefix\> is the same as argument 3 to adc_max_simple_plot_gmt.sh
    echo and level is the zoom level for which you want the montage.
    echo For example, if I build the tiles with 
    echo adc_max_simple_plot_gmt.sh ~bblanton/maxele.63 nciv6a Hanna5 3
    echo then makeMontage.sh Hanna5 3 
    echo produces a montage of the 3rd level of Hanna5 tiles.
    echo target defines the machine on which the montage is created.
    echo box defines the lat/lon bounding box for the figure.
#	echo Usage: $0 \<prefix> \<thisLevel> \<nLevels>
	exit 1
else 
   prefix=$1
   level=$2
   TARGET=$3
   BOX=$4
   source $PPDIR/config_simple_gmt_pp.sh $TARGET $BOX
   MONTAGE=$ImageMagick/montage
fi
#nLevels=$3
arg=""
nTiles=$(echo "2^($level - 1)" |bc)
for ((x=nTiles; x>=1; x--))
do
    arg="$arg $prefix.$level.?.$x.png"
done
arg="$arg -tile ${nTiles}x${nTiles} $prefix-montage.$level.png"
$MONTAGE $arg
# ***************************************************************************
#
# RENCI Open Source Software License
# The University of North Carolina at Chapel Hill
#
# The University of North Carolina at Chapel Hill (the "Licensor") through
# its Renaissance Computing Institute (RENCI) is making an original work of
# authorship (the "Software") available through RENCI upon the terms set
# forth in this Open Source Software License (this "License").  This License
# applies to any Software that has placed the following notice immediately
# following the copyright notice for the Software:  Licensed under the RENCI
# Open Source Software License v. 1.0.
#
# Licensor grants You, free of charge, a world-wide, royalty-free,
# non-exclusive, perpetual, sublicenseable license to do the following to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# . Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimers.
#
# . Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimers in the
# documentation and/or other materials provided with the distribution.
#
# . Neither You nor any sublicensor of the Software may use the names of
# Licensor (or any derivative thereof), of RENCI, or of contributors to the
# Software without explicit prior written permission.  Nothing in this
# License shall be deemed to grant any rights to trademarks, copyrights,
# patents, trade secrets or any other intellectual property of Licensor
# except as expressly stated herein.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE CONTIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
# You may use the Software in all ways not otherwise restricted or
# conditioned by this License or by law, and Licensor promises not to
# interfere with or be responsible for such uses by You.  This Software may
# be subject to U.S. law dealing with export controls.  If you are in the
# U.S., please do not mirror this Software unless you fully understand the
# U.S. export regulations.  Licensees in other countries may face similar
# restrictions.  In all cases, it is licensee's responsibility to comply
# with any export regulations applicable in licensee's jurisdiction.
#
# ***************************************************************************#
