#!/bin/bash
#------------------------------------------------------------------------
# rotateRUNDIR.sh : move existing ASGS run directory out of the way
#------------------------------------------------------------------------
# Copyright(C) 2018 Jason Fleming
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
STATEFILE=$1 # statefile of the ASGS instance to be rotated out of the way
#
. $STATEFILE # pick up the value of RUNDIR and SYSLOG
ROTATEDATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
# copy asgs log file into the run directory
cp $SYSLOG $RUNDIR 
# parse the name of the asgs instance out of the statefile name
INSTANCENAME=`basename $STATEFILE .state`
# symbolically link the run directory to a useful name
ln -s $RUNDIR ${INSTANCENAME}.${ROTATEDATETIME}
# move the statefile into the run directory for later reference and to 
# make sure it is out of the way if the instance is restarted
mv $STATEFILE $RUNDIR
