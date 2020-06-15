#!/bin/sh
#-------------------------------------------------------------------
#-------------------------------------------------------------------
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# ASGS is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------
#
#-------------------------------------------------------------------
# General configuration 
#-------------------------------------------------------------------
STORM=RITA  # storm number, e.g. 05=ernesto in 2006 
STORMNAME=RITA
YEAR= # year of the storm (useful for historical storms) 
CSDATE=20050831000000
OUTPUTDIR=~/asgs/trunk/output # dir containing post processing scripts
INPUTDATADIR=/scratch/01053/rweaver/RITA_FILES/
TRACKDIR=/work/01053/rweaver/PartTrack/TRACKING_FILES
ADVISORY=RITA
ADVISDIR=
ENSTORM=
KIND=Hurricane      # Hurricane or NAM
PARTICLEFILE=/corral/hurricane/mthoward/p20100623_composite_1000m.txt
PARTFILETYPE=0    # 1 particle numbers 4 columns , 0 no part numbers 3 columns
NORTH=31.0
SOUTH=26.0
EAST=-87.5
WEST=-94.2
SYSLOG=$WORK/PartTrack/$STORMNAME/parttrack_log_${STORMNAME}.log
NOTIFYUSER=rjweaver@email.unc.edu
ACCOUNT=TG-DMS100024
GSHOME2=/share/home/01053/rweaver/ghostscript-8.71/bin/
GMTHOME2=/work/01053/rweaver/GMT4.5.2/bin/
VECTORLIM=50
CONTOURLIM=-1,4


