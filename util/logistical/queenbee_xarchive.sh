#!/bin/sh
#---------------------------------------------------------------------------
# queenbee_xarchive.sh: collects executable files so they can be run from 
# a separate account on this machine.  
#-------------------------------------------------------------------
# Copyright(C) 2015 Jason Fleming
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
# This script should be run from the home directory of the Operator that 
# wants to transfer the executables. 
#
#---------------------------------------------------------------------------
# Change the following parameters if needed. The receiving Operator will
# also have set up their corresponding directory structure to receive
# the files when they are unpacked. 
# 
ADCIRCDIR=./adcirc/v51release/work  # dir where ADCIRC executables are stored
ASGSDIR=./asgs/2014stable           # dir where ASGS script is installed
#
TARFILE=queenbee_xarchive.tar       # archive of executable files 
#---------------------------------------------------------------------------
#
# ADCIRC executables
for executable in adcirc padcirc padcswan adcprep hstime aswip ; do
   tar uvf $TARFILE $ADCIRCDIR/$executable
done
#
# ASGS executables
tar uvf $TARFILE $ASGSDIR/tides/tide_fac.x
tar uvf $TARFILE $ASGSDIR/awip_lambert_interp.x
tar uvf $TARFILE $ASGSDIR/wgrib2
#
# ASGS input files for CPRA2017 mesh
tar uvf $TARFILE $ASGSDIR/input/meshes/cpra2017/cpra_2017_v07a_chk.grd
tar uvf $TARFILE $ASGSDIR/input/meshes/cpra2017/cpra_2017_v07a.13
