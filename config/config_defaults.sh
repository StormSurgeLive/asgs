#!/bin/bash
#----------------------------------------------------------------
#
# config_defaults.sh: This script provides the default
# configuration parameters for the ASGS.
#
#----------------------------------------------------------------
# Copyright(C) 2014--2022 Jason Fleming
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
#----------------------------------------------------------------
#
# This gets re-read at the start of each cycle and scenario; it
# should not contain anything that changes with the ASGS state
# (e.g., the scenario package counter variable $si) or
# anything associated exclusively with the initialization of
# asgs (e.g., HPCENV).
THIS=$(basename -- $0)

   BACKGROUNDMET=on
   TIDEFAC=off
   TROPICALCYCLONE=off
   WAVES=off
   VARFLUX=off
   MINMAX=continuous
   USERIVERFILEONLY=no
   STORMNAME=stormname
   ELEVSTATIONS=null
   VELSTATIONS=null
   METSTATIONS=null
   GRIDFILE=fort.14
   GRIDNAME=fort14
   OUTPUTOPTIONS=
   HOTSTARTCOMP=fulldomain
   MAILINGLIST=null
   QUEUESYS=null
   QUEUENAME=null
   SERQUEUE=null
   QCHECKCMD=null
   NCPU=null
   JOBTYPE=null
   NUMWRITERS=0
   ACCOUNT=desktop
   SUBMITSTRING=null
   NOTIFYUSER=null
   HOTSTARTFORMAT=null
   STORMDIR=stormdir
   SSHKEY=null
   PPN=1
   VELOCITYMULTIPLIER=1.0
   HOTSWAN=off
   NCPUCAPACITY=2  # total number of CPUs available to run jobs
   ENSTORM=hindcast
   CYCLETIMELIMIT="05:00:00"
   RESERVATION=null # for SLURM
   CONSTRAINT=null # for SLURM
   QOS=null
   PERIODICFLUX=null
   SPATIALEXTRAPOLATIONRAMP=yes
   SPATIALEXTRAPOLATIONRAMPDISTANCE=1.0
   INITPOST=( null_post.sh )
   # Operators should set the value of this parameter
   # to their email address in their ~/.asgsh_profile files
   # on each platform
   ASGSADMIN=null
