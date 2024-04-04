#!/bin/bash
#----------------------------------------------------------------
# io_defaults.sh : Functions required for initializing
# parameters that are only related to input/output controls.
#----------------------------------------------------------------
# Copyright(C) 2019--2024 Jason Fleming
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
THIS=$(basename -- $0)
#
NSCREEN=${NSCREEN:-"-1000"} # frequency (in time steps) of output to adcirc.log
nfover=${nfover:-"1 20.0 1 20 100.0"}       # nonfatal override; warnelev and errorelev
log_level=${log_level:-"INFO"}              # NABOUT (DEBUG=-1, ECHO=0, INFO=1, WARNING=2, ERROR=3)
outputInventory=${outputInventory:-"full"}  # full|metonly
# io
ELEVSTATIONS=${ELEVSTATIONS:-"null"}
VELSTATIONS=${VELSTATIONS:-"null"}
METSTATIONS=${METSTATIONS:-"null"}
OUTPUTOPTIONS=${OUTPUTOPTIONS:-"notset"}
HOTSTARTCOMP=${HOTSTARTCOMP:-"fulldomain"}
MAILINGLIST=${MAILINGLIST:-"null"}
NOTIFYUSER=${NOTIFYUSER:-"null"}
HOTSTARTFORMAT=${HOTSTARTFORMAT:-"null"}
#
# water surface elevation station output
FORT61="--fort61freq 300.0 --fort61netcdf"
# water current velocity station output
FORT62="--fort62freq 0"
# full domain water surface elevation output
FORT63="--fort63freq 3600.0 --fort63netcdf"
# full domain water current velocity output
FORT64="--fort64freq 3600.0 --fort64netcdf"
# met station output
FORT7172="--fort7172freq 300.0 --fort7172netcdf"
# full domain meteorological output
FORT7374="--fort7374freq 3600.0 --fort7374netcdf"
#SPARSE="--sparse-output"
SPARSE=""
NETCDF4="--netcdf4"
OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
# fulldomain or subdomain hotstart files
if [[ $HOTSTARTCOMP != "subdomain" ]]; then
   HOTSTARTCOMP="fulldomain"
fi
# binary, netcdf, or netcdf3 hotstart files
if [[ $HOTSTARTFORMAT != "netcdf3" && $HOTSTARTFORMAT != "binary" ]]; then
   HOTSTARTFORMAT="netcdf"
fi
# "continuous" or "reset" for maxele.63 etc files
MINMAX=reset
#
createWind10mLayer="no"  # yes|no ; applies to all scenarios that have meteorological forcing
#
# settings to take care of explicitly defined Wind10m scenarios
# old config files (or new config files copied from old config files)
# may use the deprecated variable ENSTORM rather than the preferred
# SCENARIO paramter
if [[ $ENSTORM == *"Wind10m" || $SCENARIO == *"Wind10m" ]]; then
   WAVES=off                   # deactivate wave forcing
   POSTPROCESS=( null_post.sh )
   OPENDAPNOTIFY="null"
fi
