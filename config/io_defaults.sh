#!/bin/bash
#----------------------------------------------------------------
# io_defaults.sh : Functions required for initializing
# parameters that are only related to input/output controls.
#----------------------------------------------------------------
# Copyright(C) 2019--2025 Jason Fleming
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
# nonfatal override; warnelev and errorelev
declare -g -A nfover
nfover["NFOVER"]=1
nfover["WarnElev"]=20.0
nfover["iWarnElevDump"]=1
nfover["WarnElevDumpLimit"]=20
nfover["ErrorElev"]=100.0
# log level
log_level=${log_level:-"INFO"}              # NABOUT (DEBUG=-1, ECHO=0, INFO=1, WARNING=2, ERROR=3)
outputInventory=${outputInventory:-"full"}  # full|metonly
# email
MAILINGLIST=${MAILINGLIST:-"null"}
NOTIFYUSER=${NOTIFYUSER:-"null"}
#
# the use of the OUTPUTOPTIONS string is deprecated
# station lists
ELEVSTATIONS=${ELEVSTATIONS:-"null"}
VELSTATIONS=${VELSTATIONS:-"null"}
METSTATIONS=${METSTATIONS:-"null"}
OUTPUTOPTIONS=null
# stations
declare -g -A fort61
declare -g -A fort62
declare -g -A fort7172
fort61["format"]="netcdf4"
fort61["incr_seconds"]=300.0
fort61["append"]="yes"
fort62["format"]="netcdf4"
fort62["incr_seconds"]=0.0
fort62["append"]="yes"
fort7172["format"]="netcdf4"
fort7172["incr_seconds"]=300.0
fort7172["append"]="yes"
# fulldomain
declare -g -A fort63
declare -g -A fort64
declare -g -A fort7374
fort63["format"]="netcdf4"
fort63["incr_seconds"]=3600.0
fort63["append"]="yes"
fort64["format"]="netcdf4"
fort64["incr_seconds"]=3600.0
fort64["append"]="yes"
fort7374["format"]="netcdf4"
fort7374["incr_seconds"]=3600.0
fort7374["append"]="yes"
# hotstart files
HOTSTARTCOMP=${HOTSTARTCOMP:-"fulldomain"} # fulldomain or subdomain
HOTSTARTFORMAT=${HOTSTARTFORMAT:-"netcdf"} # netcdf3 or netcdf (meaning netcdf4) or binary
hotStartInputFormat=$HOTSTARTFORMAT
hotStartOutputFormat=$HOTSTARTFORMAT
# "continuous" or "reset" for maxele.63 etc files
MINMAX=reset
#
createWind10mLayer=${createWind10mLayer:-"no"}  # yes|no ; applies to all scenarios that have meteorological forcing
#
# settings to take care of explicitly defined Wind10m scenarios
# old config files (or new config files copied from old config files)
# may use the deprecated variable ENSTORM rather than the preferred
# SCENARIO parameter. Wave coupling will be turned off in generateDynamicInput.sh.
if [[ $ENSTORM == *"Wind10m" || $SCENARIO == *"Wind10m" ]]; then
   POSTPROCESS=( null_post.sh )
   OPENDAPNOTIFY="null"
fi
