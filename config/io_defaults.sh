#!/bin/bash
#----------------------------------------------------------------
# io_defaults.sh : Functions required for initializing
# parameters that are only related to input/output controls.
#----------------------------------------------------------------
# Copyright(C) 2019--2022 Jason Fleming
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
consoleMessage "Setting default parameters for input/output."
#
NSCREEN=${NSCREEN:-"-1000"} # frequency (in time steps) of output to adcirc.log
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
if [[ ${ENSTORM:(-7)} = "Wind10m" ]]; then
   scenarioMessage "$THIS: Setting parameters to trigger ADCIRC met-only mode for ${ENSTORM}."
   ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
   FORECASTWALLTIME="01:00:00" # forecast wall clock time
   CONTROLTEMPLATE=$CONTROLTEMPLATENOROUGH  # CONTROLTEMPLATENOROUGH set in config/mesh_defaults.sh
   TIMESTEPSIZE=300.0          # 15 minute time steps
   NCPU=15                     # dramatically reduced resource requirements
   NUMWRITERS=1                # multiple writer procs might collide
   WAVES=off                   # deactivate wave forcing
   FORT61="--fort61freq 0"     # turn off water surface elevation station output
   FORT62="--fort62freq 0"     # turn off water current velocity station output
   FORT63="--fort63freq 0"     # turn off full domain water surface elevation output
   FORT64="--fort64freq 0"     # turn off full domain water current velocity output
   FORT7172="--fort7172freq 300.0 --fort7172netcdf"    # met station output
   FORT7374="--fort7374freq 3600.0 --fort7374netcdf"   # full domain meteorological output
   #SPARSE="--sparse-output"
   SPARSE=""
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   POSTPROCESS=( null_post.sh )
fi
