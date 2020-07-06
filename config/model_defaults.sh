#!/bin/bash
#----------------------------------------------------------------
#
# model_defaults.sh: This script provides the default 
# configuration parameters for the models being driven by the 
# ASGS.
#
#----------------------------------------------------------------
# Copyright(C) 2014 Jason Fleming
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
TIMESTEPSIZE=1.0
DEBUG=null                  # "full" or "null"
SWAN=disable                # "enable" or "disable"
NETCDF=disable              # "enable" or "disable"
NETCDF4=disable             # "enable" or "disable" 
NETCDF4_COMPRESSION=disable # "enable" or "disable"
XDMF=disable                # "enable" or "disable"
SOURCEURL=https://adcirc.renci.org/svn/adcirc/branches/v50release
AUTOUPDATE=off              # "on" or "off"
EXEBASEPATH=~/adcirc/asgs   # main directory for ASGS executables
SWANMACROSINC=nullmacros    # env_dispatch will pick the right file
ADCOPTIONS='compiler=intel' # env_dispatch will pick the right options
