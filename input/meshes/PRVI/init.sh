#!/bin/bash
#----------------------------------------------------------------
# init.sh
#----------------------------------------------------------------
# Copyright(C) 2025 Jason Fleming
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
STDMESHNAME=PRVI15
nodes=2733258
elements=5392748
GRIDFILE=PRVI15_fort.14  # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=PRVI15_fort.15.template  # designed for 2s timestep (any adcirc version)
h0=0.05
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=PRVI15_norough_fort.15.template
ELEVSTATIONS=hsofs_stations_20180907.txt
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
NAFILE=PRVI15_fort.13
# interaction between mesh and models:
SWANDT=600                 # swan timestep / coupling interval (seconds)
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
UNITOFFSETFILE=null