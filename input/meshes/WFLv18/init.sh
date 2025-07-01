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
STDMESHNAME=WFLv18
nodes=1147947
elements=2283949
GRIDFILE=fema_wfl_fort.14  # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.nc.properties
CONTROLTEMPLATE=fema_wfl_fort.15.template
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=fema_wfl_nowindreduction.fort.15.template
h0=0.05
ELEVSTATIONS=fema_wfl_stations_20191114.txt
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
NAFILE=fema_wfl_fort.13
SWANDT=1800                 # swan timestep / coupling interval (seconds)
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
# unit offset url
UNITOFFSETFILE=null