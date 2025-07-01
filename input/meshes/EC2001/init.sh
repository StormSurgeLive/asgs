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
STDMESHNAME=EC2001v2e
nodes=254565
elements=492179
GRIDFILE=ec2001_v2e.grd   # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=ec2001_v2e_fort.15.template   # fort.15 template (designed for 1s timestep)
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=$CONTROLTEMPLATE  # same b/c no inundation coverage
ELEVSTATIONS=hsofs_stations_20180907.txt
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
NAFILE=ec2001_v2e.13
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="06:00:00" # hindcast wall clock time
ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="01:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="01:00:00" # forecast wall clock time
UNITOFFSETFILE=null