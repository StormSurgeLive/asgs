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
#
#
nodes=1593485
elements=3102441
GRIDFILE=LA_v19k-WithUpperAtch_chk.grd   # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=LA_v19k-WithUpperAtch.15.template
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=LA_v19k-WithUpperAtch.nowindreduction.15.template
ELEVSTATIONS=combined_stations_20190327.txt
VELSTATIONS=combined_stations_20190327.txt
METSTATIONS=combined_stations_20190327.txt
NAFILE=LA_v19k-WithUpperAtch_chk.13
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
# unit offset url https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_LA_v19k-WithUpperAtch_chk.dat.xz
UNITOFFSETFILE=unit_offset_LA_v19k-WithUpperAtch_chk.dat