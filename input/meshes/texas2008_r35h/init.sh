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
nodes=3352598
elements=6675517
GRIDFILE=tx2008_r35h.grd # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=tx2008r35h_template.15   # fort.15 template
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=tx2008r35h_norough_template.15
ELEVSTATIONS=tx2008r35h_stations_20170618.txt
VELSTATIONS=tx2008r35h_stations_20170618.txt
METSTATIONS=tx2008r35h_stations_20170618.txt
NAFILE=tx2008_r35h.13
nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2763"
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
# unit offset url https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_tx2008_r35h.grd.dat.xz
UNITOFFSETFILE=unit_offset_tx2008_r35h.grd.dat