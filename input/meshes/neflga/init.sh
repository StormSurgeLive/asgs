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
STDMESHNAME=NEFLGAv12b
nodes=2968735
elements=5910443
GRIDFILE=neflga_v12_geo.14 # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.properties
if [[ $GRIDNAME = "NEFLGAv12b" ]];  then
    GRIDFILE=neflga_v12b_geo.14 # mesh (fort.14) file
fi
CONTROLTEMPLATE=neflga_v12_geo_template.15   # fort.15 template
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=neflga_v12_geo_norough_template.15
ELEVSTATIONS=neflga_stations_20190809.txt
VELSTATIONS=${ELEVSTATIONS}
METSTATIONS=${ELEVSTATIONS}
NAFILE=neflga_v12.13
nodal_attribute_default_values["sea_surface_height_above_geoid"]="-0.003"
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
# FIXME: no unit offset url