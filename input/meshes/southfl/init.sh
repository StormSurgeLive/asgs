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
nodes=2249093
elements=4480230
INPUTDIR=$SCRIPTDIR/input/meshes/southfl
GRIDFILE=southfl_v11-1_final.grd
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=southfl-v11-1.template.15
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=southfl-v11-1.nowindreduction.template.15
ELEVSTATIONS=southfl_stations_20190502.txt
VELSTATIONS=southfl_stations_20190502.txt
METSTATIONS=southfl_stations_20190502.txt
NAFILE=southfl_v11-1_final-production.13
nodal_attribute_default_values["sea_surface_height_above_geoid"]="-0.155"
HINDCASTRIVERFLUX=null
# interaction between mesh and models:
TIMESTEPSIZE=0.5            # adcirc time step size (seconds)
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
# FIXME: no unit offset url