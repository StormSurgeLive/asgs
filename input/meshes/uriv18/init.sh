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
nodes=2617704
elements=5149248
GRIDFILE=hsofs_NE-hires_v18_weir_rivers_depsm2_nopump.grd
MESHPROPERTIES=${GRIDFILE}.nc.properties
CONTROLTEMPLATE=fort.15.template
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=norough_fort.15.template
ELEVSTATIONS=elev_stations.txt
VELSTATIONS=vel_stations.txt
METSTATIONS=met_stations.txt
NAFILE=hsofs_NE-hires_v18_weir_rivers_depsm2.13
# interaction between mesh and models:
TIMESTEPSIZE=0.5            # adcirc time step size (seconds)
SWANDT=1800                 # swan timestep / coupling interval (seconds)
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
# unit offset url https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_hsofs.dat.xz
UNITOFFSETFILE=unit_offset_hsofs.dat