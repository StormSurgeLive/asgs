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
STDMESHNAME=FEMAR3
nodes=1875689
elements=3731099
GRIDFILE=FEMA_R3_20110303_MSL.grd
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=FEMA_R3_fort.15.template
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=FEMA_R3_nowindreduction_fort.15.template
ELEVSTATIONS=cera_stations_20180810.txt
VELSTATIONS=${ELEVSTATIONS}
METSTATIONS=${ELEVSTATIONS}
NAFILE=FEMA_R3_20110303_MSL.13
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
# https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_FEMA_R3_20110303_MSL.dat.xz
UNITOFFSETFILE=unit_offset_FEMA_R3_20110303_MSL.dat
