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
STDMESHNAME=EC95d
nodes=31435
elements=58369
GRIDFILE=ec_95d.grd   # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=ec_95_fort.15_template   # fort.15 template
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=ec_95_nowindreduction.fort.15_template
ELEVSTATIONS=cera_stations.txt
VELSTATIONS=cera_stations.txt
METSTATIONS=cera_stations.txt
NAFILE=null
# interaction between mesh and models:
TIMESTEPSIZE=30.0           # adcirc time step size (seconds)
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="06:00:00" # hindcast wall clock time
ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="01:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="01:00:00" # forecast wall clock time
# FIXME: no unit offset url