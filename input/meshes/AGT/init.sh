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
# AGT
#
# adcirc-global-test mesh, from ADCIRC test suite repository
# https://github.com/adcirc/adcirc-testsuite
STDMESHNAME=AGT
nodes=27330
elements=50859
GRIDFILE=agt.14
MESHPROPERTIES=${GRIDFILE}.properties
CONTROLTEMPLATE=m2-agt.fort.15.template
# wind at 10m fort.15 template
CONTROLTEMPLATENOROUGH=$CONTROLTEMPLATE # no wind roughness in AGT fort.13
ELEVSTATIONS=elev_stations_agt.txt
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
NAFILE=agt.13
# interaction between mesh and models:
TIMESTEPSIZE=720.0          # adcirc time step size (seconds)
zNorth="112.8516 40.3289  ! China-Argentina"
selfAttractionEarthLoadTide="m2-agt.fort.24"
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="01:00:00" # hindcast wall clock time
ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="01:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="01:00:00" # forecast wall clock time