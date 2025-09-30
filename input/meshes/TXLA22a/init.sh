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
nodes=1947485
elements=3832707
INPUTDIR=$SCRIPTDIR/input/meshes/TXLA22a
GRIDFILE=TXLA22a.14          # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.properties
ELEVSTATIONS=tx2008r35h_stations_20170618.txt
VELSTATIONS=tx2008r35h_stations_20170618.txt
METSTATIONS=tx2008r35h_stations_20170618.txt
# intersection between mesh, models, hpc platform, and n compute cores:
HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
ADCPREPWALLTIME="10:00:00"  # adcprep wall clock time,including partmesh
NOWCASTWALLTIME="10:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="14:00:00" # forecast wall clock time
# relax quality control
QUALITYSETTING="allow-nonfatal-instability"
case $parameterPackage in
"hardcoded")
    CONTROLTEMPLATE=TXLA22a_fort.15.template  # fort.15 template
    # wind at 10m fort.15 template
    CONTROLTEMPLATENOROUGH=$CONTROLTEMPLATE
    NAFILE=TXLA22a.13
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2347"
    TIMESTEPSIZE=2.0           # adcirc time step size (seconds)
    ;;
"2025.1"|"default")
    CONTROLTEMPLATE=TXLA22a.15.ASGS2024.1.template
    # numerics/physics (fort.15)
    advection="on"                        # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
    solver_time_integration="explicit"    # implicit|explicit|full-gravity-wave-implicit
    time_weighting_coefficients="0.0 1.0 0.0" # A00 B00 C00 in fort.15
    lateral_turbulence="smagorinsky"      # "smagorinsky" or "eddy_viscosity"
    smagorinsky_coefficient="0.05"        # ESLM
    bottom_friction_limit=0.0005          # min when using Manning's n (CF/FFACTOR)
    h0=0.1                                # min depth (m) to be considered wet
    velmin=0.01
    TIMESTEPSIZE=2.0                      # adcirc time step size (seconds)
    metControl["WindDragLimit"]="0.002"   # max wind drag coefficient, unitless
    nodal_attribute_activate=( sea_surface_height_above_geoid )
    nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
    nodal_attribute_activate+=( mannings_n_at_sea_floor )
    nodal_attribute_activate+=( internal_tide_friction )
    nodal_attribute_activate+=( subgrid_barrier )
    # tidal forcing
    tidalConstituents=( "q1" "o1" "p1" "k1" "n2" "m2" "s2" "k2" )
    # nodal attributes file
    NAFILE=TXLA22a.13.template
    nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.022"
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2763"
    nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
    nodal_attribute_default_values["internal_tide_friction"]="0.0 0.0 0.0"
    nodal_attribute_default_values["subgrid_barrier"]="99999.0"
    ;;
*)
    fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
    ;;
esac
