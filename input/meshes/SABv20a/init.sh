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
nodes=5584241
elements=11066018
GRIDFILE=SABv20a.14  # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.properties
ELEVSTATIONS=hsofs_stations_20180907.txt
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="36:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="24:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="24:00:00" # forecast wall clock time
case $parameterPackage in
"hardcoded")
    CONTROLTEMPLATE=SABv20a.15.template
    # wind at 10m fort.15 template
    CONTROLTEMPLATENOROUGH=SABv20a.nowindreduction.15.template
    NAFILE=SABv20a.13
    # interaction between mesh and models:
    TIMESTEPSIZE=0.5            # adcirc time step size (seconds)
    ;;
"2025.1"|"default")
    CONTROLTEMPLATE=SABv20a.15.ASGS2024.1.template
    # numerics/physics (fort.15)
    TIMESTEPSIZE=1.0                      # adcirc time step size (seconds)
    advection="on"                        # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
    solver_time_integration="explicit"    # implicit|explicit|full-gravity-wave-implicit # for explicit, last digit in IM is 2
    time_weighting_coefficients="0.0 1.0 0.0" # A00 B00 C00 in fort.15
    lateral_turbulence="smagorinsky"      # "smagorinsky" or "eddy_viscosity"
    smagorinsky_coefficient="1.0"         # first digit in IM is 5
    eddy_viscosity_coefficient="50.0"     # ESLM
    bottom_friction_limit=0.001           # min when using Manning's n (CF/FFACTOR)
    h0=0.1                                # min depth (m) to be considered wet
    velmin=0.5
    #nodal_attribute_activate=( average_horizontal_eddy_viscosity_in_sea_water_wrt_depth )
    #nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
    nodal_attribute_activate=( mannings_n_at_sea_floor )
    nodal_attribute_activate+=( elemental_slope_limiter )
    nodal_attribute_activate+=( surface_canopy_coefficient )
    nodal_attribute_activate+=( surface_directional_effective_roughness_length )
    nodal_attribute_activate+=( advection_state )
    nodal_attribute_activate+=( sea_surface_height_above_geoid )
    # tidal forcing
    tidalConstituents=( "k1" "o1" "p1" "q1" "n2" "m2" "s2" "k2" )
    # nodal attributes file
    NAFILE=SABv20a.13.template
    nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="50.0"
    nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
    nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.02"
    nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
    nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.0"
    nodal_attribute_default_values["elemental_slope_limiter"]="99999.0"
    nodal_attribute_default_values["advection_state"]="-100.0"
    # meteorological forcing
    metControl["WindDragLimit"]="0.002"                      # max wind drag coefficient, unitless
    metControl["invertedBarometerOnElevationBoundary"]="yes"
    # wetting and drying
    wetDryControl["slim"]=1000000000.0
    # wave coupling
    SWANDT=1800
    ;;
*)
    fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
    ;;
esac
