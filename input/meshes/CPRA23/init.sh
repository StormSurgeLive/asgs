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
nodes=1577268
elements=3072131
GRIDFILE=cpra_2023_hurricane_v05b_chk.grd     # mesh (fort.14) file
if [[ $GRIDNAME == "CPRA23v05c" ]]; then
    GRIDFILE=cpra_2023_hurricane_v05c_chk.grd  # mesh (fort.14) file
fi
MESHPROPERTIES=${GRIDFILE}.properties
ELEVSTATIONS=combined_stations_20230824.txt
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
case $parameterPackage in
"hardcoded")
    CONTROLTEMPLATE=CPRA23v01c_5kcms_fort.15.template
    # wind at 10m fort.15 template
    CONTROLTEMPLATENOROUGH=CPRA23v01c_nowindreduction_fort.15.template
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.362102"
    NAFILE=cpra_2023_hurricane_v05b.13
    SWANTEMPLATE=CPRA23v01c_fort.26.template
    if [[ $GRIDNAME == "CPRA23v05c" ]]; then
    NAFILE=cpra_2023_hurricane_v05c.13
    fi
    ;;
"2025.1"|"default")
    CONTROLTEMPLATE=CPRA23.15.ASGS2024.1.template
    # numerics/physics (fort.15)
    advection="off"                       # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
    solver_time_integration="implicit"    # implicit|explicit|full-gravity-wave-implicit
    time_weighting_coefficients="0.35 0.3 0.35" # A00 B00 C00 in fort.15
    lateral_turbulence="eddy_viscosity"   # "smagorinsky" or "eddy_viscosity"
    eddy_viscosity_coefficient="2.0"      # ESLM
    bottom_friction_limit=0.001           # min when using Manning's n (CF/FFACTOR)
    h0=0.1                                # min depth (m) to be considered wet
    velmin=0.01
    nodal_attribute_activate=( surface_submergence_state )
    nodal_attribute_activate+=( surface_directional_effective_roughness_length )
    nodal_attribute_activate+=( surface_canopy_coefficient )
    nodal_attribute_activate+=( mannings_n_at_sea_floor )
    nodal_attribute_activate+=( average_horizontal_eddy_viscosity_in_sea_water_wrt_depth )
    nodal_attribute_activate+=( sea_surface_height_above_geoid )
    nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
    nodal_attribute_activate+=( elemental_slope_limiter )
    # tidal forcing
    tidalConstituents=( "q1" "o1" "p1" "k1" "n2" "m2" "s2" "k2" )
    # river boundary forcing
    PERIODICFLUX=$INPUTDIR/CPRA23_default_river_flux.txt
    # nodal attributes file
    NAFILE=cpra_2023_hurricane_v05b.13.template
    if [[ $GRIDNAME == "CPRA23v05c" ]]; then
    NAFILE=cpra_2023_hurricane_v05c.13.template
    fi
    nodal_attribute_default_values["surface_submergence_state"]="0.0"
    nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
    nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
    nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.022"
    nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="20.0"
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.362102"
    nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
    nodal_attribute_default_values["elemental_slope_limiter"]="0.05"
    nodal_attribute_default_values["advection_state"]="-9999.0"
    ;;
*)
    fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
    ;;
esac