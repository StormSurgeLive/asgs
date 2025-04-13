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
nodes=624782
elements=1234231
GRIDFILE=nc_inundation_v9.99a_w_rivers.grd
MESHPROPERTIES=${GRIDFILE}.properties
ELEVSTATIONS=ncv999_stations_20180907.txt
VELSTATIONS=${ELEVSTATIONS}
METSTATIONS=${ELEVSTATIONS}
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
# unit offset url https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_nc_inundation_v9.99_rivers.dat.xz
UNITOFFSETFILE=unit_offset_nc_inundation_v9.99_rivers.dat
case $parameterPackage in
"hardcoded")
    CONTROLTEMPLATE=nc_9.99wrivers_vortex_fort.15.template
    # wind at 10m fort.15 template
    CONTROLTEMPLATENOROUGH=nc_9.99wrivers.nowindreduction.fort.15.template
    NAFILE=nc_inundation_v9.99_rivers.13
    RIVERINIT=v6brivers.88
    RIVERFLUX=v6brivers_fort.20_default
    HINDCASTRIVERFLUX=v6brivers_fort.20_hc_default
    # interaction between mesh and models:
    TIMESTEPSIZE=0.5           # adcirc time step size (seconds)
    ;;
"2025.1"|"default")
    CONTROLTEMPLATE=nc_9.99wrivers_vortex_fort.15.template
    # tidal forcing
    tidalConstituents=( "m2" "s2" "n2" "k1" "k2" "o1" "q1" )
    # river boundary forcing
    RIVERFLUX=v6brivers_fort.20_default
    HINDCASTRIVERFLUX=v6brivers_fort.20_hc_default
    VARFFLUX=default
    # numerics/physics
    TIMESTEPSIZE=0.5                      # adcirc time step size (seconds)
    advection="on"                        # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
    solver_time_integration="explicit"    # implicit|explicit|full-gravity-wave-implicit
    time_weighting_coefficients="0.0 1.0 0.0" # A00 B00 C00 in fort.15
    lateral_turbulence="eddy_viscosity"   # "smagorinsky" or "eddy_viscosity"
    eddy_viscosity_coefficient="2.0"      # ESLM
    bottom_friction_limit=0.003           # min when using Manning's n (CF/FFACTOR)
    h0=0.1                                # min depth (m) to be considered wet
    velmin=0.01
    # nodal attributes
    NAFILE=nc_inundation_v9.99_rivers.13.template
    nodal_attribute_activate=( surface_directional_effective_roughness_length mannings_n_at_sea_floor )
    nodal_attribute_activate+=( surface_canopy_coefficient primitive_weighting_in_continuity_equation )
    nodal_attribute_activate+=( average_horizontal_eddy_viscosity_in_sea_water_wrt_depth )
    nodal_attribute_activate+=( initial_river_elevation sea_surface_height_above_geoid )
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.0"
    nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
    nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="10.0"
    nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
    nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
    nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.02"
    nodal_attribute_default_values["initial_river_elevation"]="0.0"
    ;;
*)
    fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
    ;;
esac