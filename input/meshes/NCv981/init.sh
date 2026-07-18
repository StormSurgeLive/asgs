#!/bin/bash
#----------------------------------------------------------------
# init.sh
#----------------------------------------------------------------
# Copyright(C) 2026 Jason Fleming
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
STDMESHNAME=NCv981
nodes=567973
elements=1121434
GRIDFILE=nc_v9.81_MSL_AK_Riv.14  # original
if [[ meshVersion == "9.81.001" ]]; then
   GRIDFILE=NCv981.001.14  # boundaries 10->20, 12->22, etc
fi
MESHPROPERTIES=${GRIDFILE}.properties
ELEVSTATIONS=ncv999_stations_20180907.txt
VELSTATIONS=${ELEVSTATIONS}
METSTATIONS=${ELEVSTATIONS}
#
case $parameterPackage in
# there is no hardcoded option for this mesh
"default"|"hardcoded")
    CONTROLTEMPLATE=NCv981_2026.1.fort.15.template
    # tidal forcing
    tidalConstituents=( "m2" "s2" "n2" "k2" "k1" "o1" "p1" "q1" )
    # river boundary forcing
    RIVERFLUX=NCv981_fort.20_noflux
    HINDCASTRIVERFLUX=NCv981_fort.20_hc_noflux
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
    NAFILE=nc_v9.81_MSL_AK_Riv.13.template
    nodal_attribute_activate=( surface_directional_effective_roughness_length )
    nodal_attribute_activate+=( surface_canopy_coefficient )
    nodal_attribute_activate+=( mannings_n_at_sea_floor )
    nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
    nodal_attribute_activate+=( average_horizontal_eddy_viscosity_in_sea_water_wrt_depth )
    nodal_attribute_activate+=( sea_surface_height_above_geoid )
    #nodal_attribute_activate+=( initial_river_elevation )
    #
    nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
    nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
    nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.02"
    nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
    nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="4.0"
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.0"
    nodal_attribute_default_values["initial_river_elevation"]="0.0"
    #
    # intersection between mesh, models, hpc platform, and number of compute cores:
    HINDCASTLENGTH=18.0         # days
    HINDCASTWALLTIME="72:00:00" # hindcast wall clock time
    ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
    NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
    FORECASTWALLTIME="24:00:00" # forecast wall clock time
    ;;
    #
"full-implicit")
    CONTROLTEMPLATE=NCv981_2026.2.fort.15.template
    coordinateSystem["reprojection"]="CPP"
    # tidal forcing
    tidalConstituents=( "m2" "s2" "n2" "k2" "k1" "o1" "p1" "q1" )
    # river boundary forcing
    RIVERFLUX=NCv981_fort.20_noflux
    HINDCASTRIVERFLUX=NCv981_fort.20_hc_noflux
    VARFFLUX=default
    # numerics/physics
    TIMESTEPSIZE=5.0                      # adcirc time step size (seconds)
    advection="on"                        # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
    solver_time_integration="full-gravity-wave-implicit"    # implicit|explicit|full-gravity-wave-implicit
    time_weighting_coefficients="0.5 0.5 0.0" # A00 B00 C00 in fort.15
    lateral_turbulence="smagorinsky"      # "smagorinsky" or "eddy_viscosity"
    smagorinsky_coefficient="0.2"
    bottom_friction_limit=0.0025          # min when using Manning's n (CF/FFACTOR)
    h0=0.1                                # min depth (m) to be considered wet
    velmin=0.01
    TAU0=0.32
    # nodal attributes
    NAFILE=NCv981.001.mesh001.13.template
    nodal_attribute_activate=( surface_directional_effective_roughness_length )
    nodal_attribute_activate+=( surface_canopy_coefficient )
    nodal_attribute_activate+=( mannings_n_at_sea_floor )
    nodal_attribute_activate+=( sea_surface_height_above_geoid )
    nodal_attribute_activate+=( elemental_slope_limiter )
    #nodal_attribute_activate+=( initial_river_elevation )
    #
    nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
    nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
    nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.02"
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.0"
    nodal_attribute_default_values["initial_river_elevation"]="0.0"
    nodal_attribute_default_values["elemental_slope_limiter"]="-0.001"
    #
    # intersection between mesh, models, hpc platform, and number of compute cores:
    HINDCASTLENGTH=18.0         # days
    HINDCASTWALLTIME="72:00:00" # hindcast wall clock time
    ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
    NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
    FORECASTWALLTIME="24:00:00" # forecast wall clock time
    ;;
*)
    fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
    ;;
esac