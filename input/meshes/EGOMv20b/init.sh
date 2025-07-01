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
STDMESHNAME=EGOMv20b
nodes=2219482
elements=4398700
GRIDFILE=EGOM-RT_v20b_chk.grd
MESHPROPERTIES=${GRIDFILE}.properties
ELEVSTATIONS=EGOM-RT_v20b_stations.txt
VELSTATIONS=EGOM-RT_v20b_stations.txt
METSTATIONS=EGOM-RT_v20b_stations.txt
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="10:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
case $parameterPackage in
"hardcoded")
    CONTROLTEMPLATE=EGOM-RT_v20b.15.template   # fort.15 template
    # wind at 10m fort.15 template
    CONTROLTEMPLATENOROUGH=EGOM-RT_v20b.norough.15.template
    NAFILE=EGOM-RT_v20b_asgs_chk.13
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="-0.106"
    ;;
"2025.1"|"default")
    CONTROLTEMPLATE=EGOMv20b.15.ASGS2024.1.template
    # numerics/physics (fort.15)
    advection="on"                            # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
    solver_time_integration="explicit"        # implicit|explicit|full-gravity-wave-implicit
    time_weighting_coefficients="0.0 1.0 0.0" # A00 B00 C00 in fort.15
    lateral_turbulence="smagorinsky"          # "smagorinsky" or "eddy_viscosity"
    eddy_viscosity_coefficient="20.0"         # ESLM
    smagorinsky_coefficient="0.2"
    bottom_friction_limit=0.001               # min when using Manning's n (CF/FFACTOR)
    h0=0.1                                    # min depth (m) to be considered wet
    velmin=0.01
    nodal_attribute_activate=( sea_surface_height_above_geoid )
    nodal_attribute_activate+=( advection_state )
    nodal_attribute_activate+=( mannings_n_at_sea_floor )
    nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
    nodal_attribute_activate+=( surface_canopy_coefficient )
    nodal_attribute_activate+=( surface_directional_effective_roughness_length )
    nodal_attribute_activate+=( elemental_slope_limiter )
    # tidal forcing
    tidalConstituents=( "k1" "o1" "p1" "q1" "n2" "m2" "s2" "k2" )
    # nodal attributes file
    NAFILE=EGOM-RT_v20b_asgs_chk.13.template
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="-0.106"
    nodal_attribute_default_values["advection_state"]="-99999.0"
    nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.025"
    nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
    nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
    nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
    nodal_attribute_default_values["elemental_slope_limiter"]="0.02"
    # meteorological forcing
    metControl["WindDragLimit"]="0.0025"  # max wind drag coefficient, unitless
    metControl["invertedBarometerOnElevationBoundary"]="yes" # yes|no to include inverse barometer effect on boundary
    # wetting and drying
    wetDryControl["slim"]=1000000000.0    # value of slope limiter for wet/dry
    # SWAN coupling
    SWANDT=1800
    ;;
*)
    fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
    ;;
esac