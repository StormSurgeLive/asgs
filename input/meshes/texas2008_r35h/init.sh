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
STDMESHNAME=TX2008
nodes=3352598
elements=6675517
GRIDFILE=tx2008_r35h.grd # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.properties
ELEVSTATIONS=tx2008r35h_stations_20170618.txt
VELSTATIONS=tx2008r35h_stations_20170618.txt
METSTATIONS=tx2008r35h_stations_20170618.txt
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
case $parameterPackage in
"hardcoded")
    CONTROLTEMPLATE=tx2008r35h_template.15   # fort.15 template
    # wind at 10m fort.15 template
    CONTROLTEMPLATENOROUGH=tx2008r35h_norough_template.15
    # wind at 10m fort.15 template
    CONTROLTEMPLATENOROUGH=$CONTROLTEMPLATE
    NAFILE=tx2008_r35h.13
    ;;
"2025.1"|"default")
    CONTROLTEMPLATE=TX2008.15.ASGS2024.1.template
    # numerics/physics (fort.15)
    advection="off"                       # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
    solver_time_integration="implicit"    # implicit|explicit|full-gravity-wave-implicit
    time_weighting_coefficients="0.35 0.30 0.35" # A00 B00 C00 in fort.15
    lateral_turbulence="eddy_viscosity"   # "smagorinsky" or "eddy_viscosity"
    eddy_viscosity_coefficient="50.0"     # ESL
    bottom_friction_limit=0.0             # min when using Manning's n (CF/FFACTOR)
    h0=0.1                                # min depth (m) to be considered wet
    velmin=0.01
    metControl["WindDragLimit"]="0.002"   # max wind drag coefficient, unitless
    nodal_attribute_activate=( surface_submergence_state )
    nodal_attribute_activate+=( surface_directional_effective_roughness_length )
    nodal_attribute_activate+=( surface_canopy_coefficient )
    nodal_attribute_activate+=( mannings_n_at_sea_floor )
    nodal_attribute_activate+=( sea_surface_height_above_geoid )
    nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
    nodal_attribute_activate+=( average_horizontal_eddy_viscosity_in_sea_water_wrt_depth )
    # tidal forcing
    tidalConstituents=( "k1" "o1" "m2" "s2" "n2" "k2" "q1" )
    # nodal attributes file
    NAFILE=tx2008_r35h.13.template
    nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2763"
    nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
    nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="5.0"
    nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.02"
    nodal_attribute_default_values["surface_submergence_state"]="0.0"
    nodal_attribute_default_values["surface_canopy_coefficient"]="0.0"
    nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
    ;;
*)
    fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
    ;;
esac