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
nodes=1813443
elements=3564104
GRIDFILE=hsofs.14  # mesh (fort.14) file
MESHPROPERTIES=${GRIDFILE}.nc.properties
ELEVSTATIONS=hsofs_stations_20180907.txt
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
TIMESTEPSIZE=2.0            # adcirc time step size (seconds)
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
case $parameterPackage in
"hardcoded")
   CONTROLTEMPLATE=hsofs_explicit.15.template
   # wind at 10m fort.15 template
   CONTROLTEMPLATENOROUGH=hsofs.nowindreduction.15.template
   NAFILE=hsofs.13
   # interaction between mesh and models:
   SWANDT=1800                 # swan timestep / coupling interval (seconds)
   # unit offset url https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_hsofs.dat.xz
   UNITOFFSETFILE=unit_offset_hsofs.dat
   ;;
"2025.1"|"default")
   CONTROLTEMPLATE=HSOFS.15.2025.1.template
   # interaction between mesh and models:
   SWANDT=1800                 # swan timestep / coupling interval (seconds)
   # tidal forcing
   tidalConstituents=( "k1" "o1" "p1" "q1" "n2" "m2" "s2" "k2" )
   # numerics/physics
   advection="off"                           # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
   solver_time_integration="explicit"        # implicit|explicit|full-gravity-wave-implicit
   time_weighting_coefficients="0.0 1.0 0.0" # A00 B00 C00 in fort.15
   lateral_turbulence="eddy_viscosity"       # "smagorinsky" or "eddy_viscosity"
   eddy_viscosity_coefficient="10.0"         # ESLM
   bottom_friction_limit=0.0025              # min when using Manning's n (CF/FFACTOR)
   h0=0.05                                   # min depth (m) to be considered wet
   velmin=0.05                               # pseudovelocity threshold
   metControl["WindDragLimit"]="0.0028"      # max wind drag coefficient, unitless
   # nodal attributes
   NAFILE=hsofs-parameters-v2.13
   nodal_attribute_activate=( mannings_n_at_sea_floor )
   nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
   nodal_attribute_activate+=( surface_canopy_coefficient )
   nodal_attribute_activate+=( surface_directional_effective_roughness_length  )
   nodal_attribute_activate+=( surface_submergence_state )
   nodal_attribute_activate+=( sea_surface_height_above_geoid )
   nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.0"
   nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
   nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="10.0"
   nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
   nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
   nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.02"
   nodal_attribute_default_values["surface_submergence_state"]="0.0"
   ;;
*)
   fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
   ;;
esac