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
# NGOMv19b
#
nodes=2051450
elements=4065608
GRIDFILE=NGOM_RT_v19b_chk.grd
MESHPROPERTIES=${GRIDFILE}.properties
ELEVSTATIONS=NGOM_RT_v19b_stations_08282018.txt
VELSTATIONS=NGOM_RT_v19b_stations_08282018.txt
METSTATIONS=NGOM_RT_v19b_stations_08282018.txt
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="10:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="07:00:00" # forecast wall clock time
case $parameterPackage in
"hardcoded")
   CONTROLTEMPLATE=NGOM_RT_v19b.15.template_18kcms   # fort.15 template
   # wind at 10m fort.15 template
   CONTROLTEMPLATENOROUGH=NGOM_RT_v19b.nowindreduction.15.template
   NAFILE=NGOM_RT_v19b_chk.13
   nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2"
   ;;
"2025.1"|"default")  # "default" is deprecated
   CONTROLTEMPLATE=NGOMv19.15.ASGS2024.1.template
   advection="off"                       # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
   solver_time_integration="implicit"    # implicit|explicit|full-gravity-wave-implicit
   time_weighting_coefficients="0.35 0.3 0.35" # A00 B00 C00 in fort.15
   lateral_turbulence="eddy_viscosity"   # "smagorinsky" or "eddy_viscosity"
   eddy_viscosity_coefficient="20.0"     # ESLM
   bottom_friction_limit=0.0             # min when using Manning's n (CF/FFACTOR)
   # numerics/physics (fort.15)
   h0=0.1                                # min depth (m) to be considered wet
   velmin=0.01
   metControl["WindDragLimit"]="0.002"
   metControl["invertedBarometerOnElevationBoundary"]="yes" # yes|no to include inverse barometer effect on boundary
   wetDryControl["slim"]=1000000000.0
   nodal_attribute_activate=( sea_surface_height_above_geoid )
   nodal_attribute_activate+=( elemental_slope_limiter )
   nodal_attribute_activate+=( surface_submergence_state )
   nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
   nodal_attribute_activate+=( mannings_n_at_sea_floor )
   nodal_attribute_activate+=( surface_canopy_coefficient )
   nodal_attribute_activate+=( surface_directional_effective_roughness_length )
   # tidal forcing
   tidalConstituents=( "q1" "o1" "p1" "k1" "n2" "m2" "s2" "k2" )
   # river boundary forcing
   PERIODICFLUX=$INPUTDIR/NGOMv19_default_river_flux.txt
   # nodal attributes file
   NAFILE=NGOM_RT_v19b_chk.13.template
   nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2"
   nodal_attribute_default_values["elemental_slope_limiter"]="0.02"
   nodal_attribute_default_values["surface_submergence_state"]="0.0"
   nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
   nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.022"
   nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
   nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
   # subvariant of 2025.1 parameter package to test smagorinsky
   case $subvariant in
   "smagorinsky")
      advection="on"                        # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
      solver_time_integration="explicit"    # implicit|explicit|full-gravity-wave-implicit
      lateral_turbulence="smagorinsky"      # "smagorinsky" or "eddy_viscosity"
      smagorinsky_coefficient="0.2"         # ESLM
      bottom_friction_limit=0.001           # min when using Manning's n (CF/FFACTOR)
      wetDryControl["slim"]=1000000000.0
      # Remove the eddy viscosity nodal attribute from the active list
      declare -a _na_active
      for na in ${nodal_attribute_activate[@]} ; do
         if [[ $na != "average_horizontal_eddy_viscosity_in_sea_water_wrt_depth" ]]; then
            _na_active+=( $na )
         fi
      done
      nodal_attribute_activate=( ${na[*]} )
      unset _na_active
      # SWAN coupling
      SWANDT=1800
      ;;
   "null")
      logMessage "The parameter package '$parameterPackage' did not specify any subvariant."
      ;;
   *)
      logMessage "The parameter package '$parameterPackage' specified the '$subvariant' subvariant but it was not recognized so no subvariant parameters will be used."
      ;;
   esac
   ;;
*)
   fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
   ;;
esac