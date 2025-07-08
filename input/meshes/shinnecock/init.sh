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
STDMESHNAME=Shinnecock
nodes=3070
elements=5780
GRIDFILE=shinnecock_inlet_coarse.grd
MESHPROPERTIES=${GRIDFILE}.properties
ELEVSTATIONS=shinnecock_stations.txt
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
TIMESTEPSIZE=6.0           # adcirc time step size (seconds)
# intersection between mesh, models, hpc platform, and number of compute cores:
HINDCASTWALLTIME="01:00:00" # hindcast wall clock time
ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
NOWCASTWALLTIME="01:00:00"  # longest nowcast wall clock time
FORECASTWALLTIME="01:00:00" # forecast wall clock time
case $parameterPackage in
"hardcoded")
    CONTROLTEMPLATE=shinnecock_asgs.fort.15.template
    # wind at 10m fort.15 template
    CONTROLTEMPLATENOROUGH=shinnecock_asgs.fort.15.template
    h0=0.05
    NAFILE="null"
    ;;
"2025.1"|"default")
    CONTROLTEMPLATE=shinnecock-parameters.fort.15.template
    NAFILE=shinnecock_nodal_attributes.template
    # default physics parameters (that differ from the settings in config/model_defaults.sh
    eddy_viscosity_coefficient="5.0"  # ESLM
    h0="0.05"                   # min depth (m) to be considered wet
    velmin="0.02"               # min pseudovelocity (m/s) from wet to dry to change state
    bottom_friction_limit="0.0025"  # min bottom friction when using Manning's n (CF/FFACTOR)
    metControl["WindDragLimit"]="0.0025"   # max wind drag coefficient, unitless
    # nodal attributes listed in fort.15 file
    nodal_attribute_activate=( "sea_surface_height_above_geoid" )
    # SWAN parameters (fort.26) template file
    SWANTEMPLATE=adcirc_swan_v53_parameters_fort.26.template # found in input/meshes/common/swan
    ;;
*)
    fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
    ;;
esac