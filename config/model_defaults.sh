#!/bin/bash
#----------------------------------------------------------------
#
# model_defaults.sh: This script provides the default
# physics parameters (and related output controls) for the models
# being driven by the ASGS.
#
#----------------------------------------------------------------
# Copyright(C) 2014--2024 Jason Fleming
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
# ADCIRC parameters (fort.15) file
parameterPackage="hardcoded"  # use old (mostly) hardcoded fort.15 template and static nodal attributes
controlParametersTemplate=$SCRIPTDIR/control-parameters-template.yaml
adcircVersions=( "notset" )
CONTROLTEMPLATENOROUGH="null"
TIMESTEPSIZE="1.0"            # ADCIRC time step (DTDP) in seconds
metOnlyTimeStepSize="300.0"   # ADCIRC time step (DTDP) for meteorology-only mode
WTIMINC=900                   # time increment of meteorological data sets in seconds
advection="on"                # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
#
# For ADCIRC versions v55.01 and later, rotated spherical coordinate
# systems are available that are useful for global meshes to place the
# poles on land to avoid numerical distortion. The use of this capability
# and the specification of the coordinates of the north pole are controlled
# via the zNorth parameter and triggered by a negative value of ICS in the
# fort.15 file. The north pole location $zNorth is written to a fort.rotm file.
# Options include the fcollowing:
# zNorth="northpole"         ! no coordinate system rotation
# zNorth="-42.8906  72.3200  ! Greenland-Antarctica"
# zNorth="112.8516  40.3289  ! China-Argentina"
# zNorth="114.16991  0.77432 ! Borneo-Brazil"
# Coordinate rotation reference:     https://wiki.adcirc.org/Fort.rotm
# Model coordinate system reference: https://wiki.adcirc.org/ICS
zNorth="northpole"
declare -g -A coordinateSystem
coordinateSystem["projection"]="geographic" # cartesian|geographic
coordinateSystem["reprojection"]="CPP"      # CPP|equal-area|merator|miller|gall-stereographic
coordinateSystem["earthCurvature"]="no"     # no|yes
coordinateSystem["rotation"]="northpole"    # greenland-antarctica|china-argentina|borneo-brazil
#
solver_time_integration="implicit"          # implicit|explicit|full-gravity-wave-implicit
# A00 B00 C00 in fort.15, valid value sets as follows:
# "0.35 0.30  0.35"  ! implicit time stepping, oldest and most used values
# "0.00 1.00  0.00"  ! explicit time stepping
# "0.50 0.50  0.00"  ! implicit gravity wave enabled
# "0.80 0.20  0.00"  ! implicit gravity wave enabled
time_weighting_coefficients="0.35 0.3 0.35" # A00 B00 C00 in fort.15
lateral_turbulence="eddy_viscosity"         # "smagorinsky" or "eddy_viscosity"
    eddy_viscosity_coefficient="50.0"       # ESLM
    smagorinsky_coefficient="0.2"           # smagorinsky coef
# smagorinsky controls
declare -g -A Smag_Control
Smag_Control["smag_comp_flag"]="off"
Smag_Control["smag_upper_lim"]=100.0
Smag_Control["smag_lower_lim"]="1.0e-8"
#
h0=0.1                        # min depth (m) to be considered wet
velmin=0.1                    # min pseudovelocity (m/s) from wet to dry to change state
bottom_friction_limit=0.001   # min bottom friction when using Manning's n (CF/FFACTOR)
#
# nodal attributes listed in fort.15 file
declare -g -a nodal_attribute_activate
nodal_attribute_activate=( )
# possible list elements include
#    primitive_weighting_in_continuity_equation
#    surface_submergence_state
#    surface_directional_effective_roughness_length
#    overland_reduction_factor
#    surface_canopy_coefficient
#    mannings_n_at_sea_floor
#    sea_surface_height_above_geoid
#    average_horizontal_eddy_viscosity_in_sea_water_wrt_depth
#    elemental_slope_limiter
#    advection_state
#    initial_river_elevation
#    internal_tide_friction
#    subgrid_barrier
# e.g.: nodal_attribute_activate=( "sea_surface_height_above_geoid" "mannings_n_at_sea_floor" )
#
# &metControl WindDragLimit=floatValue, DragLawString='stringValue', rhoAir=floatValue, outputWindDrag=logicalValue /
declare -g -A metControl
metControl["WindDragLimit"]="0.0025"  # max wind drag coefficient, unitless
metControl["DragLawString"]="garratt" # "garratt" or "powell"
metControl["outputWindDrag"]="no"     # "yes" or "no" to write fulldomain time varying wind drag coefficient
metControl["rhoAir"]="1.293"          # kg/m^3, not often modified
metControl["invertedBarometerOnElevationBoundary"]="no" # yes|no to include inverse barometer effect on boundary
metControl["nPowellSearchDomains"]="-1"                 # default to searching all domains for min pressure (v55release or later)
#
# &wetDryControl outputNodeCode=logicalValue, outputNOFF=logicalValue, noffActive=logicalValue /
declare -g -A wetDryControl
# available in v53release and later
wetDryControl["outputNodeCode"]="no"  # yes|no to write out fulldomain time varying integer node wet/dry state
wetDryControl["outputNOFF"]="no"      # yes|no to write out fulldomain time varying integer element wet/dry state
wetDryControl["noffActive"]="on"      # on|off to use element wet/dry state in calculations
# available starting in v55release
wetDryControl["StatPartWetFix"]="off" # on|off to use nearby node in elements with less than 3 wet nodes
wetDryControl["How2FixStatPartWet"]=0 # 0: use nearest neighbor if wet and H > 0.8H0, 1: use nearest neighbor if wet regardless if H > 0.8H0
# available starting in v56.0.3
wetDryControl["slim"]=1.d9            # value of slope limiter for wet/dry
wetDryControl["windlim"]="off"        # on|off to limit wind stress calculations in shallow water
wetDryControl["directvelWD"]="off"    # on|off to apply direct velocity calculation in wetting
wetDryControl["useHF"]="off"          # on|off to use high friction in shallow inundated areas
#
# &inundationOutputControl inundationOutput=logicalValue, inunThresh =floatValue /
declare -g -A inundationOutputControl
inundationOutputControl["inundationOutput"]="yes" # yes|no to write extra fulldomain inundation data at end of execution
inundationOutputControl["inunThresh"]="0.6"       # inundation reference depth (m) used in inundation output calculations
#
# &SWANOutputControl SWAN_OutputTPS=logicalValue, SWAN_OutputTM01=logicalValue, SWAN_OutputHS=logicalValue, SWAN_OutputDIR=logicalValue, SWAN_OutputTMM10=logicalValue, SWAN_OutputTM02=logicalValue /
declare -g -A SWANOutputControl
SWANOutputControl["SWAN_OutputTPS"]="yes"
SWANOutputControl["SWAN_OutputTM01"]="yes"
SWANOutputControl["SWAN_OutputHS"]="yes"
SWANOutputControl["SWAN_OutputDIR"]="yes"
SWANOutputControl["SWAN_OutputTMM10"]="yes"
SWANOutputControl["SWAN_OutputTM02"]="yes"
#
# netCDF metadata at or near the bottom of the fort.15 file
declare -g -A netcdf_metadata
netcdf_metadata["NCPROJ"]="ASGS"                      # project title
netcdf_metadata["NCINST"]="Seahorse Consulting"       # institution
netcdf_metadata["NCSOUR"]="ADCIRC"                    # source (model, instrument type)
netcdf_metadata["NCHIST"]="ASGS Workflow"             # history (audit trail of processing operations)
netcdf_metadata["NCREF"]="https://doi.org/10.1061/40990(324)48"   # reference (publications, URLs)
netcdf_metadata["NCCOM"]="Trusted since 2006."        # comments
netcdf_metadata["NCHOST"]="www.seahorsecoastal.com"   # host
netcdf_metadata["NCCONV"]="CF"                        # conventions
netcdf_metadata["NCCONT"]="jason.fleming@adcirc.live" # contact information
# strongly suggest NCDATE be hardcoded to "%CSYEAR%-%CSMONTH%-%CSDAY% %CSHOUR%:00:00"
# in the control file (fort.15) template
netcdf_metadata["NCDATE"]="2010-05-01 00:00:00 UTC"   # cold start date and time (with time zone)
#
# ADCIRC nodal attributes (fort.13) file
declare -g -A nodal_attribute_default_values
nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
nodal_attribute_default_values["surface_submergence_state"]="1"
nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
nodal_attribute_default_values["overland_reduction_factor"]="0.0333"
nodal_attribute_default_values["surface_canopy_coefficient"]="1"
nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.02"
nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.0"
nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="10.0"
nodal_attribute_default_values["elemental_slope_limiter"]="0.05"
nodal_attribute_default_values["advection_state"]="-100.0"
nodal_attribute_default_values["initial_river_elevation"]="0.0"
nodal_attribute_default_values["internal_tide_friction"]="0.0  0.0  0.0"
nodal_attribute_default_values["subgrid_barrier"]="99999.0"
#
# SWAN parameters (fort.26) file
SWANDT=1200 # swan timestep / coupling interval (seconds)
declare -g -A swan
swan["MXITNS"]="20"   # max number of iterations per timestep
swan["NPNTS"]="95"    # percent of mesh vertices required to meet convergence criteria per timestep