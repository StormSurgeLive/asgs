#!/bin/bash
#----------------------------------------------------------------
# mesh_defaults.sh : Functions required for initializing
# parameters that are mesh dependent.
#----------------------------------------------------------------
# Copyright(C) 2019--2024 Jason Fleming
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
THIS=$(basename -- $0)
source $SCRIPTDIR/monitoring/logging.sh
# mesh
MESHURL=https://asgs-static-assets.sfo2.digitaloceanspaces.com/meshes
NODALATTRIBUTESURL=https://asgs-static-assets.sfo2.digitaloceanspaces.com/nodal-attributes
OFFSETURL=https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets
UNITOFFSETFILE=null
LOADTIDEURL=https://asgs-static-assets.sfo2.digitaloceanspaces.com/tides
#
# The following is common to all meshes and is only used
# for SWAN (WAVES=on); may not give good numerical results
# with ADCIRC+SWAN v52; not clear whether it is correctly
# formatted with ADCIRC+SWAN v55 yet.
SWANTEMPLATE=adcirc_swan_v53_fort.26.template # found in input/meshes/common/swan
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
#
# Self Attraction/Earth Load Tide Forcing File (fort.24)
# The Not all meshes will have one of these available, although the global meshes do.
# This file is required if NTIP=2 (tidal potential parameter) in fort.15
selfAttractionEarthLoadTide="notprovided"
#
# wind at 10m fort.15 template
# if this is set to a file name (rather than null) then
# it will be used to generate the wind10m layer; this requires
# the Operator to set up a specific scenario to generate the
# Wind10m layer; the wind10m layer will not be produced automatically
CONTROLTEMPLATENOROUGH="null"
#
#
case $GRIDNAME in
   "CPRA23v05b")
      #
      nodes=1577268
      elements=3072131
      INPUTDIR=$SCRIPTDIR/input/meshes/CPRA23
      GRIDFILE=cpra_2023_hurricane_v05b_chk.grd  # mesh (fort.14) file
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
         ;;
      "default")
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
         nodal_attribute_default_values["surface_submergence_state"]="0.0"
         nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
         nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
         nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.022"
         nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="20.0"
         nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.362102"
         nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
         nodal_attribute_default_values["elemental_slope_limiter"]="0.05"
         ;;
      *)
         fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
         ;;
      esac
      ;;
   "AGT")
      # adcirc-global-test mesh, from ADCIRC test suite repository
      # https://github.com/adcirc/adcirc-testsuite
      nodes=27330
      elements=50859
      INPUTDIR=${SCRIPTDIR}/input/meshes/AGT # mesh and other input files
      GRIDFILE=agt.14
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=m2-agt.fort.15.template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=$CONTROLTEMPLATE # no wind roughness in AGT fort.13
      ELEVSTATIONS=elev_stations_agt.txt
      VELSTATIONS=$ELEVSTATIONS
      METSTATIONS=$ELEVSTATIONS
      NAFILE=agt.13
      # interaction between mesh and models:
      TIMESTEPSIZE=720.0          # adcirc time step size (seconds)
      zNorth="112.8516 40.3289  ! China-Argentina"
      selfAttractionEarthLoadTide="m2-agt.fort.24"
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="01:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="01:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="01:00:00" # forecast wall clock time
      ;;
   "LA_v19k-WithUpperAtch_chk")
      nodes=1593485
      elements=3102441
      INPUTDIR=$SCRIPTDIR/input/meshes/LA_v19k
      GRIDFILE=LA_v19k-WithUpperAtch_chk.grd   # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=LA_v19k-WithUpperAtch.15.template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=LA_v19k-WithUpperAtch.nowindreduction.15.template
      ELEVSTATIONS=combined_stations_20190327.txt
      VELSTATIONS=combined_stations_20190327.txt
      METSTATIONS=combined_stations_20190327.txt
      NAFILE=LA_v19k-WithUpperAtch_chk.13
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      # unit offset url https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_LA_v19k-WithUpperAtch_chk.dat.xz
      UNITOFFSETFILE=unit_offset_LA_v19k-WithUpperAtch_chk.dat
      ;;
      #
   "LA_v20a-WithUpperAtch_chk"|"LAv20a")
      #
      nodes=1593485
      elements=3102441
      INPUTDIR=$SCRIPTDIR/input/meshes/LA_v20a
      GRIDFILE=LA_v20a-WithUpperAtch_chk.grd   # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      ELEVSTATIONS=combined_stations_20200929.txt
      VELSTATIONS=$ELEVSTATIONS
      METSTATIONS=$ELEVSTATIONS
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      case $parameterPackage in
      "hardcoded")
         CONTROLTEMPLATE=LA_v20a-WithUpperAtch.15.template
         # wind at 10m fort.15 template
         CONTROLTEMPLATENOROUGH=LA_v20a-WithUpperAtch.nowindreduction.15.template
         NAFILE=LA_v20a-WithUpperAtch_chk.13
         nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.228184"
         ;;
      "default")
         CONTROLTEMPLATE=LA_v20a-WithUpperAtch.15.ASGS2024.1.template
         # numerics/physics (fort.15)
         advection="off"                       # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
         solver_time_integration="implicit"    # implicit|explicit|full-gravity-wave-implicit
         time_weighting_coefficients="0.35 0.3 0.35" # A00 B00 C00 in fort.15
         lateral_turbulence="eddy_viscosity"   # "smagorinsky" or "eddy_viscosity"
         eddy_viscosity_coefficient="50.0"     # ESLM
         bottom_friction_limit=0.0             # min when using Manning's n (CF/FFACTOR)
         h0=0.1                                # min depth (m) to be considered wet
         velmin=0.01
         nodal_attribute_activate=( surface_directional_effective_roughness_length mannings_n_at_sea_floor )
         nodal_attribute_activate+=( surface_canopy_coefficient primitive_weighting_in_continuity_equation )
         nodal_attribute_activate+=( elemental_slope_limiter average_horizontal_eddy_viscosity_in_sea_water_wrt_depth )
         nodal_attribute_activate+=( surface_submergence_state sea_surface_height_above_geoid )
         # tidal forcing
         tidalConstituents=( "q1" "o1" "p1" "k1" "n2" "m2" "s2" "k2" )
         # river boundary forcing
         PERIODICFLUX=$INPUTDIR/LAv20a_10kcms.txt
         # nodal attributes file
         NAFILE=LA_v20a-WithUpperAtch_chk.13.template
         nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.228184"
         nodal_attribute_default_values["surface_submergence_state"]="0.0"
         nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
         nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="20.0"
         nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
         nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
         nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.022"
         nodal_attribute_default_values["elemental_slope_limiter"]="0.05"
         ;;
      *)
         fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
         ;;
      esac
      ;;
   "LAERDCv5k")
      #
      # This is the successor to the LAv20a mesh (Louisiana) for 2022.
      # v5j is the version number (they elected not to use the year in
      # the version number unfortunately). Not sure why ERDC is in
      # the file name, that's how we received it.
      # This mesh requires ADCIRC v55.02.
      nodes=3085851
      elements=6050219
      INPUTDIR=$SCRIPTDIR/input/meshes/LAERDC
      GRIDFILE=LA_ERDC_v05j_chk.grd  # the j mesh goes with the k nodal attributes file
      MESHPROPERTIES=${GRIDFILE}.properties
      ELEVSTATIONS=combined_stations_20200929.txt
      VELSTATIONS=$ELEVSTATIONS
      METSTATIONS=$ELEVSTATIONS
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      case $parameterPackage in
      "hardcoded")
         CONTROLTEMPLATE=LAERDCv5i_10.194kcms.15.template
         # wind at 10m fort.15 template
         CONTROLTEMPLATENOROUGH=LAERDCv5i_10.194kcms.norough.15.template
         NAFILE=LA_ERDC_v05k_chk.13
         nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.228184"
         ;;
      "default")
         CONTROLTEMPLATE=LAERDCv5.15.ASGS2024.1.template
         # numerics/physics (fort.15)
         advection="on"                        # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
         solver_time_integration="explicit"    # implicit|explicit|full-gravity-wave-implicit
         time_weighting_coefficients="0.0 1.0 0.0" # A00 B00 C00 in fort.15
         lateral_turbulence="eddy_viscosity"   # "smagorinsky" or "eddy_viscosity"
         smagorinsky_coefficient="0.2"
         bottom_friction_limit=0.001           # min when using Manning's n (CF/FFACTOR)
         h0=0.1                                # min depth (m) to be considered wet
         velmin=0.01
         nodal_attribute_activate=( sea_surface_height_above_geoid )
         nodal_attribute_activate+=( elemental_slope_limiter )
         nodal_attribute_activate+=( surface_directional_effective_roughness_length )
         nodal_attribute_activate+=( surface_canopy_coefficient )
         nodal_attribute_activate+=( mannings_n_at_sea_floor )
         nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
         nodal_attribute_activate+=( surface_submergence_state )
         # tidal forcing
         tidalConstituents=( "q1" "o1" "p1" "k1" "n2" "m2" "s2" "k2" )
         # river boundary forcing
         PERIODICFLUX=$INPUTDIR/LAERDC_default_river_flux.txt
         # nodal attributes file
         NAFILE=cpra_2023_hurricane_v05b.13.template
         nodal_attribute_default_values["surface_submergence_state"]="0.0"
         nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
         nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
         nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.022"
         nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="20.0"
         nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.362102"
         nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
         nodal_attribute_default_values["elemental_slope_limiter"]="0.05"
         ;;
      *)
         fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
         ;;
      esac

      ;;
      #
   "ec95d"|"EC95d")
      #
      nodes=31435
      elements=58369
      INPUTDIR=$SCRIPTDIR/input/meshes/ec95d
      GRIDFILE=ec_95d.grd   # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=ec_95_fort.15_template   # fort.15 template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=ec_95_nowindreduction.fort.15_template
      ELEVSTATIONS=cera_stations.txt
      VELSTATIONS=cera_stations.txt
      METSTATIONS=cera_stations.txt
      NAFILE=null
      # interaction between mesh and models:
      TIMESTEPSIZE=30.0           # adcirc time step size (seconds)
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="06:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="01:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="01:00:00" # forecast wall clock time
      # FIXME: no unit offset url
      ;;
      #
   "tx2008_r35h"|"TX2008")
      #
      nodes=3352598
      elements=6675517
      INPUTDIR=$SCRIPTDIR/input/meshes/texas2008_r35h
      GRIDFILE=tx2008_r35h.grd # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=tx2008r35h_template.15   # fort.15 template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=tx2008r35h_norough_template.15
      ELEVSTATIONS=tx2008r35h_stations_20170618.txt
      VELSTATIONS=tx2008r35h_stations_20170618.txt
      METSTATIONS=tx2008r35h_stations_20170618.txt
      NAFILE=tx2008_r35h.13
      nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2763"
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      # unit offset url https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_tx2008_r35h.grd.dat.xz
      UNITOFFSETFILE=unit_offset_tx2008_r35h.grd.dat
      ;;
      #
   "tx2017"|"CTXCS2017")
      #
      nodes=4535035
      elements=8975076
      INPUTDIR=$SCRIPTDIR/input/meshes/tx2017
      GRIDFILE=ctx_gr_p01E01.grd # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=tx2017_template.15   # fort.15 template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=tx2017_norough_template.15
      ELEVSTATIONS=tx2008r35h_stations_20170618.txt
      VELSTATIONS=tx2008r35h_stations_20170618.txt
      METSTATIONS=tx2008r35h_stations_20170618.txt
      NAFILE=ctx_gr_p01E02_na_p02_fort.13
      nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2347"
      # interaction between mesh and models:
      TIMESTEPSIZE=0.5           # adcirc time step size (seconds)
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="10:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="10:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="14:00:00" # forecast wall clock time
      # FIXME: no unit offset url
      ;;
      #
   "tx2020a"|"TX2020a")
      #
      nodes=4266444
      elements=8456596
      INPUTDIR=$SCRIPTDIR/input/meshes/tx2020
      GRIDFILE=tx2020a.14 # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=tx2020a_esl_template.15   # fort.15 template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=tx2020a_norough_template.15
      ELEVSTATIONS=tx2008r35h_stations_20170618.txt
      VELSTATIONS=tx2008r35h_stations_20170618.txt
      METSTATIONS=tx2008r35h_stations_20170618.txt
      NAFILE=tx2020a.13
      nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2347"
      # interaction between mesh and models:
      TIMESTEPSIZE=0.5           # adcirc time step size (seconds)
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="10:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="14:00:00" # forecast wall clock time
      # FIXME: no unit offset url
      ;;
      #
   "TXLA22a")
      #
      nodes=1947485
      elements=3832707
      INPUTDIR=$SCRIPTDIR/input/meshes/TXLA22a
      GRIDFILE=TXLA22a.14          # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=TXLA22a_fort.15.template  # fort.15 template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=$CONTROLTEMPLATE
      ELEVSTATIONS=tx2008r35h_stations_20170618.txt
      VELSTATIONS=tx2008r35h_stations_20170618.txt
      METSTATIONS=tx2008r35h_stations_20170618.txt
      NAFILE=TXLA22a.13
      nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2347"
      # interaction between mesh and models:
      TIMESTEPSIZE=2.0           # adcirc time step size (seconds)
      # intersection between mesh, models, hpc platform, and n compute cores:
      HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="10:00:00"  # adcprep wall clock time,including partmesh
      NOWCASTWALLTIME="10:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="14:00:00" # forecast wall clock time
      ;;
      #
   "neflga_v12_geo"|"NEFLGAv12"|"NEFLGAv12b")
      nodes=2968735
      elements=5910443
      INPUTDIR=$SCRIPTDIR/input/meshes/neflga
      GRIDFILE=neflga_v12_geo.14 # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      if [[ $GRIDNAME = "NEFLGAv12b" ]];  then
         GRIDFILE=neflga_v12b_geo.14 # mesh (fort.14) file
      fi
      CONTROLTEMPLATE=neflga_v12_geo_template.15   # fort.15 template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=neflga_v12_geo_norough_template.15
      ELEVSTATIONS=neflga_stations_20190809.txt
      VELSTATIONS=${ELEVSTATIONS}
      METSTATIONS=${ELEVSTATIONS}
      NAFILE=neflga_v12.13
      nodal_attribute_default_values["sea_surface_height_above_geoid"]="-0.003"
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      # FIXME: no unit offset url
      ;;
      #
   "NCv6d")
      #
      nodes=295328
      elements=575512
      INPUTDIR=$SCRIPTDIR/input/meshes/nc_v6b
      GRIDFILE=nc_inundation_v6d_rivers_msl.grd
      MESHPROPERTIES=${GRIDFILE}.properties
      ELEVSTATIONS=v6brivers_elev_stations.txt
      VELSTATIONS=${ELEVSTATIONS}
      METSTATIONS=v6brivers_met_stations.txt
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      case $parameterPackage in
      "hardcoded"|"default")
         # we don't have a hardcoded template set up for NCv6d
         CONTROLTEMPLATE=NCv6d_2024.fort.15.template
         # tidal forcing
         tidalConstituents=( "m2" "s2" "n2" "k1" "k2" "o1" "q1" "p1" )
         # river boundary forcing
         RIVERFLUX=v6brivers_fort.20_default
         HINDCASTRIVERFLUX=v6brivers_fort.20_hc_default
         VARFLUX=default             # mesh has aperiodic flux boundary
         # numerics/physics
         TIMESTEPSIZE=0.5                      # adcirc time step size (seconds)
         advection="on"                        # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
         solver_time_integration="explicit"    # implicit|explicit|full-gravity-wave-implicit
         time_weighting_coefficients="0.0 1.0 0.0" # A00 B00 C00 in fort.15
         lateral_turbulence="eddy_viscosity"   # "smagorinsky" or "eddy_viscosity"
         eddy_viscosity_coefficient="10.0"     # ESLM
         bottom_friction_limit=0.003           # min when using Manning's n (CF/FFACTOR)
         h0=0.02                               # min depth (m) to be considered wet
         velmin=0.02
         # nodal attributes
         NAFILE=v6brivers_rlevel.13.template
         nodal_attribute_activate=( surface_directional_effective_roughness_length mannings_n_at_sea_floor )
         nodal_attribute_activate+=( surface_canopy_coefficient primitive_weighting_in_continuity_equation )
         nodal_attribute_activate+=( average_horizontal_eddy_viscosity_in_sea_water_wrt_depth )
         nodal_attribute_activate+=( initial_river_elevation )
         nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.0"
         nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
         nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="10.0"
         nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
         nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
         nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.02"
         nodal_attribute_default_values["initial_river_elevation"]="-99999"
         ;;
      *)
         fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
         ;;
         esac
      ;;
      #
   "nc_inundation_v9.99_w_rivers"|"NCv999")
      #
      nodes=624782
      elements=1234231
      INPUTDIR=$SCRIPTDIR/input/meshes/nc_v9.99_w_rivers
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
      "default")
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
      ;;
   "uriv18"|"URIv18")
      nodes=2617704
      elements=5149248
      INPUTDIR=$SCRIPTDIR/input/meshes/uriv18
      GRIDFILE=hsofs_NE-hires_v18_weir_rivers_depsm2_nopump.grd
      MESHPROPERTIES=${GRIDFILE}.nc.properties
      CONTROLTEMPLATE=fort.15.template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=norough_fort.15.template
      ELEVSTATIONS=elev_stations.txt
      VELSTATIONS=vel_stations.txt
      METSTATIONS=met_stations.txt
      NAFILE=hsofs_NE-hires_v18_weir_rivers_depsm2.13
      # interaction between mesh and models:
      TIMESTEPSIZE=0.5            # adcirc time step size (seconds)
      SWANDT=1800                 # swan timestep / coupling interval (seconds)
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      # unit offset url https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_hsofs.dat.xz
      UNITOFFSETFILE=unit_offset_hsofs.dat
      ;;
   "hsofs"|"HSOFS")
      #
      nodes=1813443
      elements=3564104
      INPUTDIR=$SCRIPTDIR/input/meshes/hsofs
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
      # unit offset url https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_hsofs.dat.xz
      UNITOFFSETFILE=unit_offset_hsofs.dat
      case $parameterPackage in
      "hardcoded")
         CONTROLTEMPLATE=hsofs_explicit.15.template
         # wind at 10m fort.15 template
         CONTROLTEMPLATENOROUGH=hsofs.nowindreduction.15.template
         NAFILE=hsofs.13
         # interaction between mesh and models:
         SWANDT=1800                 # swan timestep / coupling interval (seconds)
         ;;
      "default")
         CONTROLTEMPLATE=hsofs-parameters.15.template
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
         NAFILE=hsofs-parameters.13
         nodal_attribute_activate=( mannings_n_at_sea_floor )
         nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
         nodal_attribute_activate+=( surface_canopy_coefficient )
         nodal_attribute_activate+=( surface_directional_effective_roughness_length  )
         nodal_attribute_activate+=( surface_submergence_state )
         nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.0"
         nodal_attribute_default_values["primitive_weighting_in_continuity_equation"]="0.03"
         nodal_attribute_default_values["average_horizontal_eddy_viscosity_in_sea_water_wrt_depth"]="10.0"
         nodal_attribute_default_values["surface_canopy_coefficient"]="1.0"
         nodal_attribute_default_values["surface_directional_effective_roughness_length"]="0.0  0.0  0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0"
         nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.02"
         ;;
      *)
         fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
         ;;
      esac
      ;;
   "SABv20a")
      #
      nodes=5584241
      elements=11066018
      INPUTDIR=$SCRIPTDIR/input/meshes/SABv20a
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
      "default")
         CONTROLTEMPLATE=SABv20a.15.ASGS2024.1.template
         # numerics/physics (fort.15)
         TIMESTEPSIZE=0.5                      # adcirc time step size (seconds)
         advection="off"                       # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
         solver_time_integration="explicit"    # implicit|explicit|full-gravity-wave-implicit
         time_weighting_coefficients="0.0 1.0 0.0" # A00 B00 C00 in fort.15
         lateral_turbulence="eddy_viscosity"   # "smagorinsky" or "eddy_viscosity"
         eddy_viscosity_coefficient="50.0"     # ESLM
         bottom_friction_limit=0.0             # min when using Manning's n (CF/FFACTOR)
         h0=0.1                                # min depth (m) to be considered wet
         velmin=0.01
         nodal_attribute_activate=( average_horizontal_eddy_viscosity_in_sea_water_wrt_depth )
         nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
         nodal_attribute_activate+=( mannings_n_at_sea_floor )
         nodal_attribute_activate+=( elemental_slope_limiter )
         nodal_attribute_activate+=( surface_canopy_coefficient )
         nodal_attribute_activate+=( surface_directional_effective_roughness_length )
         nodal_attribute_activate+=( advection_state )
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
         metControl["WindDragLimit"]="0.002"  # max wind drag coefficient, unitless
         ;;
      *)
         fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
         ;;
      esac
      ;;
      #
   "WFLv18")
      #
      nodes=1147947
      elements=2283949
      INPUTDIR=$SCRIPTDIR/input/meshes/WFLv18
      GRIDFILE=fema_wfl_fort.14  # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.nc.properties
      CONTROLTEMPLATE=fema_wfl_fort.15.template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=fema_wfl_nowindreduction.fort.15.template
      h0=0.05
      ELEVSTATIONS=fema_wfl_stations_20191114.txt
      VELSTATIONS=$ELEVSTATIONS
      METSTATIONS=$ELEVSTATIONS
      NAFILE=fema_wfl_fort.13
      SWANDT=1800                 # swan timestep / coupling interval (seconds)
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      # unit offset url
      UNITOFFSETFILE=null
      ;;
      #
   "southfl_v11-1_final"|"SFLv111")
      #
      nodes=2249093
      elements=4480230
      INPUTDIR=$SCRIPTDIR/input/meshes/southfl
      GRIDFILE=southfl_v11-1_final.grd
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=southfl-v11-1.template.15
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=southfl-v11-1.nowindreduction.template.15
      ELEVSTATIONS=southfl_stations_20190502.txt
      VELSTATIONS=southfl_stations_20190502.txt
      METSTATIONS=southfl_stations_20190502.txt
      NAFILE=southfl_v11-1_final-production.13
      nodal_attribute_default_values["sea_surface_height_above_geoid"]="-0.155"
      HINDCASTRIVERFLUX=null
      # interaction between mesh and models:
      TIMESTEPSIZE=0.5            # adcirc time step size (seconds)
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      # FIXME: no unit offset url
      ;;
      #
   "CenFlv7"|"eccl_v7_geo_z")
      #
      nodes=1406658
      elements=2793617
      INPUTDIR=$SCRIPTDIR/input/meshes/cenfl
      GRIDFILE=eccl_v7_geo_z.grd
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=cenfl.fort.15.template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=cenfl.norough.fort.15.template
      ELEVSTATIONS=cenfl_stations_20190809.txt
      VELSTATIONS=${ELEVSTATIONS}
      METSTATIONS=${ELEVSTATIONS}
      NAFILE=cenfl_v7.13
      nodal_attribute_default_values["sea_surface_height_above_geoid"]="-0.17"
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      # FIXME: no unit offset url
      ;;
      #
   "FEMAR3")
      #
      nodes=1875689
      elements=3731099
      INPUTDIR=$SCRIPTDIR/input/meshes/femar3
      GRIDFILE=FEMA_R3_20110303_MSL.grd
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=FEMA_R3_fort.15.template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=FEMA_R3_nowindreduction_fort.15.template
      ELEVSTATIONS=cera_stations_20180810.txt
      VELSTATIONS=${ELEVSTATIONS}
      METSTATIONS=${ELEVSTATIONS}
      NAFILE=FEMA_R3_20110303_MSL.13
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      # https://asgs-static-assets.sfo2.digitaloceanspaces.com/offsets/unit_offset_FEMA_R3_20110303_MSL.dat.xz
      UNITOFFSETFILE=unit_offset_FEMA_R3_20110303_MSL.dat
      ;;
      #
   "FEMAR2")
      #
      nodes=604790
      elements=1188640
      INPUTDIR=$SCRIPTDIR/input/meshes/femar2
      GRIDFILE=FEMA_R2_norivers_gcs_mNAVD.grd
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=FEMA_R2_fort.15.template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=FEMA_R2.noswanrefrac.nowindreduction.fort.15.template
      ELEVSTATIONS=cera_stations_20180810.txt
      VELSTATIONS=${ELEVSTATIONS}
      METSTATIONS=${ELEVSTATIONS}
      NAFILE=FEMA_R2_01262012_refrac_fort.13
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      # no scalable unit water level correction file
      ;;
      #
   "NAC2014")
      #
      nodes=3110470
      elements=6167588
      INPUTDIR=$SCRIPTDIR/input/meshes/naccs
      GRIDFILE=NAC2014_R01_ClosedRivers.grd
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=NAC2014_R01.15.template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=NAC2014_R01.nowindreduction.15.template
      ELEVSTATIONS=cera_stations_20180810.txt
      VELSTATIONS=${ELEVSTATIONS}
      METSTATIONS=${ELEVSTATIONS}
      NAFILE=NAC2014_R01.13
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      # no scalable unit water level correction file
      ;;
      #
   "NGOMv19b")
      #
      nodes=2051450
      elements=4065608
      INPUTDIR=${SCRIPTDIR}/input/meshes/NGOMv19b # grid and other input files
      GRIDFILE=NGOM_RT_v19b_chk.grd
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=NGOM_RT_v19b.15.template_18kcms   # fort.15 template
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=NGOM_RT_v19b.nowindreduction.15.template
      ELEVSTATIONS=NGOM_RT_v19b_stations_08282018.txt
      VELSTATIONS=NGOM_RT_v19b_stations_08282018.txt
      METSTATIONS=NGOM_RT_v19b_stations_08282018.txt
      NAFILE=NGOM_RT_v19b_chk.13
      nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.2"
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="10:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      UNITOFFSETFILE=oi_surface_NGOM_RT_v19b_chk.grd.dat
      ;;
      #
      #
   "EGOMv20b")
      #
      nodes=2219482
      elements=4398700
      INPUTDIR=${SCRIPTDIR}/input/meshes/EGOMv20b # grid and other input files
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
      "default")
         CONTROLTEMPLATE=EGOMv20b.15.ASGS2024.1.template
         # numerics/physics (fort.15)
         advection="on"                            # on|off for advection (NOLICA=1|0/NOLICAT=1|0)
         solver_time_integration="explicit"        # implicit|explicit|full-gravity-wave-implicit
         time_weighting_coefficients="0.0 1.0 0.0" # A00 B00 C00 in fort.15
         lateral_turbulence="smagorinsky"          # "smagorinsky" or "eddy_viscosity"
         eddy_viscosity_coefficient="20.0"         # ESLM
         smagorinsky_coefficient="0.05"
         bottom_friction_limit=0.001               # min when using Manning's n (CF/FFACTOR)
         h0=0.1                                    # min depth (m) to be considered wet
         velmin=0.01
         nodal_attribute_activate=( sea_surface_height_above_geoid )
         nodal_attribute_activate+=( advection_state )
         nodal_attribute_activate+=( mannings_n_at_sea_floor )
         nodal_attribute_activate+=( primitive_weighting_in_continuity_equation )
         nodal_attribute_activate+=( surface_canopy_coefficient )
         nodal_attribute_activate+=( surface_directional_effective_roughness_length )
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
         metControl["WindDragLimit"]="0.003"  # max wind drag coefficient, unitless
         ;;
      *)
         fatal "The parameter package '$parameterPackage' is not supported for the mesh '$GRIDNAME'."
         ;;
      esac
      ;;
      #
   "Shinnecock")
      #
      nodes=3070
      elements=5780
      INPUTDIR=${SCRIPTDIR}/input/meshes/shinnecock # grid and other input files
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
      "default")
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
      ;;
      #
   "ec2001_v2e"|"EC2001v2e")
      #
      nodes=254565
      elements=492179
      INPUTDIR=$SCRIPTDIR/input/meshes/EC2001
      GRIDFILE=ec2001_v2e.grd   # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=ec2001_v2e_fort.15.template   # fort.15 template (designed for 1s timestep)
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=$CONTROLTEMPLATE  # same b/c no inundation coverage
      ELEVSTATIONS=hsofs_stations_20180907.txt
      VELSTATIONS=$ELEVSTATIONS
      METSTATIONS=$ELEVSTATIONS
      NAFILE=ec2001_v2e.13
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="06:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="01:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="01:00:00" # forecast wall clock time
      UNITOFFSETFILE=null
      ;;
   "OPENWATERv1e")
      #
      nodes=616113
      elements=1198417
      INPUTDIR=$SCRIPTDIR/input/meshes/OPENWATER
      GRIDFILE=openwater.grd  # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=openwater.fort.15.template  # designed for 2s timestep (any adcirc version)
      TIMESTEPSIZE=2.0            # adcirc time step size (seconds)
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=${CONTROLTEMPLATE} # same b/c no inundation area
      ELEVSTATIONS=hsofs_stations_20180907.txt
      VELSTATIONS=$ELEVSTATIONS
      METSTATIONS=$ELEVSTATIONS
      NAFILE=openwater.13
      # interaction between mesh and models:
      TIMESTEPSIZE=2.0            # adcirc time step size (seconds)
      SWANDT=1800                 # swan timestep / coupling interval (seconds)
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      UNITOFFSETFILE=null
      ;;
   "PRVI15")
      #
      nodes=2733258
      elements=5392748
      INPUTDIR=$SCRIPTDIR/input/meshes/PRVI
      GRIDFILE=PRVI15_fort.14  # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=PRVI15_fort.15.template  # designed for 2s timestep (any adcirc version)
      h0=0.05
      # wind at 10m fort.15 template
      CONTROLTEMPLATENOROUGH=PRVI15_norough_fort.15.template
      ELEVSTATIONS=hsofs_stations_20180907.txt
      VELSTATIONS=$ELEVSTATIONS
      METSTATIONS=$ELEVSTATIONS
      NAFILE=PRVI15_fort.13
      # interaction between mesh and models:
      SWANDT=600                 # swan timestep / coupling interval (seconds)
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="24:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      UNITOFFSETFILE=null
      ;;

   *)
      LOCAL_MESH_DEFAULTS="${ASGS_LOCAL_DIR}/config/mesh_defaults.sh"
      if [[ -n "$ASGS_LOCAL_DIR" && -e "$LOCAL_MESH_DEFAULTS" ]]; then
        consoleMessage "'$GRIDNAME' not found in officially supported mesh list,"
        consoleMessage "... looking in '$LOCAL_MESH_DEFAULTS'"
        source "${LOCAL_MESH_DEFAULTS}"
        if [[ -n "$nodes" ]]; then
          consoleMessage "... found '$GRIDNAME', nodes: $nodes, elements: $elements"
        fi
      else
        warn "cycle $CYCLE: $SCENARIO: $THIS: Mesh GRIDNAME $GRIDNAME was not recognized."
      fi
      # final sanity check
      if [[ "$nodes" -le 0 ]]; then
        consoleMessage "cycle $CYCLE: $SCENARIO: $THIS: Mesh GRIDNAME $GRIDNAME was not recognized."
        exit 1
      fi
      ;;
esac
