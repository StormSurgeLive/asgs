#!/bin/bash
#----------------------------------------------------------------
# mesh_defaults.sh : Functions required for initializing
# parameters that are mesh dependent.  
#----------------------------------------------------------------
# Copyright(C) 2019 Jason Fleming
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
THIS=config/mesh_defaults.sh
allMessage "$THIS: Setting default values for the mesh ${MESH}."
MESHURL=https://asgs-static-assets.sfo2.digitaloceanspaces.com/meshes
NODALATTRIBUTESURL=https://asgs-static-assets.sfo2.digitaloceanspaces.com/nodal-attributes
#
case $GRIDNAME in
   "LA_v19k-WithUpperAtch_chk")
      #
      INPUTDIR=$SCRIPTDIR/input/meshes/LA_v19k
      GRIDFILE=LA_v19k-WithUpperAtch_chk.grd   # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=LA_v19k-WithUpperAtch.15.template
      CONTROLTEMPLATENOROUGH=LA_v19k-WithUpperAtch.nowindreduction.15.template
      CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
      ELEVSTATIONS=combined_stations_20190327.txt
      VELSTATIONS=combined_stations_20190327.txt
      METSTATIONS=combined_stations_20190327.txt
      NAFILE=LA_v19k-WithUpperAtch_chk.13
      NAPROPERTIES=${NAFILE}.properties
      SWANTEMPLATE=LA_v19k-WithUpperAtch.26.template   # only used if WAVES=on
      RIVERINIT=null                           # this mesh has no rivers ...
      RIVERFLUX=null
      HINDCASTRIVERFLUX=null
      # interaction between mesh and models:
      TIMESTEPSIZE=1.0           # adcirc time step size (seconds)
      SWANDT=1200                # swan timestep / coupling interval (seconds)
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="18:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="02:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="07:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="07:00:00" # forecast wall clock time
      ;;
      #
   "ec95d")
      #   
      INPUTDIR=$SCRIPTDIR/input/meshes/ec95d
      GRIDFILE=ec_95d.grd   # mesh (fort.14) file
      MESHPROPERTIES=${GRIDFILE}.properties
      CONTROLTEMPLATE=ec_95_fort.15_template   # fort.15 template
      CONTROLTEMPLATENOROUGH=ec_95_nowindreduction.fort.15_template
      CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
      ELEVSTATIONS=cera_stations.txt
      VELSTATIONS=cera_stations.txt
      METSTATIONS=cera_stations.txt
      NAFILE=null
      NAPROPERTIES=${NAFILE}.properties
      SWANTEMPLATE=fort.26.nolimiter.template   # only used if WAVES=on
      RIVERINIT=null                            # this mesh has no rivers ...
      RIVERFLUX=null
      HINDCASTRIVERFLUX=null
      # interaction between mesh and models:
      TIMESTEPSIZE=30.0           # adcirc time step size (seconds)
      SWANDT=1200                # swan timestep / coupling interval (seconds)
      # intersection between mesh, models, hpc platform, and number of compute cores:
      HINDCASTWALLTIME="01:00:00" # hindcast wall clock time
      ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
      NOWCASTWALLTIME="01:00:00"  # longest nowcast wall clock time
      FORECASTWALLTIME="01:00:00" # forecast wall clock time
      ;;
   *)
      warn "cycle $CYCLE: $SCENARIO: $THIS: Mesh GRIDNAME $GRIDNAME was not recognized."
      ;;
esac
