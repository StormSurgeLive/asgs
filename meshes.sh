#!/bin/bash
#----------------------------------------------------------------
# meshes.sh: This file contains default parameters related
# to different meshes.  
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
#
GRIDNAME=$1
#
THIS=meshes.sh
#
asgsStaticAssetsBaseURL=https://asgs-static-assets.sfo2.digitaloceanspaces.com
meshURL=$asgsStaticAssetsBaseURL/meshes
nodalAttributesURL=$asgsStaticAssetsBaseURL/nodal-attributes
offsetsURL=$asgsStaticAssetsBaseURL/offsets
#
case $GRIDNAME in
  "hsofs") logMessage "meshes.sh: HSOFS configuration found."
    TIMESTEPSIZE=2.0
    GRIDFILE=hsofs.14  # mesh (fort.14) file
    MESHPROPERTIES=${GRIDFILE}.ng.properties
    CONTROLTEMPLATE=hsofs_explicit.15.template
    CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
    ELEVSTATIONS=hsofs_stations_20180907.txt
    VELSTATIONS=$ELEVSTATIONS
    METSTATIONS=$ELEVSTATIONS
    NAFILE=hsofs.13
    NAPROPERTIES=${NAFILE}.properties
    #SWANTEMPLATE=fort.26.template # only used if WAVES=on
    SWANTEMPLATE=fort.26.nolimiter.template # need to use this with ADCIRC+SWAN v53
    RIVERINIT=null                          # this mesh has no rivers ...
    RIVERFLUX=null
    HINDCASTRIVERFLUX=null
    ;;
  "tx2008_r35h") logMessage "meshes.sh: Texas 2008_r35h mesh configuration found."
    TIMESTEPSIZE=2.0
    GRIDFILE=tx2008_r35h.grd # mesh (fort.14) file
    GRIDNAME=tx2008_r35h
    MESHPROPERTIES=${GRIDFILE}.properties
    CONTROLTEMPLATE=tx2008r35h_template.15   # fort.15 template
    CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
    ELEVSTATIONS=tx2008r35h_stations_20170618.txt
    VELSTATIONS=tx2008r35h_stations_20170618.txt
    METSTATIONS=tx2008r35h_stations_20170618.txt
    NAFILE=tx2008_r35h.13
    NAPROPERTIES=${NAFILE}.properties
    SWANTEMPLATE=nolimiter.fort.26.ut.template    # only used if WAVES=on
    RIVERINIT=null                           # this mesh has no rivers ...
    RIVERFLUX=null
    HINDCASTRIVERFLUX=null
    ;;
    "ec95d") logMessage "meshes.sh: East Coast 95d mesh configuration found."
    TIMESTEPSIZE=30.0
    GRIDFILE=ec_95d.grd   # mesh (fort.14) file
    MESHPROPERTIES=${GRIDFILE}.properties
    CONTROLTEMPLATE=ec_95_fort.15_template   # fort.15 template
    CONTROLPROPERTIES=${CONTROLTEMPLATE}.properties
    ELEVSTATIONS=cera_stations.txt
    VELSTATIONS=cera_stations.txt
    METSTATIONS=cera_stations.txt
    NAFILE=null
    NAPROPERTIES=${NAFILE}.properties
    SWANTEMPLATE=fort.26.nolimiter.template   # only used if WAVES=on
    RIVERINIT=null                           # this mesh has no rivers ...
    RIVERFLUX=null
    HINDCASTRIVERFLUX=null
    ;;
esac
if [[ ! -e $SCRIPTDIR/input/meshes/$GRIDFILE ]]; then
    curl -O $meshURL/${GRIDFILE}.xz 
fi