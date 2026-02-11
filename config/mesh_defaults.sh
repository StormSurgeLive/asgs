#!/bin/bash
#----------------------------------------------------------------
# mesh_defaults.sh : Functions required for initializing
# parameters that are mesh dependent.
#----------------------------------------------------------------
# Copyright(C) 2019--2025 Jason Fleming
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
INPUTDIR="null"
#
# Initialization (tidal and/or river inflow) will be required
# for any mesh with elevation (tidal) or flux (river) boundary
# conditions
meshInitialization="on"
#
case $GRIDNAME in
   "CPRA23v05b"|"CPRA23v05c")
      INPUTDIR=$SCRIPTDIR/input/meshes/CPRA23
      ;;
   "CPRA24v04a"|"CPRA24v04b"|"CPRA24v04b2"|"CPRA24v04c")
      INPUTDIR=$SCRIPTDIR/input/meshes/CPRA24
      ;;
   "CPRA25v02a")
      INPUTDIR=$SCRIPTDIR/input/meshes/CPRA25
      ;;
   "AGT")
      # adcirc-global-test mesh, from ADCIRC test suite repository
      # https://github.com/adcirc/adcirc-testsuite
      INPUTDIR=$SCRIPTDIR/input/meshes/AGT
      ;;
   "LA_v19k-WithUpperAtch_chk")
      INPUTDIR=$SCRIPTDIR/input/meshes/LA_v19k
      ;;
   "LA_v20a-WithUpperAtch_chk"|"LAv20a")
      INPUTDIR=$SCRIPTDIR/input/meshes/LA_v20a
      ;;
   "LAERDCv5k")
      # This is the successor to the LAv20a mesh (Louisiana) for 2022.
      # v5j is the version number (they elected not to use the year in
      # the version number unfortunately). Not sure why ERDC is in
      # the file name, that's how we received it.
      # This mesh requires ADCIRC v55.02.
      INPUTDIR=$SCRIPTDIR/input/meshes/LAERDC
      ;;
   "ec95d"|"EC95d")
      INPUTDIR=$SCRIPTDIR/input/meshes/ec95d
      ;;
   "tx2008_r35h"|"TX2008")
      INPUTDIR=$SCRIPTDIR/input/meshes/texas2008_r35h
      ;;
   "tx2017"|"CTXCS2017")
      INPUTDIR=$SCRIPTDIR/input/meshes/tx2017
      ;;
   "tx2020a"|"TX2020a")
      INPUTDIR=$SCRIPTDIR/input/meshes/tx2020
      ;;
   "TXLA22a")
      INPUTDIR=$SCRIPTDIR/input/meshes/TXLA22a
      ;;
   "neflga_v12_geo"|"NEFLGAv12"|"NEFLGAv12b")
      INPUTDIR=$SCRIPTDIR/input/meshes/neflga
      ;;
   "NCv6d")
      INPUTDIR=$SCRIPTDIR/input/meshes/nc_v6b
      ;;
   "nc_inundation_v9.99_w_rivers"|"NCv999")
      INPUTDIR=$SCRIPTDIR/input/meshes/nc_v9.99_w_rivers
      ;;
   "uriv18"|"URIv18")
      INPUTDIR=$SCRIPTDIR/input/meshes/uriv18
      ;;
   "hsofs"|"HSOFS")
      INPUTDIR=$SCRIPTDIR/input/meshes/hsofs
      ;;
   "SABv20a")
      INPUTDIR=$SCRIPTDIR/input/meshes/SABv20a
      ;;
   "WFLv18")
      INPUTDIR=$SCRIPTDIR/input/meshes/WFLv18
      ;;
   "southfl_v11-1_final"|"SFLv111")
      INPUTDIR=$SCRIPTDIR/input/meshes/southfl
      ;;
   "CenFlv7"|"eccl_v7_geo_z")
      INPUTDIR=$SCRIPTDIR/input/meshes/cenfl
      ;;
   "FEMAR3")
      INPUTDIR=$SCRIPTDIR/input/meshes/femar3
      ;;
   "FEMAR2")
      INPUTDIR=$SCRIPTDIR/input/meshes/femar2
      ;;
   "NAC2014")
      INPUTDIR=$SCRIPTDIR/input/meshes/naccs
      ;;
   "NGOMv19b")
      INPUTDIR=$SCRIPTDIR/input/meshes/NGOMv19b
      ;;
   "EGOMv20b")
      INPUTDIR=$SCRIPTDIR/input/meshes/EGOMv20b
      ;;
   "Shinnecock")
      INPUTDIR=$SCRIPTDIR/input/meshes/shinnecock
      ;;
   "ec2001_v2e"|"EC2001v2e"|"EC2001")
      INPUTDIR=$SCRIPTDIR/input/meshes/EC2001
      ;;
   "OPENWATERv1e")
      INPUTDIR=$SCRIPTDIR/input/meshes/OPENWATER
      ;;
   "PRVI15")
      INPUTDIR=$SCRIPTDIR/input/meshes/PRVI15
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
#
# set the parameters
if [[ $INPUTDIR != "null" && -e $INPUTDIR/init.sh ]]; then
   source $INPUTDIR/init.sh
fi
