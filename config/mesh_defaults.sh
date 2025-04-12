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
   "CPRA23v05b"|"CPRA23v05c")
      source $SCRIPTDIR/config/parameter_packages/CPRA23.sh
      ;;
   "CPRA24v04a"|"CPRA24v04b"|"CPRA24v04b2"|"CPRA24v04c")
      source $SCRIPTDIR/config/parameter_packages/CPRA24.sh
      ;;
   "AGT")
      # adcirc-global-test mesh, from ADCIRC test suite repository
      # https://github.com/adcirc/adcirc-testsuite
      source $SCRIPTDIR/config/parameter_packages/AGT.sh
      ;;
   "LA_v19k-WithUpperAtch_chk")
      source $SCRIPTDIR/config/parameter_packages/LAv19.sh
      ;;
   "LA_v20a-WithUpperAtch_chk"|"LAv20a")
      source $SCRIPTDIR/config/parameter_packages/LAv20.sh
      ;;
   "LAERDCv5k")
      # This is the successor to the LAv20a mesh (Louisiana) for 2022.
      # v5j is the version number (they elected not to use the year in
      # the version number unfortunately). Not sure why ERDC is in
      # the file name, that's how we received it.
      # This mesh requires ADCIRC v55.02.
      source $SCRIPTDIR/config/parameter_packages/LAERDCv5k.sh
      ;;
   "ec95d"|"EC95d")
      source $SCRIPTDIR/config/parameter_packages/EC95d.sh
      ;;
   "tx2008_r35h"|"TX2008")
      source $SCRIPTDIR/config/parameter_packages/TX2008.sh
      ;;
   "tx2017"|"CTXCS2017")
      source $SCRIPTDIR/config/parameter_packages/CTXCS2017.sh
      ;;
   "tx2020a"|"TX2020a")
      source $SCRIPTDIR/config/parameter_packages/TX2020a.sh
      ;;
   "TXLA22a")
      source $SCRIPTDIR/config/parameter_packages/TXLA22a.sh
      ;;
   "neflga_v12_geo"|"NEFLGAv12"|"NEFLGAv12b")
      source $SCRIPTDIR/config/parameter_packages/NEFLGAv12.sh
      ;;
   "NCv6d")
      source $SCRIPTDIR/config/parameter_packages/NCv6d.sh
      ;;
   "nc_inundation_v9.99_w_rivers"|"NCv999")
      source $SCRIPTDIR/config/parameter_packages/NCv999.sh
      ;;
   "uriv18"|"URIv18")
      source $SCRIPTDIR/config/parameter_packages/URIv18.sh
      ;;
   "hsofs"|"HSOFS")
      source $SCRIPTDIR/config/parameter_packages/HSOFS.sh
      ;;
   "SABv20a")
      source $SCRIPTDIR/config/parameter_packages/SABv20a.sh
      ;;
   "WFLv18")
      source $SCRIPTDIR/config/parameter_packages/WFLv18.sh
      ;;
   "southfl_v11-1_final"|"SFLv111")
      source $SCRIPTDIR/config/parameter_packages/SFLv111.sh
      ;;
   "CenFlv7"|"eccl_v7_geo_z")
      source $SCRIPTDIR/config/parameter_packages/CenFlv7.sh
      ;;
   "FEMAR3")
      source $SCRIPTDIR/config/parameter_packages/FEMAR3.sh
      ;;
   "FEMAR2")
      source $SCRIPTDIR/config/parameter_packages/FEMAR2.sh
      ;;
   "NAC2014")
      source $SCRIPTDIR/config/parameter_packages/NAC2014.sh
      ;;
   "NGOMv19b")
      source $SCRIPTDIR/config/parameter_packages/NGOMv19b.sh
      ;;
   "EGOMv20b")
      source $SCRIPTDIR/config/parameter_packages/EGOMv20b.sh
      ;;
   "Shinnecock")
      source $SCRIPTDIR/config/parameter_packages/Shinnecock.sh
      ;;
   "ec2001_v2e"|"EC2001v2e")
      source $SCRIPTDIR/config/parameter_packages/EC2001v2e.sh
      ;;
   "OPENWATERv1e")
      source $SCRIPTDIR/config/parameter_packages/OPENWATER.sh
      ;;
   "PRVI15")
      source $SCRIPTDIR/config/parameter_packages/PRVI15.sh
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
