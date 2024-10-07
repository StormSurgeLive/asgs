#!/usr/bin/env bash

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

# Note:
# If an operator wishes to use the ssh form of the git url, they will need
# to update the environmental variable ADCIRC_GIT_URL to use the url,
#
#   export ADCIRC_GIT_URL=git@github.com:adcirc
#
# before running 'build adcirc' in asgsh
#

trap 'echo && exit 1' SIGINT

if [ "${1}" = "clean" ]; then
  echo "'clean' not implemented for optional ADCIRC/SWAN step at this time, clean up for ADCIRC and SWAN must be done manually."
  exit 0
fi

while getopts "bN:" optname; do
   case $optname in
      b) BATCH=1
         ;;
      N) SELECTED_VERSION=${OPTARG}
         ;;
   esac
done

if [[ "$BATCH" == 1 && -z "$SELECTED_VERSION" ]]; then
  SELECTED_VERSION=v56.0.2 # default when "-b" is specified, but -N is not
fi

ADCIRCS=()
NUM_ADC=0
_ASGS_ADCIRC_BASE=$SCRIPTDIR/opt/models/adcircs

# support function for menu detecting selection
_is_a_num()
{
  re='^[1-9][0-9]?$'
  if [[ "${1}" =~ $re ]] ; then
    echo -n $1
  else
    echo -n -1
  fi
  return
}

_set_compilers()
{
    # get compiler info
    case "$ADCIRC_COMPILER" in
    intel)
      FC=ifort
      MPIF90=mpif90
      CC=icc
    ;;
    intel-oneapi)
      # default, above
      FC=ifx
      MPIF90=mpiifx
      CC=icx
    ;;
    gfortran)
      FC=gfortran
      MPIF90=mpif90
      CC=gcc
    ;;
    *)
      echo '${W} unknown compiler is unsupported...; defaulting to "intel"'
    esac
}

_set_compilers

#
# M E N U  D I S P L A Y  &  S E L E C T I O N  L O G I C
#

# This function is just to support the menu that comes up when the utility is run
_show_supported_versions()
{
  echo
  printf "/## ASGS ADCIRC Builder ##############\ \n"
  local num=0
  _ADCIRCS=
  for VERSION in $(ls -1 $SCRIPTDIR/patches/ADCIRC); do
    _ADCIRCS="$_ADCIRCS $VERSION"
    local about="patchset for Version, $VERSION"
    if [ -e $SCRIPTDIR/patches/ADCIRC/$VERSION/about.txt ]; then
      about=$(cat $SCRIPTDIR/patches/ADCIRC/$VERSION/about.txt | sed 's/\n//g')
    fi
    num=$(($num+1))
    printf "%2s. %-33s | %-66s\n" $num $VERSION "$about"
  done
  max_supported=$num
  # local ADCIRC patch support
  if [[ -n "$ASGS_LOCAL_DIR" && -d "$ASGS_LOCAL_DIR/patches/ADCIRC" ]]; then
    for VERSION in $(ls -1 $ASGS_LOCAL_DIR/patches/ADCIRC); do
      _ADCIRCS="$_ADCIRCS $VERSION"
      local about="local patchset for Version, $VERSION"
      if [ -e $ASGS_LOCAL_DIR/patches/ADCIRC/$VERSION/about.txt ]; then
        about=$(cat $ASGS_LOCAL_DIR/patches/ADCIRC/$VERSION/about.txt | sed 's/\n//g')
      fi
      num=$(($num+1))
      printf "%2s. %-33s | %-66s |\n" $num $VERSION "$about"
    done
  fi
  # final menu entry for custom directory
  num=$(($num+1))
  printf "%2s. %-33s | %-66s\n" $num custom "select this option for custom directory *,**"
  num=$(($num+1))
  printf "%2s. %-33s | %-66s\n" $num quit "type 'quit', 'q', or 'ctrl-c' to quit"
  echo  "--"
  echo "* Contact <help@support.adcirc.live> if privately patched ADCIRC support is required."
  echo "** See more about ADCIRC version support options at https://tools.adcirc.live/install"
  echo
  ADCIRCS=($_ADCIRCS custom)
  if [ "${1}" != "noexit" ]; then
    # exits on error if '1' is optionally passed, defaults to 0 (no error)
    exit ${1:-0}
  fi
  NUM_ADC=$num
}

if [ "${1}" = "supported" ]; then
  _show_supported_versions
fi

# preconditions
if [ -z "$ADCIRC_META_DIR" ]; then
  echo "ADCIRC_META_DIR is not set. Run interactively through asgsh or automatically via asgs-brew.pl."
  echo
  echo "Please note that this tool is meant to be run within an ASGS environment and is not supported "
  echo "as a general purpose too. Per the license of the code, however, anyone is free to adapt it.   "
  echo
  exit 1
fi

if [[ -z "$SELECTED_VERSION" ]]; then
  _show_supported_versions noexit
  # get branch/tag/sha to checkout
  __SELECTED_VERSION=${ADCIRCS[0]} # current preferred default
  if [[ -z "$BATCH" ]]; then
    read -p "What supported 'version' of the ADCIRC source do you wish to build (by name or select 1-${NUM_ADC})? [$__SELECTED_VERSION] " _SELECTED_VERSION
    echo
  fi
  
  # Handle selection by number
  if [ -n "$_SELECTED_VERSION" ]; then
    if [[ "$_SELECTED_VERSION" == "q" || "$_SELECTED_VERSION" == "quit" ]]; then
      exit
    fi
    # check for number selection
    _isnum=$(_is_a_num $_SELECTED_VERSION)
    if [[ -n "$_isnum" && $_isnum -gt -1 ]]; then
      _SELECTED_VERSION=${ADCIRCS[$(($_isnum-1))]} # zero indexed
      if [ -z "$_SELECTED_VERSION" ]; then
        echo "(fatal) invalid value..."
        echo
        exit 1
      fi
    fi
    # do not export, don't affect current environment after build
    SELECTED_VERSION=$_SELECTED_VERSION
  else
    SELECTED_VERSION=$__SELECTED_VERSION
  fi
fi

if [[ -z "$BATCH" ]]; then
  echo "${I} Version Selected: '$SELECTED_VERSION'"
  echo
fi

# obtain patch information for the selection
__ADCIRC_PATCHSET_BASE=${SCRIPTDIR}/patches/ADCIRC
if [[ $_isnum -gt $max_supported ]]; then
  __ADCIRC_PATCHSET_BASE=${ASGS_LOCAL_DIR}/patches/ADCIRC
fi
FLAVOR_NAME=${SELECTED_VERSION}
PATCHSET_DIR=${__ADCIRC_PATCHSET_BASE}/${FLAVOR_NAME}
SWANDIR=
CUSTOM_SRC=

#
# S O U R C E  S E L E C T I O N ' S  I N F O . S H
#

# This case statement simply picks between 'custom', which
# has no info.sh file and anything else, that does have a
# info.sh file in $SCRIPTDIR/patches/ADCIRC
case "$SELECTED_VERSION" in
  custom)
    ADCIRC_SRC_TYPE=custom
    # handle 'custom' which assumes a local directory that is already
    # set to build properly, and has no patchsets in $SCRIPTDIR/patches/ADCIRC
    PATCHSET_DIR=
    read -p "What directory is the custom ADCIRC source code? " _CUSTOM_SRC
    if [[ -z "$_CUSTOM_SRC" || ! -d $(readlink -f $_CUSTOM_SRC/work) ]]; then
      echo "For 'custom' ADCIRC, a local source code directory is currently required. Please try again."
      exit 1
    fi
    CUSTOM_SRC=$(readlink -f $_CUSTOM_SRC)
    echo
    echo "Found: '$CUSTOM_SRC'"
    echo
    FLAVOR_NAME=${SELECTED_VERSION} # will be 'custom'
    # find swan directory
    if [[ -d $CUSTOM_SRC/thirdparty/swan ]]; then
      SWANDIR=thirdparty/swan  # version >= 55
    elif [[ -d $CUSTOM_SRC/swan ]]; then
      SWANDIR=swan             # version  < 55
    else
      echo "FATAL ERROR: SWAN directory not found in '$CUSTOM_SRC', doesn't appear to be a valid ADCIRC source directory."
      exit 1
    fi

    # required
    __ADCIRC_PROFILE_NAME=custom-${ADCIRC_COMPILER}-${ASGS_MACHINE_NAME}
    ;;
  *)
    source $PATCHSET_DIR/info.sh
    __ADCIRC_PROFILE_NAME=${FLAVOR_NAME}-${ADCIRC_COMPILER}-${ASGS_MACHINE_NAME}
    ;;
esac

#
# A S K  A B O U T  D E B U G  O P T I O N S  F I R S T
#

_DEBUG=1
if [[ -z "$BATCH" ]]; then
  cat <<EOF
Please choose a debug method [1-10]:
  1. none (default)
  2. trace
  3. full
  4. buserror
  5. netcdf
  6. netcdf_trace
  7. valgrind
  8. compiler-warnings
  9. full-not-fpe
 10. full-not-warnelev
EOF
  echo
  read -p "Choose 1-10 [${_DEBUG}]: " DEBUG
fi
  
if [[ -z "$DEBUG" ]]; then
  DEBUG=$_DEBUG
fi

case "${DEBUG}" in
  1) DEBUG=none
    ;;
  2) DEBUG=trace
    ;;
  3) DEBUG=full
    ;;
  4) DEBUG=buserror
    ;;
  5) DEBUG=netcdf
    ;;
  6) DEBUG=netcdf_trace
    ;;
  7) DEBUG=valgrind
    ;;
  8) DEBUG=compiler-warnings
    ;;
  9) DEBUG=full-not-fpe
    ;;
 10) DEBUG=full-not-warnelev
    ;;
esac

if [[ -n "${DEBUG}" && "${DEBUG}" != 'none' ]]; then
  __ADCIRC_PROFILE_NAME=${__ADCIRC_PROFILE_NAME}-DEBUG-${DEBUG}
fi

#
# C H O O S E  P A R A L L E L  O R  S E R I A L
#

_BUILD_PARALLEL_ADCIRC=yes
if [[ -z "$BATCH" ]]; then
   echo
   read -p "Do you wish to build parallel ADCIRC? [${_BUILD_PARALLEL_ADCIRC}] " BUILD_PARALLEL_ADCIRC
fi
if [ -z "$BUILD_PARALLEL_ADCIRC" ]; then
  BUILD_PARALLEL_ADCIRC=$_BUILD_PARALLEL_ADCIRC
fi
if [ "$BUILD_PARALLEL_ADCIRC" != "yes" ]; then
  BUILD_PARALLEL_ADCIRC=no
  __ADCIRC_PROFILE_NAME=${__ADCIRC_PROFILE_NAME}-DEBUG-${DEBUG}-serial-only
fi

#
# C H O O S E  A D C I R C  P R O F I L E  N A M E
#

if [[ -z "$BATCH" ]]; then
  echo
  read -p "What would you like to name this ADCIRC build profile? [$__ADCIRC_PROFILE_NAME] " _ADCIRC_PROFILE_NAME
fi

if [ -n "$_ADCIRC_PROFILE_NAME" ]; then
  ADCIRC_PROFILE_NAME=$_ADCIRC_PROFILE_NAME
else
  ADCIRC_PROFILE_NAME=$__ADCIRC_PROFILE_NAME
fi
echo

#
# C H O O S E  B U I L D  A N D  I N S T A L L  L O C A T I O N
#

# default location of the build is going to be within ASGS_INSTALL_PATH
__ADCIRCBASE=${_ASGS_ADCIRC_BASE}/adcirc-cg-${ADCIRC_PROFILE_NAME}
if [[ -z "$BATCH" ]]; then
  read -p "In what directory would you like to build ADCIRC? [${__ADCIRCBASE}] " _ADCIRCBASE
fi

if [ -n "${_ADCIRCBASE}" ]; then
  ADCIRCBASE=$_ADCIRCBASE
else
  ADCIRCBASE=$__ADCIRCBASE
fi
echo

# skip if $BATCH is set -b
if [[ -d "${ADCIRCBASE}" && -z "${BATCH}" ]]; then
  _delete=no
  echo "$W \"$ADCIRCBASE\" exists!"
  echo $W
  read -p "${W} Delete this directory and continue? [$_delete] " delete
  echo
  if [ -z "$delete" ]; then
    delete=$_delete
  fi
  if [[ "${delete,,}" != "no" ]]; then
    echo " ... deleting '$ADCIRCBASE'"
    rm -rf $ADCIRCBASE
    echo
  else
    echo "exiting ADCIRC building ..."
    exit
  fi
fi

# if batch is set (-b) and $ADCIRCBASE exists, delete
# and start over (could be not what you want?)
if [[ -d "${ADCIRCBASE}" && "${BATCH}" == 1 ]]; then
  echo "$W \"$ADCIRCBASE\" exists!"
  echo " ... deleting '$ADCIRCBASE'"
  rm -rf $ADCIRCBASE
  echo
fi

# some variables based on ADCIRCBASE that we can define now
ADCIRCDIR=${ADCIRCBASE}/work
BUILDSCRIPT=${ADCIRCBASE}/asgs-build.sh
ADCIRC_BUILD_INFO=${ADCIRCBASE}/adcirc.bin.buildinfo.json
ADCIRC_BUILD_INFO_TMP=${ADCIRC_BUILD_INFO}.tmp

#
# L O C A T E  &  G E T  C O D E  L O G I C
#

case "${ADCIRC_SRC_TYPE}" in
  custom)
    # must mkdir $ADCIRCBASE explicitly here
    mkdir -p $ADCIRCBASE
    # copy across local file system
    echo " ... copying from from '$CUSTOM_SRC' to '$ADCIRCBASE'"
    cp -rf $CUSTOM_SRC/* $ADCIRCBASE
    # clean up destination
    echo " ... cleaning up cruft from '$ADCIRCBASE'"
    rm -rf "$ADCIRCBASE/.git" 2> /dev/null
    rm "${BUILDSCRIPT}"       2> /dev/null
    pushd $ADCIRCDIR
    make clobber
    popd
    ;;
  remote-zip)
    if [ ! -d "$ADCIRCBASE" ]; then
      # must mkdir $ADCIRCBASE explicitly here
      mkdir -p $ADCIRCBASE
      # remote .zip file needing to be fetched
      pushd $ASGS_TMPDIR
      if [ -d $ADCIRC_EXTRACT_DIR ]; then
        echo ./$ADCIRC_EXTRACT_DIR detected, deleting ...
        rm -rf $ADCIRC_SRC_FILE $ADCIRC_EXTRACT_DIR   # force clean for dev
      fi
      wget "${ADCIRC_ARCHIVE_URL}"
      ${ADCIRC_EXTRACT_CMD} $ADCIRC_SRC_FILE
      # clean up .zip
      rm -r $ADCIRC_SRC_FILE
      cp -r $ADCIRC_EXTRACT_DIR/* $ADCIRCBASE
    else
      echo "$I '$ADCIRCBASE' already exists, so not retrieving from $ADCIRC_ARCHIVE_URL"
    fi
    ;;
  git)
    # $ADCIRCBASE is made by virtue of the "git clone" command in this case
    if [ ! -d ${ADCIRCBASE} ]; then
      _answer=yes
      if [[ -z "$BATCH" ]]; then
        read -p "Clone (download) ADCIRC git repository from GitHub? [$_answer] " answer
      fi
      if [ -z "$answer" ]; then
        answer=$_answer
      fi
      if [ "$answer" != 'no' ]; then
        [[ -z "$BATCH" ]] && echo
        git clone ${ADCIRC_GIT_URL}/${ADCIRC_GIT_REPO}.git ${ADCIRCBASE}
	pushd $ADCIRCBASE
      fi
      [[ -z "$BATCH" ]] && echo
      # always do checkout here
      repo=$ADCIRC_GIT_BRANCH
      do_checkout=yes
      if [ "$do_checkout" == "yes" ]; then
        CURRENT_BRANCH=$(git branch | egrep '^\*' | awk '{ print $2 }')
        if [ "${ADCIRC_GIT_BRANCH}" != "${CURRENT_BRANCH}" ]; then
          _GIT_OUT=$(git checkout ${ADCIRC_GIT_BRANCH} 2>&1)
          EXIT=$?
          if [ $EXIT -gt 0 ]; then
            echo $_GIT_OUT
            echo "(fatal) error checking out git repository. Exiting ($EXIT)."
            exit $EXIT
          fi
        fi
      fi
    else
        # still cd to this directory
	pushd $ADCIRCBASE
    fi
    ;;
  *)
    echo ADCIRC source retrieval not supported, if needed please file a feature request
    exit 1
    ;;
esac

#
# C R E A T E  B U I L D  C O M M A N D S
#

# deal with SWAN coupling build based on supported ADCIRC branches (versions):
# and potential patching of ADCIRC or SWAN
__ADCIRC_PATCHSET_BASE=${SCRIPTDIR}/patches/ADCIRC
SWANDIR=${ADCIRCBASE}/${SWANDIR}

# first class ADCIRC related binaries
# + the SWAN=enable is only relevant to adcprep but
#   should not cause any issues for the other targets
if [ "$BUILD_PARALLEL_ADCIRC" == "yes" ]; then
  ADCIRC_BINS="padcirc adcirc adcprep hstime aswip"
else
  ADCIRC_BINS="adcirc adcprep hstime aswip"
fi
ADCIRC_MAKE_CMD="make $ADCIRC_BINS SWAN=enable compiler=${ADCIRC_COMPILER} NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable NETCDFHOME=${NETCDFHOME} MACHINENAME=${ASGS_MACHINE_NAME} DEBUG=${DEBUG}"

# for building coupled adcswan/padcswan (include all netCDF flags, no 'SWAN=enable')
if [ "$BUILD_PARALLEL_ADCIRC" == "yes" ]; then
  ADCSWAN_BINS="padcswan adcswan"
else
  ADCSWAN_BINS="adcswan"
fi
ADCSWAN_MAKE_CMD="make $ADCSWAN_BINS compiler=${ADCIRC_COMPILER} MACHINENAME=${ASGS_MACHINE_NAME} NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable NETCDFHOME=${NETCDFHOME} DEBUG=${DEBUG}"

# SWAN related utilities other than adcswan/padcswan
#  + do not include anything related to netCDF
SWAN_UTIL_BINS="unhcat.exe"
SWAN_UTIL_BINS_MAKE_CMD="make unhcat compiler=${ADCIRC_COMPILER} MACHINENAME=${ASGS_MACHINE_NAME} DEBUG=${DEBUG}"

#
# W R I T E   M E T A D A T A  &  B U I L D  A D C I R C
#

# used to get values of specific variables out of SWAN's macros.inc
function splitMacrosInc
{
  local VARIABLE=$1
  local MACROSINC=$2
  local line=$(grep $VARIABLE $MACROSINC | sed 's/ *= */ /' | cut -d' ' -f2- | head -n 1)
  echo $line
}

function dumpJSON()
{
    local patchJSON="$1"
    local ADCIRC_BUILD_INFO_TMP="$2"
    local BUILD_TIME=$(date +%Y-%b-%d-T%H:%M:%S%z)
    local MODULE_LIST=$(module list 2>&1 | grep '1)');

    local _FC_VERSION=$($FC --version | head -n 1)
    local _CC_VERSION=$($CC --version | head -n 1)

    # get SHA of ADCIRC git repo before patching
    pushd $ADCIRCBASE 2> /dev/null
    BASE_SHA=$(git log | head -n 1 | awk '{print $2}')
    popd 2> /dev/null

    # grep out details from $SWANDIR/macros.inc
    MACROSINC="$SWANDIR/macros.inc"
    local F90_SER=$(splitMacrosInc     F90_SER     $MACROSINC)
    local F90_OMP=$(splitMacrosInc     F90_OMP     $MACROSINC)
    local F90_MPI=$(splitMacrosInc     F90_MPI     $MACROSINC)
    local FLAGS_OPT=$(splitMacrosInc   FLAGS_OPT   $MACROSINC)
    local FLAGS_MSC=$(splitMacrosInc   FLAGS_MSC   $MACROSINC)
    local FLAGS90_MSC=$(splitMacrosInc FLAGS90_MSC $MACROSINC)
    local FLAGS_SER=$(splitMacrosInc   FLAGS_SER   $MACROSINC)
    local FLAGS_OMP=$(splitMacrosInc   FLAGS_OMP   $MACROSINC)
    local FLAGS_MPI=$(splitMacrosInc   FLAGS_MPI   $MACROSINC)

    # output JSON, redact $USE:
    cat <<JSON | sed "s/$USER/\$USER/g" > $ADCIRC_BUILD_INFO_TMP
  {
    "adcirc.build.swan.macros-inc"             : "$MACROSINC",
    "adcirc.build.swan.macros-inc.F90_SER"     : "$F90_SER",
    "adcirc.build.swan.macros-inc.F90_MPI"     : "$F90_MPI",
    "adcirc.build.swan.macros-inc.F90_OMP"     : "$F90_OMP",
    "adcirc.build.swan.macros-inc.FLAGS_OPT"   : "$FLAGS_OPT",
    "adcirc.build.swan.macros-inc.FLAGS_MSC"   : "$FLAGS_MSC",
    "adcirc.build.swan.macros-inc.FLAGS90_MSC" : "$FLAGS90_MSC",
    "adcirc.build.swan.macros-inc.FLAGS_SER"   : "$FLAGS_SER",
    "adcirc.build.swan.macros-inc.FLAGS_MPI"   : "$FLAGS_MPI",
    "adcirc.build.swan.macros-inc.FLAGS_OMP"   : "$FLAGS_OMP",
    "time.adcirc.executables.built"      : "$BUILD_TIME",
    "adcirc.source.commit"               : "$BASE_SHA",
    "adcirc.source.asgs.patches.set"     : "$FLAVOR_NAME",
    "adcirc.source.asgs.adcirc.flavor"   : "$FLAVOR_NAME",
    "adcirc.source.asgs.patches.applied" : [
$patchJSON
    ],
    "adcirc.source.branch-base"           : "$ADCIRC_GIT_BRANCH",
    "env.adcirc.build.ASGS_HOME"          : "$ASGS_HOME",
    "env.adcirc.build.ASGS_MACHINE_NAME"  : "$ASGS_MACHINE_NAME",
    "env.adcirc.build.NETCDFHOME"         : "$NETCDFHOME",
    "env.adcirc.build.ADCIRCBASE"         : "$ADCIRCBASE",
    "env.adcirc.build.ADCIRCDIR"          : "$ADCIRCDIR",
    "env.adcirc.build.SWANDIR"            : "$SWANDIR",
    "env.adcirc.build.CUSTOM_SRC"         : "${CUSTOM_SRC:-0}",
    "env.adcirc.build.ADCIRC_COMPILER"    : "${ADCIRC_COMPILER:-0}",
    "env.adcirc.build.ADCIRC_GIT_BRANCH"  : "${ADCIRC_GIT_BRANCH:-0}",
    "env.adcirc.build.ADCIRC_GIT_URL"     : "${ADCIRC_GIT_URL:-0}",
    "env.adcirc.build.ADCIRC_GIT_REPO"    : "${ADCIRC_GIT_REPO:-0}",
    "env.adcirc.build.ADCIRC_SRC_TYPE"    : "${ADCIRC_SRC_TYPE:-0}",
    "env.adcirc.build.ADCIRC_BASE_URL"    : "${ADCIRC_BASE_URL:-0}",
    "env.adcirc.build.ADCIRC_SRC_FILE"    : "${ADCIRC_SRC_FILE:-0}",
    "env.adcirc.build.ADCIRC_EXTRACT_DIR" : "${ADCIRC_EXTRACT_DIR:-0}",
    "env.adcirc.build.ADCIRC_ARCHIVE_URL" : "${ADCIRC_ARCHIVE_URL:-0}",
    "env.adcirc.build.ASGS_MAKEJOBS"      : "$ASGS_MAKEJOBS",
    "env.adcirc.build.ADCIRC_MAKE_CMD"    : "$ADCIRC_MAKE_CMD",
    "env.adcirc.build.SWAN_UTIL_BINS_MAKE_CMD" : "$SWAN_UTIL_BINS_MAKE_CMD",
    "env.adcirc.build.ADCSWAN_MAKE_CMD"        : "$ADCSWAN_MAKE_CMD",
    "env.adcirc.build.ADCIRC_PROFILE_NAME"     : "$ADCIRC_PROFILE_NAME",
    "env.adcirc.build.ADCIRC_BINS"        : "$ADCIRC_BINS",
    "env.adcirc.build.ADCSWAN_BINS"       : "$ADCSWAN_BINS",
    "env.adcirc.build.SWAN_UTIL_BINS"     : "$SWAN_UTIL_BINS",
    "env.adcirc.build.PATH"               : "$PATH",
    "env.adcirc.build.LD_LIBRARY_PATH"    : "$LD_LIBRARY_PATH",
    "env.adcirc.build.LD_INCLUDE_PATH"    : "$LD_INCLUDE_PATH",
    "adcirc.build.built_parallel_adcirc"  : "$BUILD_PARALLEL_ADCIRC",
    "adcirc.build.fortran.mpif90"         : "$MPIF90",
    "adcirc.build.fortran.compiler"       : "$_FC",
    "adcirc.build.c.compiler"             : "$_CC",
    "adcirc.build.c.compiler.version"     : "$_CC_VERSION",
    "adcirc.build.modules.loaded"         : "$MODULE_LIST",
    "adcirc.build.fortran.compiler.version" : "$_FC_VERSION",
    "adcirc.build.debug"                  : "$DEBUG"
  }
JSON
}

function dumpMETA()
{
  local ADCIRC_META_FILE="$1"
  cat <<EOMETA > $ADCIRC_META_FILE
export ASGS_HOME='$ASGS_HOME'
export ASGS_MACHINE_NAME='$ASGS_MACHINE_NAME'
export NETCDFHOME='$NETCDFHOME'
export ADCIRCBASE='$ADCIRCBASE'
export ADCIRCDIR='$ADCIRCDIR'
export SWANDIR='$SWANDIR'
export ADCIRC_COMPILER='$ADCIRC_COMPILER'
export ADCIRC_BUILD_INFO='$ADCIRC_BUILD_INFO'
export ADCIRC_GIT_BRANCH='$ADCIRC_GIT_BRANCH'
export ADCIRC_GIT_URL='$ADCIRC_GIT_URL'
export ADCIRC_GIT_REPO='$ADCIRC_GIT_REPO'
export ASGS_MAKEJOBS=$ASGS_MAKEJOBS
export ADCIRC_MAKE_CMD='$ADCIRC_MAKE_CMD'
export SWAN_UTIL_BINS_MAKE_CMD='$SWAN_UTIL_BINS_MAKE_CMD'
export ADCSWAN_MAKE_CMD='$ADCSWAN_MAKE_CMD'
export ADCIRC_PROFILE_NAME='$ADCIRC_PROFILE_NAME'
export ADCIRC_BINS='$ADCIRC_BINS'
export ADCSWAN_BINS='$ADCSWAN_BINS'
export SWAN_UTIL_BINS='$SWAN_UTIL_BINS'
EOMETA
}

#
# A P P L Y  P A T C H S E T
#

# Notes:
# 1. Fails if patchset is define, but directory doesn't exit
#
# 2. Informs and skips patching if patches have already been applied
#
# 3. Adding a patch set is not too difficult, but is not done here; look "up"
#
# NOTE: Currently, patches are only available when the patchset
# directory is present in $SCRIPTDIR/patches/ADCIRC; they are
# not applied what CUSTOM_SRC is defined (i.e., pointing to a
# custom source that is presumably already to build without patching)

pushd $ADCIRCBASE > /dev/null

patch_files() {
  if [ -e "${BUILDSCRIPT}" ]; then
    echo "(info) patches already applied, skipping ..."
    ALREADYPATCHED=1
  else
    if [[ ! -d "${PATCHSET_DIR}" && -z "${CUSTOM_SRC}" ]]; then
      echo "(fatal) patch set directory not found. Exiting."
      exit 1
    fi
    echo
    echo applying patches from $PATCHSET_DIR ...
    # apply from perspective of $ADCIRCBASE, since it's possible we could also
    # be patching in a third party directory
    pCOUNT=1
    patches=()
    for diff in $(find ${PATCHSET_DIR} -type f -name *.patch  | sort -n); do
      patches+=($diff)
      printf "applying patch %02d ... %s\n" $pCOUNT $diff
      OUT=$(patch -p1 < $diff 2>&1)
      EXIT=$?
      if [ $EXIT -gt 0 ]; then
        echo $OUT
        echo "(fatal) error applying patch: $diff"
        echo Exiting.
       exit $EXIT
      fi
      _app_date=$(date "+%D %T %Z")
      echo "# $_app_date $diff" >> $BUILDSCRIPT
      pCOUNT=$((pCOUNT+1))
    done
    # generates JSON array for list of patches
    patchJSON=$(
    for i in "${patches[@]}"
    do
      if [ "$i" != "${patches[@]: -1}" ]; then
        echo "      \"$i\"",
      fi
    done
    echo "      \"${patches[@]: -1}\"")
  fi
}

case "${ADCIRC_SRC_TYPE}" in
  git|remote-zip)
    _answer=yes
    if [[ -z "$BATCH" ]]; then
      read -p "Apply patches? [$_answer]" answer
    fi
    if [ -z "$answer" ]; then
      answer=$_answer
    fi
    if [ "$answer" == "yes" ]; then
      patch_files # defined above
    fi
  ;;
esac

echo "cd $SWANDIR && \\"                 >> ${BUILDSCRIPT}
echo "   make clean && \\"               >> ${BUILDSCRIPT}
echo "   CC=$CC FC=$FC perl platform.pl && \\" >> ${BUILDSCRIPT}
echo "   $SWAN_UTIL_BINS_MAKE_CMD && \\" >> ${BUILDSCRIPT}
echo "cd $ADCIRCDIR && \\"               >> ${BUILDSCRIPT}
echo "   make clobber clean && \\"       >> ${BUILDSCRIPT}
echo "   $ADCIRC_MAKE_CMD && \\"         >> ${BUILDSCRIPT}
echo "   $ADCSWAN_MAKE_CMD"              >> ${BUILDSCRIPT}

echo
cat ${BUILDSCRIPT} | grep -v '#'
echo
echo Build command contained in file, ${BUILDSCRIPT}
echo

# dump JSON with build details into $ADCIRCBASE
echo
echo JSON build info in ${ADCIRC_BUILD_INFO_TMP}
echo
dumpJSON "$patchJSON" "$ADCIRC_BUILD_INFO_TMP"
echo

_answer=yes
if [[ -z "$BATCH" ]]; then
  read -p "Proceed to build? [$_answer] " answer
  echo
fi
if [ -z "$answer" ]; then
  answer=$_answer
fi
if [ "$answer" != 'yes' ]; then
  echo "build stopped. Exiting."
  exit 1
fi

# attempt to build
bash ${BUILDSCRIPT}

EXIT=$?

if [ $EXIT -gt 0 ]; then
  echo "build failed ($EXIT)"
  exit $EXIT
fi

# create directory to track ADCIRC installations
mkdir -p $ADCIRC_META_DIR 2> /dev/null

dumpMETA "$ADCIRC_META_DIR/$ADCIRC_PROFILE_NAME"

adcircflags2json $ADCIRCDIR/actualflags.txt $ADCIRC_BUILD_INFO_TMP > $ADCIRC_BUILD_INFO

rm -v $ADCIRC_BUILD_INFO_TMP

echo '                               _                                 '
echo '                             ,"_".                               '
echo '                          _ /_| |_\ _                            '
echo '                         /_\|  ^  |/_\                           '
echo '                         | || /|\ || |                           '
echo '                         | |||   ||| |                           '
echo '                         | |||. .||| |                           '
echo '                         | ||| u ||| |                           '
echo '                         | |||   ||| |                           '
echo '                         | |||   ||| |                           '
echo '                         | ,"  V  ". |                           '
echo '                         ,"_x_   _x_".                           '
echo '                        /____  |  ____\                          '
echo '                         /_\ ||||| /_\                           '
echo '                         .:   : :   :.                           '
echo '                         : .  : .  : :                           '
echo '                        .:  .   : :   ..                         '
echo '                                                                 '
echo '                        S U C C E S S !                          '
echo
if [[ -z "$BATCH" ]]; then
  echo ADCIRC has been build and the ADCIRC profile registered in asgsh.
  echo To load this profile, at the asgsh prompt, enter:
  echo
  echo   load adcirc $ADCIRC_PROFILE_NAME
  echo
  echo Once loaded, save the current asgsh profile using the the 'save' command.
  echo
  echo Information on the build itself is available in JSON format in,
  echo   $ADCIRC_BUILD_INFO
  echo
else
  echo "ADCIRC has been built and available under ADCIRC Live (c)"
  echo "To load this flavor of ADCIRC, use the 'adcirclive' command as follows," 
  echo
  echo    adcirclive load $ADCIRC_PROFILE_NAME
  echo
  echo Information on the build itself is available in JSON format in,
  echo
  echo   $ADCIRC_BUILD_INFO
  echo
fi
