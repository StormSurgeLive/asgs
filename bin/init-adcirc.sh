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

# Batch mode must never prompt. Keep BATCH as a compatibility input, but use
# one consistently named, numeric flag throughout this script.
BATCHMODE=${BATCHMODE:-${BATCH:-0}}
BUILD_ALL=${BUILD_ALL:-0}

_usage()
{
  cat <<EOF
Usage: ${0##*/} [OPTIONS] [COMMAND]

Build a supported ADCIRC version for the active ASGS environment.

Options:
  -b, --batch                 Run noninteractively using defaults.
  -A, --build-all             Build all supported ADCIRC versions without
                              confirmation; implies --batch.
  -N, --version VERSION       Select an ADCIRC version by name.
      --adcirc-version VERSION
                              Alias for --version.
  -h, --help                  Show this help and exit.

Commands:
  supported                   List supported ADCIRC versions and exit.
  clean                       Report the current clean-operation status.

A version can control its menu position and declare itself the preferred
default by placing these fields in its info.sh file:

  ADCIRC_VERSION_ORDER=10
  ADCIRC_VERSION_DEFAULT=yes

Lower order values are listed first. Versions with the same order value are
sorted lexicographically by version name. Versions without an order value are
listed last and sorted lexicographically within that final group.

If multiple versions are marked as the default, the script warns and uses the
last marked version found while scanning the main patch directory and then the
local patch directory, each lexicographically. Menu ordering does not change
which marked version wins. For otherwise identical main and local entries, the
local patchset is listed last and therefore wins named-selection resolution.

The interactive menu also provides a build-all option and asks for explicit
confirmation before invoking:

  ./cloud/general/t/build-all-adcirc.sh

Batch mode never prompts. -A invokes that script without confirmation. For a
custom batch build, CUSTOM_SRC must be set.
EOF
}

# Parse both short and long options without requiring the external getopt
# command. Options may appear before or after the supported/clean command.
COMMAND=
VERSION_OPTION_SET=0
while (( $# )); do
  case "$1" in
    -b|--batch)
      BATCHMODE=1
      ;;
    -A|--build-all)
      BUILD_ALL=1
      BATCHMODE=1
      ;;
    -N|--version|--adcirc-version)
      if (( $# < 2 )); then
        echo "(fatal) $1 requires an ADCIRC version argument." >&2
        echo "Run '${0##*/} --help' for usage." >&2
        exit 2
      fi
      SELECTED_VERSION=$2
      VERSION_OPTION_SET=1
      shift
      ;;
    -N?*)
      SELECTED_VERSION=${1#-N}
      VERSION_OPTION_SET=1
      ;;
    --version=*|--adcirc-version=*)
      SELECTED_VERSION=${1#*=}
      VERSION_OPTION_SET=1
      if [[ -z "$SELECTED_VERSION" ]]; then
        echo "(fatal) $1 requires a nonempty ADCIRC version." >&2
        exit 2
      fi
      ;;
    -h|--help)
      _usage
      exit 0
      ;;
    supported|clean)
      if [[ -n "$COMMAND" ]]; then
        echo "(fatal) only one command may be specified." >&2
        exit 2
      fi
      COMMAND=$1
      ;;
    --)
      shift
      if (( $# )); then
        echo "(fatal) unexpected argument after '--': '$1'." >&2
        exit 2
      fi
      break
      ;;
    -* )
      echo "(fatal) unknown option '$1'." >&2
      echo "Run '${0##*/} --help' for usage." >&2
      exit 2
      ;;
    *)
      echo "(fatal) unexpected argument '$1'." >&2
      echo "Run '${0##*/} --help' for usage." >&2
      exit 2
      ;;
  esac
  shift
done

if (( BUILD_ALL && VERSION_OPTION_SET )); then
  echo "(fatal) -A/--build-all cannot be combined with -N/--version." >&2
  exit 2
fi

if (( BUILD_ALL )) && [[ -n "$COMMAND" ]]; then
  echo "(fatal) -A/--build-all cannot be combined with the '$COMMAND' command." >&2
  exit 2
fi

if (( BUILD_ALL )); then
  SELECTED_VERSION=build-all
fi

if [[ "$COMMAND" == "clean" ]]; then
  echo "'clean' not implemented for optional ADCIRC/SWAN step at this time, clean up for ADCIRC and SWAN must be done manually."
  exit 0
fi

ADCIRCS=()
NUM_ADC=0
_ASGS_ADCIRC_BASE=$SCRIPTDIR/opt/models/adcircs

# support function for menu detecting selection
_is_a_num()
{
  re='^[1-9][0-9]?$'
  if [[ "${1}" =~ $re ]] ; then
    echo -n "$1"
  else
    echo -n -1
  fi
  return
}

# Read a value consistently. In batch mode, use the default without prompting.
_prompt_value()
{
  local result_var="$1"
  local prompt="$2"
  local default_value="$3"
  local response

  if (( BATCHMODE )); then
    printf -v "$result_var" '%s' "$default_value"
    return
  fi

  read -r -p "$prompt [default: $default_value]: " response
  printf -v "$result_var" '%s' "${response:-$default_value}"
  echo
}

# Read and normalize a yes/no response. The capitalized choice is the default.
# In batch mode, use the supplied default without prompting.
_prompt_yes_no()
{
  local result_var="$1"
  local prompt="$2"
  local default_answer="${3,,}"
  local choices response

  case "$default_answer" in
    y|yes) default_answer=yes; choices='Y/n' ;;
    n|no)  default_answer=no;  choices='y/N' ;;
    *)
      echo "(fatal) invalid default answer '$3' for yes/no prompt."
      exit 1
      ;;
  esac

  if (( BATCHMODE )); then
    printf -v "$result_var" '%s' "$default_answer"
    return
  fi

  while true; do
    read -r -p "$prompt [$choices]: " response
    response="${response:-$default_answer}"
    case "${response,,}" in
      y|yes)
        printf -v "$result_var" '%s' yes
        break
        ;;
      n|no)
        printf -v "$result_var" '%s' no
        break
        ;;
      *)
        echo "Please enter 'yes' or 'no'."
        ;;
    esac
  done
  echo
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
      echo "${W} unknown compiler '$ADCIRC_COMPILER' is unsupported; defaulting to 'intel'."
      ADCIRC_COMPILER=intel
      FC=ifort
      MPIF90=mpif90
      CC=icc
    esac
}

_set_compilers

#
# M E N U  D I S P L A Y  &  S E L E C T I O N  L O G I C
#

# Read the last simple assignment to a named variable from an info.sh file.
# This intentionally does not source the file during discovery.
_info_assignment_value()
{
  local info_file="$1"
  local variable_name="$2"
  local value

  [[ -f "$info_file" ]] || return 1

  value=$(
    sed -nE \
      "s/^[[:space:]]*(export[[:space:]]+)?${variable_name}[[:space:]]*=[[:space:]]*([^#[:space:]]+).*/\\2/p" \
      "$info_file" | tail -n 1
  )

  [[ -n "$value" ]] || return 1

  value=${value#\"}
  value=${value%\"}
  value=${value#\'}
  value=${value%\'}
  printf '%s\n' "$value"
}

# Return success when an info.sh file marks its version as the preferred
# default. The last assignment in the file wins, just as it would if sourced.
_info_marks_default()
{
  local info_file="$1"
  local value

  value=$(_info_assignment_value "$info_file" ADCIRC_VERSION_DEFAULT) || return 1

  case "${value,,}" in
    1|yes|true|on) return 0 ;;
    *)             return 1 ;;
  esac
}

# Print a valid numeric ordering value. A missing value returns 1; an invalid
# value returns 2 so the caller can warn and place the version in the final,
# unordered group.
_info_version_order()
{
  local info_file="$1"
  local value

  value=$(_info_assignment_value "$info_file" ADCIRC_VERSION_ORDER) || return 1

  if [[ "$value" =~ ^-?[0-9]+$ ]]; then
    printf '%s\n' "$value"
    return 0
  fi

  printf '%s\n' "$value"
  return 2
}

# Invoke the existing build-all driver from the ASGS source root so its
# documented relative path and any relative paths it uses remain valid.
_run_build_all_adcirc()
{
  local relative_script="./cloud/general/t/build-all-adcirc.sh"
  local script_path="${SCRIPTDIR}/cloud/general/t/build-all-adcirc.sh"
  local exit_status

  if [[ ! -f "$script_path" ]]; then
    echo "(fatal) ADCIRC build-all script not found:" >&2
    echo "  $script_path" >&2
    exit 1
  fi

  echo "Invoking ADCIRC build-all script:"
  echo "  $relative_script"
  echo

  (
    cd "$SCRIPTDIR" || exit 1
    bash "$relative_script"
  )
  exit_status=$?

  if (( exit_status != 0 )); then
    echo "(fatal) ADCIRC build-all script failed with exit status $exit_status." >&2
  fi

  exit "$exit_status"
}

# Discover and optionally display supported versions. Ordered versions are
# sorted numerically by ADCIRC_VERSION_ORDER and then lexicographically by
# version name. Versions without a valid order value are sorted last, also
# lexicographically. Main entries precede otherwise identical local entries,
# preserving local override behavior when the last matching name is selected.
_show_supported_versions()
{
  local mode="${1:-show}"
  local root version about info_file order_value order_status default_index
  local candidate_index sorted_index root_number=0
  local last_default_candidate_index=-1
  local -a roots=()
  local -a root_labels=()
  local -a candidate_versions=()
  local -a candidate_bases=()
  local -a candidate_abouts=()
  local -a candidate_order_groups=()
  local -a candidate_orders=()
  local -a candidate_root_numbers=()
  local -a default_files=()

  ADCIRCS=()
  ADCIRC_PATCHSET_BASES=()
  ADCIRC_ABOUTS=()
  ADCIRC_VERSION_ORDERS=()

  roots+=("$SCRIPTDIR/patches/ADCIRC")
  root_labels+=("patchset")

  if [[ -n "$ASGS_LOCAL_DIR" && -d "$ASGS_LOCAL_DIR/patches/ADCIRC" ]]; then
    roots+=("$ASGS_LOCAL_DIR/patches/ADCIRC")
    root_labels+=("local patchset")
  fi

  for root in "${roots[@]}"; do
    if [[ ! -d "$root" ]]; then
      root_number=$((root_number + 1))
      continue
    fi

    while IFS= read -r version; do
      [[ -n "$version" ]] || continue

      info_file="$root/$version/info.sh"
      candidate_versions+=("$version")
      candidate_bases+=("$root")
      candidate_root_numbers+=("$root_number")

      about="${root_labels[$root_number]} for Version, $version"
      if [[ -f "$root/$version/about.txt" ]]; then
        about=$(tr -d '\n' < "$root/$version/about.txt")
      fi
      candidate_abouts+=("$about")

      if _info_marks_default "$info_file"; then
        last_default_candidate_index=$((${#candidate_versions[@]} - 1))
        default_files+=("$info_file")
      fi

      order_value=$(_info_version_order "$info_file")
      order_status=$?
      case "$order_status" in
        0)
          candidate_order_groups+=(0)
          candidate_orders+=("$order_value")
          ;;
        1)
          candidate_order_groups+=(1)
          candidate_orders+=(0)
          ;;
        2)
          echo "(warning) ignoring invalid ADCIRC_VERSION_ORDER='$order_value' in:" >&2
          echo "  $info_file" >&2
          echo "(warning) expected an integer; '$version' will be sorted with unordered versions." >&2
          candidate_order_groups+=(1)
          candidate_orders+=(0)
          ;;
      esac
    done < <(
      find "$root" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' 2>/dev/null |
        LC_ALL=C sort
    )

    root_number=$((root_number + 1))
  done

  if (( ${#candidate_versions[@]} == 0 )); then
    echo "(fatal) no ADCIRC patchsets were found." >&2
    exit 1
  fi

  # Sort only compact numeric/index records so paths and descriptions can
  # safely contain spaces. The final root-number key keeps main before local
  # when order and version name are otherwise identical.
  while IFS=$'\t' read -r _ _ _ _ sorted_index; do
    ADCIRCS+=("${candidate_versions[$sorted_index]}")
    ADCIRC_PATCHSET_BASES+=("${candidate_bases[$sorted_index]}")
    ADCIRC_ABOUTS+=("${candidate_abouts[$sorted_index]}")

    if (( candidate_order_groups[$sorted_index] == 0 )); then
      ADCIRC_VERSION_ORDERS+=("${candidate_orders[$sorted_index]}")
    else
      ADCIRC_VERSION_ORDERS+=("")
    fi

    if (( sorted_index == last_default_candidate_index )); then
      default_index=$((${#ADCIRCS[@]} - 1))
    fi
  done < <(
    for candidate_index in "${!candidate_versions[@]}"; do
      printf '%s\t%s\t%s\t%s\t%s\n' \
        "${candidate_order_groups[$candidate_index]}" \
        "${candidate_orders[$candidate_index]}" \
        "${candidate_versions[$candidate_index]}" \
        "${candidate_root_numbers[$candidate_index]}" \
        "$candidate_index"
    done | LC_ALL=C sort -t $'\t' \
      -k1,1n -k2,2n -k3,3 -k4,4n -k5,5n
  )

  if (( last_default_candidate_index < 0 )); then
    # Compatibility fallback for repositories that do not yet use the marker.
    default_index=0
  fi

  DEFAULT_ADCIRC_INDEX=$default_index
  DEFAULT_ADCIRC_VERSION=${ADCIRCS[$DEFAULT_ADCIRC_INDEX]}

  if (( ${#default_files[@]} > 1 )); then
    echo "(warning) multiple ADCIRC versions set ADCIRC_VERSION_DEFAULT=yes:" >&2
    printf '  %s\n' "${default_files[@]}" >&2
    echo "(warning) using the last one found: '$DEFAULT_ADCIRC_VERSION'" >&2
  fi

  max_supported=${#ADCIRCS[@]}

  if [[ "$mode" != "quiet" ]]; then
    echo
    printf '/## ASGS ADCIRC Builder ##############\\ \n'

    local index display_version
    for index in "${!ADCIRCS[@]}"; do
      display_version=${ADCIRCS[$index]}
      if (( index == DEFAULT_ADCIRC_INDEX )); then
        display_version="$display_version (default)"
      fi
      printf "%2s. %-33s | %-66s\n" \
        "$((index + 1))" "$display_version" "${ADCIRC_ABOUTS[$index]}"
    done

    printf "%2s. %-33s | %-66s\n" "$((max_supported + 1))" build-all \
      "build every supported ADCIRC version"
    printf "%2s. %-33s | %-66s\n" "$((max_supported + 2))" custom \
      "select this option for custom directory *,**"
    printf "%2s. %-33s | %-66s\n" "$((max_supported + 3))" quit \
      "type 'quit', 'q', or 'ctrl-c' to quit"
    echo "--"
    echo "* Contact <help@support.adcirc.live> if privately patched ADCIRC support is required."
    echo "** See more about ADCIRC version support options at https://tools.adcirc.live/install"
    echo
  fi

  ADCIRCS+=(build-all custom quit)
  ADCIRC_PATCHSET_BASES+=("" "" "")
  ADCIRC_VERSION_ORDERS+=("" "" "")
  NUM_ADC=${#ADCIRCS[@]}

  if [[ "$mode" == "exit" ]]; then
    exit 0
  fi
}

# Normalize a numeric or named menu selection. Named duplicates resolve to the
# last one found, matching local override and default-selection behavior.
_resolve_version_selection()
{
  local selection="$1"
  local result_var="$2"
  local selected_index=-1
  local numeric index

  case "${selection,,}" in
    all|build-all|"build all")
      printf -v "$result_var" '%s' build-all
      RESOLVED_VERSION_INDEX=$((${#ADCIRCS[@]} - 3))
      return 0
      ;;
    q|quit)
      printf -v "$result_var" '%s' quit
      RESOLVED_VERSION_INDEX=$((${#ADCIRCS[@]} - 1))
      return 0
      ;;
  esac

  numeric=$(_is_a_num "$selection")
  if (( numeric > -1 )); then
    selected_index=$((numeric - 1))
    if (( selected_index < 0 || selected_index >= NUM_ADC )); then
      return 1
    fi
  else
    for index in "${!ADCIRCS[@]}"; do
      if [[ "${ADCIRCS[$index]}" == "$selection" ]]; then
        selected_index=$index
      fi
    done
    (( selected_index > -1 )) || return 1
  fi

  RESOLVED_VERSION_INDEX=$selected_index
  printf -v "$result_var" '%s' "${ADCIRCS[$selected_index]}"
  return 0
}

if [[ "$COMMAND" == "supported" ]]; then
  _show_supported_versions exit
fi

# preconditions
if [ -z "$ADCIRC_META_DIR" ]; then
  echo "ADCIRC_META_DIR is not set. Run interactively through asgsh or automatically via asgs-brew.pl."
  echo
  echo "Please note that this tool is meant to be run within an ASGS environment and is not supported "
  echo "as a general purpose tool. Per the license of the code, however, anyone is free to adapt it."
  echo
  exit 1
fi

# Always discover the version set so command-line values can be validated and
# the preferred default can be resolved. Show the menu only when it is useful.
if [[ -z "$SELECTED_VERSION" && BATCHMODE -eq 0 ]]; then
  _show_supported_versions show
else
  _show_supported_versions quiet
fi

if [[ -z "$SELECTED_VERSION" ]]; then
  __SELECTED_VERSION=$DEFAULT_ADCIRC_VERSION

  while true; do
    _prompt_value _SELECTED_VERSION \
      "Select an ADCIRC version, build-all, or q to quit" \
      "$__SELECTED_VERSION"

    if _resolve_version_selection "$_SELECTED_VERSION" SELECTED_VERSION; then
      break
    fi

    if (( BATCHMODE )); then
      echo "(fatal) invalid default ADCIRC version '$_SELECTED_VERSION'." >&2
      exit 1
    fi

    echo "Invalid selection. Enter a number from 1-${NUM_ADC}, a displayed name, 'all', or 'q'."
    echo
  done
else
  _REQUESTED_VERSION=$SELECTED_VERSION
  if ! _resolve_version_selection "$_REQUESTED_VERSION" SELECTED_VERSION; then
    echo "(fatal) unsupported ADCIRC version or menu selection '$_REQUESTED_VERSION'." >&2
    echo "Run '${0##*/} supported' to list available versions." >&2
    exit 1
  fi
fi

case "${SELECTED_VERSION,,}" in
  q|quit)
    exit 0
    ;;
  build-all)
    if (( ! BATCHMODE )); then
      _prompt_yes_no BUILD_ALL_CONFIRMED \
        "Build all supported ADCIRC versions using ./cloud/general/t/build-all-adcirc.sh?" \
        no

      if [[ "$BUILD_ALL_CONFIRMED" != "yes" ]]; then
        echo "Build-all cancelled."
        exit 0
      fi
    fi

    _run_build_all_adcirc
    ;;
esac

SELECTED_PATCHSET_BASE=${ADCIRC_PATCHSET_BASES[$RESOLVED_VERSION_INDEX]}

if (( ! BATCHMODE )); then
  echo "${I} Version selected: '$SELECTED_VERSION'"
  echo
fi

# Use the exact patchset root discovered for the selected menu item. Named
# duplicates resolve to the last one found, allowing local patchsets to override
# main patchsets consistently.
__ADCIRC_PATCHSET_BASE=${SELECTED_PATCHSET_BASE:-${SCRIPTDIR}/patches/ADCIRC}
FLAVOR_NAME=${SELECTED_VERSION}
PATCHSET_DIR=${__ADCIRC_PATCHSET_BASE}/${FLAVOR_NAME}
SWANDIR=
CUSTOM_SRC=${CUSTOM_SRC:-}

if [[ "$SELECTED_VERSION" != "custom" && ! -f "$PATCHSET_DIR/info.sh" ]]; then
  echo "(fatal) supported ADCIRC version '$SELECTED_VERSION' has no info.sh file." >&2
  echo "Expected: $PATCHSET_DIR/info.sh" >&2
  exit 1
fi

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
    if (( BATCHMODE )); then
      _CUSTOM_SRC=$CUSTOM_SRC
      if [[ -z "$_CUSTOM_SRC" ]]; then
        echo "(fatal) batch mode with version 'custom' requires CUSTOM_SRC to be set."
        exit 1
      fi
    else
      read -r -p "Enter the custom ADCIRC source directory: " _CUSTOM_SRC
      echo
    fi
    if [[ -z "$_CUSTOM_SRC" || ! -d "$(readlink -f "$_CUSTOM_SRC")/work" ]]; then
      echo "For 'custom' ADCIRC, a local source directory containing work/ is required."
      exit 1
    fi
    CUSTOM_SRC=$(readlink -f "$_CUSTOM_SRC")
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

_normalize_debug_selection()
{
  local selection="${1,,}"
  local result_var="$2"
  local normalized

  case "$selection" in
    1|none)              normalized=none ;;
    2|trace)             normalized=trace ;;
    3|full)              normalized=full ;;
    4|buserror)          normalized=buserror ;;
    5|netcdf)            normalized=netcdf ;;
    6|netcdf_trace)      normalized=netcdf_trace ;;
    7|valgrind)          normalized=valgrind ;;
    8|compiler-warnings) normalized=compiler-warnings ;;
    9|full-not-fpe)      normalized=full-not-fpe ;;
    10|full-not-warnelev) normalized=full-not-warnelev ;;
    *) return 1 ;;
  esac

  printf -v "$result_var" '%s' "$normalized"
  return 0
}

_DEBUG=1
if (( BATCHMODE )) && [[ -n "${DEBUG:-}" ]]; then
  _DEBUG=$DEBUG
fi
if (( ! BATCHMODE )); then
  cat <<EOF
Please choose a debug method:
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
fi

while true; do
  _prompt_value _DEBUG_SELECTION "Select a debug method by number or name" "$_DEBUG"

  if _normalize_debug_selection "$_DEBUG_SELECTION" DEBUG; then
    break
  fi

  if (( BATCHMODE )); then
    echo "(fatal) invalid batch debug method '$_DEBUG_SELECTION'." >&2
    exit 1
  fi

  echo "Invalid debug method. Enter a number from 1-10 or a displayed name."
  echo
done

if [[ -n "${DEBUG}" && "${DEBUG}" != 'none' ]]; then
  __ADCIRC_PROFILE_NAME=${__ADCIRC_PROFILE_NAME}-DEBUG-${DEBUG}
fi

#
# C H O O S E  P A R A L L E L  O R  S E R I A L
#

_BUILD_PARALLEL_ADCIRC=yes
if (( BATCHMODE )) && [[ -n "${BUILD_PARALLEL_ADCIRC:-}" ]]; then
  _BUILD_PARALLEL_ADCIRC=$BUILD_PARALLEL_ADCIRC
fi
_prompt_yes_no BUILD_PARALLEL_ADCIRC "Build parallel ADCIRC?" "$_BUILD_PARALLEL_ADCIRC"
if [[ "$BUILD_PARALLEL_ADCIRC" != "yes" ]]; then
  BUILD_PARALLEL_ADCIRC=no
  __ADCIRC_PROFILE_NAME=${__ADCIRC_PROFILE_NAME}-serial-only
fi
echo

#
# S I N G U L A R I T Y  S U P P O R T
#
# Note:
#   ASGS_SINGULARITY_CMD must be exported and set for 
#   this given platform in order to get the Singularity
#   dialog.
#

if [[ -n "$ASGS_SINGULARITY_CMD" ]] && (( ! BATCHMODE )); then
  singularity_supported=no
  use_singularity=no
  # finds .sif associated with SELECTED_VERSION, if listed in ADCIRC_SINGULARITY_MANIFEST file
  if [[ -e "$ADCIRC_SINGULARITY_MANIFEST" ]]; then
    # attempt to find mapping of ADCIRC version to .sif
    # in base directory of ADCIRC_SINGULARITY_MANIFEST
    OLDIFS=$IFS
    IFS=$'\n'
    default_sif="<enter> if you don't know"
    for line in $(cat ${ADCIRC_SINGULARITY_MANIFEST} | grep -v '#'); do
      adcirc_version=$(echo $line | awk '{print $1}')
      adcirc_sif=$(echo $line | awk '{print $2}')
  
      ## *note* match on adcirc version selected, if found offer
      #  that .sif as the default in the following dialog
      if [[ "$SELECTED_VERSION" == "$adcirc_version" ]]; then
        default_sif=$adcirc_sif
        singularity_supported=yes
        break
      fi
    done
    IFS=$OLDIFS
  fi

  # if found for SELECTED_VERSION, asks user if they want to use Singularity at all
  if [[ "$singularity_supported" == "yes" ]]; then
    default_answer=yes
    _prompt_yes_no use_singularity \
      "Singularity is supported for version $SELECTED_VERSION. Use it?" \
      "$default_answer"
  fi
  
  # if they do want to use Singularity, it asks which .sif and defaults to the one
  #   found in ADCIRC_SINGULARITY_MANIFEST
  if [[ "$use_singularity" == "yes" ]]; then
    _prompt_value _ADCIRC_SINGULARITY_SIF \
      "Enter the full path to the Singularity .sif" \
      "$default_sif"

    if [[ ! -e "$_ADCIRC_SINGULARITY_SIF" ]]; then
      echo "Can't find '$_ADCIRC_SINGULARITY_SIF'! exiting ADCIRC building ..."
      exit
    fi
  
    if [ -n "${_ADCIRC_SINGULARITY_SIF}" ]; then
      ADCIRC_SINGULARITY_SIF=$_ADCIRC_SINGULARITY_SIF
      __ADCIRC_PROFILE_NAME=${__ADCIRC_PROFILE_NAME}-singularity
    fi
  fi
fi

if (( BATCHMODE )) && [[ -n "${ADCIRC_SINGULARITY_SIF:-}" ]]; then
  if [[ -z "$ASGS_SINGULARITY_CMD" ]]; then
    echo "(fatal) ADCIRC_SINGULARITY_SIF is set, but ASGS_SINGULARITY_CMD is not."
    exit 1
  fi
  if [[ ! -e "$ADCIRC_SINGULARITY_SIF" ]]; then
    echo "(fatal) Singularity image not found: '$ADCIRC_SINGULARITY_SIF'."
    exit 1
  fi
  __ADCIRC_PROFILE_NAME=${__ADCIRC_PROFILE_NAME}-singularity
fi

#
# C H O O S E  A D C I R C  P R O F I L E  N A M E
#

_ADCIRC_PROFILE_DEFAULT=$__ADCIRC_PROFILE_NAME
if (( BATCHMODE )) && [[ -n "${_ADCIRC_PROFILE_NAME:-}" ]]; then
  _ADCIRC_PROFILE_DEFAULT=$_ADCIRC_PROFILE_NAME
fi
_prompt_value ADCIRC_PROFILE_NAME \
  "Enter the ADCIRC build profile name" \
  "$_ADCIRC_PROFILE_DEFAULT"

#
# C H O O S E  B U I L D  A N D  I N S T A L L  L O C A T I O N
#

# default location of the build is going to be within ASGS_INSTALL_PATH
__ADCIRCBASE=${_ASGS_ADCIRC_BASE}/adcirc-cg-${ADCIRC_PROFILE_NAME}
if (( BATCHMODE )) && [[ -n "${_ADCIRCBASE:-}" ]]; then
  __ADCIRCBASE=$_ADCIRCBASE
fi
_prompt_value ADCIRCBASE \
  "Enter the ADCIRC build directory" \
  "$__ADCIRCBASE"

# In interactive mode, existing build directories default to being preserved.
if [[ -d "${ADCIRCBASE}" ]] && (( ! BATCHMODE )); then
  _delete=no
  echo "$W \"$ADCIRCBASE\" exists!"
  _prompt_yes_no delete "${W} Delete this directory and continue?" "$_delete"
  if [[ "$delete" == "yes" ]]; then
    echo " ... deleting '$ADCIRCBASE'"
    rm -rf "$ADCIRCBASE"
    echo
  else
    echo "ADCIRC build cancelled; existing directory was not changed."
    exit 0
  fi
fi

# Preserve the existing batch behavior: rebuild from a clean directory.
if [[ -d "${ADCIRCBASE}" ]] && (( BATCHMODE )); then
  echo "$W \"$ADCIRCBASE\" exists!"
  echo " ... deleting '$ADCIRCBASE'"
  rm -rf "$ADCIRCBASE"
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
      _prompt_yes_no clone_answer \
        "Clone the ADCIRC git repository from GitHub?" \
        "$_answer"
      if [[ "$clone_answer" == 'yes' ]]; then
        git clone "${ADCIRC_GIT_URL}/${ADCIRC_GIT_REPO}.git" "$ADCIRCBASE"
        pushd "$ADCIRCBASE"
      else
        echo "ADCIRC build cancelled; source repository was not cloned."
        exit 0
      fi
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
    local MODULE_LIST=$(module list 2>&1 | grep '1)')

    local _FC_VERSION=$($FC --version | head -n 1)
    local _CC_VERSION=$($CC --version | head -n 1)

    local JSON_MPIF90="$MPIF90"
    if [ "$BUILD_PARALLEL_ADCIRC" != "yes" ]; then
        JSON_MPIF90=
    fi

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

    # output JSON, redact $USER:
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
    "time.adcirc.executables.built"            : "$BUILD_TIME",
    "adcirc.source.commit"                     : "$BASE_SHA",
    "adcirc.source.asgs.patches.set"           : "$FLAVOR_NAME",
    "adcirc.source.asgs.adcirc.flavor"         : "$FLAVOR_NAME",
    "adcirc.source.asgs.patches.applied"       : [
$patchJSON
    ],
    "adcirc.source.branch-base"                : "${ADCIRC_GIT_BRANCH:-0}",
    "env.adcirc.build.ASGS_HOME"               : "$ASGS_HOME",
    "env.adcirc.build.ASGS_MACHINE_NAME"       : "$ASGS_MACHINE_NAME",
    "env.adcirc.build.NETCDFHOME"              : "$NETCDFHOME",
    "env.adcirc.build.ADCIRCBASE"              : "$ADCIRCBASE",
    "env.adcirc.build.ADCIRCDIR"               : "$ADCIRCDIR",
    "env.adcirc.build.SWANDIR"                 : "$SWANDIR",
    "env.adcirc.build.CUSTOM_SRC"              : "${CUSTOM_SRC:-0}",
    "env.adcirc.build.ADCIRC_COMPILER"         : "${ADCIRC_COMPILER:-0}",
    "env.adcirc.build.ADCIRC_GIT_BRANCH"       : "${ADCIRC_GIT_BRANCH:-0}",
    "env.adcirc.build.ADCIRC_GIT_URL"          : "${ADCIRC_GIT_URL:-0}",
    "env.adcirc.build.ADCIRC_GIT_REPO"         : "${ADCIRC_GIT_REPO:-0}",
    "env.adcirc.build.ADCIRC_SRC_TYPE"         : "${ADCIRC_SRC_TYPE:-0}",
    "env.adcirc.build.ADCIRC_BASE_URL"         : "${ADCIRC_BASE_URL:-0}",
    "env.adcirc.build.ADCIRC_SRC_FILE"         : "${ADCIRC_SRC_FILE:-0}",
    "env.adcirc.build.ADCIRC_EXTRACT_DIR"      : "${ADCIRC_EXTRACT_DIR:-0}",
    "env.adcirc.build.ADCIRC_ARCHIVE_URL"      : "${ADCIRC_ARCHIVE_URL:-0}",
    "env.adcirc.build.ASGS_MAKEJOBS"           : "$ASGS_MAKEJOBS",
    "env.adcirc.build.ADCIRC_MAKE_CMD"         : "$ADCIRC_MAKE_CMD",
    "env.adcirc.build.SWAN_UTIL_BINS_MAKE_CMD" : "$SWAN_UTIL_BINS_MAKE_CMD",
    "env.adcirc.build.ADCSWAN_MAKE_CMD"        : "$ADCSWAN_MAKE_CMD",
    "env.adcirc.build.ADCIRC_PROFILE_NAME"     : "$ADCIRC_PROFILE_NAME",
    "env.adcirc.build.ADCIRC_BINS"             : "$ADCIRC_BINS",
    "env.adcirc.build.ADCSWAN_BINS"            : "$ADCSWAN_BINS",
    "env.adcirc.build.SWAN_UTIL_BINS"          : "$SWAN_UTIL_BINS",
    "env.adcirc.build.PATH"                    : "$PATH",
    "env.adcirc.build.LD_LIBRARY_PATH"         : "$LD_LIBRARY_PATH",
    "env.adcirc.build.LD_INCLUDE_PATH"         : "$LD_INCLUDE_PATH",
    "adcirc.build.built_parallel_adcirc"       : "$BUILD_PARALLEL_ADCIRC",
    "adcirc.build.fortran.mpif90"              : "$JSON_MPIF90",
    "adcirc.build.fortran.compiler"            : "$FC",
    "adcirc.build.c.compiler"                  : "$CC",
    "adcirc.build.c.compiler.version"          : "$_CC_VERSION",
    "adcirc.build.modules.loaded"              : "$MODULE_LIST",
    "adcirc.build.fortran.compiler.version"    : "$_FC_VERSION",
    "adcirc.build.debug"                       : "$DEBUG",
    "adcirc.singularity.sif"                   : "${ADCIRC_SINGULARITY_SIF:-}",
    "adcirc.singularity.manifest"              : "${ADCIRC_SINGULARITY_MANIFEST:-}",
    "asgs.container.cmd"                       : "${ASGS_SINGULARITY_CMD:-}"
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
  # Singularity support requires there be a SIF file
  # defined in the ADCIRC metadata file in $SCRIPTDIR/.adcirc-meta
  if [ -n "${ADCIRC_SINGULARITY_SIF}" ]; then
    cat <<EOMETA >> $ADCIRC_META_FILE
export ADCIRC_SINGULARITY_SIF='$ADCIRC_SINGULARITY_SIF'
EOMETA
  fi
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
    _prompt_yes_no patch_answer "Apply the ASGS patch set?" "$_answer"
    if [[ "$patch_answer" == "yes" ]]; then
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
echo "Build summary:"
printf "  %-18s %s\n" "Version:" "$SELECTED_VERSION"
printf "  %-18s %s\n" "Debug method:" "$DEBUG"
printf "  %-18s %s\n" "Parallel ADCIRC:" "$BUILD_PARALLEL_ADCIRC"
printf "  %-18s %s\n" "Profile:" "$ADCIRC_PROFILE_NAME"
printf "  %-18s %s\n" "Build directory:" "$ADCIRCBASE"
if [[ -n "${ADCIRC_SINGULARITY_SIF:-}" ]]; then
  printf "  %-18s %s\n" "Singularity SIF:" "$ADCIRC_SINGULARITY_SIF"
fi
echo
grep -v '#' "${BUILDSCRIPT}"
echo
echo "Build commands are contained in ${BUILDSCRIPT}"
echo

# dump JSON with build details into $ADCIRCBASE
echo
echo JSON build info in ${ADCIRC_BUILD_INFO_TMP}
echo
dumpJSON "$patchJSON" "$ADCIRC_BUILD_INFO_TMP"
echo

_answer=yes
_prompt_yes_no proceed_answer "Proceed with this build?" "$_answer"
if [[ "$proceed_answer" != 'yes' ]]; then
  echo "ADCIRC build cancelled."
  exit 0
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
if (( ! BATCHMODE )); then
  echo ADCIRC has been built and the ADCIRC profile registered in asgsh.
  echo To load this profile, at the asgsh prompt, enter:
  echo
  echo   load adcirc $ADCIRC_PROFILE_NAME
  echo
  echo Once loaded, save the current asgsh profile using the 'save' command.
  echo
  echo Information on the build itself is available in JSON format in,
  echo   $ADCIRC_BUILD_INFO
  echo
else
  echo "ADCIRC has been built and is available under ADCIRC Live."
  echo "To load this flavor of ADCIRC, use the 'adcirclive' command as follows," 
  echo
  echo    adcirclive load $ADCIRC_PROFILE_NAME
  echo
  echo Information on the build itself is available in JSON format in,
  echo
  echo   $ADCIRC_BUILD_INFO
  echo
fi
