#!/usr/bin/env bash
set -euo pipefail

#
# Build all supported ADCIRC versions for CI testing.
#
# Supported versions are discovered from:
#
#   patches/ADCIRC/*/info.sh
#
# Versions are ordered using ADCIRC_VERSION_ORDER from info.sh:
#
#   1. Ordered versions, lowest number first
#   2. Versions with equal order values, lexicographically a-z
#   3. Versions without an order value, lexicographically a-z
#
# Assumes init-adcirc.sh is available in PATH.
#

if ! command -v init-adcirc.sh >/dev/null 2>&1; then
  echo "ERROR: init-adcirc.sh is not in PATH" >&2
  echo "PATH=$PATH" >&2
  exit 127
fi

INIT_ADCIRC=$(command -v init-adcirc.sh)

# This script normally resides at:
#
#   cloud/general/t/build-all-adcirc.sh
#
SCRIPT_DIR=$(
  cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
  pwd -P
)

ASGS_ROOT=$(
  cd -- "${SCRIPT_DIR}/../../.."
  pwd -P
)

# Allow an alternate patch directory for testing if necessary.
PATCH_ROOT=${ADCIRC_PATCHES_DIR:-"${ASGS_ROOT}/patches/ADCIRC"}

if [[ ! -d "$PATCH_ROOT" ]]; then
  echo "ERROR: ADCIRC patch directory not found: $PATCH_ROOT" >&2
  exit 1
fi

records_file=$(mktemp)
trap 'rm -f "$records_file"' EXIT

_read_version_order()
{
  local info_file=$1

  (
    # An info.sh may reference optional ASGS variables.
    set +u

    SCRIPTDIR=$ASGS_ROOT
    ADCIRC_VERSION_ORDER=

    # info.sh files are trusted files from the ASGS source tree.
    # shellcheck disable=SC1090
    source "$info_file" >/dev/null

    printf '%s\n' "${ADCIRC_VERSION_ORDER:-}"
  )
}

while IFS= read -r -d '' info_file; do
  version_dir=$(dirname -- "$info_file")
  version=$(basename -- "$version_dir")
  order=

  if ! order=$(_read_version_order "$info_file"); then
    echo "WARNING: Could not read ordering from $info_file" >&2
    echo "WARNING: Treating '$version' as unordered." >&2
    order=
  fi

  if [[ -n "$order" ]]; then
    if [[ "$order" =~ ^-?[0-9]+$ ]]; then
      # Group 0 contains explicitly ordered versions.
      printf '0\t%s\t%s\n' "$order" "$version" >> "$records_file"
    else
      echo \
        "WARNING: Invalid ADCIRC_VERSION_ORDER='$order' in $info_file" \
        >&2
      echo "WARNING: Treating '$version' as unordered." >&2

      # Group 1 contains unordered versions and sorts last.
      printf '1\t0\t%s\n' "$version" >> "$records_file"
    fi
  else
    printf '1\t0\t%s\n' "$version" >> "$records_file"
  fi
done < <(
  find "$PATCH_ROOT" \
    -mindepth 2 \
    -maxdepth 2 \
    -type f \
    -name info.sh \
    -print0
)

mapfile -t versions < <(
  LC_ALL=C sort \
    -t $'\t' \
    -k1,1n \
    -k2,2n \
    -k3,3 \
    "$records_file" |
  cut -f3-
)

if (( ${#versions[@]} == 0 )); then
  echo "ERROR: No supported ADCIRC versions found under $PATCH_ROOT" >&2
  exit 1
fi

echo "Using init-adcirc.sh at: $INIT_ADCIRC"
echo "Using ADCIRC patchsets from: $PATCH_ROOT"
echo
echo "ADCIRC versions to build (${#versions[@]}):"

for version in "${versions[@]}"; do
  printf '  %s\n' "$version"
done

echo

for version in "${versions[@]}"; do
  echo "========================================"
  echo "Building ADCIRC version: $version"
  echo "========================================"

  "$INIT_ADCIRC" -b -N "$version"
done
