#!/usr/bin/env bash
set -euo pipefail

#
# Build all supported ADCIRC versions for CI testing.
# Assumes init-adcirc.sh is available in PATH.
#

if ! command -v init-adcirc.sh >/dev/null 2>&1; then
  echo "ERROR: init-adcirc.sh is not in PATH" >&2
  echo "PATH=$PATH" >&2
  exit 127
fi

versions=(
  v53release
  v55.01-5bc04d6
  v55.02
  v56.0.2
  v56.0.4
  v56.0.4.live.0
)

echo "Using init-adcirc.sh at: $(command -v init-adcirc.sh)"

for v in "${versions[@]}"; do
  echo "========================================"
  echo "Building ADCIRC version: $v"
  echo "========================================"
  init-adcirc.sh -b -N "$v"
done
