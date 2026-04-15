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
  v55.01-5bc04d6
  v55.02
  v56.0.2
  v56.0.4
  v53release
)

echo "Using init-adcirc.sh at: $(command -v init-adcirc.sh)"

for v in "${versions[@]}"; do
  echo "========================================"
  echo "Building ADCIRC version: $v"
  echo "========================================"
  bash bin/init-adcirc.sh -b -N "$v"
done
