#!/bin/bash
#----------------------------------------------------------------
# startup-checks.sh: subroutines that do static checks
#----------------------------------------------------------------
# Copyright(C) 2026 Brett Estrade
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

assert_asgslint()
{
  asgs-lint             # assumed to be in PATH
  local lint_err=$?
  if [[ $lint_err -ne 0 ]]; then
    local default_ans="N"
    read -r -p "asgs-lint detected FATAL errors on start up. Proceed? [y/${default_ans}] " ans
    # Use default if empty
    local ans=${ans:-$default_ans}
    # Normalize to uppercase
    local ans=${ans^^}
    if [[ $ans != "Y" ]]; then
      exit 1
    fi
  fi
}
