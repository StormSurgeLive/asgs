#!/usr/bin/env bash
#----------------------------------------------------------------
# lint-checks.sh: subroutines that do static checks
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

# To adjust LINTSETTING from the default of 'strict',
# one may set LINTSETTINGS in their ASGS_CONFIG to one
# of following values:
#   prompt - pauses ASGS, asks user to proceed or not (default is to to Not proceed)
#   warn   - simply issues a warning (!! warning), but ASGS continues
#   info   - prints warning from asgs-ling, but as an 'info' line

run_asgslint()
{
  asgs-lint             # assumed to be in PATH
  local lint_err=$?
  local options="strict, prompt, warn, info, or off"
  if [[ $lint_err -ne 0 ]]; then
    case "$LINTSETTING" in
      info) 
        echo "${I} asgs-lint detected FATAL errors on start up. (LINTSETTING set to '$LINTSETTING')"
        echo "${I} ... to halt use 'strict', pause use 'prompt', to notify use 'warn' or 'info'; to silence use 'off'"
      ;;
      off)
        # noop
      ;;
      prompt)
        local default_ans="N"
        echo "${W} asgs-lint detected FATAL errors on start up. (LINTSETTING set to '$LINTSETTING')"
        echo "${I} ... to halt use 'strict', pause use 'prompt', to notify use 'warn' or 'info'; to silence use 'off'"
        read -r -p "Proceed? [y/${default_ans}] " ans
        # Use default if empty
        local ans=${ans:-$default_ans}
        # Normalize to uppercase
        local ans=${ans^^}
        if [[ $ans != "Y" ]]; then
          exit $lint_err 
        fi
      ;;
      warn)
        echo "${W} asgs-lint detected FATAL errors on start up. (LINTSETTING set to '$LINTSETTING')"
        echo "${I} ... to halt use 'strict', pause use 'prompt', to notify use 'warn' or 'info'; to silence use 'off'"
      ;;
      strict)
        echo "${W} asgs-lint detected FATAL errors on start up. (LINTSETTING set to '$LINTSETTING')"
        echo "${I} ... to halt use 'strict', pause use 'prompt', to notify use 'warn' or 'info'; to silence use 'off'"
        exit $lint_err
      ;;
      *)
       echo "${W} '${LINTSETTING}' is not a valid option for 'LINTSETTING'."
        echo "${I} ... to halt use 'strict', pause use 'prompt', to notify use 'warn' or 'info'; to silence use 'off'"
       exit 1
      ;;
    esac
  fi
}

# calls function
run_asgslint
