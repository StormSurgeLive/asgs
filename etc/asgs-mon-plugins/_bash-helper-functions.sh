#!/usr/bin/env bash
#--------------------------------------------------------------------------
# etc/asgs-mon-plugins/000-asgs_main-pid-check
#--------------------------------------------------------------------------
# Copyright(C) 2024 Brett Estrade
# Copyright(C) 2024 Jason Fleming
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
#--------------------------------------------------------------------------

# Function library for ASGS Monitor plugins
BK=$(tput setaf 0)
RD=$(tput setaf 1)
GR=$(tput setaf 2)
YW=$(tput setaf 3)
BL=$(tput setaf 4)
MG=$(tput setaf 5)
CY=$(tput setaf 6)
WH=$(tput setaf 6)
R=$(tput sgr0)
B=$(tput bold)

get_instancefile() {
  local statefile=$1
  if [[ -e "$statefile" ]]; then
    source "$statefile"
    echo "$RUNDIR/status/asgs.instance.status.json"
  fi
}
get_hookfile() {
  local statefile=$1
  if [[ -e "$statefile" ]]; then
    source "$statefile"
    echo "$RUNDIR/status/hook.status.json"
  fi
}
