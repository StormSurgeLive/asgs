#!/usr/bin/env bash

# load environment related things like an ADCIRC environment or saved ASGS environment
load() {
  CHOICES=();
  case "${1}" in
    adcirc)
      if [ -z "${2}" ]; then
        for i in $(list adcirc | awk '{print $2}'); do
          CHOICES+=("$i")
        done
        C=${#CHOICES[@]}
        if [ $C -eq 0 ]; then
          echo "No versions of ADCIRC are available. Run 'build adcirc' to install one."
          return
        elif [ $C -eq 1 ]; then
          __ADCIRC_BUILD=$(list adcirc | awk '{print $2}')
        elif [ $C -gt 1 ]; then
          list adcirc
          read -p "Choose a number (1-$C) or name from above: " _SELECTION
          _isnum=$(_is_a_num $_SELECTION)
          if [[ $_isnum -gt -1 && $_isnum -le $C ]]; then
            __ADCIRC_BUILD=${CHOICES[$(($_isnum-1))]}
          elif [ -n "$_SELECTION" ]; then
            __ADCIRC_BUILD=$_SELECTION
          else
            echo "A valid selection must be made to proceed."
            return
          fi
        fi
      else
        __ADCIRC_BUILD=${2}
      fi
      echo "${I} loading ADCIRC build, '$__ADCIRC_BUILD'."
      if [ -e "${ADCIRC_META_DIR}/${__ADCIRC_BUILD}" ]; then
          # source it
          . ${ADCIRC_META_DIR}/${__ADCIRC_BUILD}
          echo "${I} prepending ADCIRCDIR and SWANDIR to PATH"
          echo "${I}   + $ADCIRCDIR"
          echo "${I}   + $SWANDIR"
          PATH=${SWANDIR}:${ADCIRCDIR}:${PATH}
          export PATH
      else
          echo "ADCIRC build, '$__ADCIRC_BUILD' does not exist. Use 'list adcirc' to see a which ADCIRCs are available to load"
      fi
      ;;
  esac
}

for A in $(list adcirc | awk '{print $2}'); do
  load adcirc "$A"
  verify adcirc
  ERR=$?
  if [ $ERR != 0 ]; then
    echo error verifying "$A"
    exit $ERR
  fi 
done
