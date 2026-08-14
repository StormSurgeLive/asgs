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

#
# D O  N O T   M A N U A L L Y  A D D  P L A T F O R M S  H E R E  A N Y M O R E
# See ./platforms/README
#

# set terminal color codes
if [ -t 1 ] && [ -n "${TERM:-}" ] && [ "$TERM" != "dumb" ] && command -v tput >/dev/null 2>&1; then
  BK=$(tput setaf 0)
  RD=$(tput setaf 1)
  GR=$(tput setaf 2)
  YW=$(tput setaf 3)
  BL=$(tput setaf 4)
  MG=$(tput setaf 5)
  CY=$(tput setaf 6)
  WH=$(tput setaf 7)
   R=$(tput sgr0)
   B=$(tput bold)
else
  BK=
  RD=
  GR=
  YW=
  BL=
  MG=
  CY=
  WH=
   R=
   B=
fi

export I="(${CY}info${R})"
export W="(${B}${RD}!! warning${R})"

_prompt_value()
{
  local prompt=$1
  local default_value=${2:-}
  local answer

  if [[ -n "$default_value" ]]; then
    read -r -p "${prompt} [default: ${default_value}]: " answer
    printf '%s\n' "${answer:-$default_value}"
  else
    read -r -p "${prompt}: " answer
    printf '%s\n' "$answer"
  fi
}

_prompt_yes_no()
{
  local prompt=$1
  local default_answer=${2,,}
  local choices answer

  case "$default_answer" in
    y|yes)
      default_answer=yes
      choices='[Y/n]'
      ;;
    n|no)
      default_answer=no
      choices='[y/N]'
      ;;
    *)
      echo "(fatal) invalid default answer '$2' for prompt: $prompt" >&2
      return 2
      ;;
  esac

  while true; do
    read -r -p "${prompt} ${choices}: " answer
    answer=${answer:-$default_answer}

    case "${answer,,}" in
      y|yes) return 0 ;;
      n|no)  return 1 ;;
      *) echo "Please answer 'yes' or 'no'." ;;
    esac
  done
}

SCRIPTDIR=$(pwd)

# check to see if we're installing from a .zip (no .git directory will exist)
[[ ! -d "./.git" ]] && ADCIRCLIVE=1

# preflight checkes and warnings
if [ -n "$_ASGSH_PID" ]; then
  echo
  echo "$0 can't be run inside of asgsh."
  echo
  exit 1
fi

DEFAULT_COMPILER=intel
while getopts "Abc:L:mp:x:" optname; do
   case $optname in
      b) BATCH=1
         ;;
      c) # update DEFAULT_COMPILER for use with -b, really
	 FORCE_DEFAULT_COMPILER=${OPTARG}
	 ;;
      L|p)
         # Fail early so a typo in -L/-p does not silently fall back to repo assets.
         if [[ ! -d "${OPTARG}" ]]; then
           echo "${W} Local assets directory specified with -L/-p does not exist: ${OPTARG}" >&2
           exit 1
         fi
         export ASGS_LOCAL_DIR=$(readlink -f "${OPTARG}") # get full path
         ;;
      x) # add extra arbitrary options to asgs-brew.pl command
         if [ -z "${EXTRA_ASGSBREW_OPTS}" ]; then
           EXTRA_ASGSBREW_OPTS=${OPTARG}
         else
           echo "-x can't be used with -m"
           exit 1
         fi
         ;;
      A) # invokes minimal asgs-brew.pl to support ADCIRC
         if [ -z "${EXTRA_ASGSBREW_OPTS}" ]; then
           EXTRA_ASGSBREW_OPTS="--run-steps setup-env,openmpi,hdf5,netcdf4"
         else
           echo "-A can't be used with -x"
           exit 1
         fi
         ;;
      m) # invokes minimal asgs-brew.pl command using "--run-steps setup-env"
         if [ -z "${EXTRA_ASGSBREW_OPTS}" ]; then
           EXTRA_ASGSBREW_OPTS="--run-steps setup-env"
         else
           echo "-m can't be used with -x"
           exit 1
         fi
         ;;
   esac
done

# can tweak ASGS_TMPDIR default if TMPDIR is set in the environment
ASGS_HOME=${ASGS_HOME:-$(pwd)}
ASGS_TMPDIR=${TMPDIR:-$ASGS_HOME/tmp}

if [ -z "$BATCH" ]; then
  echo "Available platform environments:"
  # DO NOT ADD TO THIS LIST MANUALLY ANYMORE, See ./platforms/README
  printf "  %-14s - %s\n" "queenbeeC" "QueenbeeC (LONI)"    # qbC
  printf "  %-14s - %s\n" "supermic"  "SuperMIC (LSU HPC)"  # smic
  printf "  %-14s - %s\n" "frontera"  "Frontera (TACC)"     # frontera
fi

# Preferred way to add platforms now ... load platforms from $SCRIPTDIR/platforms/
# See ./platforms/README
declare -A _PLATFORM_INIT=()
_PLATFORMS=()
if [ -d "./platforms" ]; then
  FOUND_LOCAL=0
  for platform in $(find ./platforms/ -mindepth 1 -maxdepth 1 -type d -exec basename {} \;); do
    _PLATFORMS+=($platform);
  done
  for platform in "${_PLATFORMS[@]}"; do
    if [ -e ./platforms/${platform}/init.sh ]; then
      about='(repo defined)'
      _PLATFORM_INIT[$platform]=$(readlink -f "./platforms/${platform}/init.sh")
      about=
      if [ -e ./platforms/${platform}/about.txt ]; then
        about=$(cat ./platforms/${platform}/about.txt | sed 's/\n//g')
      fi
      seereadme=
      if [ -e ./platforms/${platform}/README ]; then
        seereadme=" (see, ./platforms/$platform/README)"
      fi
      if [ -z "$BATCH" ]; then
        printf "  %-14s - %s%s\n" "$platform" "$about" "$seereadme"
      fi
    fi
  done
  unset _PLATFORMS
fi

_PLATFORMS=()
if [[ -n "${ASGS_LOCAL_DIR}" &&  -d "${ASGS_LOCAL_DIR}/platforms" ]]; then
  FOUND_LOCAL=0
  for platform in $(find ${ASGS_LOCAL_DIR}/platforms/ -mindepth 1 -maxdepth 1 -type d -exec basename {} \;); do
    _PLATFORMS+=($platform);
  done
  for platform in "${_PLATFORMS[@]}"; do
    if [ -e "${ASGS_LOCAL_DIR}/platforms/${platform}/init.sh" ]; then
      _PLATFORM_INIT[$platform]="${ASGS_LOCAL_DIR}/platforms/${platform}/init.sh"
      about=
      if [ -e ${ASGS_LOCAL_DIR}/platforms/${platform}/about.txt ]; then
        about=$(cat ${ASGS_LOCAL_DIR}/platforms/${platform}/about.txt | sed 's/\n//g')
      fi
      if [ -z "$BATCH" ]; then
        printf "  %-14s - %s\n" "$platform" "(custom) $about"
      fi
    fi
  done
fi

default_platform=$(./bin/guess platform)
if [ -z "$BATCH" ]; then
  echo
  platform=$(_prompt_value "Select a platform environment for ASGS bootstrapping" "$default_platform")
else
  platform=$default_platform
fi

export WORK
export SCRATCH
export ASGS_TMPDIR
export QUEUESYS
export QCHECKCMD
export SUBMITSTRING

# DO NOT ADD TO THIS LIST MANUALLY ANYMORE, See ./platforms/README
# catch WORK and SCRATCH as early as possible
case "$platform" in
  queenbeeC|supermic)
    WORK=${WORK:-"/work/$USER"}
    SCRATCH=${SCRATCH:-"/scratch/$USER"}
    ;;
  frontera|lonestar5|stampede2)
    # WORK and SCRATCH assumed to be set in default env
    if [[ -z "$WORK" || -z "$SCRATCH" ]]; then
      echo "'WORK' and 'SCRATCH' are expected to be part of the default TACC environment. Please fix this."
      exit 1
    fi
    ;;
  *) # fall back to new method of getting platform defaults for building
     if [ -e "${_PLATFORM_INIT[$platform]}" ]; then
       export PLATFORM_INIT=$(readlink -f "${_PLATFORM_INIT[$platform]}")
       source "$PLATFORM_INIT"
     else
      echo "Can't find 'init.sh' for '$platform' or Unknown defaults for platform '$platform', using "$ASGS_HOME" as 'WORK' and 'SCRATCH' directories..."
      WORK=${WORK:-$ASGS_HOME}
      SCRATCH=${SCRATCH:-$ASGS_HOME}
      DEFAULT_COMPILER=gfortran
    fi
    ;;
esac

if [[ -z "$platform" && -z "$default_platform" ]]; then
  echo "A platform must be selected."
  exit 1
elif [[ -z "$platform" && -n "$default_platform" ]]; then
  platform=$default_platform
fi

_compiler=$DEFAULT_COMPILER

# force default from -c COMPILER
if [ -n "$FORCE_DEFAULT_COMPILER" ]; then
  _compiler=$FORCE_DEFAULT_COMPILER
fi
PLATFORM_INIT_OPT=
if [ -n "$PLATFORM_INIT" ]; then
  PLATFORM_INIT_OPT="--platform-init $PLATFORM_INIT"
fi

# Note: if BATCH is set, then "." is assumed and no "git checkout" is performed
if [ -z "$BATCH" ]; then
  # offer some tweaking of the queue system, this is the biggest
  # difference when considering environments of the same OS (e.g., RHEL or related)
  if [ "$QSYSASK" == "YES" ]; then
    QUEUESYS=$(_prompt_value "Queue system" "$QUEUESYS")
    QCHECKCMD=$(_prompt_value "Queue check command" "$QCHECKCMD")
    SUBMITSTRING=$(_prompt_value "Queue submit command" "$SUBMITSTRING")
    echo
  fi

  echo
  echo "Platform name       : $platform"
  if [ -n "$ASGS_LOCAL_DIR" ]; then
    echo "Local Site Dir      : $ASGS_LOCAL_DIR"
  fi
  if [ -n "$PLATFORM_INIT" ]; then
    echo "Platform Init       : $PLATFORM_INIT"
  fi
  echo "SCRIPTDIR           : $SCRIPTDIR"
  echo "ASGS HOME           : $ASGS_HOME"
  echo "WORK                : $WORK"
  echo "SCRATCH             : $SCRATCH"
  echo "ASGS build directory: $ASGS_TMPDIR"
  if [ "$QSYSASK" == "YES" ]; then
    echo "Batch queue system  : $QUEUESYS"
    echo "Batch queue check   : $QCHECKCMD"
    echo "Batch queue submit  : $SUBMITSTRING"
  fi
  echo

  if ! _prompt_yes_no "Does the above system information look correct?" yes; then
    echo "Set up aborted. Ensure the platform is supported, then try again."
    exit
  fi

  # skip if not a git repo (can't find ./.git)
  if [[ -z "$ADCIRCLIVE" ]]; then
    echo
    repo=$(_prompt_value "Git branch to check out ('current' leaves it unchanged)" "current")
  fi

  # skip if not a git repo (can't find ./.git)
  if [[ -z "$ADCIRCLIVE" && "$repo" != "." && ${repo,,} != "current" ]]; then
    git checkout $repo 2> /dev/null
    if [ $? -gt 0 ]; then
     echo
     echo "Error checking out '$repo'."
     if ! _prompt_yes_no "Skip checkout and proceed?" no; then
       echo "Exiting."
       exit
     fi
    fi
  elif [[ -z "$ADCIRCLIVE" ]]; then
    echo
    echo "skipping 'git checkout', branch untouched ..."
  fi
  echo

  compiler=$(_prompt_value "Compiler family (gfortran, intel, or intel-oneapi)" "$_compiler")
else
  compiler=$_compiler # BATCH is set here
fi

if [[ "$compiler" != 'gfortran' && "$compiler" != "intel" && "$compiler" != "intel-oneapi" ]]; then
  echo
  echo "'$compiler' is not valid, compiler must be 'gfortran', 'intel', 'intel-oneapi'"
  exit 1
fi

_default_profile=default-$(basename "$(pwd)")
if [[ -z "$BATCH" && -z "$ADCIRCLIVE" ]]; then
  echo
  profile=$(_prompt_value "Installation name" "$_default_profile")
fi

if [ -z "$profile" ]; then
  profile=$_default_profile
fi

if [[ -z "$BATCH" && -z "$ADCIRCLIVE" ]]; then
  if [ -e $ASGS_HOME/profiles/$profile ]; then
    echo
    if ! _prompt_yes_no "${W} Profile '$profile' already exists. Proceed and overwrite it?" no; then
      echo "Exiting."
      exit
    fi
  fi
fi

_default_installpath=$ASGS_HOME/opt
if [ -z "$BATCH" ]; then
  echo
  installpath=$(_prompt_value "Install base for libraries and utilities" "$_default_installpath")
fi

if [ -z "$installpath" ]; then
  installpath=$_default_installpath
fi

if [ -z "$BATCH" ]; then
  if [ -d "$installpath/$profile" ]; then
    echo
    if ! _prompt_yes_no "${W} '$installpath/$profile' exists. Continue anyway?" no; then
      echo "Exiting."
      exit
    fi
  fi
fi

if [ -e $HOME/bin/asgsh ]; then
  echo "${W} '$HOME/bin/asgsh' has been detected. Use of this path is deprecated and will be removed."
fi
if [ -e $HOME/bin/update-asgs ]; then
  echo "${W} $HOME/bin/update-asgs' has been detected. Use of this path is deprecated and will be removed."
fi

cmd="cloud/general/asgs-brew.pl --install-path=$installpath --asgs-profile=$profile --compiler=$compiler --machinename=$platform --home=${ASGS_HOME} --tmpdir=${ASGS_TMPDIR} ${PLATFORM_INIT_OPT} ${EXTRA_ASGSBREW_OPTS}"

if [ -z "$BATCH" ]; then
  echo
  echo $cmd
  echo
  if _prompt_yes_no "Run the command above?" yes; then
    run=y
  else
    run=n
  fi
fi

rm -v $HOME/bin/asgsh       2> /dev/null
rm -v $HOME/bin/update-asgs 2> /dev/null

# creates a script that is basically a wrapper around the asgs-brew.pl
# command that results from the use of this guide installation
if [[ "${run,,}" == "y" || -n "$BATCH" ]]; then
  scriptdir=$(pwd)
  echo "Writing ASGSH shell command wrapper 'update-asgs' for later use..."
  {
    cat <<EOF
#!/usr/bin/env bash
# Automatically generated by $0.
# Rename this file if you do not want it replaced the next time $0 is run.
EOF

    if [ -n "$ASGS_LOCAL_DIR" ]; then
      printf 'export ASGS_LOCAL_DIR=%q\n' "$ASGS_LOCAL_DIR"
    fi
    if [ -n "$PLATFORM_INIT" ]; then
      printf 'export PLATFORM_INIT=%q\n' "$PLATFORM_INIT"
    fi
    if [ -n "$WORK" ]; then
      printf 'export WORK=%q\n' "$WORK"
    fi
    if [ -n "$SCRATCH" ]; then
      printf 'export SCRATCH=%q\n' "$SCRATCH"
    fi

    printf '\nupdate_asgs_root=%q\n' "$scriptdir"
    printf 'asgs_brew_cmd=(\n'
    printf '  %q\n' "$scriptdir/cloud/general/asgs-brew.pl"
    printf '  %q\n' "--install-path=$installpath"
    printf '  %q\n' "--asgs-profile=$profile"
    printf '  %q\n' "--compiler=$compiler"
    printf '  %q\n' "--machinename=$platform"
    printf '  %q\n' "--home=$ASGS_HOME"
    printf '  %q\n' "--tmpdir=$ASGS_TMPDIR"
    if [ -n "$PLATFORM_INIT" ]; then
      printf '  %q\n' "--platform-init"
      printf '  %q\n' "$PLATFORM_INIT"
    fi

    cat <<'EOF'
)

if [[ -n "${_ASGSH_PID:-}" ]]; then
  echo "This needs to be run outside of the asgsh environment."
  echo "Exiting."
  exit 1
fi

if (( $# == 0 )); then
  echo "You didn't provide additional arguments (e.g., --update-shell)."
  echo "See the documentation for more information."
  echo "Exiting."
  exit 1
fi

echo
echo "You are about to run the following command:"
echo
printf '  '
printf '%q ' "${asgs_brew_cmd[@]}" "$@"
printf '\n\n'

while true; do
  read -r -p "Proceed? [Y/n]: " run
  run=${run:-yes}

  case "${run,,}" in
    y|yes)
      cd "$update_asgs_root" || exit 1
      "${asgs_brew_cmd[@]}" "$@"
      exit $?
      ;;
    n|no)
      echo
      echo "Exiting."
      exit 0
      ;;
    *)
      echo "Please answer 'yes' or 'no'."
      ;;
  esac
done
EOF
  } > ./update-asgs
  chmod 700 ./update-asgs
  # run command
  $cmd
else
  echo "You chose '$run', so the wizard has exited without executing the install command"
  echo
  echo "If you changed your mind or made mistake, re-run init-asgs.sh (the following "
  echo "command is designed to be executed by this wrapper script):"
  echo
  printf "\t$cmd\n"
  echo
fi
