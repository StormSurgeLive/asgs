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

I="(info)"
W="(!! warning)"

# preflight checkes and warnings
if [ -n "$_ASGSH_PID" ]; then
  echo
  echo "$0 can't be run inside of asgsh."
  echo
  exit 1
fi

while getopts "bmp:x:" optname; do
   case $optname in
      b) BATCH=1
         ;;
      m) # invokes minimal asgs-brew.pl command using "--run-steps setup-env"
         if [ -z "${EXTRA_ASGSBREW_OPTS}" ]; then
           EXTRA_ASGSBREW_OPTS="--run-steps setup-env"
         else
           echo "-m can't be used with -x"
           exit 1
         fi
         ;;
      p) export ASGS_LOCAL_DIR=$(readlink -f "${OPTARG}") # get full path
         ;;
      x) # add extra arbitrary options to asgs-brew.pl command
         if [ -z "${EXTRA_ASGSBREW_OPTS}" ]; then
           EXTRA_ASGSBREW_OPTS=${OPTARG}
         else
           echo "-x can't be used with -m"
           exit 1
         fi
         ;;
   esac
done

echo $ASGS_LOCAL_DIR

# can tweak ASGS_TMPDIR default if TMPDIR is set in the environment
ASGS_HOME=${ASGS_HOME:-$(pwd)}
ASGS_TMPDIR=${TMPDIR:-$ASGS_HOME/tmp}

# DO NOT ADD TO THIS LIST MANUALLY ANYMORE, See ./platforms/README
echo "hatteras       - Hatteras (RENCI)"    # ht4
echo "queenbee       - Queenbee (LONI)"     # qb2
echo "queenbeeC      - QueenbeeC (LONI)"    # qbC
echo "supermic       - SuperMIC (LSU HPC)"  # smic
echo "lonestar5      - Lonestar (TACC)"     # ls5
echo "stampede2      - Stampede2 (TACC)"    # stampede2
echo "frontera       - Frontera (TACC)"     # frontera
echo "desktop        - desktop"
echo "desktop-serial - desktop-serial"
echo "poseidon       - Poseidon"
echo "docker         - Docker container environment"
echo "vagrant        - vagrant/virtual box (local workstation)"

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
      if [ -e ./platforms/${platform}/about.txt ]; then
        about=$(cat ./platforms/${platform}/about.txt | sed 's/\n//g')
      fi
      printf "% -14s - %s\n" "$platform" "$about"
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
      if [ -e ${ASGS_LOCAL_DIR}/platforms/${platform}/about.txt ]; then
        about=$(cat ${ASGS_LOCAL_DIR}/platforms/${platform}/about.txt | sed 's/\n//g')
      fi
      printf "% -14s - %s\n" "$platform" "(custom) $about"
    fi
  done
fi

default_platform=$(./bin/guess platform)
if [ -n "$default_platform" ]; then
  _default_platform=" [$default_platform]"
fi

if [ -z "$BATCH" ]; then
  echo
  read -p "Which platform environment would you like to use for ASGS bootstrapping?$_default_platform " platform
fi

if [ -z "$platform" ]; then
  platform=$default_platform
fi

# DO NOT ADD TO THIS LIST MANUALLY ANYMORE, See ./platforms/README
# catch WORK and SCRATCH as early as possible
DEFAULT_COMPILER=intel
case "$platform" in
  vagrant|desktop|desktop-serial)
    WORK=${WORK:-$ASGS_HOME}
    SCRATCH=${SCRATCH:-$ASGS_HOME}
    DEFAULT_COMPILER=gfortran
    ;;
  docker)
    WORK=${WORK:-/work}
    SCRATCH=${SCRATCH:-/scratch}
    DEFAULT_COMPILER=gfortran
    ;;
  hatteras)
    WORK=${WORK:-$ASGS_HOME}
    SCRATCH=${SCRATCH:-"/projects/$USER"}
    ;;
  queenbee|queenbeeC|supermic)
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

export WORK
export SCRATCH
export ASGS_TMPDIR

if [[ -z "$platform" && -z "$default_platform" ]]; then
  echo "A platform must be selected."
  exit 1
elif [[ -z "$platform" && -n "$default_platform" ]]; then
  platform=$default_platform
fi

echo
echo "Platform name       : $platform"
if [ -n "$ASGS_LOCAL_DIR" ]; then
  echo "Local Site Dir      : $ASGS_LOCAL_DIR"
fi
if [ -n "$PLATFORM_INIT" ]; then
  echo "Platform Init       : $PLATFORM_INIT"
fi
echo "SCRIPTDIR           : $(pwd)"
echo "ASGS HOME           : $ASGS_HOME"
echo "WORK                : $WORK"
echo "SCRATCH             : $SCRATCH"
echo "ASGS Build directory: $ASGS_TMPDIR"
echo "Default Compiler    : $DEFAULT_COMPILER"
echo

# Note: if BATCH is set, then "." is assumed and no "git checkout" is performed
if [ -z "$BATCH" ]; then
  read -p "Does the above system information look correct? [Y/n] " _looks_correct
  if [[ -n "$_looks_correct" && "${_looks_correct^^}" != Y ]]; then
    echo Set up aborted. Ensure platform is supported, then try again. exiting...
    exit
  fi
  echo
  read -p "If you'd like to checkout a specific branch, specify here. [<enter> if you don't know] " repo

  if [ -z "$repo" ]; then
    repo=current
  fi

  if [[ "$repo" != "." && ${repo,,} != "current" ]]; then
    git checkout $repo 2> /dev/null
    if [ $? -gt 0 ]; then
     echo
     echo error checking out $repo
     read -p "skip checkout and proceed? [y/N] " skip
     if [[ -z "$skip" || "${skip^^}" = "N" ]]; then
       echo exiting ...
       exit
     fi
    fi
  else
    echo
    echo "skipping 'git checkout', branch untouched ..."
  fi
fi

_compiler=$DEFAULT_COMPILER
if [ -z "$BATCH" ]; then
  echo
  read -p "Which compiler 'family' would you like to use, 'gfortran' or 'intel'? [$_compiler] " compiler
fi

if [ -z "$compiler" ]; then
  compiler=$_compiler
fi

if [[ "$compiler" != 'gfortran' && "$compiler" != "intel" ]]; then
  echo
  echo "'$compiler' is not valid, compiler must be 'gfortran' or 'intel'"
  exit 1
fi

_default_profile=default-$(basename "$(pwd)")
if [ -z "$BATCH" ]; then
  echo
  read -p "Name of this installation? [\"$_default_profile\"] " profile
fi

if [ -z "$profile" ]; then
  profile=$_default_profile
fi

if [ -z "$BATCH" ]; then
  if [ -e $ASGS_HOME/profiles/$profile ]; then
    echo
    read -p "${W} it appears an '$profile' profile already exists from a previous installation. Is it okay to proceed and overwrite? [y/N] " overwrite
    if [[ -z "$overwrite" || "${overwrite^^}" == "N" ]]; then
      echo exiting ...
      exit
    fi
  fi
fi

_default_installpath=$ASGS_HOME/opt
if [ -z "$BATCH" ]; then
  echo
  read -p "Install base for libraries and some utilities? [$_default_installpath] " installpath
fi

if [ -z "$installpath" ]; then
  installpath=$_default_installpath
fi

if [ -z "$BATCH" ]; then
  if [ -d "$installpath/$profile" ]; then
    echo
    read -p "${W} '$installpath/$profile' exists. To prevent overwriting existing files, would you like to quit and do the needful? [Y/n] " quit
    if [[ -z "$quit" || "${quit^^}" == Y ]]; then
      echo exiting ...
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

cmd="cloud/general/asgs-brew.pl --install-path=$installpath --asgs-profile=$profile --compiler=$compiler --machinename=$platform --home=${ASGS_HOME} ${EXTRA_ASGSBREW_OPTS}"

if [ -z "$BATCH" ]; then
  echo
  echo $cmd
  echo
  read -p "Run command above, y/N? [N] " run
fi

rm -v $HOME/bin/asgsh       2> /dev/null
rm -v $HOME/bin/update-asgs 2> /dev/null

# creates a script that is basically a wrapper around the asgs-brew.pl
# command that results from the use of this guide installation
if [[ "${run,,}" == "y" || -n "$BATCH" ]]; then
  scriptdir=$(pwd)
  #
  base_cmd="cloud/general/asgs-brew.pl --install-path=$installpath --asgs-profile=$profile --compiler=$compiler --machinename=$platform --home=${ASGS_HOME}"
  full_command=$scriptdir/$base_cmd
  echo Writing wrapper ASGSH Shell command wrapper "'update-asgs'" for use later...
  echo "#!/usr/bin/env bash"                               > ./update-asgs
  echo "#---automatically generated by $0 - rename if you don't wish to lose it next tim $0 is run---#" >> ./update-asgs
  if [ -n "$ASGS_LOCAL_DIR" ]; then
    echo "export ASGS_LOCAL_DIR=${ASGS_LOCAL_DIR}"        >> ./update-asgs
  fi
  if [ -n "$PLATFORM_INIT" ]; then
    echo "export PLATFORM_INIT=${PLATFORM_INIT}"        >> ./update-asgs
  fi
  echo "if [ -n \"\$_ASGSH_PID\" ]; then"                 >> ./update-asgs
  echo "  echo This needs to be run outside of the asgsh environment" >> ./update-asgs
  echo "  echo exiting ..."                               >> ./update-asgs
  echo "  exit"                                           >> ./update-asgs
  echo "fi"                                               >> ./update-asgs
  echo                                                    >> ./update-asgs
  echo "if [ -z \"\$@\" ]; then"                          >> ./update-asgs
  echo "  echo \"You didn't provide additional arguments (e.g., --update-shell)\"" >> ./update-asgs
  echo "  echo See documentation for more information"    >> ./update-asgs
  echo "  echo exiting ..."                               >> ./update-asgs
  echo "exit 1"                                           >> ./update-asgs
  echo "fi"                                               >> ./update-asgs
  echo "echo"                                             >> ./update-asgs
  echo "echo you are about to run the following command:" >> ./update-asgs
  echo "echo"                                             >> ./update-asgs
  echo "echo \"  $full_command \$@\""                     >> ./update-asgs
  echo "echo"                                             >> ./update-asgs
  echo "read -p \"proceed? [y] \" run"                    >> ./update-asgs
  echo "if [[ -z "\$run" || \$run = \"y\" ]]; then"       >> ./update-asgs
  echo "  cd $scriptdir"                                  >> ./update-asgs
  echo "  $full_command \$@"                              >> ./update-asgs
  echo "else"                                             >> ./update-asgs
  echo "  echo"                                           >> ./update-asgs
  echo "  echo exiting ..."                               >> ./update-asgs
  echo "fi"                                               >> ./update-asgs
  chmod 700 ./update-asgs
  # run command
  $cmd
else
  echo "You chose '$run', so the wizard has exited without executing the install command"
  echo
  echo "If you changed your mind or made mistake, copy/paste the following and hit <enter>,"
  echo
  printf "\t$cmd\n"
  echo
fi
