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

export I="(${CY}info${R})"
export W="(${B}${RD}!! warning${R})"

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
      L|p) export ASGS_LOCAL_DIR=$(readlink -f "${OPTARG}") # get full path
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
  # DO NOT ADD TO THIS LIST MANUALLY ANYMORE, See ./platforms/README
  echo "queenbee       - Queenbee (LONI)"     # qb2
  echo "queenbeeC      - QueenbeeC (LONI)"    # qbC
  echo "supermic       - SuperMIC (LSU HPC)"  # smic
  echo "frontera       - Frontera (TACC)"     # frontera
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
        printf "% -14s - %s%s\n" "$platform" "$about" "$seereadme"
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
  if [ -z "$platform" ]; then
    platform=$default_platform
  fi
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
    read -p "Queue system (<enter> if you don't know) [$QUEUESYS]? " _QUEUESYS
    echo
    if [ -n "$_QUEUESYS" ]; then
      QUEUESYS=$_QUEUESYS
    fi

    read -p "Queue check command (<enter> if you don't know) [$QCHECKCMD]? " _QCHECKCMD
    if [ -n "$_QCHECKCMD" ]; then
      QCHECKCMD=$_QCHECKCMD
    fi
    echo

    read -p "Queue submit command (<enter> if you don't know) [$SUBMITSTRING]? " _SUBMITSTRING
    if [ -n "$_SUBMITSTRING" ]; then
      SUBMITSTRING=$_SUBMITSTRING
    fi
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

  read -p "Does the above system information look correct? [Y/n] " _looks_correct
  if [[ -n "$_looks_correct" && "${_looks_correct^^}" != Y ]]; then
    echo Set up aborted. Ensure platform is supported, then try again. exiting...
    exit
  fi

  # skip if not a git repo (can't find ./.git)
  if [[ -z "$ADCIRCLIVE" ]]; then
    echo
    read -p "If you'd like to checkout a specific branch, specify here. [<enter> if you don't know] " repo
    if [ -z "$repo" ]; then
      repo=current
    fi
  fi

  # skip if not a git repo (can't find ./.git)
  if [[ -z "$ADCIRCLIVE" && "$repo" != "." && ${repo,,} != "current" ]]; then
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
  elif [[ -z "$ADCIRCLIVE" ]]; then
    echo
    echo "skipping 'git checkout', branch untouched ..."
  fi
  echo

  read -p "Which compiler 'family' would you like to use, 'gfortran', 'intel', 'intel-oneapi'? [$_compiler] " compiler
  if [ -z "$compiler" ]; then
    compiler=$_compiler
  fi
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
  read -p "Name of this installation? [\"$_default_profile\"] " profile
fi

if [ -z "$profile" ]; then
  profile=$_default_profile
fi

if [[ -z "$BATCH" && -z "$ADCIRCLIVE" ]]; then
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

cmd="cloud/general/asgs-brew.pl --install-path=$installpath --asgs-profile=$profile --compiler=$compiler --machinename=$platform --home=${ASGS_HOME} --tmpdir=${ASGS_TMPDIR} ${PLATFORM_INIT_OPT} ${EXTRA_ASGSBREW_OPTS}"

if [ -z "$BATCH" ]; then
  echo
  echo $cmd
  echo
  _run=y
  read -p "Run command above, [Y/n]? " run
  if [[ -z "$run" ]]; then
    run=$_run
  fi
fi

rm -v $HOME/bin/asgsh       2> /dev/null
rm -v $HOME/bin/update-asgs 2> /dev/null

# creates a script that is basically a wrapper around the asgs-brew.pl
# command that results from the use of this guide installation
if [[ "${run,,}" == "y" || -n "$BATCH" ]]; then
  scriptdir=$(pwd)
  #
  base_cmd="cloud/general/asgs-brew.pl --install-path=$installpath --asgs-profile=$profile --compiler=$compiler --machinename=$platform --home=${ASGS_HOME} --tmpdir=${ASGS_TMPDIR} ${PLATFORM_INIT_OPT}"
  full_command=$scriptdir/$base_cmd
  echo Writing wrapper ASGSH Shell command wrapper "'update-asgs'" for use later...
  echo "#!/usr/bin/env bash"                             > ./update-asgs
  echo "#---automatically generated by $0 - rename if you don't wish to lose it next tim $0 is run---#" >> ./update-asgs
  if [ -n "$ASGS_LOCAL_DIR" ]; then
    echo "export ASGS_LOCAL_DIR=${ASGS_LOCAL_DIR}"      >> ./update-asgs
  fi
  if [ -n "$PLATFORM_INIT" ]; then
    echo "export PLATFORM_INIT=${PLATFORM_INIT}"        >> ./update-asgs
  fi
  if [ -n "$WORK" ]; then
    echo "export WORK=${WORK}"        >> ./update-asgs
  fi
  if [ -n "$SCRATCH" ]; then
    echo "export SCRATCH=${SCRATCH}"        >> ./update-asgs
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
  echo "read -p \"proceed? [Y/n] \" run"                  >> ./update-asgs
  echo "if [[ -z "\$run" || \$run == \"Y\" ]]; then"      >> ./update-asgs
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
  echo "If you changed your mind or made mistake, re-run init-asgs.sh (the following "
  echo "command is designed to be executed by this wrapper script):"
  echo
  printf "\t$cmd\n"
  echo
fi

# For ADCIRC pro's who up their game with ADCIRC Live (c) #
#   via https://tools.adcirc.live                         #
if [[ -d $SCRIPTDIR/adcirclive/etc ]]; then
  echo "export PATH=$SCRIPTDIR/adcirclive/bin:\$PATH"                             > adcirclive/etc/bashrc
  echo "export ADCIRCLIVE_ROOT=$SCRIPTDIR/adcirclive"                            >> adcirclive/etc/bashrc
  echo "export ADCIRCLIVE_DEFAULT_PROFILE=$SCRIPTDIR/profiles/$profile"          >> adcirclive/etc/bashrc
  echo "alias adl='adcirclive'"                                                  >> adcirclive/etc/bashrc
  echo "alias adcl='pushd \$ADCIRCLIVE_ROOT; dirs -c'"                           >> adcirclive/etc/bashrc
  echo "alias sd='pushd \$ADCIRCLIVE_ROOT/..; dirs -c'"                          >> adcirclive/etc/bashrc
  echo "alias asgs='pushd \$ADCIRCLIVE_ROOT/..; dirs -c'"                        >> adcirclive/etc/bashrc

  ln -sf $SCRIPTDIR/asgsh $SCRIPTDIR/adcirclive/bin
  cp adcirclive/etc/bashrc adcirclive/etc/initial-bashrc

  echo "adcirclive build v55.02"                                                 >> adcirclive/etc/initial-bashrc
  echo                                                                           >> adcirclive/etc/initial-bashrc 
  echo "adcirclive load adcirc"                                                  >> adcirclive/etc/initial-bashrc
  echo                                                                           >> adcirclive/etc/initial-bashrc
  echo "adcirclive verify adcirc"                                                >> adcirclive/etc/initial-bashrc
  echo                                                                           >> adcirclive/etc/initial-bashrc
  echo "cat<<EOF"                                                                >> adcirclive/etc/initial-bashrc
  echo "ADCIRC v55.02 and the ADCIRC Live (c) cli is now installed and ready:"   >> adcirclive/etc/initial-bashrc
  echo                                                                           >> adcirclive/etc/initial-bashrc
  echo "If you have any questions, please email us at help@support.adcirc.live"  >> adcirclive/etc/initial-bashrc
  echo                                                                           >> adcirclive/etc/initial-bashrc
  echo "The following line was added to your ~/.bashrc file:"                    >> adcirclive/etc/initial-bashrc
  echo                                                                           >> adcirclive/etc/initial-bashrc
  echo "  source $SCRIPTDIR/adcirclive/adcirclive/etc/bashrc"                    >> adcirclive/etc/initial-bashrc
  echo                                                                           >> adcirclive/etc/initial-bashrc
  echo "To get started, type the command                    "                    >> adcirclive/etc/initial-bashrc
  echo                                                                           >> adcirclive/etc/initial-bashrc
  echo "  adcirclive help                                   "                    >> adcirclive/etc/initial-bashrc
  echo                                                                           >> adcirclive/etc/initial-bashrc
  echo "EOF"                                                                     >> adcirclive/etc/initial-bashrc
fi
