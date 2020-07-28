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

BATCH=$1

 guess_platform()
{
  guess=unknown
  if [ "$USER" = vagrant ]; then
    guess=vagrant
  elif [ "$_ASGS_CONTAINER" = "docker" ]; then
    guess=docker
  elif [ 1 -eq $(hostname --fqdn | grep -c ht3) ]; then
    guess=hatteras
  elif [ 1 -eq $(hostname --fqdn | grep -c ht4) ]; then
    guess=hatteras
  elif [ 1 -eq $(hostname --fqdn | grep -c qb1) ]; then
    guess=queenbee
  elif [ 1 -eq $(hostname --fqdn | grep -c qb2) ]; then
    guess=queenbee
  elif [ 1 -eq $(hostname --fqdn | grep -c qbc) ]; then
    guess=queenbeeC
  elif [ 1 -eq $(hostname --fqdn | grep -c smic) ]; then
    guess=supermic
  elif [ 1 -eq $(hostname --fqdn | grep -c ls5) ]; then
    guess=lonestar5
  elif [ 1 -eq $(hostname --fqdn | grep -c stampede2) ]; then
    guess=stampede2
  elif [ 1 -eq $(hostname --fqdn | grep -c frontera) ]; then
    guess=frontera
  fi
  if [ $guess != unknown ]; then
    echo "$guess"
  fi
}

echo "pod            - POD (Penguin)"
echo "hatteras       - Hatteras (RENCI)"    # ht4
echo "supermike      - Supermike (LSU)"
echo "queenbee       - Queenbee (LONI)"     # qb2
echo "queenbeeC      - QueenbeeC (LONI)"    # qbC
echo "supermic       - SuperMIC (LSU HPC)"  # smic
echo "lonestar5      - Lonestar (TACC)"     # ls5
echo "stampede2      - Stampede2 (TACC)"    # stampede2
echo "frontera       - Frontera (TACC)"     # frontera
echo "desktop        - desktop"
echo "desktop-serial - desktop-serial"
echo "poseidon       - Poseidon"
echo "penguin        - Penguin"
echo "rostam         - Rostam"
echo "docker         - Docker container environment"
echo "vagrant        - vagrant/virtual box (local workstation)"

default_platform=$(guess_platform)
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

# catch WORK and SCRATCH as early as possible
DEFAULT_COMPILER=intel
case "$platform" in
  vagrant|desktop|desktop-serial)
    WORK=${WORK:-$HOME}
    SCRATCH=${SCRATCH:-$HOME}
    DEFAULT_COMPILER=gfortran
    ;; 
  docker)
    WORK=${WORK:-/work}
    SCRATCH=${SCRATCH:-/scratch}
    DEFAULT_COMPILER=gfortran
    ;; 
  hatteras)
    WORK=${WORK:-$HOME}
    SCRATCH=${SCRATCH:-"/scratch/$USER"}
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
  *) echo "Unknown defaults for platform '$platform', using "$HOME" as 'WORK' and 'SCRATCH' directories..."
    WORK=${WORK:-$HOME}
    SCRATCH=${SCRATCH:-$HOME}
    DEFAULT_COMPILER=gfortran
    ;;
esac
export WORK
export SCRATCH

if [[ -z "$platform" && -z "$default_platform" ]]; then
  echo "A platform must be selected."
  exit 1
elif [[ -z "$platform" && -n "$default_platform" ]]; then
  platform=$default_platform
fi

echo
echo "Platform name: $platform"
echo "WORK         : $WORK"
echo "SCRATCH      : $SCRATCH"
echo

# Note: if BATCH is set, then "." is assumed and no "git checkout" is performed
if [ -z "$BATCH" ]; then
  read -p "Does the above system information look correct? [Y] " _looks_correct
  if [[ -n "$_looks_correct" && "$_looks_correct" != Y ]]; then
    echo Set up aborted. Ensure platform is supported, then try again. exiting...
    exit
  fi
  echo
  read -p "Which asgs branch would you like to checkout from Github ('.' to skip checkout)? [master] " repo

  if [ -z "$repo" ]; then
    repo=master
  fi
 
  if [ "$repo" != "." ]; then
    git checkout $repo 2> /dev/null
    if [ $? -gt 0 ]; then
     echo error checking out $repo
     read -p "skip checkout and proceed? [n] " skip
     if [[ -z "$skip" || "$skip" = "n" ]]; then
       echo exiting ...
       exit
     fi
    fi
  else
    echo leaving git repo in current state 
  fi
fi

_compiler=$DEFAULT_COMPILER
if [ -z "$BATCH" ]; then
  echo
  read -p "Which compiler family would you like to use, 'gfortran' or 'intel'? [$_compiler] " compiler
fi

if [ -z "$compiler" ]; then
  compiler=$_compiler
fi

if [[ "$compiler" != 'gfortran' && "$compiler" != "intel" ]]; then
  echo "compiler must be 'gfortran' or 'intel'"
  exit 1
fi

_default_installpath=$WORK/opt
if [ -z "$BATCH" ]; then
  echo
  echo "(note: shell variables like \$HOME or \$WORK will not be expanded)?"
  read -p "Where do you want to install libraries and some utilities? [$_default_installpath] " installpath
fi

if [ -z "$installpath" ]; then
  installpath=$_default_installpath
fi

if [ -z "$BATCH" ]; then
  if [ -d "$installpath" ]; then
    echo
    read -p "warning - '$installpath' exists. To prevent overwriting existing files, would you like to quit and do the needful? [y] " quit 
    if [[ -z "$quit" || "$quit" = y ]]; then
      echo exiting ...
      exit 
    fi
  fi
fi

_default_profile=default
if [ -z "$BATCH" ]; then
  echo
  read -p "What is a short name you'd like to use to name the asgsh profile associated with this installation? [\"$_default_profile\"] " profile
fi

if [ -z "$profile" ]; then
  profile=$_default_profile
fi

if [ -z "$BATCH" ]; then
  if [ -e $HOME/.asgs/default ]; then
    echo
    read -p "warning - it appears an 'default' profile already exists from a previous installation. Is it okay to proceed and overwrite? [n] " overwrite
    if [[ -z "$overwrite" || "$overwrite" = "no" ]]; then
      echo exiting ...
      exit
    fi
  fi
fi

cmd="cloud/general/asgs-brew.pl --install-path=$installpath --asgs-profile=$profile --compiler=$compiler --machinename=$platform"

if [ -z "$BATCH" ]; then
  echo
  echo $cmd
  echo
  read -p "Run command above, y/N? [N] " run
fi

# creates a script that is basically a wrapper around the asgs-brew.ps
# command that results from the use of this guide installation
if [[ "$run" = "y" || -n "$BATCH" ]]; then
  mkdir $HOME/bin 2> /dev/null
  scriptdir=$(pwd)
  full_command=$scriptdir/$cmd
  echo Writing wrapper ASGSH Shell command wrapper "'asgs-update'" for use later...
  echo "#!/usr/bin/env bash"                               > ./update-asgsh
  echo "#---automatically generated by $0 - rename if you don't wish to lose it next tim $0 is run---#" >> ./update-asgsh
  echo "if [ -n \"\$_ASGSH_PID\" ]; then"                 >> ./update-asgsh
  echo "  echo This needs to be run outside of the asgsh environment" >> ./update-asgsh
  echo "  echo exiting ..."                               >> ./update-asgsh
  echo "  exit"                                           >> ./update-asgsh
  echo "fi"                                               >> ./update-asgsh
  echo                                                    >> ./update-asgsh
  echo "if [ -z \"\$@\" ]; then"                          >> ./update-asgsh
  echo "  echo \"You didn't provide additional arguments (e.g., --update-shell)\"" >> ./update-asgsh
  echo "  echo See documentation for more information"    >> ./update-asgsh
  echo "  echo exiting ..."                               >> ./update-asgsh
  echo "exit 1"                                           >> ./update-asgsh
  echo "fi"                                               >> ./update-asgsh
  echo "echo"                                             >> ./update-asgsh
  echo "echo you are about to run the following command:" >> ./update-asgsh
  echo "echo"                                             >> ./update-asgsh
  echo "echo \"  $full_command \$@\""                     >> ./update-asgsh
  echo "echo"                                             >> ./update-asgsh
  echo "read -p \"proceed? [y] \" run"                    >> ./update-asgsh
  echo "if [[ -z "\$run" || \$run = \"y\" ]]; then"       >> ./update-asgsh
  echo "  cd $scriptdir"                                  >> ./update-asgsh 
  echo "  $full_command \$@"                              >> ./update-asgsh
  echo "else"                                             >> ./update-asgsh
  echo "  echo"                                           >> ./update-asgsh
  echo "  echo exiting ..."                               >> ./update-asgsh
  echo "fi"                                               >> ./update-asgsh  
  chmod 700 ./update-asgsh
  mv ./update-asgsh $HOME/bin
  $cmd
else
  echo command not run
fi
