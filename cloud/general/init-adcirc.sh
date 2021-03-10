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

# Note:
# If an operator wishes to use the ssh form of the git url, they will need
# to update the environmental variable ADCIRC_GIT_URL to use the url,
#    
#   export ADCIRC_GIT_URL=git@github.com:adcirc
#
# before running 'initadcirc' in asgsh
#

if [ "${1}" = "clean" ]; then
  echo "'clean' not implemented for optional ADCIRC/SWAN step at this time, clean up for ADCIRC and SWAN must be done manually."
  exit 0
fi

# preconditions
if [ -z "$ADCIRC_META_DIR" ]; then
  echo "ADCIRC_META_DIR is not set. Run interactively through asgsh or automatically via asgs-brew.pl."
  echo
  echo "Please note that this tool is meant to be run within an ASGS environment and is not supported "
  echo "as a general purpose too. Per the license of the code, however, anyone is free to adapt it.   "
  echo
  exit 1
fi

INTERACTIVE=
if [ -n "$_ASGSH_PID" ]; then
  # indicates further down to interactively guide user through (in asgsh env)
  # the building of ADCIRC, otherwise it does things automatically
  # assuming the required environment is provided through asgs-brew.pl

  INTERACTIVE=yes
fi

# Ask user for preferences, but offer defaults in the present in the asgsh environment
if [ "$INTERACTIVE" == "yes" ]; then
  echo NOTE: ASGS generally uses the "'v53release'" version of ADCIRC. 
  echo
  # get branch/tag/sha to checkout
  __ADCIRC_GIT_BRANCH=v53release # current preferred default
  read -p "What git branch (or tag, commit SHA) of the ADCIRC source do you wish to build? [$__ADCIRC_GIT_BRANCH] " _ADCIRC_GIT_BRANCH
  if [ -n "$_ADCIRC_GIT_BRANCH" ]; then
    # do not export, don't affect current environment after build
    ADCIRC_GIT_BRANCH=$_ADCIRC_GIT_BRANCH
  else
    ADCIRC_GIT_BRANCH=$__ADCIRC_GIT_BRANCH
  fi

  echo
  # determine what to name the ADCIRC profile
  __ADCIRC_PROFILE_NAME=$ADCIRC_GIT_BRANCH-$ADCIRC_COMPILER
  read -p "What would you like to name this ADCIRC build profile? [$__ADCIRC_PROFILE_NAME] " _ADCIRC_PROFILE_NAME
  if [ -n "$_ADCIRC_PROFILE_NAME" ]; then
    ADCIRC_PROFILE_NAME=$_ADCIRC_PROFILE_NAME
  else
    ADCIRC_PROFILE_NAME=$__ADCIRC_PROFILE_NAME
  fi
  echo

  # determine where to look for source directory or checkout git repo for the build
  if [ -e "$WORK" ]; then
    __ADCIRCBASE=$WORK/adcirc-cg-$ADCIRC_PROFILE_NAME
  else
    __ADCIRCBASE=$ASGS_HOME/adcirc-cg-$ADCIRC_PROFILE_NAME
  fi
  read -p "In what directory would you like to build ADCIRC? [$__ADCIRCBASE] " _ADCIRCBASE
  if [ -n "$_ADCIRCBASE" ]; then
    ADCIRCBASE=$_ADCIRCBASE
  else
    ADCIRCBASE=$__ADCIRCBASE
  fi
  echo
fi

# meant to be run under the asgs-brew.pl environment
# looks for:
# ASGS_MACHINE_NAME - passed to MACHINENAME of ADCIRC's Makefile, internally determines compiler flags and some library paths
# NETCDFHOME        - tells ADCIRC where to look for NetCDF libraries
# ADCIRCBASE        - main directory containing ADCIRC source code
# ADCIRC_COMPILER   - "intel" or "gfortran", set via --compiler in asgs-brew.pl
# ADCIRC_GIT_BRANCH - passed to `git checkout`, set via --adcirc-git-branch in asgs-brew.pl
# ADCIRC_GIT_URL    - git repo remote URL, set via --adcirc-git-remote in asgs-brew.pl
# ADCIRC_GIT_REPO   - git repository (likely 'adcirc-cg')

# first class ADCIRC related binaries
ADCIRC_BINS="padcirc adcirc adcprep hstime aswip"
ADCIRC_MAKE_CMD="make $ADCIRC_BINS compiler=${ADCIRC_COMPILER} NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable NETCDFHOME=${NETCDFHOME} NETCDFROOT=${NETCDFROOT} MACHINENAME=${ASGS_MACHINE_NAME}"

# for building coupled adcswan/padcswan (no netCDF)
ADCSWAN_BINS="adcswan padcswan"
ADCSWAN_MAKE_CMD="make $ADCSWAN_BINS SWAN=enable compiler=${ADCIRC_COMPILER} MACHINENAME=${ASGS_MACHINE_NAME}"

# SWAN related utilities other than adcswan/padcswan
SWAN_UTIL_BINS="unhcat.exe"
SWAN_UTIL_BINS_MAKE_CMD="make unhcat compiler=${ADCIRC_COMPILER} NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable NETCDFHOME=${NETCDFHOME} NETCDFROOT=${NETCDFROOT} MACHINENAME=${ASGS_MACHINE_NAME}"

if [ ! -d ${ADCIRCBASE} ]; then
  if [ "$INTERACTIVE" == "yes" ]; then
    _answer=yes
    read -p "Create directory, '$ADCIRCBASE'? [$_answer] " answer
    if [ -z "$answer" ]; then
      answer=$_answer
    fi
    if [ "$answer" != 'yes' ]; then
      echo 'no directory was created. Exiting install.'
      exit
    fi
    echo
  fi
  mkdir -p ${ADCIRCBASE} 2> /dev/null
else
  echo Found directory, ${ADCIRCBASE}
  echo
fi
if [ ! -d ${ADCIRCBASE}/.git ]; then
  if [ "$INTERACTIVE" == "yes" ]; then
    _answer=yes
    read -p "Download ADCIRC git repository from GitHub? [$_answer] " answer
    if [ -z "$answer" ]; then
      answer=$_answer
    fi
    if [ "$answer" != 'no' ]; then
      echo
      git clone ${ADCIRC_GIT_URL}/${ADCIRC_GIT_REPO}.git ${ADCIRCBASE}
    fi
    echo
  fi
else
  echo "$ADCIRCBASE appears to already contain a git repository."
fi

# current branch management is naive and assumes that the branch
# requested is available locally (via origin or any other manually
# added remotes (see, git remote --help for more information)
if [ ! -d "$ADCIRCBASE" ]; then
  echo "$ADCIRCBASE not found. Exiting install."
  exit 1
else
  cd ${ADCIRCBASE}
fi

# only try to checkout branch/tag/SHA if it's a git directory (looks for .git)

if [ -d "$ADCIRCBASE/.git" ]; then
  if [ "$INTERACTIVE" == "yes" ]; then
    echo
    read -p "Which ADCIRC branch would you like to checkout from GitHub ('.' to skip checkout)? [$ADCIRC_GIT_BRANCH] " repo
    if [ -z "$repo" ]; then
      repo=$ADCIRC_GIT_BRANCH
    fi
    do_checkout=yes
    if [ "$repo" == "." ]; then
      do_checkout=no
    fi
    echo
  fi
  if [ "$do_checkout" == "yes" ]; then
    CURRENT_BRANCH=$(git branch | egrep '^\*' | awk '{ print $2 }')
    if [ "${ADCIRC_GIT_BRANCH}" != "${CURRENT_BRANCH}" ]; then
      git checkout ${ADCIRC_GIT_BRANCH}
      EXIT=$?
      if [ $EXIT -gt 0 ]; then
        echo "error checking out git repository. Exiting ($EXIT)."
        exit $EXIT
      fi
      # check to make sure we're really on the desired branch
      CURRENT_BRANCH=$(git branch | egrep '^\*' | awk '{ print $2 }')
      if [ "${ADCIRC_GIT_BRANCH}" != "${CURRENT_BRANCH}" ]; then
        echo "git branch in $ADCIRCBASE isn't '$ADCIRC_GIT_BRANCH' (currently '$CURRENT_BRANCH')"
        exit 1
      fi
    fi
  fi
fi

ADCIRCDIR=${ADCIRCBASE}/work

# deal with SWAN coupling build based on supported ADCIRC branches (versions):
case "${ADCIRC_GIT_BRANCH}" in
  v53release|v53release-qbc|v54release)
    SWANDIR=${ADCIRCBASE}/swan
    ;;
  v55release|v55release-qbc)
    SWANDIR=${ADCIRCBASE}/thirdparty/swan
    ;;   
  *)
    echo Branch \"${ADCIRC_GIT_BRANCH}\" is not officially supported at this time. 
    exit 1
esac

# final check to make sure it looks like the expected ADCIRC source
if [ ! -d "$ADCIRCDIR" ]; then
  echo "$ADCIRCDIR is missing the './work' directory. Exiting install."
  exit 1
fi

if [ "$INTERACTIVE" = "yes" ]; then
  answer=
  echo
  echo "About to build ADCIRC in $ADCIRCDIR with the following command:"
  echo
  echo "cd $SWANDIR && \\"
  echo "   $SWAN_UTIL_BINS_MAKE_CMD && \\"
  echo "cd $ADCIRCDIR && \\"
  echo "   $ADCIRC_MAKE_CMD && \\"
  echo "   $ADCSWAN_MAKE_CMD"
  echo
  _answer=yes
  read -p "Proceed to build? [$_answer] " answer
  echo
  if [ -z "$answer" ]; then
    answer=$_answer
  fi
  if [ "$answer" != 'yes' ]; then
    echo "build stopped. Exiting."
    exit 1
  fi
fi

# attempt to build
cd $SWANDIR          && \
$SWAN_UTIL_BINS_MAKE_CMD && \
cd $ADCIRCDIR        && \
$ADCIRC_MAKE_CMD     && \
$ADCSWAN_MAKE_CMD

# catch failed exit status, for both interactive and initial asgs-brew.pl build
EXIT=$?
if [ $EXIT -gt 0 ]; then
  echo "build failed ($EXIT)"
  exit $EXIT
fi

# create directory to track ADCIRC installations
mkdir -p $ADCIRC_META_DIR 2> /dev/null

# set default if not set
ADCIRC_META_FILE=$ADCIRC_META_DIR/$ADCIRC_PROFILE_NAME
echo 'export ASGS_HOME='$ASGS_HOME                               >  $ADCIRC_META_FILE
echo 'export ASGS_MACHINE_NAME='$ASGS_MACHINE_NAME               >> $ADCIRC_META_FILE
echo 'export NETCDFHOME='$NETCDFHOME                             >> $ADCIRC_META_FILE
echo 'export ADCIRCBASE='$ADCIRCBASE                             >> $ADCIRC_META_FILE
echo 'export ADCIRCDIR='$ADCIRCDIR                               >> $ADCIRC_META_FILE
echo "export SWANDIR='$SWANDIR'"                                 >> $ADCIRC_META_FILE
echo 'export ADCIRC_COMPILER='$ADCIRC_COMPILER                   >> $ADCIRC_META_FILE
echo 'export ADCIRC_GIT_BRANCH='$ADCIRC_GIT_BRANCH               >> $ADCIRC_META_FILE
echo 'export ADCIRC_GIT_URL='$ADCIRC_GIT_URL                     >> $ADCIRC_META_FILE
echo 'export ADCIRC_GIT_REPO='$ADCIRC_GIT_REPO                   >> $ADCIRC_META_FILE
echo 'export ASGS_MAKEJOBS='$ASGS_MAKEJOBS                       >> $ADCIRC_META_FILE
echo "export ADCIRC_MAKE_CMD='$ADCIRC_MAKE_CMD'"                 >> $ADCIRC_META_FILE
echo "export SWAN_UTIL_BINS_MAKE_CMD='$SWAN_UTIL_BINS_MAKE_CMD'" >> $ADCIRC_META_FILE
echo "export ADCSWAN_MAKE_CMD='$ADCSWAN_MAKE_CMD'"               >> $ADCIRC_META_FILE
echo "export ADCIRC_PROFILE_NAME=$ADCIRC_PROFILE_NAME"           >> $ADCIRC_META_FILE
echo "export ADCIRC_BINS='$ADCIRC_BINS'"                         >> $ADCIRC_META_FILE
echo "export ADCSWAN_BINS='$ADCSWAN_BINS'"                       >> $ADCIRC_META_FILE
echo "export SWAN_UTIL_BINS='$SWAN_UTIL_BINS'"                   >> $ADCIRC_META_FILE

if [ "$INTERACTIVE" == "yes" ]; then
  echo
  echo ADCIRC has been build and the ADCIRC profile registered in asgsh.
  echo To load this profile, at the asgsh prompt, enter:
  echo
  echo   load adcirc $ADCIRC_PROFILE_NAME
  echo
  echo "Once loaded, save the current asgsh profile using the the 'save' command."
  echo
fi
