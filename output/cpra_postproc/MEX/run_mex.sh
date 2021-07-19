#!/bin/sh
# script for execution of deployed applications
#
# Sets up the MATLAB Runtime environment for the current $ARCH and executes 
# the specified command.
#--------------------------------------------------------------------------
#
THIS=$(basename -- $0)
# give usage statement and exit if there are no command line options 
if [[ $# -lt 1 ]]; then
  echo "$THIS: Usage:"
  echo "$0 MCRROOT MEXFILE"
  echo "$THIS: MCRROOT and MEXFILE should both be full paths."
  exit
fi
# set up Matlab environment
echo "$THIS: Setting up environment variables for Matlab MEX file."
MCRROOT="$1"
MEXFILE="$2"
echo ---
LD_LIBRARY_PATH=.:${MCRROOT}/runtime/glnxa64 ;
LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${MCRROOT}/bin/glnxa64 ;
LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${MCRROOT}/sys/os/glnxa64;
LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${MCRROOT}/sys/opengl/lib/glnxa64;
export LD_LIBRARY_PATH
echo "$THIS: LD_LIBRARY_PATH is '${LD_LIBRARY_PATH}'."
echo "$THIS: Finished setting up environment variables for Matlab MEX file. Executing '$MEXFILE'."
$MEXFILE
