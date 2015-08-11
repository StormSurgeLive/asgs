#!/bin/bash
DEBUG=$1
SWAN=$2
NETCDF=$3
NETCDF4=$4
NETCDF4_COMPRESSION=$5
XDMF=$6
SOURCEURL=$7
AUTOUPDATE=$8
EXEBASEPATH=$9
SCRIPTDIR=${10}
HPCENV=${11}
SYSLOG=${12}
#
echo "Executing the wrong script!"
#
# example of this script being called:
#../trunk/get_adcirc.sh full enable enable enable enable null https://adcirc.renci.org/svn/adcirc/branches/GAHM_jie off ~/asgs/temp ~/asgs/trunk desktop ~/asgs/temp/syslog.log
#
# Load up functions used for logging.
. ${SCRIPTDIR}/logging.sh 2>> $SYSLOG
# Grab the code configuration details for this HPC platform.
. ${SCRIPTDIR}/platforms.sh 2>> $SYSLOG
env_dispatch $HPCENV
# Set the name of the properties file that describes the executables that
# we're about to generate.
PROPERTIES=executables.properties
#
logMessage "Checking for suitable ADCIRC(+SWAN) executables."
#
# Check to see if we already have executables in a directory that
# matches the specification.
#
# Start by generating the path that is specified by this combination of
# parameters.  Assume the SOURCEURL is an http URL for an svn repository;
# extract the end of the path for use in naming the directory where the
# executables will be compiled.
EXEPATH=`basename $SOURCEURL`
#
# Add the first letter of each of the arguments to the path name 
EXEPATH="${EXEPATH}_D${DEBUG:0:1}S${SWAN:0:1}N${NETCDF:0:1}N4${NETCDF4:0:1}N4C${NETCDF4_COMPRESSION:0:1}X${XDMF:0:1}"
#
# Prepend the base executables path to the path we've constructed.
EXEPATH=$EXEBASEPATH/$EXEPATH
#
# Check for existence of executables.
EXEFOUND=t
for executable in adcirc padcirc padcswan adcprep hstime aswip; do
   if [[ ! -e $EXEPATH/work/$executable ]]; then
      EXEFOUND=f
   fi
done
#
# If we found all the executables, and we aren't supposed to try to 
# update and recompile them, then we're done.
if [[ $EXEFOUND = t && $AUTOUPDATE = off ]]; then
   logMessage "Existing ADCIRC(+SWAN) executables were successfully found."
   return $EXEPATH/work  # this is ADCIRCDIR
fi
#
logMessage "ADCIRC(+SWAN) executables were not found. They will be (re)built."
#
# Check the code out of svn and into the specified directory. 
# TODO: Deal with svn username/password.
if [[ ! -d $EXEPATH ]]; then
   mkdir -p $EXEPATH 2>> $SYSLOG
fi
cd $EXEPATH 2>> $SYSLOG
#
# Check the source code out of the repository. TODO: Enable other sources
# of source code, e.g., a tar.gz file on the local file system. 
logMessage "Retrieving source code."
# TODO: Figure out how/when to 'svn update' source code already in place if
# AUTOUPDATE is on.
svn checkout $SOURCEURL . >> build.log 2>> $SYSLOG
mv build.log $EXEPATH/work 2>> $SYSLOG
#
# Now build the ADCIRC and ADCIRC+SWAN executables.
cd $EXEPATH/work 2>> $SYSLOG
#
# Write properties file to record what this script is attempting to do
# and make it easy to look in the executables directory to see how the 
# code was compiled.
echo "DEBUG : $DEBUG" > $PROPERTIES
echo "SWAN : $SWAN" >> $PROPERTIES
echo "NETCDF : $NETCDF" >> $PROPERTIES
echo "NETCDF4 : $NETCDF4" >> $PROPERTIES
echo "NETCDF4_COMPRESSION : $NETCDF4_COMPRESSION" >> $PROPERTIES
echo "XDMF : $XDMF" >> $PROPERTIES
echo "SOURCEURL : $SOURCEURL" >> $PROPERTIES
echo "AUTOUPDATE : $AUTOUPDATE" >> $PROPERTIES
echo "EXEBASEPATH : $EXEBASEPATH" >> $PROPERTIES
echo "SCRIPTDIR : $SCRIPTDIR" >> $PROPERTIES
echo "HPCENV : $HPCENV" >> $PROPERTIES
echo "SYSLOG : $SYSLOG" >> $PROPERTIES
#
# Set the correct SWAN compiler flags for this HPC platform.
cp ../swan/$SWANMACROSINC ../swan/macros.inc 2>> $SYSLOG
#
# Build the executables using the settings listed in the platforms.sh file.
logMessage "Building executables."
for executable in adcirc padcirc padcswan adcprep hstime aswip; do 
   logMessage "Building ${executable}."
   MAKECMDLINE="make $executable $ADCOPTIONS DEBUG=$DEBUG SWAN=$SWAN NETCDF=$NETCDF NETCDF4=$NETCDF4 NETCDF4_COMPRESSION=$NETCDF4_COMPRESSION XDMF=$XDMF"
   echo "MAKECMDLINE is $MAKECMDLINE" >> build.log 2>> $SYSLOG
   $MAKECMDLINE >> build.log 2>&1
   if [[ $? == 0 ]]; then
      logMessage "Successfully built ${executable}."
   else
      warn "Failed to build ${executable}."
      return 1
   fi
done
# All executables were built successfully; return the ADCIRCDIR back to the 
# calling script.
return $EXEPATH/work
